# =============================================================================
# Extract GP Clinical and HES Diagnoses from UK Biobank
# =============================================================================
# Purpose : Extract diagnostic events from TWO linked clinical sources:
#             1. GP clinical records — Read v2 and CTV3 codes from primary care
#             2. Hospital Episode Statistics (HES) — ICD-10 and ICD-9 codes
#                from inpatient hospital episodes
#           Both sources are queried in a single call to ukbrapR::get_diagnoses()
#           using a user-provided code list CSV.
#
#           Must be run in the RAP RStudio environment.
#
# PREREQUISITE (run once per project before this script will work):
#   ukbrapR must export the UKB linked tables to RAP persistent storage.
#   This is a one-time table-exporter job (~10 GB, takes several minutes).
#   Run in a fresh RAP RStudio session:
#
#     ukbrapR::export_tables()
#
#   This exports GP clinical, HES, cancer registry, and death records.
#   ukbrapR::get_diagnoses() will not find any data until this has been run.
#   You only need to do this once per RAP project.
#
# How data is accessed:
#   ukbrapR handles all file paths internally. It reads from the exported
#   tables in ~/ukbrapr_data/ on RAP (downloaded automatically if needed).
#   You do not need to know or set any file path for GP or HES data.
#
# Inputs  : A CSV code list with (at minimum) three columns:
#             condition  — short label for the condition (e.g. "hypertension")
#             vocab_id   — Read2 | CTV3 | ICD10 | ICD9  (no hyphens)
#             code       — the diagnostic code (uppercase, no trailing spaces)
#           See docs/code-lists-guide.qmd for the full 7-column schema and
#           guidance on building or sourcing code lists.
#
# Outputs : Two tidy data frames:
#             gp_events  — GP clinical events (eid, date, code, source = "GP")
#             hes_events — HES events (eid, date, diag_icd10, diag_icd9, source = "HES")
#           Both can be combined with dplyr::bind_rows() for downstream analysis.
#
# Dependencies: ukbrapR, dplyr, readr, arrow, here
# =============================================================================


# -----------------------------------------------------------------------------
# UKB placeholder dates
# -----------------------------------------------------------------------------
# UK Biobank GP records use three specific dates to encode "date unknown" or
# "date not recorded". They are not real clinical dates — they look like valid
# dates and will be silently treated as real events unless explicitly removed.
#
#   1901-01-01  — date unknown / not recorded
#   1902-02-02  — date recorded as before the participant's GP registration
#   1903-03-03  — date recorded as before study entry
#
# These must be replaced with NA before any date-based analysis (e.g. survival
# analysis, time-to-event, or classifying prevalent vs incident cases).
PLACEHOLDER_DATES <- as.Date(c("1901-01-01", "1902-02-02", "1903-03-03"))


# =============================================================================
# Step 1: Load your code list
# =============================================================================
# Provide the path to your CSV code list. The file must contain at least:
#   condition  — a short label (consistent across rows for the same condition)
#   vocab_id   — one of: Read2, CTV3, ICD10, ICD9
#   code       — the diagnostic code (uppercase, no trailing whitespace)
#
# For validated Read v2 and CTV3 code lists covering 212 conditions, see:
#   Prigge R et al. Commun Med (London). 2025;5:283.
#   Repository: https://github.com/rprigge-uoe/mltc-codelists

codes <- readr::read_csv(
  here::here("data-raw/my_condition_codes.csv"),     # Replace with your file path
  show_col_types = FALSE
)

# Normalise vocab_id: ukbrapR requires "ICD10" and "ICD9" without hyphens.
# If your CSV uses "ICD-10" or "ICD-9" (common in published code lists),
# this corrects them automatically. Read2 and CTV3 need no changes.
codes <- codes |>
  dplyr::mutate(
    vocab_id = dplyr::recode(vocab_id,
      "ICD-10" = "ICD10",                            # Remove hyphen for ukbrapR
      "ICD-9"  = "ICD9"                              # Remove hyphen for ukbrapR
    ),
    code = toupper(trimws(code))                      # Uppercase, strip whitespace
  )

# Guard: confirm required columns are present before querying.
required_cols <- c("condition", "vocab_id", "code")
missing_cols  <- setdiff(required_cols, names(codes))
if (length(missing_cols) > 0) {
  stop(
    "Code list is missing required columns: ", paste(missing_cols, collapse = ", "),
    "\nRequired: ", paste(required_cols, collapse = ", "),
    "\nFound:    ", paste(names(codes), collapse = ", "),
    "\nSee docs/code-lists-guide.qmd for the expected format."
  )
}

message("Code list loaded: ", nrow(codes), " codes across ",
        length(unique(codes$condition)), " condition(s): ",
        paste(unique(codes$condition), collapse = ", "))


# =============================================================================
# Step 2: Query UK Biobank records via ukbrapR
# =============================================================================
# ukbrapR::get_diagnoses() searches both GP clinical (Read v2, CTV3) and
# HES (ICD-10, ICD-9) for codes matching your list.
#
# It returns a named list with two elements:
#   $gp_clinical — GP records (columns include: eid, event_dt, read_2, read_3)
#   $hesin_diag  — HES records (columns include: eid, epistart, diag_icd10, diag_icd9)
#
# Note: linked GP data is available for approximately 45% of UKB participants.
# Coverage is unequal by country: England has the broadest coverage (~80% of
# English participants are linked); Scotland and Wales have partial coverage;
# Northern Ireland has very limited linkage. If your analysis compares across
# UK nations, this differential coverage is a potential source of bias.
# Participants without GP linkage will have no GP events — this is structural
# absence, not missing data. Use has_gp_data (from demographics) to distinguish
# "no GP linkage" from "GP-linked but no diagnosis".
#
# Important: get_diagnoses() returns ALL matching events across the follow-up
# period — potentially multiple rows per participant if the code appears more
# than once. This is intentional. If you need the first (earliest) event per
# participant (e.g. for incident case classification), extract it downstream:
#   gp_events |> dplyr::group_by(eid) |> dplyr::slice_min(date, n = 1)

message("Querying UK Biobank records for ", nrow(codes), " codes ",
        "(", length(unique(codes$vocab_id)), " vocabularies)...")

raw <- ukbrapR::get_diagnoses(codes)


# =============================================================================
# Step 3: Process GP clinical events
# =============================================================================
# ukbrapR returns GP records with event_dt as the date column.
# The code column name varies across ukbrapR versions; we detect it here.

gp_events <- NULL

if (!is.null(raw$gp_clinical) && nrow(raw$gp_clinical) > 0) {

  # Different ukbrapR versions name the GP code column differently:
  #   read_2      — Read v2 code (older versions)
  #   read_3      — CTV3 code (older versions)
  #   read_code   — combined code column (some versions)
  #   code        — generic name (newer versions)
  # intersect() returns only those names from our list that actually exist in
  # the returned data frame. [1] takes the first match found.
  code_col <- intersect(
    c("read_2", "read_3", "read_code", "code"),
    names(raw$gp_clinical)
  )[1]

  if (is.na(code_col)) {
    warning("Could not identify the code column in gp_clinical. ",
            "Columns present: ", paste(names(raw$gp_clinical), collapse = ", "))
    code_col <- NULL
  }

  gp_events <- raw$gp_clinical |>
    dplyr::mutate(
      eid    = as.integer(eid),                       # Coerce to integer for safe joins
      date   = as.Date(event_dt),                     # Rename event_dt → date for consistency
      # Replace UKB sentinel dates with NA.
      # dplyr::if_else() takes three arguments:
      #   1. condition  — is this date one of the three UKB placeholder values?
      #   2. if TRUE    — replace with NA, typed as Date (as.Date(NA), not just NA)
      #   3. if FALSE   — keep the original date unchanged
      #
      # Why as.Date(NA) and not plain NA?
      #   dplyr::if_else() requires both branches to be the same type. Plain NA
      #   is logical by default; as.Date(NA) is a Date-typed NA. Using plain NA
      #   here would cause a type mismatch error.
      #
      # Why dplyr::if_else() and not base ifelse()?
      #   Base ifelse() strips the Date class from the result and returns
      #   a numeric vector. dplyr::if_else() preserves the Date class.
      date   = dplyr::if_else(
        date %in% PLACEHOLDER_DATES, as.Date(NA), date
      ),
      source = "GP"
    ) |>
    dplyr::filter(!is.na(date))                       # Drop rows with no usable date

  # Rename the code column to a generic "code" for consistency
  if (!is.null(code_col) && code_col != "code") {
    gp_events <- gp_events |>
      dplyr::rename(code = dplyr::all_of(code_col))
  }

  gp_events <- gp_events |>
    dplyr::select(eid, date, code, source)

  message("GP events: ", nrow(gp_events), " rows from ",
          length(unique(gp_events$eid)), " participants")

} else {
  message("No GP clinical events returned.")
}


# =============================================================================
# Step 4: Process HES diagnosis events
# =============================================================================
# HES records use epistart (episode start date) as the date column.
# Both ICD-10 and ICD-9 columns are retained: HES switched from ICD-9 to
# ICD-10 in April 1995. Querying ICD-10 alone misses diagnoses before 1995.
#
# HES data quality note: records before ~1997 are less complete and coding
# consistency is lower. ICD-9 records in particular may be less reliable than
# ICD-10. If your study is restricted to events after 1997, ICD-9 is less
# critical — but including it avoids silently missing early cases.

hes_events <- NULL

if (!is.null(raw$hesin_diag) && nrow(raw$hesin_diag) > 0) {

  hes_events <- raw$hesin_diag |>
    dplyr::mutate(
      eid    = as.integer(eid),                       # Coerce to integer for safe joins
      date   = as.Date(epistart),                     # HES episode start date → renamed to date
      # Same placeholder-date replacement as GP above: UKB uses 1901-01-01,
      # 1902-02-02, and 1903-03-03 as sentinel values for unknown dates in HES
      # records. Replace with typed Date NA to avoid treating them as real events.
      date   = dplyr::if_else(
        date %in% PLACEHOLDER_DATES, as.Date(NA), date
      ),
      source = "HES"
    ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::select(eid, date, diag_icd10, diag_icd9, source)

  message("HES events: ", nrow(hes_events), " rows from ",
          length(unique(hes_events$eid)), " participants")

} else {
  message("No HES events returned.")
}


# =============================================================================
# Step 5: Combine and save
# =============================================================================
# GP and HES events have different columns (code vs diag_icd10/diag_icd9).
# bind_rows() fills missing columns with NA, which is correct here.

all_events <- dplyr::bind_rows(gp_events, hes_events)

message("Total events: ", nrow(all_events), " from ",
        length(unique(all_events$eid)), " participants")
message("Source breakdown: ",
        paste(names(table(all_events$source)),
              table(all_events$source), sep = " = ", collapse = " | "))

# Save locally and to RAP user folder (see setup.R for the save pattern)
arrow::write_parquet(all_events, here::here("data/diagnosis_events.parquet"))

ukbAid::rap_copy_to(
  local_path = here::here("data/diagnosis_events.parquet"),
  rap_path   = "/users/your_username/diagnosis_events.parquet"  # Replace path
)


# =============================================================================
# Step 6: Validate output
# =============================================================================
# Run these checks after saving to confirm the extraction produced clean,
# usable output. Any warning here should be investigated before analysis.

# Check 1: Column names and types
# Shows every column name and its R class. eid must be integer for safe joins;
# date must be Date. If either is wrong, the coercion in steps 3–4 silently
# failed and should be re-run before continuing.
message("Output columns and types:")
print(sapply(all_events, class))

expected_cols <- c("eid", "date", "source")
missing_cols  <- setdiff(expected_cols, names(all_events))
if (length(missing_cols) > 0) {
  warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}
if (!is.integer(all_events$eid)) {
  warning("eid is ", class(all_events$eid), ", not integer. ",
          "Re-run: all_events <- all_events |> dplyr::mutate(eid = as.integer(eid))")
}
if (!inherits(all_events$date, "Date")) {
  warning("date is ", class(all_events$date), ", not Date. ",
          "Check the as.Date() coercion in steps 3 and 4.")
}

# Check 2: No UKB placeholder dates survived cleaning
# (1901-01-01, 1902-02-02, 1903-03-03 should have been replaced with NA)
n_placeholder <- sum(all_events$date %in% PLACEHOLDER_DATES, na.rm = TRUE)
if (n_placeholder > 0) {
  warning(n_placeholder, " rows still contain UKB placeholder dates — ",
          "check the PLACEHOLDER_DATES definition at the top of this script.")
} else {
  message("OK: no placeholder dates in output.")
}

# Check 3: source column contains only "GP" and "HES"
unexpected_source <- setdiff(unique(all_events$source), c("GP", "HES"))
if (length(unexpected_source) > 0) {
  warning("Unexpected values in source column: ",
          paste(unexpected_source, collapse = ", "))
} else {
  message("OK: source values are GP and HES only.")
}

# Check 4: Date range sanity
# UKB GP records begin when participants registered with their practice
# (earliest ~1938); HES begins 1987. Events after today indicate a data error.
date_range <- range(all_events$date, na.rm = TRUE)
message("Date range: ", date_range[1], " to ", date_range[2])
if (date_range[1] < as.Date("1930-01-01")) {
  warning("Dates before 1930 found — possible remaining placeholder or ",
          "implausible dates in source data.")
}
if (date_range[2] > Sys.Date()) {
  warning("Future dates found — check for data entry errors in source data.")
}

# Check 5: Distribution of events per participant
# Very high counts may be expected for common or chronic conditions,
# but extreme values can indicate a join error or miscoded condition label.
event_freq <- table(all_events$eid)
message("Events per participant: median = ", median(event_freq),
        ", max = ", max(event_freq))
n_high <- sum(event_freq > 50)
if (n_high > 0) {
  message(n_high, " participants have >50 events — ",
          "check whether this is expected for your condition(s).")
}
