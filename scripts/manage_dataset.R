# =============================================================================
# Dataset Management and Merging
# =============================================================================
# Purpose : Combine extracted datasets into an analysis-ready format.
#           Covers data structure, GP availability filtering, first-date
#           derivation, merging, and renaming/recoding.
#
# !! THIS SCRIPT IS AN EXAMPLE !!
# Adapt all steps to your study variables and design before running.
# Do not run unchanged.
#
# Inputs  : data/dataset.parquet             (from setup.R)
#           data/diagnosis_events.parquet    (Approach A from extract_diagnoses.R)
#         OR data/diagnosis_df.parquet       (Approach B from extract_diagnoses.R)
#           data/prescription_events.parquet (from extract_medications.R)
#
# Outputs : data/analysis_dataset.parquet
#
# Dependencies: dplyr, arrow, here, tidyr, magrittr, ukbAid
# =============================================================================
#
# Contents:
#   Step 1  Load extracted datasets
#   Step 2  Inspect and understand data structure
#   Step 3  Filter to participants with GP data
#   Step 4  Derive first diagnosis date (Approach A — all events)
#   Step 5  Merge datasets
#   Step 6  Inspect merged data
#   Step 7  Rename and recode variables
#   Step 8  Note on UKB built-in ICD-10 first occurrence fields
#   Step 9  Save
# =============================================================================

library(dplyr)
library(arrow)
library(here)
library(tidyr)
library(magrittr)
library(ukbAid)


# =============================================================================
# Step 1: Load extracted datasets
# =============================================================================
# Load the output that matches the approach you used in extract_diagnoses.R:
#   Approach A (all events) → diagnosis_events.parquet
#   Approach B (get_df)     → diagnosis_df.parquet

dataset          <- arrow::read_parquet(here::here("data/dataset.parquet"))
diagnosis_events <- arrow::read_parquet(here::here("data/diagnosis_events.parquet"))
prescriptions    <- arrow::read_parquet(here::here("data/prescription_events.parquet"))

# If using Approach B, load diagnosis_df.parquet instead:
# diagnosis_df <- arrow::read_parquet(here::here("data/diagnosis_df.parquet"))


# =============================================================================
# Step 2: Inspect and understand data structure
# =============================================================================
# Always inspect before merging — joins change row counts and can silently
# coerce column types. You need a baseline to compare against.
#
# After extraction you have:
#
#   dataset          — one row per participant; UKB phenotypic variables
#
#   diagnosis_events — LONG format (Approach A): one row per diagnostic event.
#                      A participant with 5 AF records appears 5 times.
#                      The source column shows whether each event is GP or HES.
#
#   diagnosis_df     — WIDE format (Approach B): one row per participant, with
#                      pre-computed first dates and binary flags per condition.
#
#   prescriptions    — LONG format: one row per prescription record.

dplyr::glimpse(dataset)
dplyr::glimpse(diagnosis_events)   # or diagnosis_df if using Approach B
dplyr::glimpse(prescriptions)

message("dataset:          ", nrow(dataset), " rows | ",
        length(unique(dataset$eid)), " participants")
message("diagnosis_events: ", nrow(diagnosis_events), " rows | ",
        length(unique(diagnosis_events$eid)), " participants with events")
message("prescriptions:    ", nrow(prescriptions), " rows | ",
        length(unique(prescriptions$eid)), " participants with prescriptions")


# =============================================================================
# Step 3: Filter to participants with GP data
# =============================================================================
# Only about 45% of UKB participants have linked GP records. Participants
# without GP linkage have NO GP events in the extraction output — not because
# they had no diagnoses, but because their records were never linked to UKB.
#
# If your analysis uses GP diagnoses or GP prescriptions, restrict to
# GP-linked participants before analysis. Including non-linked participants
# treats "no GP records" as "no disease" — systematic selection bias.
#
# UKB provides three count fields for GP linkage. Include these Field IDs in
# setup.R step 4 if not already done:
#
#   p42040 — count of linked GP clinical event records  <- most useful
#   p42039 — count of linked GP prescription records
#   p42038 — count of linked GP registration records
#
# !! EXAMPLE — adjust field name if you renamed p42040 !!

message("Participants with GP clinical records (p42040 > 0): ",
        sum(dataset$p42040 > 0, na.rm = TRUE), " of ", nrow(dataset))

dataset_gp <- dataset %>%
  dplyr::filter(p42040 > 0)

message("After GP filter: ", nrow(dataset_gp), " participants")

# Use dataset_gp for GP-based analyses.
# Use dataset (unfiltered) for HES-only or comparison analyses.


# =============================================================================
# Step 4: Derive first diagnosis date  [Approach A only]
# =============================================================================
# Skip this step if you used Approach B (get_df()) — first dates and binary
# flags are already in diagnosis_df.
#
# From the long-format diagnosis_events, derive the first event per
# participant and condition.

# Earliest date across GP and HES combined:
first_diagnosis <- diagnosis_events %>%
  dplyr::group_by(eid, condition) %>%
  dplyr::slice_min(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# Earliest date separately per source — useful for sensitivity analyses.
# GP and HES may give different first dates for the same participant.
first_by_source <- diagnosis_events %>%
  dplyr::group_by(eid, condition, source) %>%
  dplyr::slice_min(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# To check how often GP and HES first dates agree:
# first_by_source %>%
#   tidyr::pivot_wider(names_from = source, values_from = date) %>%
#   dplyr::filter(!is.na(GP) & !is.na(HES)) %>%
#   dplyr::mutate(agree = GP == HES) %>%
#   dplyr::count(agree)


# =============================================================================
# Step 5: Merge datasets
# =============================================================================
# Join first_diagnosis (or diagnosis_df) with prescriptions and demographics.
#
# ALWAYS coerce eid to integer in every table before joining.
# A type mismatch (integer vs character) silently produces zero rows.

dataset         <- dataset         %>% dplyr::mutate(eid = as.integer(eid))
first_diagnosis <- first_diagnosis %>% dplyr::mutate(eid = as.integer(eid))
prescriptions   <- prescriptions   %>% dplyr::mutate(eid = as.integer(eid))

# -----------------------------------------------------------------------------
# 5.1  left_join — keep all participants (most common)
# -----------------------------------------------------------------------------
# Keeps every row from dataset. Participants with no events get NA.
# Use for cohort studies comparing cases and non-cases.

analysis_dataset <- dataset %>%
  dplyr::left_join(first_diagnosis, by = "eid") %>%
  dplyr::left_join(prescriptions,   by = "eid")

# -----------------------------------------------------------------------------
# 5.2  inner_join — keep only participants with events
# -----------------------------------------------------------------------------
# Drops participants with no matching event. Use for case-only analyses.

# analysis_dataset <- dataset %>%
#   dplyr::inner_join(first_diagnosis, by = "eid")

# -----------------------------------------------------------------------------
# 5.3  Count events, then left_join
# -----------------------------------------------------------------------------
# One row per participant with event count. Non-cases get 0, not NA.

# event_counts <- diagnosis_events %>%
#   dplyr::count(eid, condition, name = "n_events") %>%
#   dplyr::mutate(eid = as.integer(eid))
#
# analysis_dataset <- dataset %>%
#   dplyr::left_join(event_counts, by = "eid") %>%
#   dplyr::mutate(n_events = dplyr::coalesce(n_events, 0L))


# =============================================================================
# Step 6: Inspect merged data
# =============================================================================
# Always inspect after merging — joins change row counts and can coerce types.
# Compare to the row counts from Step 2.

dplyr::glimpse(analysis_dataset)

message("Rows after merge:    ", nrow(analysis_dataset))
message("Unique participants: ", length(unique(analysis_dataset$eid)))

n_dup <- sum(duplicated(analysis_dataset$eid))
if (n_dup > 0) {
  warning(n_dup, " duplicate eids after merge — a one-to-many join likely ",
          "introduced extra rows. Check that first_diagnosis has one row ",
          "per participant before joining.")
} else {
  message("OK: no duplicate eids.")
}


# =============================================================================
# Step 7: Rename and recode variables
# =============================================================================
# After merging, column names are still p-coded (p31, p53_i0 etc.).
# Rename and recode before analysis.
#
# !! EXAMPLE — replace Field IDs and labels with those in YOUR dataset !!
# Find Field IDs at: https://biobank.ndph.ox.ac.uk/showcase/
#
# !! Always run glimpse() on the MERGED dataset after renaming — not on the
# inputs alone. Joining can silently coerce column types. !!
# Key types to verify: eid = integer, date columns = Date, factors = factor.

# -----------------------------------------------------------------------------
# 7.1  Rename p-coded columns
# -----------------------------------------------------------------------------
# Only renames columns that actually exist — prevents errors if a field was
# not included in your extraction.

rename_variables <- function(data) {
  renames <- c(
    sex                     = "p31",
    year_birth              = "p34",
    month_birth             = "p52",
    date_baseline           = "p53_i0",
    date_lost_to_followup   = "p191",
    current_tobacco_smoking = "p1239_i0",
    ethnicity               = "p21000_i0",
    physical_activity       = "p22032_i0",
    gp_clinical_records     = "p42040"
  )
  renames_present <- renames[renames %in% names(data)]
  dplyr::rename(data, dplyr::all_of(renames_present))
}

analysis_dataset <- rename_variables(analysis_dataset)

# -----------------------------------------------------------------------------
# 7.2  Convert types
# -----------------------------------------------------------------------------
# Run Step 1 glimpse() first to see which conversions are actually needed.
# Do not convert a column that is already the right type.

analysis_dataset <- analysis_dataset %>%
  dplyr::mutate(
    eid                   = as.integer(eid),
    date_baseline         = as.Date(date_baseline),
    date_lost_to_followup = as.Date(date_lost_to_followup),
    year_birth            = as.integer(year_birth),
    month_birth           = as.integer(month_birth)
  )

# -----------------------------------------------------------------------------
# 7.3  Recode categoricals to labelled factors
# -----------------------------------------------------------------------------
# UKB stores categoricals as integers (e.g. sex: 0 = Female, 1 = Male).
# UKB uses negative codes for special responses:
#   -1 = Do not know,  -3 = Prefer not to answer
# Assign explicit labels rather than collapsing these into "Unknown".
#
# !! Check the Data Showcase for correct codes for YOUR variables !!

recode_variables <- function(data) {
  data %>%
    dplyr::mutate(

      # Field 31: Sex
      sex_cat = factor(
        dplyr::case_when(
          sex == 0   ~ "Female",
          sex == 1   ~ "Male",
          is.na(sex) ~ NA_character_,
          TRUE       ~ "Unknown"
        ),
        levels = c("Female", "Male", "Unknown")
      ),

      # Field 1239: Current tobacco smoking
      current_tobacco_smoking_cat = factor(
        dplyr::case_when(
          current_tobacco_smoking ==  0  ~ "No",
          current_tobacco_smoking ==  1  ~ "Yes",
          current_tobacco_smoking == -3  ~ "Prefer not to answer",
          is.na(current_tobacco_smoking) ~ NA_character_,
          TRUE                           ~ "Unknown"
        ),
        levels = c("No", "Yes", "Prefer not to answer", "Unknown")
      ),

      # Field 21000: Ethnicity
      # UKB uses hierarchical codes: parent (1 = White) and sub-codes
      # (1001 = British, 1002 = Irish) both appear — group them together.
      ethnicity_group = factor(
        dplyr::case_when(
          ethnicity %in% c(1, 1001, 1002, 1003)       ~ "White",
          ethnicity %in% c(2, 2001, 2002, 2003, 2004) ~ "Mixed",
          ethnicity %in% c(3, 3001, 3002, 3003, 3004) ~ "Asian",
          ethnicity %in% c(4, 4001, 4002, 4003)       ~ "Black",
          ethnicity %in% c(5, 5001)                   ~ "Chinese",
          ethnicity %in% c(6, 6001, 6002)             ~ "Other",
          is.na(ethnicity)                            ~ NA_character_,
          TRUE                                        ~ "Unknown / not coded"
        ),
        levels = c("White", "Mixed", "Asian", "Black", "Chinese",
                   "Other", "Unknown / not coded")
      )
    )
}

analysis_dataset <- recode_variables(analysis_dataset)

# -----------------------------------------------------------------------------
# 7.4  Derived variables
# -----------------------------------------------------------------------------
# UKB provides year and month of birth but not day (privacy). Use 15th as
# convention — introduces up to 15 days of error in age calculations.

analysis_dataset <- analysis_dataset %>%
  dplyr::mutate(
    date_birth   = as.Date(paste(year_birth,
                                 sprintf("%02d", month_birth),
                                 "15", sep = "-")),
    age_baseline = as.numeric(
      difftime(date_baseline, date_birth, units = "days")
    ) / 365.25
  )

message("Age at baseline: mean = ",
        round(mean(analysis_dataset$age_baseline, na.rm = TRUE), 1),
        " years (expected 40–69 for UKB baseline cohort)")


# =============================================================================
# Step 8: Note on UKB built-in ICD-10 first occurrence fields
# =============================================================================
# UKB provides pre-computed first-occurrence ICD-10 fields from HES:
#
#   Field 41270 — all ICD-10 diagnoses (all episodes, repeated)
#   Field 41280 — date of each entry in Field 41270 (index-matched; yes, there
#                 is a date)
#   Field 41202 — first occurrence ICD-10, main diagnosis position
#   Field 41204 — first occurrence ICD-10, secondary diagnosis position
#
# Data Showcase: https://biobank.ndph.ox.ac.uk/showcase/
#
# !! KEY LIMITATION: HES records only !!
# No GP data. For conditions commonly diagnosed in primary care (hypertension,
# depression, most chronic conditions), these fields substantially undercount
# cases compared to combining GP and HES.


# =============================================================================
# Step 9: Save
# =============================================================================
arrow::write_parquet(analysis_dataset,
                     here::here("data/analysis_dataset.parquet"))

ukbAid::rap_copy_to(
  local_path = here::here("data/analysis_dataset.parquet"),
  rap_path   = "/users/your_username/analysis_dataset.parquet"  # Replace
)

message("Saved: ", nrow(analysis_dataset), " participants | ",
        ncol(analysis_dataset), " variables.")
