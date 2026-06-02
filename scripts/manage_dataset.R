# =============================================================================
# Dataset Management and Merging
# =============================================================================
# Purpose : Combine extracted datasets into an analysis-ready format.
#           Covers data structure, GP availability filtering, first-date
#           derivation, and merging across diagnoses, medications, and
#           demographics.
#
# !! THIS SCRIPT IS AN EXAMPLE !!
# Adapt all steps to your study variables and design before running.
# Do not run unchanged.
#
# Inputs  : data/dataset.parquet             (from setup.R)
#           data/diagnosis_events.parquet    (from extract_diagnoses.R)
#           data/prescription_events.parquet (from extract_medications.R)
#
# Outputs : data/analysis_dataset.parquet
#
# Dependencies: dplyr, arrow, here, tidyr, magrittr, ukbAid
# =============================================================================
#
# Contents:
#   Step 1   Load extracted datasets
#   Step 2   Inspect and understand data structure
#   Step 3   Filter to participants with GP data
#   Step 4   Two approaches: all events vs. first event per condition
#   Step 5   Approach A — derive first diagnosis date from all events
#   Step 6   Approach B — use get_df() for automatic first-date derivation
#   Step 7   Merge datasets
#   Step 8   Inspect merged data
#   Step 9   Note on renaming and recoding
#   Step 10  Note on UKB built-in ICD-10 first occurrence fields
#   Step 11  Save
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

dataset          <- arrow::read_parquet(here::here("data/dataset.parquet"))
diagnosis_events <- arrow::read_parquet(here::here("data/diagnosis_events.parquet"))
prescriptions    <- arrow::read_parquet(here::here("data/prescription_events.parquet"))


# =============================================================================
# Step 2: Inspect and understand data structure
# =============================================================================
# Always inspect before merging. Joining changes row counts and can silently
# coerce column types — you need a baseline to compare against.
#
# What you have after extraction:
#
#   dataset          — one row per participant; your UKB phenotypic variables
#
#   diagnosis_events — LONG format: one row per diagnostic event.
#                      A participant with 5 AF records across GP and HES
#                      appears 5 times. Columns: eid, date, code, source.
#                      The source column shows whether the event came from GP
#                      or HES — important for source-specific analyses.
#
#   prescriptions    — LONG format: one row per prescription record.
#                      Columns: eid, date, drug_name, bnf_code, drug_class.

dplyr::glimpse(dataset)
dplyr::glimpse(diagnosis_events)
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
# If your analysis uses GP diagnoses or prescriptions, you must restrict to
# GP-linked participants before analysis. Including non-linked participants
# treats "no GP records" as "no disease", introducing systematic selection bias.
#
# UKB provides three count fields for GP linkage. Include these Field IDs in
# setup.R step 4 if you have not already done so:
#
#   p42040 — count of linked GP clinical event records  <- most useful
#   p42039 — count of linked GP prescription records
#   p42038 — count of linked GP registration records
#
# A participant has GP clinical data if p42040 > 0.
# !! EXAMPLE — adjust field name if you renamed it in your dataset !!

message("Participants with GP clinical records (p42040 > 0): ",
        sum(dataset$p42040 > 0, na.rm = TRUE), " of ", nrow(dataset))

# Filter to GP-linked participants:
dataset_gp <- dataset %>%
  dplyr::filter(p42040 > 0)

message("After GP filter: ", nrow(dataset_gp), " participants")

# Use dataset_gp for GP-based analyses.
# Use dataset (unfiltered) for HES-only analyses or when comparing linked
# vs non-linked groups.


# =============================================================================
# Step 4: Two approaches — all events vs. first event per condition
# =============================================================================
# The diagnosis_events data frame from extract_diagnoses.R contains ALL
# matching events for every participant. Two approaches depending on need:
#
# -----------------------------------------------------------------------
# Approach A — all events  (extract_diagnoses.R output, handled in Step 5)
# -----------------------------------------------------------------------
# What you get : one row per event — every GP and HES record that matched
#                your code list, with its date and source.
# Use when     : time-to-event analysis; event counting; studying patterns
#                over time; when you need all occurrence dates.
# How          : see Step 5 — derive first date with slice_min() if needed.
#
# -----------------------------------------------------------------------
# Approach B — first event per condition  (ukbrapR::get_df(), Step 6)
# -----------------------------------------------------------------------
# What you get : wide format — one row per participant, pre-computed first
#                dates and binary flags for each condition:
#
#   {condition}_gp_df   — first GP diagnosis date
#   {condition}_hes_df  — first HES diagnosis date
#   {condition}_df      — derived earliest date across all sources
#   {condition}_bin     — 1 if ever diagnosed, 0 if not
#   {condition}_bin_pre — 1 if diagnosed before baseline
#
# Use when     : you only need to know if/when a participant was first
#                diagnosed; multimorbidity research; prevalent/incident
#                case classification.
# Note         : get_df() also uses cancer registry and death records in
#                addition to GP and HES — more complete than GP + HES alone.


# =============================================================================
# Step 5: Approach A — derive first diagnosis date from all events
# =============================================================================
# From the long-format diagnosis_events, derive the first event per
# participant and condition.

# First event overall (earliest date, regardless of source):
first_diagnosis <- diagnosis_events %>%
  dplyr::group_by(eid, condition) %>%
  dplyr::slice_min(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# First event separately by source (GP and HES may give different first dates).
# Useful for sensitivity analyses and when you want source-specific dates.
first_by_source <- diagnosis_events %>%
  dplyr::group_by(eid, condition, source) %>%
  dplyr::slice_min(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# To check how often GP and HES first dates agree for a condition:
# first_by_source %>%
#   tidyr::pivot_wider(names_from = source, values_from = date) %>%
#   dplyr::filter(!is.na(GP) & !is.na(HES)) %>%
#   dplyr::mutate(agree = GP == HES) %>%
#   dplyr::count(agree)


# =============================================================================
# Step 6: Approach B — use get_df() for automatic first-date derivation
# =============================================================================
# If you have codes_df_combined (built from create_codes_df3 in the code
# lists guide), get_diagnoses() and get_df() derive first dates and binary
# flags automatically across all sources.
#
# Run instead of extract_diagnoses.R if you want wide-format output:

# diagnosis_list <- ukbrapR::get_diagnoses(codes_df_combined)
# diagnosis_df   <- ukbrapR::get_df(diagnosis_list, group_by = "condition")
#
# After get_df(), clean GP placeholder dates before any date-based analysis.
# UKB uses 1901-01-01, 1902-02-02, and 1903-03-03 to mean "unknown date"
# in GP records — they look like real dates and must be replaced with NA:

# diagnosis_df <- diagnosis_df %>%
#   dplyr::mutate(dplyr::across(
#     dplyr::ends_with("_gp_df"),
#     ~ dplyr::case_when(
#       . == as.Date("1901-01-01") ~ as.Date(NA),
#       . == as.Date("1902-02-02") ~ as.Date(NA),
#       . == as.Date("1903-03-03") ~ as.Date(NA),
#       TRUE ~ .
#     )
#   ))


# =============================================================================
# Step 7: Merge datasets
# =============================================================================
# Join diagnoses (first_diagnosis from Step 5, or diagnosis_df from Step 6)
# with prescriptions and demographics by eid.
#
# ALWAYS coerce eid to integer in every table before joining.
# A type mismatch (integer vs character) silently produces zero rows.

dataset         <- dataset         %>% dplyr::mutate(eid = as.integer(eid))
first_diagnosis <- first_diagnosis %>% dplyr::mutate(eid = as.integer(eid))
prescriptions   <- prescriptions   %>% dplyr::mutate(eid = as.integer(eid))

# -----------------------------------------------------------------------------
# 7.1  left_join — keep all participants (most common)
# -----------------------------------------------------------------------------
# Keeps every row from dataset. Participants with no events get NA.
# Use for cohort studies comparing cases and non-cases.

analysis_dataset <- dataset %>%
  dplyr::left_join(first_diagnosis, by = "eid") %>%
  dplyr::left_join(prescriptions,   by = "eid")

# -----------------------------------------------------------------------------
# 7.2  inner_join — keep only participants with events
# -----------------------------------------------------------------------------
# Drops participants with no matching event. Use for case-only analyses.

# analysis_dataset <- dataset %>%
#   dplyr::inner_join(first_diagnosis, by = "eid")

# -----------------------------------------------------------------------------
# 7.3  Count events, then left_join
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
# Step 8: Inspect merged data
# =============================================================================
# Always inspect after merging — joins change row counts and can coerce types.

dplyr::glimpse(analysis_dataset)

message("Rows after merge:    ", nrow(analysis_dataset))
message("Unique participants: ", length(unique(analysis_dataset$eid)))

n_dup <- sum(duplicated(analysis_dataset$eid))
if (n_dup > 0) {
  warning(n_dup, " duplicate eids after merge. ",
          "A one-to-many join likely introduced extra rows — check that ",
          "first_diagnosis has one row per participant before joining.")
} else {
  message("OK: no duplicate eids.")
}


# =============================================================================
# Step 9: Note on renaming and recoding
# =============================================================================
# After merging, column names are still p-coded (p31, p53_i0 etc.).
# Rename them before analysis so code is readable.
#
# See _ignore/manage_dataset.R for rename_variables() and recode_variables()
# function templates covering sex, ethnicity, smoking, physical activity,
# and age at baseline derivation.
#
# !! Always run glimpse() on the MERGED dataset after renaming — not just
# the inputs. Joining can silently coerce column types. !!
# Key types to verify: eid = integer, date columns = Date, factors = factor.


# =============================================================================
# Step 10: Note on UKB built-in ICD-10 first occurrence fields
# =============================================================================
# UKB provides pre-computed first-occurrence ICD-10 fields extracted from HES:
#
#   Field 41270 — all ICD-10 diagnoses across all episodes (repeated)
#   Field 41280 — date of each diagnosis in Field 41270 (index-matched)
#   Field 41202 — first occurrence ICD-10, main diagnosis position only
#   Field 41204 — first occurrence ICD-10, secondary diagnosis position
#
# YES, there is a date — Field 41280 gives the date for each entry in 41270.
#
# Extract these in setup.R step 4 by adding their Field IDs to your variable
# list. Data Showcase (fields, coding, dates):
#   https://biobank.ndph.ox.ac.uk/showcase/
#
# !! KEY LIMITATION: HES records only !!
# These fields contain NO GP data. For conditions commonly diagnosed in
# primary care (hypertension, depression, most chronic conditions), Field
# 41270 alone substantially undercounts cases. The approaches in Steps 5
# and 6 combine GP and HES, giving a more complete first diagnosis date.


# =============================================================================
# Step 11: Save
# =============================================================================
arrow::write_parquet(analysis_dataset,
                     here::here("data/analysis_dataset.parquet"))

ukbAid::rap_copy_to(
  local_path = here::here("data/analysis_dataset.parquet"),
  rap_path   = "/users/your_username/analysis_dataset.parquet"  # Replace
)

message("Saved: ", nrow(analysis_dataset), " participants | ",
        ncol(analysis_dataset), " variables.")
