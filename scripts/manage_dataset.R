# =============================================================================
# Dataset Management
# =============================================================================
# Purpose : Rename, recode, type-convert, and validate the UKB dataset after
#           loading from setup.R.
#
# !! THIS SCRIPT IS AN EXAMPLE !!
# UK Biobank studies differ widely in which variables are extracted, what
# recoding makes sense, and what quality checks are relevant. Every function
# and step below must be reviewed and adapted to your own study before use.
# Do not run it unchanged.
#
# What this script covers:
#   1. Inspect raw column names and types
#   2. Rename p-coded columns to human-readable names
#   3. Convert variable types (integers, dates, numerics)
#   4. Recode numeric codes to labelled factors
#   5. Create derived variables (e.g. age at baseline)
#   6. Data quality checks (duplicates, missingness, implausible values)
#   7. Save the cleaned dataset
#
# Inputs  : data/dataset.parquet   (from setup.R step 5)
# Outputs : data/dataset_clean.parquet
#
# Dependencies: dplyr, arrow, here, tidyr, ukbAid
# =============================================================================
#
# Contents:
#   Step 1  Inspect raw column names and types
#   Step 2  Rename variables
#   Step 3  Convert variable types
#   Step 4  Recode categorical variables to labelled factors
#   Step 5  Create derived variables
#   Step 6  Data quality checks
#   Step 7  Save clean dataset
# =============================================================================

library(dplyr)
library(arrow)
library(here)
library(tidyr)
library(ukbAid)

dataset <- arrow::read_parquet(here::here("data/dataset.parquet"))


# =============================================================================
# Step 1: Inspect raw column names and types
# =============================================================================
# Run this before making any changes. UKB columns are named with a p-prefix
# followed by the Field ID (e.g. p31 = Sex, p34 = Year of birth). Instances
# are indicated by _i0, _i1, etc. (e.g. p53_i0 = assessment date, visit 1).
#
# Use the UK Biobank Data Showcase to look up what any Field ID means:
#   https://biobank.ndph.ox.ac.uk/showcase/

dplyr::glimpse(dataset)    # column names, types, and a sample of values


# =============================================================================
# Step 2: Rename variables
# =============================================================================
# !! EXAMPLE — replace with the fields in YOUR dataset !!
# Renaming from p31, p34 etc. to descriptive names makes the rest of the
# script readable and reduces the risk of using the wrong column.
#
# Only columns that exist in your dataset need a rename entry.
# Find Field IDs at: https://biobank.ndph.ox.ac.uk/showcase/

rename_variables <- function(data) {
  renames <- c(
    sex                     = "p31",
    year_birth              = "p34",
    month_birth             = "p52",
    date_baseline           = "p53_i0",
    date_lost_to_followup   = "p191",
    current_tobacco_smoking = "p1239_i0",
    ethnicity               = "p21000_i0",
    physical_activity       = "p22032_i0"
  )
  # Only rename columns that actually exist — prevents errors when a field
  # was not included in your extraction:
  renames_present <- renames[renames %in% names(data)]
  dplyr::rename(data, dplyr::all_of(renames_present))
}

dataset <- rename_variables(dataset)


# =============================================================================
# Step 3: Convert variable types
# =============================================================================
# !! EXAMPLE — run Step 1 first (glimpse), then keep only the lines below
# that your data actually needs. Do not convert a column that is already
# the right type.

convert_variable_types <- function(data) {
  data |>
    dplyr::mutate(
      eid = as.integer(eid),               # always integer for joins

      # Dates: parquet usually preserves Date type, but verify:
      date_baseline         = as.Date(date_baseline),
      date_lost_to_followup = as.Date(date_lost_to_followup),

      # Year and month may load as numeric — convert to integer:
      year_birth  = as.integer(year_birth),
      month_birth = as.integer(month_birth)
    )
}

dataset <- convert_variable_types(dataset)


# =============================================================================
# Step 4: Recode categorical variables to labelled factors
# =============================================================================
# !! EXAMPLE — the codes and labels below are specific to the UKB Field IDs
# shown. Check the Data Showcase for correct codes for YOUR variables:
#   https://biobank.ndph.ox.ac.uk/showcase/ !!
#
# UKB stores categoricals as integers (e.g. 0 = Female, 1 = Male for sex).
# Converting to factor with labels makes tables and plots self-documenting
# and prevents accidental arithmetic on categorical data.
#
# Note: UKB uses negative codes for special responses:
#   -1 = Do not know
#   -3 = Prefer not to answer
# These are worth assigning explicit labels rather than collapsing into "Unknown".

recode_variables <- function(data) {
  data |>
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
      # -3 = Prefer not to answer is distinct from NA (not asked / not linked)
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
      # UKB uses a hierarchical code scheme: parent codes (e.g. 1 = White) and
      # sub-codes (e.g. 1001 = British, 1002 = Irish) both appear depending on
      # what the participant selected. Grouping both levels is standard practice.
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
        levels = c("White", "Mixed", "Asian", "Black", "Chinese", "Other",
                   "Unknown / not coded")
      ),

      # Field 22032: Physical activity (summary measure)
      physical_activity_cat = factor(
        dplyr::case_when(
          physical_activity == 0   ~ "Low",
          physical_activity == 1   ~ "Moderate",
          physical_activity == 2   ~ "High",
          is.na(physical_activity) ~ NA_character_,
          TRUE                     ~ "Unknown"
        ),
        levels = c("Low", "Moderate", "High", "Unknown")
      )
    )
}

dataset <- recode_variables(dataset)


# =============================================================================
# Step 5: Create derived variables
# =============================================================================
# !! EXAMPLE — assumes year_birth, month_birth, and date_baseline exist !!
# Derived variables are not extracted directly from UKB but calculated from
# the raw fields. Add or remove derivations based on your study needs.

create_derived_variables <- function(data) {
  data |>
    dplyr::mutate(

      # Approximate date of birth.
      # UKB provides year and month of birth but not day (for privacy reasons).
      # Using the 15th is a common convention — be aware this introduces up to
      # 15 days of error in any age calculation.
      date_birth = as.Date(
        paste(year_birth, sprintf("%02d", month_birth), "15", sep = "-")
      ),

      # Age at baseline assessment (decimal years)
      age_baseline = as.numeric(
        difftime(date_baseline, date_birth, units = "days")
      ) / 365.25,

      # Age group — example cut-points, adjust as needed:
      age_group = cut(
        age_baseline,
        breaks = c(39, 49, 59, 69, Inf),
        labels = c("40–49", "50–59", "60–69", "70+"),
        right  = TRUE
      )
    )
}

dataset <- create_derived_variables(dataset)


# =============================================================================
# Step 6: Data quality checks
# =============================================================================
# These checks catch problems that would otherwise produce silent errors or
# biased results. Run all of them and investigate any warnings before analysis.

# -----------------------------------------------------------------------
# 6.1  Duplicate eids
# -----------------------------------------------------------------------
# Each participant should appear exactly once. Duplicates usually mean a join
# earlier in setup.R introduced extra rows. One-to-many joins are the most
# common cause — check if any join table has multiple rows per eid.
n_dup <- sum(duplicated(dataset$eid))
if (n_dup > 0) {
  warning(n_dup, " duplicate eids found. ",
          "Each participant should appear once — check joins in setup.R.")
} else {
  message("OK: no duplicate eids (", nrow(dataset), " unique participants).")
}

# -----------------------------------------------------------------------
# 6.2  Missingness summary
# -----------------------------------------------------------------------
# Shows proportion of NA values per column. High missingness in core variables
# (sex, age, date_baseline) is unusual and worth investigating. High missingness
# in optional fields (e.g. physical activity) is common in UKB.
missingness <- dataset |>
  dplyr::summarise(dplyr::across(dplyr::everything(),
                                 ~ mean(is.na(.)))) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to  = "variable",
                      values_to = "prop_missing") |>
  dplyr::arrange(dplyr::desc(prop_missing))

message("Variables with >10% missing data:")
print(missingness |> dplyr::filter(prop_missing > 0.10))

# -----------------------------------------------------------------------
# 6.3  Implausible values — age at baseline
# -----------------------------------------------------------------------
# UKB recruited participants aged 40–69 at baseline (2006–2010).
# Values outside this range indicate a date or derivation error.
age_range <- range(dataset$age_baseline, na.rm = TRUE)
message("Age at baseline: mean = ", round(mean(dataset$age_baseline, na.rm = TRUE), 1),
        ", min = ", round(age_range[1], 1), ", max = ", round(age_range[2], 1))
if (age_range[1] < 35 || age_range[2] > 80) {
  warning("Age at baseline outside expected range (40–69). ",
          "Check year_birth and date_baseline.")
}


# =============================================================================
# Step 7: Save clean dataset
# =============================================================================
arrow::write_parquet(dataset, here::here("data/dataset_clean.parquet"))

ukbAid::rap_copy_to(
  local_path = here::here("data/dataset_clean.parquet"),
  rap_path   = "/users/your_username/dataset_clean.parquet"  # Replace with your username
)
