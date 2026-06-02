# =============================================================================
# RAP Session Setup
# =============================================================================
# Purpose : Standard setup code for UK Biobank RAP RStudio sessions.
#           Run this at the start of EVERY session.
#
# The RAP environment resets when a session ends — packages are NOT persistent
# and must be re-installed each time. Any data saved only locally (not copied
# to your RAP user folder) is also lost.
#
# This script covers:
#   1. Install packages                        (every session)
#   2. Connect project to GitHub               (every session)
#   3. Install remaining packages + libraries  (every session)
#   4. Create your dataset                     (once per project — then comment out)
#   5. Load dataset                            (every session after step 4)
#   6. Extract GP diagnoses and medications    (see extract_diagnoses.R / extract_medications.R)
#   7. Join datasets by eid                    (choose one of 7.1 / 7.2 / 7.3)
#   8. Save results                            (every session)
#
# See docs/getting-started-ukb.qmd for orientation and
#     docs/guide-to-functions.qmd for explanations of every function used here.
# =============================================================================


# =============================================================================
# 1. Install packages
# =============================================================================
# pak is a package manager that handles CRAN, GitHub, and Bioconductor packages.
# It is faster than install.packages() and resolves dependencies more reliably.
install.packages("pak")
install.packages("cli")
library(cli)                                         # Progress output used by pak

# parquetize: helper for converting large UKB TSV files to Parquet format.
pak::pak("ddotta/parquetize")

# ukbAid: project management and data extraction tools for the Steno RAP platform.
# Used for: extracting UKB variables by Field ID, moving files between the
# project folder and your personal RAP user storage.
pak::pak("steno-aarhus/ukbAid")


# =============================================================================
# 2. Connect your project to GitHub
# =============================================================================
# proj_setup_rap() reconnects your RAP session to your GitHub project repository.
# Run this EVERY session — the connection resets when a session ends.
ukbAid::proj_setup_rap()

# IMPORTANT: Open your RStudio project file (.Rproj)
# so that here::here() and all relative file paths point to the correct directory.


# =============================================================================
# 3. Install remaining packages and load libraries
# =============================================================================
# ukbrapR: wraps the RAP data access layer for linked clinical data.
# Used for: GP clinical record extraction, HES diagnosis extraction,
# file management, and project data handling.
pak::pak("lcpilling/ukbrapR")

# If your project has a DESCRIPTION file, this installs all listed packages:
pak::pak()

library(ukbrapR)    # Linked clinical data access (GP, HES)
library(dplyr)      # Data manipulation: filter, mutate, select, join, case_when
library(arrow)      # Parquet read/write; memory-safe streaming of large files
library(stringr)    # String matching: str_detect, str_subset, regex
library(readr)      # CSV reading: read_csv, read_tsv
library(here)       # File paths relative to project root (portable across machines)
library(magrittr)   # Pipe operator (%>%) — see guide-to-functions.qmd for explanation


# =============================================================================
# 4. Create your dataset (once per project — then comment out)
# =============================================================================
# This section extracts the UKB variables you need from the database and saves
# them as a parquet file. Run it ONCE when setting up your project. After that,
# load from the saved file (Section 5) and keep this section commented out.
#
# How long it takes: proj_create_dataset() typically runs for 5–60 minutes
# depending on how many fields you request and platform load. Do not close
# the session while it is running.
#
# ============================================================
# 4A. Choose the Field IDs you need
# ============================================================
# UK Biobank stores each variable under a numeric Field ID. You need to know
# which Field IDs you want before running the extraction.
#
# How to find Field IDs:
#
#   Option 1 (recommended): UK Biobank Data Showcase (online, no login needed)
#     Go to: https://biobank.ndph.ox.ac.uk/showcase/
#     Search by keyword (e.g. "sex", "date of birth", "HbA1c", "assessment").
#     The Field ID appears on each variable's page.
#
#   Option 2: RAP dataset browser (inside your RAP project)
#     In the RAP web interface, open your project and browse the dataset table.
#     The Field IDs are shown as column names or in the field list panel.
#
#   Option 3: ukbAid variable list
#     ukbAid::proj_read_variables() returns the fields approved for your project.
#
# Once you have your Field IDs, create a CSV file with two columns: "id" (the
# Field ID as shown in the RAP column name, e.g. p31) and "title" (a label
# for your own reference). Save it at: data-raw/rap-variables.csv
#
# Example content of rap-variables.csv:
#
# id,title
# eid,Participant ID
# p31,Sex
# p34,Year of birth
# p52,Month of birth
# p53_i0,Date of attending assessment centre | Instance 0
# p53_i1,Date of attending assessment centre | Instance 1
# p53_i2,Date of attending assessment centre | Instance 2
# p191,Date lost to follow-up
# p40000_i0,Date of death | Instance 0
# p42038,GP registration records
# p42039,GP prescription records
# p42040,GP clinical event records
#
# See docs/dataset-reference.qmd for a full description of each field.

# ============================================================
# 4B. Extract, move, and save the dataset
# ============================================================
# Uncomment this entire block and run it once.
# Replace "your_username" everywhere with your actual RAP username — the name
# of the folder you see under /mnt/project/users/ in the RAP file browser.

# library(ukbAid)
#
# # Step i: Generate a standard output file path inside the RAP project folder.
# dataset_rap_path <- ukbAid::proj_create_path_dataset()
#
# # Step ii: Read your Field ID list and extract those variables from the UKB
# # database. This writes a CSV to dataset_rap_path on RAP.
# # This step takes 5–60 minutes — do not close the session.
# readr::read_csv(here::here("data-raw/rap-variables.csv"),
#                 show_col_types = FALSE) |>
#   dplyr::pull(id) |>                              # Pull the "id" column as a vector of Field IDs
#   ukbAid::proj_create_dataset(
#     output_path = dataset_rap_path                # Write extracted data to this path on RAP
#   )
#
# # Step iii: Move the extracted CSV from the RAP project folder to your personal
# # user folder. Files in the user folder persist across sessions; files in the
# # project folder may be overwritten. str_subset() finds your folder by username.
# ukbAid::rap_move_file(
#   dataset_rap_path,
#   ukbAid::rap_get_path_users() |>
#     stringr::str_subset("your_username")          # Replace with your RAP username
# )
#
# # Step iv: Load the CSV from your user folder into R.
# dataset <- readr::read_csv(
#   "/mnt/project/users/your_username/dataset.csv", # Replace with your username
#   show_col_types = FALSE
# )
#
# # Step v: Convert to parquet and save locally.
# # Parquet is compressed, preserves column types (dates, integers), and loads
# # in seconds in future sessions — much faster than re-reading the CSV.
# arrow::write_parquet(dataset, here::here("data/dataset.parquet"))
#
# # Step vi: Copy the parquet file to your RAP user folder so it survives
# # session reset and is accessible in all future sessions.
# ukbAid::rap_copy_to(
#   local_path = here::here("data/dataset.parquet"),
#   rap_path   = "/users/your_username/dataset.parquet"  # Replace with your username
# )


# =============================================================================
# 5. Load dataset (every session after completing step 4)
# =============================================================================
# Once step 4 has been run once, always load from the saved parquet file.
# This takes seconds and preserves all column types exactly as saved.

dataset <- arrow::read_parquet(here::here("data/dataset.parquet"))

# You can now save your dataset directly (go to step 8), or extract additional
# data and join it with other datasets (steps 6 and 7) before saving.


# =============================================================================
# 6. Extract GP diagnoses and medications
# =============================================================================
# According to the ukbrapR documentation, export_tables() must be run ONCE per
# project before get_diagnoses() will return any data. It submits a job that
# copies ~10 GB of linked UKB data (GP clinical, HES, cancer registry, death
# records) to RAP persistent storage. You do not need to repeat it in future
# sessions — the exported files persist.
#
# NOTE FOR DISCUSSION: UKDC's extraction scripts call get_diagnoses() without
# first running export_tables() and return diagnoses correctly. It is unclear
# whether the linked tables are already present in this project's RAP storage
# from a prior step, or whether this step is truly required. Clarify with
# supervisors before advising others to run or skip it.

# ukbrapR::export_tables()   # <-- uncomment and run once if needed, then comment out again

# GP clinical records, HES diagnoses, and GP prescriptions are then extracted
# using dedicated scripts. Open the relevant script and follow the instructions
# at the top of the file:
#
#   scripts/extract_diagnoses.R    — GP clinical records (Read v2, CTV3) and
#                                    Hospital Episode Statistics (HES, ICD-10/ICD-9)
#                                    using your own diagnostic code list CSV.
#
#   scripts/extract_medications.R  — GP prescriptions (gp_scripts) filtered by
#                                    BNF chapter code and/or drug-name regex.
#                                    Includes path verification, Arrow lazy loading,
#                                    and a pattern test step before full extraction.


# =============================================================================
# 7. Join datasets by eid
# =============================================================================
# All UK Biobank datasets share a common participant identifier: "eid" (integer).
# Use eid to link your main dataset with diagnosis events and/or prescription
# events into a single analysis-ready data frame.
#
# ALWAYS coerce eid to integer on both sides before joining. A mismatch between
# integer and character eid silently produces zero rows with no error or warning.
#
# !! RUN ONLY ONE BLOCK below (7.1, 7.2, or 7.3) !!
# Running more than one will silently overwrite analysis_dataset each time.
# Choose based on your study question, then skip the other two entirely.
#
#   7.1  left_join  — keep ALL participants from your main dataset.
#                    Non-cases get NA in event columns.
#                    Use for: cohort studies where you need the full population
#                    (cases and non-cases) as the denominator.
#
#   7.2  inner_join — keep ONLY participants present in BOTH tables.
#                    Participants without a matching event are dropped entirely.
#                    Use for: analyses restricted to cases only.
#
#   7.3  Count + left_join — count events per participant, then join.
#                    Non-cases get 0 (not NA). One row per participant.
#                    Use for: when the outcome is event frequency (how many times)
#                    rather than event presence (yes/no).
#
# After running your chosen join, the result is your analysis dataset.
# --> Next obligatory step: Section 8 — save it before the session ends.

# Load the datasets to join (adjust filenames to match your saved output):
dataset          <- arrow::read_parquet(here::here("data/dataset.parquet"))
diagnosis_events <- arrow::read_parquet(here::here("data/diagnosis_events.parquet"))
prescriptions    <- arrow::read_parquet(here::here("data/prescription_events.parquet"))

# Coerce eid to integer in all datasets before any join:
dataset          <- dataset          |> dplyr::mutate(eid = as.integer(eid))
diagnosis_events <- diagnosis_events |> dplyr::mutate(eid = as.integer(eid))
prescriptions    <- prescriptions    |> dplyr::mutate(eid = as.integer(eid))

# -----------------------------------------------------------------------------
# 7.1  left_join — keep all participants (most common choice)
# -----------------------------------------------------------------------------
# Keeps every row from dataset. Participants with no matching event get NA in
# the event columns — they remain in the data as non-cases.
# Use when your analysis compares cases to non-cases across the full cohort.

analysis_dataset <- dataset |>
  dplyr::left_join(diagnosis_events, by = "eid") |>  # attach events; non-cases get NA
  dplyr::left_join(prescriptions,    by = "eid")     # attach prescriptions; same rule

# -----------------------------------------------------------------------------
# 7.2  inner_join — keep only participants with a matching event
# -----------------------------------------------------------------------------
# Drops any participant not present in the event table. Use only when your
# analysis is restricted to cases and you do not need the full population.

analysis_dataset <- dataset |>
  dplyr::inner_join(diagnosis_events, by = "eid")    # excludes participants with no events

# -----------------------------------------------------------------------------
# 7.3  Count events per participant, then left_join back to the main dataset
# -----------------------------------------------------------------------------
# Produces one row per participant with a column counting how many events they
# had. Participants with no events get 0 (not NA) after coalesce().
# Use when the outcome is event frequency rather than event presence (yes/no).

event_counts <- diagnosis_events |>
  dplyr::count(eid, name = "n_events") |>            # one row per eid; n_events = count
  dplyr::mutate(eid = as.integer(eid))               # coerce eid after count

analysis_dataset <- dataset |>
  dplyr::left_join(event_counts, by = "eid") |>      # attach counts; non-cases get NA
  dplyr::mutate(n_events = dplyr::coalesce(n_events, 0L))  # replace NA with 0 for non-cases


# =============================================================================
# 8. Save results and copy to RAP user folder
# =============================================================================
# Always save in two places:
#   1. Local project folder (here::here("data/...")) — available this session.
#   2. RAP user folder via ukbAid::rap_copy_to() — persists after session ends.
#
# Use parquet for all data frames: compressed, typed, and fast to reload.
#
# Where is your local project folder?
#   getwd()              — shows the current working directory as a full path
#   here::here()         — always returns the project root (where the .Rproj file is),
#                          regardless of which sub-folder your script is in
#   here::here("data")   — the "data" subfolder inside your project root
#
# If the folder does not yet exist, create it first:
#   dir.create(here::here("data"), showWarnings = FALSE)          # create data/
#   dir.create(here::here("data/results"), showWarnings = FALSE)  # create data/results/
# showWarnings = FALSE suppresses the warning if the folder already exists.

# Save locally:
arrow::write_parquet(analysis_dataset, here::here("data/analysis_dataset.parquet"))

# Copy to RAP user folder (replace "your_username"):
ukbAid::rap_copy_to(
  local_path = here::here("data/analysis_dataset.parquet"),
  rap_path   = "/users/your_username/analysis_dataset.parquet"  # Replace with your username
)

# Load again in a future session:
analysis_dataset <- arrow::read_parquet(here::here("data/analysis_dataset.parquet"))
