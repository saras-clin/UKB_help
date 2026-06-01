# =============================================================================
# Extract Medication Prescriptions from UK Biobank
# =============================================================================
# Purpose : Filter the UK Biobank GP prescriptions file (gp_scripts.tsv or
#           gp_scripts.parquet) to a drug class of interest.
#
#           Two complementary strategies are available and explained below:
#
#             Approach 1 — BNF chapter filter
#               Filters on the structured bnf_code column. Fast and reliable
#               for rows where BNF codes are recorded — but approximately 23%
#               of rows in gp_scripts have no BNF code at all. Using BNF alone
#               silently drops a large fraction of valid prescriptions.
#
#             Approach 2 — Drug name regex
#               Matches free-text drug_name against a pattern. Captures what
#               BNF misses. Requires careful pattern design to avoid false
#               positives; always test on a sample first (Step 3 below).
#
#           Recommended: use BOTH together (Step 4). The queries are designed
#           to be mutually exclusive, so there is no double-counting.
#           This principle applies to any drug class, not just diabetes.
#
# Important: gp_scripts contains community (primary care) prescriptions only.
#            Medications dispensed during hospital stays or outpatient visits
#            are NOT captured here. For drugs commonly used in secondary care
#            (e.g. inpatient insulin, IV antibiotics), this file will
#            systematically undercount use in hospitalised patients.
#
# Inputs  : /mnt/project/ukbrapr_data/gp_scripts.tsv  (raw, ~57M rows)
#           or a pre-converted parquet version of the same file.
#
# Outputs : prescriptions  — data frame (eid, date, drug_name, bnf_code,
#                             drug_class)
#
# Dependencies: arrow, dplyr, stringr, readr, here, ukbAid
# =============================================================================


# =============================================================================
# Configuration
# =============================================================================
# Three values to set before running this script:
#
#   GP_SCRIPTS_PATH  — path to the prescriptions file on RAP.
#                      Usually correct as-is; verify if you get a file-not-found error.
#
#   BNF_PREFIX       — CHANGE to the BNF chapter for your drug class.
#
#   DRUG_PATTERN     — CHANGE to the drug names relevant to your study.
#
# Everything else in this script (Steps 1–5, Step 7) does not need editing.
# Step 6 (drug_class classification) needs to be updated to match your drug classes.

# Path to the GP prescriptions file.
# This is the standard RAP location set by ukbrapR — usually correct as-is.
# Verify the exact filename on your RAP instance if you get an error:
#   list.files("/mnt/project/ukbrapr_data/", pattern = "gp_scripts")
GP_SCRIPTS_PATH <- "/mnt/project/ukbrapr_data/gp_scripts.tsv"  # KEEP unless file is elsewhere

# BNF chapter prefix for Approach 1.
# The BNF (British National Formulary) organises drugs into numbered chapters.
# To find the right chapter for your drug:
#   1. Look up the drug on the NHS BNF website: https://bnf.nice.org.uk/
#      Each drug page shows its BNF number (e.g. "4.1.1" for a hypnotic).
#      OBS that this requires the user to be eligible. (See 3 if not eligible.)
#   2. The bnf_code column in gp_scripts uses a zero-padded 4-digit prefix
#      format: chapter "4" becomes "0401", chapter "2.12" becomes "0212".
#   3. Cross-reference with the OpenPrescribing BNF browser:
#      https://openprescribing.net/bnf/ — shows which drugs fall under each chapter
#      and their actual prescription volumes in NHS data.
#
# Common chapters:
#   "0601" — Drugs used in diabetes (OBS that Steno has an R-package (UKDC) to identify diabetes status)
#   "0205" — Drugs affecting the renin-angiotensin system (ACE inhibitors, ARBs)
#   "0206" — Diuretics
#   "0212" — Lipid-regulating drugs (statins, fibrates)
#   "0401" — Antidepressants
#   "0403" — Antipsychotics
#   "0801" — Cytotoxic drugs (cancer)
#   "1001" — Drugs used in rheumatic diseases (DMARDs, NSAIDs)
# Set to NULL to skip BNF filtering (not recommended: very slow on 57M rows).
BNF_PREFIX <- "0601"                                 # Change to your BNF chapter

# =============================================================================
# !! EXAMPLE ONLY — REPLACE WITH YOUR OWN DRUG NAMES !!
# The pattern below contains diabetes medications used as a worked example.
# You must replace these with the drugs relevant to YOUR study before running.
# =============================================================================
#
# How DRUG_PATTERN works:
#   paste() joins multiple strings into one. sep = "|" puts | between them,
#   which is the regex OR operator — any drug name in the list is a match.
#   Each line inside paste() is just a visual grouping; the result is always
#   one single combined pattern: "name1|name2|name3|..."
#   The match is case-insensitive (ignore_case = TRUE is set where it is used).
#
#   Include both generic names (e.g. "atorvastatin") and brand names
#   (e.g. "lipitor") — GP systems may record either.
#
# Example replacement for statins:
#   DRUG_PATTERN <- paste(
#     "simvastatin|zocor",
#     "atorvastatin|lipitor",
#     "rosuvastatin|crestor",
#     "pravastatin|lipostat",
#     sep = "|"
#   )
DRUG_PATTERN <- paste(
  "metformin|glucophage|bolamyn|forxiga",            # Metformin (generic + brand names)
  "insulin|lantus|novorapid|levemir|tresiba|humulin|actrapid|toujeo|basaglar",  # Insulins
  "gliclazide|glipizide|glibenclamide|diamicron|glimepiride|amaryl",            # Sulfonylureas
  "sitagliptin|januvia|linagliptin|trajenta|alogliptin|saxagliptin",            # DPP-4 inhibitors
  "pioglitazone|actos|rosiglitazone",                                           # Thiazolidinediones
  "empagliflozin|jardiance|dapagliflozin|canagliflozin|invokana",               # SGLT2 inhibitors
  "semaglutide|ozempic|rybelsus|liraglutide|victoza|saxenda|exenatide|byetta",  # GLP-1 agonists
  sep = "|"    # joins all lines with | into one combined regex pattern — do not change
)
# =============================================================================


# =============================================================================
# Step 1: Open the file lazily with Arrow
# =============================================================================
# open_delim_dataset() opens the file without loading any data into memory.
# All filters added below are pushed into the file scan by Arrow — only
# matching rows ever enter R. This is essential for a 57-million-row file.

ds <- arrow::open_delim_dataset(GP_SCRIPTS_PATH, delim = "\t")

message("File opened (lazy): ", GP_SCRIPTS_PATH)
message("Apply filters before calling collect() — see Approach 1 and 2 below.")


# =============================================================================
# Step 2: Approach 1 — BNF chapter filter
# =============================================================================
# Collect only rows where bnf_code starts with BNF_PREFIX.
# This is the fastest approach and covers all rows with valid BNF coding.

if (!is.null(BNF_PREFIX)) {

  message("Approach 1: BNF filter (bnf_code starts with '", BNF_PREFIX, "')...")

  bnf_rows <- ds |>
    dplyr::select(eid, issue_date, drug_name, bnf_code) |>
    # paste0("^", BNF_PREFIX) builds a pattern like "^0601".
    # The ^ means "must start with" — so "060112345" matches but a code
    # that happens to contain "0601" elsewhere would not.
    # Arrow pushes this filter into the file scan; only matching rows are read.
    dplyr::filter(grepl(paste0("^", BNF_PREFIX), bnf_code)) |>
    dplyr::collect()                                             # Only now does data enter R

  message("  BNF-confirmed rows: ", nrow(bnf_rows))

} else {
  bnf_rows <- dplyr::tibble(
    eid = integer(), issue_date = character(),
    drug_name = character(), bnf_code = character()
  )
}


# =============================================================================
# Step 3: Test your regex pattern on a sample BEFORE running on all 57M rows
# =============================================================================
# Always inspect pattern matches on a small sample first. A pattern that is
# too broad will include unrelated drugs; one that is too narrow will miss
# valid prescriptions. Both errors are silent.

message("Testing DRUG_PATTERN on a 1000-row sample...")

# Note: head(1000) returns the FIRST 1000 rows of the file — not a random
# sample. These rows may come from a small number of GP practices or a narrow
# date range, so naming conventions in the sample may not reflect the full
# dataset. Use the matched/unmatched output below as a quick sanity check,
# not as an exhaustive test of your pattern.
sample_names <- ds |>
  dplyr::select(drug_name) |>
  dplyr::filter(!is.na(drug_name)) |>
  head(1000) |>
  dplyr::collect() |>
  dplyr::pull(drug_name)

matched   <- sample_names[stringr::str_detect(
  sample_names, stringr::regex(DRUG_PATTERN, ignore_case = TRUE)
)]
unmatched <- sample_names[!stringr::str_detect(
  sample_names, stringr::regex(DRUG_PATTERN, ignore_case = TRUE)
)]

message("  Matched (", length(matched), " in sample): ", paste(head(matched, 10), collapse = "; "))
message("  Unmatched sample (first 10): ", paste(head(unmatched, 10), collapse = "; "))

# Inspect manually in the console before proceeding:
# View(data.frame(matched))
# View(data.frame(unmatched))


# =============================================================================
# Step 4: Approach 2 — Drug name regex on non-BNF rows
# =============================================================================
# Apply DRUG_PATTERN only to rows NOT captured by Approach 1.
# Two conditions are pushed into Arrow:
#   (a) bnf_code is missing, empty, or from a different chapter
#   (b) drug_name matches DRUG_PATTERN
#
# Together with Approach 1, this covers all 57M rows with no double-counting:
# every row is evaluated by exactly one query.

message("Approach 2: drug name regex (rows without BNF ", BNF_PREFIX, ")...")

drug_rows <- ds |>
  dplyr::select(eid, issue_date, drug_name, bnf_code) |>
  dplyr::filter(
    # Keep rows where bnf_code is missing/empty OR is a different chapter.
    # This is the complement of Approach 1: every row that BNF filtering
    # either missed (no code) or excluded (wrong chapter).
    is.na(bnf_code) | !grepl(paste0("^", BNF_PREFIX), bnf_code),
    # Of those rows, keep only ones where drug_name matches your pattern.
    grepl(DRUG_PATTERN, drug_name, ignore.case = TRUE)
  ) |>
  dplyr::collect()

message("  Drug-name rows (non-BNF): ", nrow(drug_rows))


# =============================================================================
# Step 5: Combine both approaches
# =============================================================================
# The two result sets are mutually exclusive by construction (different subsets
# of the 57M rows), so bind_rows() produces no duplicates.

prescriptions_raw <- dplyr::bind_rows(bnf_rows, drug_rows)

message("Total rows before date parsing: ", nrow(prescriptions_raw), " from ",
        length(unique(prescriptions_raw$eid)), " participants")


# =============================================================================
# Step 6: Parse dates and classify by drug class
# =============================================================================
# -----------------------------------------------------------------------
# KEEP AS IS (infrastructure — do not change):
#   eid  = as.integer(eid)              always needed for safe joins
#   date = as.Date(issue_date, ...)     always needed; UKB uses dd/mm/yyyy
#   filter(!is.na(date))                always needed; drops rows with bad dates
#   dplyr::select(...) at the end       keeps only the useful output columns
#
# CHANGE THIS (your study logic):
#   The drug_class column inside case_when().
#
# What case_when() does here:
#   This is exactly how you group individual drug names into a class.
#   Each block says: "if drug_name matches this pattern, call it this class."
#   The result is a new column (drug_class) with one label per prescription row.
#
#   For example, in diabetes research this produces three classes:
#     "insulin"    — all insulin products and brand names
#     "metformin"  — metformin and its branded versions
#     "other_oad"  — everything else (sulfonylureas, DPP-4s, SGLT2s, etc.)
#
#   The example below shows fine-grained classes (one per drug type).
#   If you want broader classes, assign multiple drug types to the same label.
#   Example — collapsing all non-insulin, non-metformin drugs into "other_oad":
#
#     drug_class = dplyr::case_when(
#       stringr::str_detect(drug_name, stringr::regex(
#         "insulin|lantus|novorapid|levemir|tresiba|humulin",
#         ignore_case = TRUE)) ~ "insulin",
#       stringr::str_detect(drug_name, stringr::regex(
#         "metformin|glucophage|bolamyn",
#         ignore_case = TRUE)) ~ "metformin",
#       TRUE ~ "other_oad"   # everything else that matched DRUG_PATTERN
#     )
#
# Rules for case_when():
#   - Conditions are evaluated in order; the FIRST match wins.
#     A drug matching both "metformin" and "other_oad" patterns gets
#     "metformin" if that block comes first — so put specific patterns
#     before broad ones.
#   - TRUE ~ "other_matched" at the end is the catch-all: any row that
#     passed DRUG_PATTERN but did not match any named block lands here.
#     Keep this line — it tells you how many rows need a new class block.
#   - If you only have one drug class, reduce to two lines:
#       one pattern block + TRUE ~ "your_class_name"
# -----------------------------------------------------------------------

prescriptions <- prescriptions_raw |>
  dplyr::mutate(
    eid  = as.integer(eid),                          # KEEP — integer for safe joins
    date = as.Date(issue_date, format = "%d/%m/%Y")  # KEEP — UKB raw date format (dd/mm/yyyy)
  ) |>
  dplyr::filter(!is.na(date)) |>                     # KEEP — drop rows with unparseable dates
  dplyr::mutate(
    # =========================================================================
    # !! EXAMPLE ONLY — REPLACE WITH YOUR OWN DRUG CLASSES !!
    # The blocks below classify diabetes drugs as an example.
    # Replace BOTH the pattern strings AND the labels after ~ with your drugs.
    # If drug classes are not relevant to your study, delete the entire
    # case_when() block and remove drug_class from the select() call below.
    # =========================================================================
    # Each str_detect block matches drug names → assigns a class label.
    # Use the same names as in DRUG_PATTERN. Order matters: first match wins.
    drug_class = dplyr::case_when(
      stringr::str_detect(drug_name, stringr::regex(
        "insulin|lantus|novorapid|levemir|tresiba|humulin|actrapid|toujeo|basaglar",
        ignore_case = TRUE)) ~ "insulin",            # all insulins → class "insulin"
      stringr::str_detect(drug_name, stringr::regex(
        "metformin|glucophage|bolamyn",
        ignore_case = TRUE)) ~ "metformin",          # metformin → class "metformin"
      stringr::str_detect(drug_name, stringr::regex(
        "gliclazide|glipizide|glibenclamide|diamicron|glimepiride|amaryl",
        ignore_case = TRUE)) ~ "sulfonylurea",       # sulfonylureas → class "sulfonylurea"
      stringr::str_detect(drug_name, stringr::regex(
        "sitagliptin|januvia|linagliptin|trajenta|alogliptin|saxagliptin",
        ignore_case = TRUE)) ~ "dpp4_inhibitor",     # DPP-4s → class "dpp4_inhibitor"
      stringr::str_detect(drug_name, stringr::regex(
        "empagliflozin|jardiance|dapagliflozin|forxiga|canagliflozin|invokana",
        ignore_case = TRUE)) ~ "sglt2_inhibitor",    # SGLT2s → class "sglt2_inhibitor"
      stringr::str_detect(drug_name, stringr::regex(
        "semaglutide|ozempic|rybelsus|liraglutide|victoza|saxenda|exenatide|byetta",
        ignore_case = TRUE)) ~ "glp1_agonist",       # GLP-1s → class "glp1_agonist"
      stringr::str_detect(drug_name, stringr::regex(
        "pioglitazone|actos|rosiglitazone",
        ignore_case = TRUE)) ~ "thiazolidinedione",  # TZDs → class "thiazolidinedione"
      TRUE ~ "other_matched"  # KEEP — matched DRUG_PATTERN but no class block above
    )
  ) |>
  dplyr::select(eid, date, drug_name, bnf_code, drug_class)  # KEEP

# Check the distribution of drug classes:
prescriptions |>
  dplyr::count(drug_class, sort = TRUE)

message("Prescriptions after date parsing: ", nrow(prescriptions), " from ",
        length(unique(prescriptions$eid)), " participants")


# =============================================================================
# Step 7: Save
# =============================================================================
arrow::write_parquet(prescriptions, here::here("data/prescription_events.parquet"))

ukbAid::rap_copy_to(
  local_path = here::here("data/prescription_events.parquet"),
  rap_path   = "/users/your_username/prescription_events.parquet"  # Replace path
)
