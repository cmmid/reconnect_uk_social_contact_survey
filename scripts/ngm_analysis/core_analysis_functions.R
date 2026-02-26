# New R script: Stratified NGM Analysis (Age-Ethnicity and Age-SES)

# --- Load Libraries ---
message("Loading required libraries...")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")
library(forcats)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)
if (!requireNamespace("qs", quietly = TRUE)) install.packages("qs")
library(qs)
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
library(scales)
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
library(viridis)
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)
if (!requireNamespace("ggh4x", quietly = TRUE)) install.packages("ggh4x")
library(ggh4x)

# Source local functions (if any beyond this script)
functions_script_path <- here::here("scripts", "analyses", "functions.R")
if (file.exists(functions_script_path)) {
    source(functions_script_path)
    message(paste("Sourced shared functions from:", functions_script_path))
} else {
    warning(paste("Shared functions script not found at:", functions_script_path))
}

# Prefer dplyr functions for clarity in some operations
select <- dplyr::select
filter <- dplyr::filter
summarise <- dplyr::summarise
group_by <- dplyr::group_by
mutate <- dplyr::mutate
arrange <- dplyr::arrange

# --- Utility Functions ---
capitalize_first <- function(x) {
    if (is.character(x) && length(x) > 0 && nchar(x) > 0) {
        s <- strsplit(x, " ")[[1]]
        # Capitalize first letter of each word, then rejoin if multiple words (e.g. "age_group" -> "Age_group")
        # However, for list indexing, we typically want just the first word capitalized e.g. "age" -> "Age"
        # The current use case is `target_var` which should be like "age", "ses", "ethnicity"
        # So, simple capitalization of the first letter of the string is sufficient.
        paste0(toupper(substring(x, 1, 1)), substring(x, 2))
    } else {
        x # Return as is if not a non-empty string or NULL
    }
}

# Efficient cross join for data.tables
# Creates a data.table containing the Cartesian product of the rows of the input data.tables.
CJ_dt <- function(...) {
    arg_list <- list(...)
    if (length(arg_list) == 0) {
        return(data.table())
    } # Return empty data.table if no arguments
    if (length(arg_list) == 1) {
        return(as.data.table(arg_list[[1]]))
    } # Return as data.table if only one argument

    # Use Reduce to iteratively cross-join pairs of data.tables
    Reduce(function(dt1, dt2) {
        dt1 <- as.data.table(dt1) # Ensure inputs are data.tables
        dt2 <- as.data.table(dt2)

        # If either data.table is empty, the cross product is empty but should have combined columns.
        if (nrow(dt1) == 0 || nrow(dt2) == 0) {
            # Create an empty data.table with all unique column names from both dt1 and dt2.
            # This attempts to preserve the structure for subsequent operations.
            all_names <- unique(c(names(dt1), names(dt2)))
            empty_dt <- setNames(
                data.table(matrix(nrow = 0, ncol = length(all_names))),
                all_names
            )
            # Attempt to set column types based on original tables if they had rows
            # This part is tricky and might need more robust type inference if used heavily with empty inputs
            for (col in all_names) {
                if (col %in% names(dt1) && nrow(dt1) > 0) {
                    empty_dt[, (col) := as.vector(vector(typeof(dt1[[col]]), 0))]
                } else if (col %in% names(dt2) && nrow(dt2) > 0) {
                    empty_dt[, (col) := as.vector(vector(typeof(dt2[[col]]), 0))]
                } else {
                    empty_dt[, (col) := character(0)]
                } # Default to character if type unknown
            }
            return(empty_dt)
        }

        # Standard cross-join logic for non-empty tables
        idx1 <- rep.int(seq_len(nrow(dt1)), nrow(dt2))
        idx2 <- rep(seq_len(nrow(dt2)), each = nrow(dt1))

        cbind(dt1[idx1, ], dt2[idx2, ])
    }, arg_list)
}

#' Reshape Flat Mean Contact Matrix to Array
#'
#' Converts a flat data.table of mean contacts (M_ij) into a
#' multi-dimensional array suitable for NGM calculations.
#'
#' @param flat_mean_contact_matrix_dt A data.table with columns for each
#'        participant stratum (e.g., part_age_group, part_ses), each contact
#'        stratum (e.g., cnt_age_group, cnt_ses), and a 'contact_rate' (M_ij).
#' @param stratification_vars A character vector of base stratification variable names
#'        (e.g., c("Age", "SES")).
#' @param all_strata_list A named list containing vectors of levels for each
#'        stratification variable (e.g., list(Age = FINAL_AGE_LEVELS, SES = FINAL_SES_LEVELS)).
#' @return A multi-dimensional array of contact rates, or NULL if dimensions are problematic.
#' @export
reshape_flat_matrix_to_array_stratified <- function(flat_mean_contact_matrix_dt,
                                                    stratification_vars,
                                                    all_strata_list) {
    if (!is.data.table(flat_mean_contact_matrix_dt)) {
        warning("Input flat_mean_contact_matrix_dt is not a data.table. Returning NULL array.")
        return(NULL)
    }
    if (nrow(flat_mean_contact_matrix_dt) > 0 && !"contact_rate" %in% names(flat_mean_contact_matrix_dt)) {
        stop("Column 'contact_rate' (M_ij) not found in non-empty flat_mean_contact_matrix_dt.")
    }

    # Construct dimension names and their levels for the array
    dim_names_list_part <- list()
    dim_lengths_part <- c()
    part_cols <- c()

    for (s_var in stratification_vars) {
        part_col_name <- paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        part_cols <- c(part_cols, part_col_name)
        current_levels <- all_strata_list[[s_var]]
        if (is.null(current_levels)) stop(paste("Levels for stratification variable", s_var, "not found in all_strata_list for participant dimensions."))
        dim_names_list_part[[part_col_name]] <- current_levels
        dim_lengths_part <- c(dim_lengths_part, length(current_levels))
    }

    dim_names_list_cnt <- list()
    dim_lengths_cnt <- c()
    cnt_cols <- c()

    for (s_var in stratification_vars) {
        cnt_col_name <- paste0("cnt_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        cnt_cols <- c(cnt_cols, cnt_col_name)
        current_levels <- all_strata_list[[s_var]]
        if (is.null(current_levels)) stop(paste("Levels for stratification variable", s_var, "not found in all_strata_list for contact dimensions."))
        dim_names_list_cnt[[cnt_col_name]] <- current_levels
        dim_lengths_cnt <- c(dim_lengths_cnt, length(current_levels))
    }

    # Final dimensions and dimnames for the array
    final_dim_lengths <- c(dim_lengths_part, dim_lengths_cnt)
    final_dim_names <- c(dim_names_list_part, dim_names_list_cnt)

    if (any(final_dim_lengths == 0)) {
        warning("One or more dimensions for the contact array have zero length. Returning NULL array.")
        return(NULL)
    }

    contact_array <- array(NA_real_, dim = final_dim_lengths, dimnames = final_dim_names)

    if (nrow(flat_mean_contact_matrix_dt) == 0) {
        message("Input flat_mean_contact_matrix_dt is empty. Returning an array with correct dimensions but no data.")
        return(contact_array)
    }

    dt_for_indexing <- copy(flat_mean_contact_matrix_dt)
    for (col_name_idx in c(part_cols, cnt_cols)) {
        if (col_name_idx %in% names(dt_for_indexing) && is.factor(dt_for_indexing[[col_name_idx]])) {
            dt_for_indexing[, (col_name_idx) := as.character(get(col_name_idx))]
        }
    }

    # Optimized population of the array using matrix indexing with match
    # Create matrix of character values from the data.table for all stratification columns
    participant_strata_char_matrix <- as.matrix(dt_for_indexing[, ..part_cols])
    contact_strata_char_matrix <- as.matrix(dt_for_indexing[, ..cnt_cols])

    # Pre-calculate match indices for all rows at once for participant dimensions
    participant_indices_matrix <- matrix(0, nrow = nrow(dt_for_indexing), ncol = length(part_cols))
    for (j in seq_along(part_cols)) {
        col_name <- part_cols[j]
        # Ensure dimname levels are not NULL before matching
        dim_levels <- final_dim_names[[col_name]] # This uses the name as in part_cols
        if (is.null(dim_levels)) stop(paste("Dimension levels for", col_name, "are NULL."))
        participant_indices_matrix[, j] <- match(participant_strata_char_matrix[, j], dim_levels)
    }

    # Pre-calculate match indices for all rows at once for contact dimensions
    contact_indices_matrix <- matrix(0, nrow = nrow(dt_for_indexing), ncol = length(cnt_cols))
    for (j in seq_along(cnt_cols)) {
        col_name <- cnt_cols[j]
        dim_levels <- final_dim_names[[col_name]] # This uses the name as in cnt_cols
        if (is.null(dim_levels)) stop(paste("Dimension levels for", col_name, "are NULL."))
        contact_indices_matrix[, j] <- match(contact_strata_char_matrix[, j], dim_levels)
    }

    # Combine indices: final_indices_matrix will have rows corresponding to dt_for_indexing rows
    # and columns corresponding to dimensions of contact_array
    final_indices_matrix <- cbind(participant_indices_matrix, contact_indices_matrix)

    # Check for any NAs in indices (meaning a stratum value wasn't found in dimnames)
    na_rows_mask <- apply(final_indices_matrix, 1, anyNA)
    if (any(na_rows_mask)) {
        warning(paste(sum(na_rows_mask), "row(s) in flat_mean_contact_matrix_dt had strata not matching array dimension names and were skipped during array population."))
        # Filter out rows with NA indices and corresponding contact_rate values
        final_indices_matrix <- final_indices_matrix[!na_rows_mask, , drop = FALSE]
        values_to_assign <- dt_for_indexing$contact_rate[!na_rows_mask]
    } else {
        values_to_assign <- dt_for_indexing$contact_rate
    }

    if (nrow(final_indices_matrix) > 0) {
        contact_array[final_indices_matrix] <- values_to_assign
    }

    return(contact_array)
}

# --- Global Definitions & Control Parameters ---
# Age breaks and labels
FINAL_AGE_LEVELS <- c(
    "Aged 4 years and under", "Aged 5 to 9 years", "Aged 10 to 15 years",
    "Aged 16 to 19 years", "Aged 20 to 24 years", "Aged 25 to 34 years",
    "Aged 35 to 49 years", "Aged 50 to 64 years", "Aged 65 to 74 years",
    "Aged 75 to 84 years", "Aged 85 years and over"
)
AGE_BREAKS <- c(0, 5, 10, 16, 20, 25, 35, 50, 65, 75, 85, 120) # Max age 120 as a proxy for Inf

# SES levels
FINAL_SES_LEVELS <- c("1", "2", "3", "4", "5", "6", "7", "Retired", "Student", "Under 17", "Unemployed", "Unknown")

# Ethnicity levels - Updated to match census data categories
FINAL_ETH_LEVELS <- c("White", "Asian", "Black", "Mixed/Other")

# Gender levels
FINAL_GENDER_LEVELS <- c("Male", "Female", "Unknown")

# Global list of all strata levels for easy access and consistency
ALL_STRATA_LEVELS_LIST <- list(
    Age = FINAL_AGE_LEVELS,
    SES = FINAL_SES_LEVELS,
    Ethnicity = FINAL_ETH_LEVELS,
    Gender = FINAL_GENDER_LEVELS
)

# Analysis control
PERFORM_BOOTSTRAP_ANALYSIS <- TRUE # Master switch for bootstrapping
N_BOOTSTRAP_ITERATIONS_GLOBAL <- 1000 # Default for all analyses unless overridden
OVERWRITE_MAIN_CACHE <- TRUE # Overwrite consolidated results for an analysis type
OVERWRITE_ITERATION_CACHE <- TRUE # Overwrite individual bootstrap iteration caches

# --- HELPER FUNCTION DEFINITIONS ---

#' Load and Preprocess Survey Data
#'
#' Loads the raw survey data, performs initial cleaning, age grouping,
#' factor level setting, and SES imputation for contacts.
#'
#' @param survey_rds_path Path to the raw survey data RDS file.
#' @param age_breaks Numeric vector for age categorization.
#' @param age_levels Character vector for age group labels.
#' @param ses_levels Character vector for SES group labels.
#' @param eth_levels Character vector for ethnicity group labels.
#' @param gender_levels Character vector for gender group labels.
#' @return A list containing two data.tables: 'participants' and 'contacts',
#'         pre-processed and with imputed contact SES.
load_and_preprocess_survey_data <- function(survey_rds_path,
                                            age_breaks, age_levels,
                                            ses_levels, eth_levels,
                                            gender_levels) {
    message("Loading and pre-processing raw survey data from: ", survey_rds_path)

    participants_dt <- NULL
    contacts_dt <- NULL

    # Support two sources:
    # 1) A single RDS file containing list(participants=..., contacts=...)
    # 2) A directory containing Zenodo CSVs in data/zenodo/ format
    if (dir.exists(survey_rds_path)) {
        # Zenodo CSV directory
        zenodo_dir <- survey_rds_path
        required_files <- c(
            "reconnect_participant_common.csv",
            "reconnect_participant_extra.csv",
            "reconnect_sday.csv",
            "reconnect_contact_common.csv",
            "reconnect_contact_extra.csv"
        )
        present <- file.exists(file.path(zenodo_dir, required_files))
        if (!all(present)) {
            missing_files <- paste(required_files[!present], collapse = ", ")
            stop(paste0("CRITICAL ERROR: Zenodo CSV directory missing required files: ", missing_files))
        }

        # Read using data.table::fread for speed and minimal deps
        part_common <- data.table::fread(file.path(zenodo_dir, "reconnect_participant_common.csv"), showProgress = FALSE)
        part_extra <- data.table::fread(file.path(zenodo_dir, "reconnect_participant_extra.csv"), showProgress = FALSE)
        part_sday <- data.table::fread(file.path(zenodo_dir, "reconnect_sday.csv"), showProgress = FALSE)
        participants_dt <- merge(part_common, part_extra, by = "part_id", all.x = TRUE, allow.cartesian = TRUE)
        participants_dt <- merge(participants_dt, part_sday, by = "part_id", all.x = TRUE, allow.cartesian = TRUE)

        cnt_common <- data.table::fread(file.path(zenodo_dir, "reconnect_contact_common.csv"), showProgress = FALSE)
        cnt_extra <- data.table::fread(file.path(zenodo_dir, "reconnect_contact_extra.csv"), showProgress = FALSE)
        contacts_dt <- merge(cnt_common, cnt_extra, by = c("cnt_id", "part_id"), all.x = TRUE, allow.cartesian = TRUE)

        # Normalise column names used downstream
        # Ensure numeric age columns exist if provided as character
        if ("part_age" %in% names(participants_dt)) participants_dt[, part_age := as.numeric(part_age)]
        if ("cnt_age_exact" %in% names(contacts_dt)) contacts_dt[, cnt_age_exact := as.numeric(cnt_age_exact)]

        # Ensure expected key columns exist
        required_part_cols <- c("part_id", "part_ethnicity", "part_ses", "part_gender")
        missing_part <- setdiff(required_part_cols, names(participants_dt))
        if (length(missing_part) > 0) stop(paste("Participants missing required columns:", paste(missing_part, collapse = ", ")))

        required_cnt_cols <- c("part_id", "cnt_ethnicity", "cnt_ses", "cnt_gender")
        missing_cnt <- setdiff(required_cnt_cols, names(contacts_dt))
        if (length(missing_cnt) > 0) stop(paste("Contacts missing required columns:", paste(missing_cnt, collapse = ", ")))
    } else if (file.exists(survey_rds_path)) {
        # RDS list format
        survey_data_raw <- readRDS(survey_rds_path)
        if (is.null(survey_data_raw) || !is.list(survey_data_raw) ||
            !all(c("participants", "contacts") %in% names(survey_data_raw))) {
            stop("CRITICAL ERROR: Survey data is not in the expected list format or missing key elements.")
        }
        participants_dt <- as.data.table(survey_data_raw$participants)
        contacts_dt <- as.data.table(survey_data_raw$contacts)
    } else {
        stop("CRITICAL ERROR: Provided survey data path does not exist: ", survey_rds_path)
    }
    initial_rows_participants <- nrow(participants_dt)
    initial_rows_contacts <- nrow(contacts_dt)

    # --- Add/Normalize Gender Columns ---
    if (!"part_gender" %in% names(participants_dt) && "part_sex" %in% names(participants_dt)) {
        participants_dt[, part_gender := part_sex]
    }
    if (!"cnt_gender" %in% names(contacts_dt) && "cnt_sex" %in% names(contacts_dt)) {
        contacts_dt[, cnt_gender := cnt_sex]
    }
    # Recode short codes to full labels expected downstream
    if ("part_gender" %in% names(participants_dt)) {
        participants_dt[, part_gender := fifelse(
            part_gender %in% c("M", "Male", "male", "Boy"), "Male",
            fifelse(
                part_gender %in% c("F", "Female", "female", "Girl"), "Female",
                fifelse(is.na(part_gender) | part_gender == "", "Unknown", as.character(part_gender))
            )
        )]
    }
    if ("cnt_gender" %in% names(contacts_dt)) {
        contacts_dt[, cnt_gender := fifelse(
            cnt_gender %in% c("M", "Male", "male", "Boy"), "Male",
            fifelse(
                cnt_gender %in% c("F", "Female", "female", "Girl"), "Female",
                fifelse(is.na(cnt_gender) | cnt_gender == "", "Unknown", as.character(cnt_gender))
            )
        )]
    }

    # --- Pre-process and Bin Ages ---
    # If pre-binned age groups exist (e.g. from Zenodo CSVs), keep values as-is for now.
    # Downstream, stratified_analyses may override to custom bands and will set factors.
    if (!"part_age_group" %in% names(participants_dt)) {
        if ("part_age" %in% names(participants_dt)) {
            participants_dt[, part_age_group := cut(part_age, breaks = age_breaks, labels = age_levels, right = FALSE, include.lowest = TRUE)]
        } else {
            stop("Participants data missing both 'part_age_group' and 'part_age'.")
        }
    } else {
        # ensure character to allow later re-leveling
        participants_dt[, part_age_group := as.character(part_age_group)]
    }

    # Provide cnt_age convenience if only exact age present
    if (!"cnt_age" %in% names(contacts_dt) && "cnt_age_exact" %in% names(contacts_dt)) {
        contacts_dt[, cnt_age := as.numeric(cnt_age_exact)]
    }

    if (!"cnt_age_group" %in% names(contacts_dt)) {
        if ("cnt_age_exact" %in% names(contacts_dt)) {
            contacts_dt[, cnt_age_group := cut(cnt_age_exact, breaks = age_breaks, labels = age_levels, right = FALSE, include.lowest = TRUE)]
        } else {
            stop("Contacts data missing both 'cnt_age_group' and 'cnt_age_exact'.")
        }
    } else {
        contacts_dt[, cnt_age_group := as.character(cnt_age_group)]
    }

    # --- Impute Missing Contact Data ---
    if ("fcn_impute" %in% ls(envir = .GlobalEnv)) {
        # Impute contact SES based on participant's SES
        message("  Imputing missing contact SES based on participant's SES...")
        contacts_dt_merged_ses <- merge(contacts_dt, participants_dt[, .(part_id, part_ses)], by = "part_id", all.x = TRUE)
        imputed_ses_data <- fcn_impute(
            data_input = contacts_dt_merged_ses,
            var = "cnt_ses",
            dependent_vars = "part_ses"
        )
        contacts_dt <- imputed_ses_data[, part_ses := NULL]

        # Impute contact Ethnicity based on participant's Ethnicity
        message("  Imputing missing contact Ethnicity based on participant's Ethnicity...")
        contacts_dt_merged_eth <- merge(contacts_dt, participants_dt[, .(part_id, part_ethnicity)], by = "part_id", all.x = TRUE)
        imputed_eth_data <- fcn_impute(
            data_input = contacts_dt_merged_eth,
            var = "cnt_ethnicity",
            dependent_vars = "part_ethnicity"
        )
        contacts_dt <- imputed_eth_data[, part_ethnicity := NULL]
    } else {
        message("Note: fcn_impute not found in global environment. Skipping contact imputation step.")
    }

    # --- Ensure Under-17s are classified as "Under 17" for SES ---
    # Align survey SES with census logic: anyone in age bands with lower bound < 16
    # should be placed in the special SES category "Under 17".
    # This prevents children from appearing in NS-SeC classes 1–7 in Age×SES analyses.
    under17_age_groups <- c("0-4", "5-9", "10-14", "15-19")
    if ("part_age_group" %in% names(participants_dt) && "part_ses" %in% names(participants_dt)) {
        participants_dt[as.character(part_age_group) %in% under17_age_groups, part_ses := "Under 17"]
    }
    if ("cnt_age_group" %in% names(contacts_dt) && "cnt_ses" %in% names(contacts_dt)) {
        contacts_dt[as.character(cnt_age_group) %in% under17_age_groups, cnt_ses := "Under 17"]
    }

    # --- Filter out "Unknown" or NA values AFTER imputation ---
    participants_dt <- participants_dt[!is.na(part_ethnicity) & part_ethnicity != "Unknown" &
        !is.na(part_gender) & part_gender != "Unknown" &
        !is.na(part_ses) & part_ses != "Unknown"]
    contacts_dt <- contacts_dt[!is.na(cnt_ethnicity) & cnt_ethnicity != "Unknown" &
        !is.na(cnt_gender) & cnt_gender != "Unknown" &
        !is.na(cnt_ses) & cnt_ses != "Unknown"]

    # --- Recode ethnicity to match census categories ---
    # Combine "Mixed" and "Other" into "Mixed/Other" to match census data
    message("  Recoding ethnicity categories to match census data...")
    participants_dt[part_ethnicity %in% c("Mixed", "Other"), part_ethnicity := "Mixed/Other"]
    contacts_dt[cnt_ethnicity %in% c("Mixed", "Other"), cnt_ethnicity := "Mixed/Other"]

    # --- Day-of-week column handling ---
    # Accept either 'day_week' (already binned weekday/weekend) or raw 'dayofweek' from Zenodo sday CSV
    if (!"day_week" %in% names(participants_dt)) {
        if ("dayofweek" %in% names(participants_dt)) {
            # Map raw weekday strings to weekday/weekend
            participants_dt[, day_week := fifelse(tolower(dayofweek) %in% c("saturday", "sunday"), "weekend", "weekday")]
            participants_dt[, day_week := factor(day_week, levels = c("weekday", "weekend"))]
        }
    } else {
        # Ensure expected factor levels if present
        participants_dt[, day_week := factor(as.character(day_week), levels = c("weekday", "weekend"))]
    }

    # Do NOT factorize to final levels here (age bands may change per analysis),
    # and do NOT drop rows yet. Downstream functions will factor and filter as needed.
    message(paste0("Survey data pre-processing complete. Participants: ", nrow(participants_dt), ", Contacts: ", nrow(contacts_dt)))

    # Note: Population weighting will be applied later in the pipeline when census data is available
    # This allows for flexible weighting based on different stratification configurations

    return(list(participants = participants_dt, contacts = contacts_dt))
}


#' Calculate Population Weights for Survey Data
#'
#' This function computes population weights by comparing survey sample proportions
#' to census population proportions for the given stratification variables.
#'
#' @param participants_dt data.table of survey participants
#' @param census_dt data.table of census population data
#' Simple day-of-week weighting function
#' @param participants_dt data.table with participant data including day_week column
#' @return data.table with part_id and population_weight columns
create_day_of_week_weights <- function(participants_dt) {
    if (!"day_week" %in% names(participants_dt)) {
        message("  No day_week column found, returning equal weights")
        return(participants_dt[, .(part_id, population_weight = 1.0)])
    }

    message("  Calculating day-of-week weights...")

    # Calculate sample proportions
    day_week_counts <- participants_dt[!is.na(day_week), .N, by = day_week]
    day_week_counts[, sample_prop := N / sum(N)]

    # True proportions (5 weekdays = 5/7, 2 weekend days = 2/7)
    true_props <- data.table(
        day_week = c("weekday", "weekend"),
        true_prop = c(5 / 7, 2 / 7) # 5 weekdays, 2 weekend days out of 7
    )

    # Calculate weights
    day_weights <- merge(day_week_counts, true_props, by = "day_week", all.x = TRUE)
    day_weights[, population_weight := true_prop / sample_prop]

    # Merge with participants
    weights_dt <- merge(
        participants_dt[, .(part_id, day_week)],
        day_weights[, .(day_week, population_weight)],
        by = "day_week",
        all.x = TRUE
    )

    # Handle missing values
    weights_dt[is.na(population_weight), population_weight := 1.0]

    message(sprintf(
        "  Day-of-week weights: weekday = %.3f, weekend = %.3f",
        day_weights[day_week == "weekday", population_weight],
        day_weights[day_week == "weekend", population_weight]
    ))

    return(weights_dt[, .(part_id, population_weight)])
}

#' @param stratification_vars character vector of variables to weight on
#' @return data.table with participant weights
calculate_population_weights <- function(participants_dt, census_dt, stratification_vars) {
    message("Calculating population weights for survey data...")

    # Ensure both inputs are data.tables
    participants_dt <- as.data.table(participants_dt)
    census_dt <- as.data.table(census_dt)

    # Map stratification variables to actual column names in both datasets
    survey_cols <- character()
    census_cols <- character()

    for (s_var in stratification_vars) {
        # Survey data uses part_ prefix and _group suffix for Age
        if (s_var == "Age") {
            survey_col <- "part_age_group"
            census_col <- "Age" # Processed census data uses standardized column names
        } else if (s_var == "Ethnicity") {
            survey_col <- "part_ethnicity"
            census_col <- "Ethnicity" # Processed census data uses standardized column names
        } else if (s_var == "SES") {
            survey_col <- "part_ses"
            census_col <- "SES"
        } else if (s_var == "Gender") {
            survey_col <- "part_gender"
            census_col <- "Gender"
        } else {
            stop(paste("Unknown stratification variable:", s_var))
        }

        survey_cols <- c(survey_cols, survey_col)
        census_cols <- c(census_cols, census_col)
    }

    # Calculate survey proportions
    survey_counts <- participants_dt[, .N, by = survey_cols]
    survey_counts[, survey_prop := N / sum(N)]

    # Calculate census proportions
    census_pop_col <- if ("Population" %in% names(census_dt)) "Population" else "population_count"
    census_proportions <- census_dt[, .(census_pop = sum(get(census_pop_col))), by = census_cols]
    census_proportions[, census_prop := census_pop / sum(census_pop)]

    # Rename columns to match for merge
    setnames(survey_counts, survey_cols, census_cols)

    # Merge and calculate weights
    weights_dt <- merge(survey_counts, census_proportions, by = census_cols, all.x = TRUE)
    weights_dt[, population_weight := census_prop / survey_prop]
    weights_dt[is.na(population_weight), population_weight := 1.0] # Default weight for missing matches

    # Rename back for participant lookup
    setnames(weights_dt, census_cols, survey_cols)

    # Create lookup table for participants
    participant_weights <- merge(participants_dt[, c("part_id", survey_cols), with = FALSE],
        weights_dt[, c(survey_cols, "population_weight"), with = FALSE],
        by = survey_cols, all.x = TRUE
    )
    participant_weights[is.na(population_weight), population_weight := 1.0]

    # Calculate day of week weights if day_week column is available
    if ("day_week" %in% names(participants_dt)) {
        message("Calculating and incorporating day of week weights...")

        # Calculate day of week proportions in sample
        day_week_counts <- participants_dt[!is.na(day_week), .N, by = day_week]
        day_week_counts[, sample_prop := N / sum(N)]

        # True proportions (5 weekdays, 2 weekend days out of 7)
        true_props <- data.table(
            day_week = factor(c("weekday", "weekend"), levels = levels(participants_dt$day_week)),
            true_prop = c(5 / 7, 2 / 7)
        )

        # Calculate day of week weights
        day_weights <- merge(day_week_counts, true_props, by = "day_week", all.x = TRUE)
        day_weights[, day_of_week_weight := true_prop / sample_prop]

        # Merge day of week weights with participant weights
        participant_day_weights <- merge(
            participants_dt[, .(part_id, day_week)],
            day_weights[, .(day_week, day_of_week_weight)],
            by = "day_week",
            all.x = TRUE
        )

        # Handle missing day_week values (default to 1.0)
        participant_day_weights[is.na(day_of_week_weight), day_of_week_weight := 1.0]

        # Merge with population weights and calculate combined weight
        participant_weights <- merge(participant_weights, participant_day_weights[, .(part_id, day_of_week_weight)], by = "part_id", all.x = TRUE)
        participant_weights[is.na(day_of_week_weight), day_of_week_weight := 1.0]
        participant_weights[, combined_weight := population_weight * day_of_week_weight]

        message(sprintf(
            "Combined weights calculated. Population weights range: %.3f to %.3f, Day of week weights range: %.3f to %.3f, Combined range: %.3f to %.3f",
            min(participant_weights$population_weight, na.rm = TRUE),
            max(participant_weights$population_weight, na.rm = TRUE),
            min(participant_weights$day_of_week_weight, na.rm = TRUE),
            max(participant_weights$day_of_week_weight, na.rm = TRUE),
            min(participant_weights$combined_weight, na.rm = TRUE),
            max(participant_weights$combined_weight, na.rm = TRUE)
        ))

        return(participant_weights[, .(part_id, population_weight = combined_weight)])
    } else {
        message(sprintf(
            "Population weights calculated. Range: %.3f to %.3f",
            min(weights_dt$population_weight, na.rm = TRUE),
            max(weights_dt$population_weight, na.rm = TRUE)
        ))

        return(participant_weights[, .(part_id, population_weight)])
    }
}

#' Load and Prepare Aggregated Census Data
#'
#' Loads pre-processed census data, renames columns for consistency,
#' and ensures demographic variables are factors with specified levels.
#'
#' @param census_qs_path Path to the pre-processed census data QS file.
#' @param age_levels Character vector for age group labels.
#' @param ses_levels Character vector for SES group labels.
#' @param eth_levels Character vector for ethnicity group labels.
#' @param gender_levels Character vector for gender group labels (optional).
#' @return A data.table with aggregated census population data, processed.
load_and_prepare_census_data <- function(census_qs_path, expected_vars, age_levels, ses_levels, eth_levels, gender_levels = NULL) { # Added gender_levels
    message("Loading and preparing aggregated census data from: ", census_qs_path)
    if (!file.exists(census_qs_path)) {
        stop("CRITICAL ERROR: Pre-processed census data file not found at ", census_qs_path)
    }
    census_data <- qs::qread(census_qs_path)
    setDT(census_data)
    current_names <- names(census_data)

    # Handle Age
    if ("age_category" %in% current_names) {
        if ("Age" %in% current_names && "age_category" != "Age") {
            message("Info: Both 'Age' and 'age_category' found in census data. Using 'age_category', renaming to 'Age'. Original 'Age' column will be removed.")
            census_data[, Age := NULL]
        }
        setnames(census_data, "age_category", "Age")
    } else if (!"Age" %in% current_names && "Age" %in% expected_vars) {
        message("Warning: Neither 'Age' nor 'age_category' found in census data. 'Age' column will be missing.")
    }

    # Handle Ethnicity
    if ("ethnic_group" %in% current_names) {
        if ("Ethnicity" %in% current_names && "ethnic_group" != "Ethnicity") {
            message("Info: Both 'Ethnicity' and 'ethnic_group' found in census data. Using 'ethnic_group', renaming to 'Ethnicity'. Original 'Ethnicity' column will be removed.")
            census_data[, Ethnicity := NULL]
        }
        setnames(census_data, "ethnic_group", "Ethnicity")
    } else if (!"Ethnicity" %in% current_names && "Ethnicity" %in% expected_vars) {
        message("Warning: Neither 'Ethnicity' nor 'ethnic_group' found in census data. 'Ethnicity' column will be missing.")
    }

    # Handle Population
    if ("population_count" %in% current_names) {
        if ("Population" %in% current_names && "population_count" != "Population") {
            message("Info: Both 'Population' and 'population_count' found in census data. Using 'population_count', renaming to 'Population'. Original 'Population' column will be removed.")
            census_data[, Population := NULL]
        }
        setnames(census_data, "population_count", "Population")
    } else if (!"Population" %in% current_names) {
        message("Warning: Neither 'Population' nor 'population_count' found in census data. 'Population' column will be missing.")
    }

    # Handle SES (assuming target is 'SES')
    if (!"SES" %in% names(census_data)) {
        if ("ses_group" %in% names(census_data)) { # Example alternative name
            message("Info: 'SES' not found, but 'ses_group' found. Renaming 'ses_group' to 'SES'.")
            setnames(census_data, "ses_group", "SES")
        } else if ("Part_ses" %in% names(census_data)) { # Another common alternative from other scripts
            message("Info: 'SES' not found, but 'Part_ses' found. Renaming 'Part_ses' to 'SES'.")
            setnames(census_data, "Part_ses", "SES")
        } else if ("SES" %in% expected_vars) {
            message("Warning: 'SES' or recognized alternatives not found in census data.") # SES might not be in all files
        }
    }

    # Handle Gender (assuming target is 'Gender')
    if (!"Gender" %in% names(census_data)) {
        if ("gender" %in% names(census_data)) { # check for lowercase 'gender'
            message("Info: 'Gender' (capitalized) not found, but 'gender' (lowercase) found. Renaming 'gender' to 'Gender'.")
            setnames(census_data, "gender", "Gender")
        } else if ("sex" %in% names(census_data)) { # Legacy compatibility: rename 'sex' to 'Gender'
            message("Info: Renaming legacy 'sex' column to 'Gender'.")
            setnames(census_data, "sex", "Gender")
        } else if ("Sex" %in% names(census_data)) { # Legacy compatibility: rename 'Sex' to 'Gender'
            message("Info: Renaming legacy 'Sex' column to 'Gender'.")
            setnames(census_data, "Sex", "Gender")
        } else if ("Gender" %in% expected_vars) {
            message("Warning: 'Gender', 'gender', 'Sex', or 'sex' not found in census data.") # Gender might not be in all files
        }
    }

    # Flexible check for present demographic columns and Population
    potential_demographic_cols <- c("Age", "SES", "Ethnicity", "Gender")
    present_demographic_cols <- intersect(potential_demographic_cols, names(census_data))

    # Population column is always essential
    if (!"Population" %in% names(census_data)) {
        stop(paste("CRITICAL ERROR: Census data missing 'Population' column from ", census_qs_path))
    }

    # Warn if any of the *expected-to-be-present* demographic columns (based on file contents)
    # are somehow lost during the renaming steps above. This check ensures that if a column
    # like 'age_category' was present but didn't become 'Age', we'd know.
    # This logic might be slightly off if a potential_demographic_col was *never* in current_names to begin with.
    # A simpler check: ensure all 'present_demographic_cols' (post renaming attempts) are still there.
    missing_after_rename_check <- setdiff(present_demographic_cols, names(census_data))
    if (length(missing_after_rename_check) > 0) {
        warning(paste("Census data from ", census_qs_path, " appears to have lost columns during renaming: ", paste(missing_after_rename_check, collapse = ", "), ". This is unexpected."))
    }

    # The rigid block that was here previously, which hardcoded expectations for Age, SES, Ethnicity, Population,
    # has been removed. The checks above are more flexible.

    # Final check for duplicates in the essential columns we care about
    # Determine essential columns based on what's ACTUALLY present plus Population
    essential_cols_for_dup_check <- unique(c(present_demographic_cols, "Population"))
    final_names <- names(census_data)
    if (anyDuplicated(final_names)) {
        dup_names <- unique(final_names[duplicated(final_names)])
        is_critical_dup <- any(dup_names %in% essential_cols_for_dup_check)
        warning_message <- paste0(
            "Census data has duplicate column names after processing: ", paste(dup_names, collapse = ", "),
            if (is_critical_dup) ". CRITICAL: Duplicates include essential columns for this dataset." else ". Duplicates are in other columns."
        )
        message(warning_message)
        if (is_critical_dup) {
            stop("Stopping due to critical duplicate columns in census data among those essential for this dataset.")
        }
    }

    # Factor conversion and filtering
    if ("Age" %in% names(census_data) && !is.null(age_levels)) {
        census_data[, Age := factor(Age, levels = age_levels, ordered = TRUE)]
    }
    if ("SES" %in% names(census_data) && !is.null(ses_levels)) {
        census_data[, SES := factor(SES, levels = ses_levels, ordered = TRUE)]
    }
    if ("Ethnicity" %in% names(census_data) && !is.null(eth_levels)) {
        census_data[, Ethnicity := factor(Ethnicity, levels = eth_levels, ordered = FALSE)]
    }
    if ("Gender" %in% names(census_data) && !is.null(gender_levels)) {
        census_data[, Gender := factor(Gender, levels = gender_levels, ordered = FALSE)]
    }

    initial_rows <- nrow(census_data)
    # Filter rows with NA in any of the *actually present* demographic columns or if Population is NA or <=0
    na_filter_cols <- intersect(present_demographic_cols, names(census_data))

    filter_conditions <- list()
    if (length(na_filter_cols) > 0) {
        filter_conditions <- c(filter_conditions, lapply(na_filter_cols, function(col) !is.na(census_data[[col]])))
    }
    # Always filter on Population presence and value
    if ("Population" %in% names(census_data)) {
        filter_conditions <- c(filter_conditions, list(!is.na(census_data[["Population"]]), census_data[["Population"]] > 0))
    } else {
        # This case should have been caught by the Population check above, but as a safeguard:
        stop(paste("Internal error: Population column disappeared before NA filtering in", census_qs_path))
    }

    if (length(filter_conditions) > 0) {
        final_filter_condition <- Reduce(`&`, filter_conditions)
        census_data <- census_data[final_filter_condition]
    } # If no conditions (e.g. no demographic cols and somehow population also missing, though that's an error), do nothing here.

    if (nrow(census_data) < initial_rows) {
        message(paste("  Filtered", initial_rows - nrow(census_data), "rows from census data due to NA in key demographics or Population <= 0."))
    }
    message("Census data prepared. Rows: ", nrow(census_data))
    return(census_data)
}

#' Summarize Bootstrap Results
#'
#' Combines a list of data.tables from bootstrap iterations and calculates
#' summary statistics (mean, median, 95% CI) for a specified value column,
#' grouped by specified demographic columns.
#'
#' @param dt_list A list of data.tables, where each table is a bootstrap iteration result.
#' @param value_col_name The name of the column containing the values to summarize.
#' @param group_cols A character vector of column names to group by (e.g., c("Age", "Ethnicity")).
#' @param all_levels_list A named list containing the factor levels for each grouping column.
#'        E.g., list(Age = FINAL_AGE_LEVELS, Ethnicity = FINAL_ETH_LEVELS).
#' @return A data.table with summary statistics for the value_col_name.
summarize_bootstrap_results <- function(dt_list, value_col_name, group_cols, all_levels_list) {
    if (length(dt_list) == 0 || all(sapply(dt_list, function(dt) is.null(dt) || nrow(dt) == 0))) {
        warning(paste("Bootstrap list for", value_col_name, "is empty or all elements are empty. Returning empty summary."))
        # Construct an empty DT with expected columns
        empty_summary_cols <- c(group_cols, paste0(value_col_name, c("_mean", "_median", "_lowerCI", "_upperCI")))
        empty_dt <- data.table(matrix(ncol = length(empty_summary_cols), nrow = 0))
        setnames(empty_dt, empty_summary_cols)
        for (col in group_cols) {
            if (col %in% names(all_levels_list)) {
                empty_dt[, (col) := factor(levels = all_levels_list[[col]], ordered = TRUE)]
            } else {
                empty_dt[, (col) := character()] # Default for non-specified group cols
            }
        }
        for (stat_col in grep(paste0("^", value_col_name, "_"), names(empty_dt), value = TRUE)) {
            empty_dt[, (stat_col) := numeric()]
        }
        return(empty_dt)
    }

    # Combine all data.tables, adding an iteration identifier
    combined_dt <- rbindlist(lapply(seq_along(dt_list), function(i) {
        dt <- dt_list[[i]]
        if (!is.null(dt) && nrow(dt) > 0 && value_col_name %in% names(dt)) {
            # Ensure grouping columns are factors with globally defined levels
            for (g_col in group_cols) {
                if (g_col %in% names(dt) && g_col %in% names(all_levels_list)) {
                    dt[, (g_col) := factor(get(g_col), levels = all_levels_list[[g_col]], ordered = TRUE)]
                }
            }
            dt[, iter := i]
            return(dt)
        }
        return(NULL) # Skip NULL or empty data.tables or those missing the value column
    }), fill = TRUE, use.names = TRUE)

    if (nrow(combined_dt) == 0 || !value_col_name %in% names(combined_dt)) {
        warning(paste("Combined bootstrap data for", value_col_name, "is empty or value column not found. Returning empty summary."))
        # Similar empty DT construction as above
        empty_summary_cols <- c(group_cols, paste0(value_col_name, c("_mean", "_median", "_lowerCI", "_upperCI")))
        empty_dt <- data.table(matrix(ncol = length(empty_summary_cols), nrow = 0))
        setnames(empty_dt, empty_summary_cols)
        for (col in group_cols) {
            if (col %in% names(all_levels_list)) {
                empty_dt[, (col) := factor(levels = all_levels_list[[col]], ordered = TRUE)]
            } else {
                empty_dt[, (col) := character()]
            }
        }
        for (stat_col in grep(paste0("^", value_col_name, "_"), names(empty_dt), value = TRUE)) {
            empty_dt[, (stat_col) := numeric()]
        }
        return(empty_dt)
    }

    # Ensure grouping columns are factors before summarization
    for (g_col in group_cols) {
        if (g_col %in% names(combined_dt) && g_col %in% names(all_levels_list)) {
            combined_dt[, (g_col) := factor(get(g_col), levels = all_levels_list[[g_col]], ordered = TRUE)]
        }
    }

    # Calculate summary statistics
    summary_dt <- combined_dt[!is.na(get(value_col_name)), .(
        Mean_Value = mean(get(value_col_name), na.rm = TRUE),
        Median_Value = median(get(value_col_name), na.rm = TRUE),
        LowerCI = quantile(get(value_col_name), probs = 0.025, na.rm = TRUE),
        UpperCI = quantile(get(value_col_name), probs = 0.975, na.rm = TRUE)
    ), by = c(group_cols)] # Group by the specified columns

    # Rename summary stat columns
    stat_names <- c("Mean_Value", "Median_Value", "LowerCI", "UpperCI")
    new_stat_names <- paste0(value_col_name, c("_mean", "_median", "_lowerCI", "_upperCI"))
    setnames(summary_dt, old = stat_names, new = new_stat_names)

    # Ensure all combinations of grouping factors are present (CJ for specified group_cols)
    # Create a list suitable for do.call(CJ, ...)
    cj_args <- lapply(group_cols, function(col_name) {
        if (col_name %in% names(all_levels_list)) {
            return(factor(all_levels_list[[col_name]], levels = all_levels_list[[col_name]], ordered = TRUE))
        } else {
            # If levels not provided for a group_col, try to get unique values from summary_dt or combined_dt
            # This case should be rare if all_levels_list is comprehensive for primary strata
            unique_vals <- unique(combined_dt[[col_name]])
            warning(paste("Levels for group_col '", col_name, "' not in all_levels_list. Using unique values from data for CJ. This might lead to incomplete summaries if not all combinations appeared in data."))
            return(factor(unique_vals, levels = unique_vals))
        }
    })
    names(cj_args) <- group_cols

    if (length(cj_args) > 0) {
        all_combinations_summary <- do.call(CJ, cj_args)
        setkeyv(all_combinations_summary, group_cols)
        setkeyv(summary_dt, group_cols) # Ensure summary_dt is keyed by the same columns

        summary_dt_final <- merge(all_combinations_summary, summary_dt, by = group_cols, all.x = TRUE)

        # Fill NA stat values with 0 (or NA if preferred, 0 is consistent with previous script)
        for (col in new_stat_names) {
            if (col %in% names(summary_dt_final)) {
                summary_dt_final[is.na(get(col)), (col) := 0]
            }
        }
    } else { # No group_cols specified, or cj_args was empty
        summary_dt_final <- summary_dt
    }

    message(paste("Summarized bootstrap results for:", value_col_name, ". Output rows:", nrow(summary_dt_final)))
    return(summary_dt_final)
}

#' Aggregate Census Population Data by Specified Stratification Variables
#'
#' Takes the full census data.table and aggregates population counts based on
#' the provided stratification variables.
#'
#' @param full_census_dt A data.table with census data, expected to have columns
#'        'Age', 'SES', 'Ethnicity', and 'Population'. Demographic columns should be factors.
#' @param stratification_vars A character vector specifying the columns to group by
#'        for aggregation (e.g., c('Age', 'Ethnicity') or c('Age')).
#' @param all_strata_levels A named list of levels for the stratification variables.
#' @return A data.table with aggregated population counts, grouped by the
#'         `stratification_vars`.
aggregate_census_population <- function(full_census_dt, stratification_vars, all_strata_levels) {
    if (!is.data.table(full_census_dt)) stop("full_census_dt must be a data.table.")
    if (!all(c(stratification_vars, "Population") %in% names(full_census_dt))) {
        stop("One or more specified stratification_vars or 'Population' not in full_census_dt names.")
    }

    message(paste0("Aggregating census population by: ", paste(stratification_vars, collapse = ", ")))

    # Ensure stratification_vars are present and are factors
    for (s_var in stratification_vars) {
        if (!s_var %in% names(full_census_dt)) {
            stop(paste("Stratification variable '", s_var, "' not found in census data."))
        }
        if (!is.factor(full_census_dt[[s_var]])) {
            warning(paste("Stratification variable '", s_var, "' in census data is not a factor. Attempting to convert."))
            # Attempt to factorize based on the provided levels list
            if (s_var %in% names(all_strata_levels)) {
                full_census_dt[, (s_var) := factor(get(s_var), levels = all_strata_levels[[s_var]], ordered = (s_var %in% c("Age", "SES")))]
            } else {
                full_census_dt[, (s_var) := as.factor(get(s_var))]
            }
        }
    }

    aggregated_census_dt <- full_census_dt[, .(Population = sum(Population, na.rm = TRUE)), by = c(stratification_vars)]

    # Ensure factor levels are preserved after aggregation for the grouping variables
    for (s_var in stratification_vars) {
        if (s_var %in% names(all_strata_levels)) {
            aggregated_census_dt[, (s_var) := factor(get(s_var), levels = all_strata_levels[[s_var]], ordered = (s_var %in% c("Age", "SES")))]
        }
        # For any other stratification_vars not in the list, their levels will be from the data itself.
    }

    # Filter out any groups that sum to zero population after aggregation (e.g. if all constituents were zero)
    original_rows <- nrow(aggregated_census_dt)
    aggregated_census_dt <- aggregated_census_dt[Population > 0]
    if (nrow(aggregated_census_dt) < original_rows) {
        message(paste("  Filtered", original_rows - nrow(aggregated_census_dt), "strata with zero total population after aggregation."))
    }

    message(paste0("  Aggregated census data has ", nrow(aggregated_census_dt), " strata."))
    return(aggregated_census_dt)
}

#' Prepare Survey Data Columns for a Specific Stratification
#'
#' Selects relevant participant and contact columns based on stratification_vars
#' and ensures they are correctly factored using global level definitions.
#'
#' @param participants_dt Full pre-processed participant data.table.
#' @param contacts_dt Full pre-processed contact data.table.
#' @param stratification_vars Character vector of base variable names to stratify by
#'        (e.g., c("Age", "Ethnicity")).
#' @param all_strata_levels A named list containing the factor levels for each of the
#'        `stratification_vars`.
#' @return A list containing two data.tables:
#'         - `participants_strat_cols`: Participants with `part_id` and selected, factored stratification columns.
#'         - `contacts_strat_cols`: Contacts with `part_id`, `unique_contact_id`, and selected, factored stratification columns.
get_survey_columns_for_stratification <- function(participants_dt, contacts_dt, stratification_vars, all_strata_levels) {
    message(paste0("Preparing survey data columns for stratification: ", paste(stratification_vars, collapse = ", ")))

    # --- Participant data preparation ---
    part_cols_to_select <- c("part_id")
    for (s_var in stratification_vars) {
        part_col_name <- paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        if (!part_col_name %in% names(participants_dt)) {
            stop(paste("Participant column", part_col_name, "derived from", s_var, "not found."))
        }
        part_cols_to_select <- c(part_cols_to_select, part_col_name)
    }
    actual_part_cols_to_select <- intersect(part_cols_to_select, names(participants_dt))

    participants_strat_cols <- participants_dt[, ..actual_part_cols_to_select]

    # Ensure factors for participants
    for (s_var in stratification_vars) {
        col_name <- paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        current_levels <- all_strata_levels[[s_var]]
        if (is.null(current_levels)) {
            stop(paste("Levels for stratification var '", s_var, "' not found in all_strata_levels list. Check config."))
        }
        participants_strat_cols[, (col_name) := factor(get(col_name), levels = current_levels, ordered = (s_var %in% c("Age", "SES")))] # Only Age/SES typically ordered
    }
    # Filter out participants with NA in any of the stratification columns
    na_check_cols_part <- setdiff(names(participants_strat_cols), "part_id")
    if (length(na_check_cols_part) > 0) {
        filter_expr_part <- paste(paste0("!is.na(", na_check_cols_part, ")"), collapse = " & ")
        participants_strat_cols <- participants_strat_cols[eval(parse(text = filter_expr_part))]
    }
    message(paste0("  Processed participant columns. Rows: ", nrow(participants_strat_cols)))

    # --- Contact data preparation ---
    contacts_proc_dt <- copy(contacts_dt)
    if (!"unique_contact_id" %in% names(contacts_proc_dt)) {
        contacts_proc_dt[, unique_contact_id := .I]
    }
    cnt_cols_to_select <- c("part_id", "unique_contact_id")
    for (s_var in stratification_vars) {
        cnt_col_name <- paste0("cnt_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        if (!cnt_col_name %in% names(contacts_proc_dt)) {
            stop(paste("Contact column", cnt_col_name, "derived from", s_var, "not found."))
        }
        cnt_cols_to_select <- c(cnt_cols_to_select, cnt_col_name)
    }
    actual_cnt_cols_to_select <- intersect(cnt_cols_to_select, names(contacts_proc_dt))

    contacts_strat_cols <- contacts_proc_dt[, ..actual_cnt_cols_to_select]

    # Ensure factors for contacts
    for (s_var in stratification_vars) {
        col_name <- paste0("cnt_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        current_levels <- all_strata_levels[[s_var]]
        if (is.null(current_levels)) {
            stop(paste("Levels for stratification var '", s_var, "' not found in all_strata_levels list. Check config."))
        }
        contacts_strat_cols[, (col_name) := factor(get(col_name), levels = current_levels, ordered = (s_var %in% c("Age", "SES")))] # Only Age/SES typically ordered
    }

    # Filter out contacts with NA in any of the stratification columns
    na_check_cols_cnt <- setdiff(names(contacts_strat_cols), c("part_id", "unique_contact_id"))
    if (length(na_check_cols_cnt) > 0) {
        filter_expr_cnt <- paste(paste0("!is.na(", na_check_cols_cnt, ")"), collapse = " & ")
        contacts_strat_cols <- contacts_strat_cols[eval(parse(text = filter_expr_cnt))]
    }
    message(paste0("  Processed contact columns. Rows: ", nrow(contacts_strat_cols)))

    return(list(
        participants_strat_cols = participants_strat_cols,
        contacts_strat_cols = contacts_strat_cols
    ))
}

#' Calculate Participant Denominators for a Given Stratification
#'
#' Aggregates participant data to count the number of survey participants
#' in each defined demographic stratum.
#'
#' @param participants_strat_cols A data.table of participant data, filtered to relevant
#'        columns for stratification and with factors set (output from `get_survey_columns_for_stratification`).
#'        Expected to have `part_id` and participant stratification columns (e.g., `part_age_group`).
#' @param stratification_vars Character vector of base variable names used for stratification
#'        (e.g., c("Age", "Ethnicity")).
#' @param all_strata_levels A named list of levels for the stratification variables.
#' @return A data.table with participant stratification columns and `part_denominator` (count of participants per stratum).
get_participant_denominators_stratified <- function(participants_strat_cols, stratification_vars, all_strata_levels, population_weights = NULL) {
    message(paste0("Calculating participant denominators for stratification: ", paste(stratification_vars, collapse = ", ")))

    if (!is.data.table(participants_strat_cols)) stop("participants_strat_cols must be a data.table.")
    if (nrow(participants_strat_cols) == 0) {
        message("  participants_strat_cols is empty. Returning empty denominators table.")
        # Construct empty DT with correct columns and types
        part_denom_group_cols <- character(0)
        for (s_var in stratification_vars) {
            part_denom_group_cols <- c(part_denom_group_cols, paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", "")))
        }

        empty_dt_list <- list()
        for (col_name in part_denom_group_cols) {
            s_var_temp <- gsub("part_|_group", "", col_name)
            s_var_temp <- paste0(toupper(substring(s_var_temp, 1, 1)), substring(s_var_temp, 2))
            if (grepl("ethnicity", col_name, ignore.case = TRUE)) s_var_temp <- "Ethnicity"
            if (grepl("sex|gender", col_name, ignore.case = TRUE)) s_var_temp <- "Gender"

            current_levels <- all_strata_levels[[s_var_temp]]
            if (is.null(current_levels)) {
                warning(paste("Levels for stratification var '", s_var_temp, "' not found in all_strata_levels list for empty DT creation. Defaulting to empty character vector."))
                current_levels <- character(0)
            }
            empty_dt_list[[col_name]] <- factor(levels = current_levels, ordered = (s_var_temp %in% c("Age", "SES")))
        }
        empty_dt_list[["part_denominator"]] <- integer()
        return(as.data.table(empty_dt_list))
    }

    part_group_by_cols <- character(0)
    for (s_var in stratification_vars) {
        col_name <- paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        if (!col_name %in% names(participants_strat_cols)) {
            stop(paste("Column", col_name, "needed for participant denominator not found."))
        }
        part_group_by_cols <- c(part_group_by_cols, col_name)
    }

    if (length(part_group_by_cols) == 0) stop("No valid grouping columns for participant denominators.")

    # Aggregate by the stratification columns to get participant counts
    # Apply population weights if provided, otherwise use simple counts
    if (!is.null(population_weights)) {
        # Merge population weights with participants data
        participants_with_weights <- merge(participants_strat_cols, population_weights, by = "part_id", all.x = TRUE)
        participants_with_weights[is.na(population_weight), population_weight := 1.0]
        message("  Applying population weights to participant denominators")
        participant_denominators_dt <- participants_with_weights[, .(part_denominator = sum(population_weight)), by = part_group_by_cols]
    } else {
        # Use simple counts (.N) if no weights provided
        participant_denominators_dt <- participants_strat_cols[, .(part_denominator = .N), by = part_group_by_cols]
    }

    # Ensure factor levels are correctly maintained after aggregation
    for (s_var in stratification_vars) {
        col_name <- paste0("part_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
        current_levels <- all_strata_levels[[s_var]]
        if (is.null(current_levels)) {
            stop(paste("Levels for stratification var", s_var, "not found in all_strata_levels list."))
        }
        if (col_name %in% names(participant_denominators_dt)) {
            participant_denominators_dt[, (col_name) := factor(get(col_name), levels = current_levels, ordered = (s_var %in% c("Age", "SES")))]
        }
    }
    message(paste0("  Calculated participant denominators. Rows: ", nrow(participant_denominators_dt)))
    return(participant_denominators_dt)
}

#' Calculate Contact Numerators (Sum of Contacts i to j) for a Given Stratification
#'
#' Merges participant and contact data based on their stratification and counts
#' the total number of contacts from participants in stratum 'i' to contacts in stratum 'j'.
#'
#' @param participants_strat_cols A data.table of participant data with `part_id` and
#'        selected stratification columns (output from `get_survey_columns_for_stratification`).
#' @param contacts_strat_cols A data.table of contact data with `part_id`, `unique_contact_id`,
#'        and selected stratification columns (output from `get_survey_columns_for_stratification`).
#' @param stratification_vars Character vector of base variable names used for stratification.
#' @param all_strata_levels A named list of levels for the stratification variables.
#' @return A data.table with participant stratification columns, contact stratification columns,
#'         and `sum_contacts_ij` (the total number of contacts from stratum i to stratum j).
get_contact_numerators_stratified <- function(participants_strat_cols, contacts_strat_cols, stratification_vars, all_strata_levels, population_weights = NULL) {
    message(paste0("Calculating contact numerators for stratification: ", paste(stratification_vars, collapse = ", ")))

    if (!is.data.table(participants_strat_cols)) stop("participants_strat_cols must be a data.table.")
    if (!is.data.table(contacts_strat_cols)) stop("contacts_strat_cols must be a data.table.")

    if (nrow(participants_strat_cols) == 0 || nrow(contacts_strat_cols) == 0) {
        message("  participants_strat_cols or contacts_strat_cols is empty. Returning empty numerators table.")
        # Construct empty DT with correct columns and types
        num_group_cols <- character(0)
        empty_dt_list <- list()
        for (prefix in c("part", "cnt")) {
            for (s_var in stratification_vars) {
                col_name <- paste0(prefix, "_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
                num_group_cols <- c(num_group_cols, col_name)

                s_var_temp <- s_var # Already the base variable name
                current_levels <- all_strata_levels[[s_var_temp]]
                if (is.null(current_levels)) stop(paste("Levels for", s_var_temp, "not in all_strata_levels."))

                empty_dt_list[[col_name]] <- factor(levels = current_levels, ordered = (s_var_temp %in% c("Age", "SES")))
            }
        }
        empty_dt_list[["sum_contacts_ij"]] <- integer()
        return(as.data.table(empty_dt_list))
    }

    # Define participant and contact grouping columns based on stratification_vars
    part_group_cols <- unname(sapply(stratification_vars, function(v) paste0("part_", tolower(v), ifelse(v == "Age", "_group", ""))))
    cnt_group_cols <- unname(sapply(stratification_vars, function(v) paste0("cnt_", tolower(v), ifelse(v == "Age", "_group", ""))))

    # Check if all required columns exist before merge
    if (!all(part_group_cols %in% names(participants_strat_cols))) {
        stop("Not all participant stratification columns found in participants_strat_cols.")
    }
    if (!all(c("part_id", cnt_group_cols) %in% names(contacts_strat_cols))) {
        stop("Not all contact stratification columns or part_id found in contacts_strat_cols.")
    }

    # Merge contact data with participant data to get participant's stratum for each contact
    # Select only part_id and the actual stratification columns from participants_strat_cols to avoid duplicate columns if any exist
    contacts_expanded_dt <- merge(contacts_strat_cols,
        participants_strat_cols[, c("part_id", part_group_cols), with = FALSE],
        by = "part_id",
        all.x = TRUE
    ) # Keep all contacts, participant info might be NA if part_id not in participants_strat_cols (should not happen with prior processing)

    # Filter out rows where merge might have failed or resulted in NAs in key strata columns
    all_strat_cols_for_na_check <- c(part_group_cols, cnt_group_cols)
    if (length(all_strat_cols_for_na_check) > 0) {
        filter_expr_na <- paste(paste0("!is.na(", all_strat_cols_for_na_check, ")"), collapse = " & ")
        contacts_expanded_dt <- contacts_expanded_dt[eval(parse(text = filter_expr_na))]
    }

    if (nrow(contacts_expanded_dt) == 0) {
        message("  contacts_expanded_dt is empty after merge and NA filtering. Returning empty numerators table.")
        # Construct empty DT again, as above
        num_group_cols <- character(0)
        empty_dt_list <- list()
        for (prefix in c("part", "cnt")) {
            for (s_var in stratification_vars) {
                col_name <- paste0(prefix, "_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
                num_group_cols <- c(num_group_cols, col_name)
                s_var_temp <- s_var
                current_levels <- all_strata_levels[[s_var_temp]]
                if (is.null(current_levels)) stop(paste("Levels for", s_var_temp, "not in all_strata_levels."))

                empty_dt_list[[col_name]] <- factor(levels = current_levels, ordered = (s_var_temp %in% c("Age", "SES")))
            }
        }
        empty_dt_list[["sum_contacts_ij"]] <- integer()
        return(as.data.table(empty_dt_list))
    }

    # Group by all participant and contact stratification columns and count contacts
    group_by_all_strat_cols <- c(part_group_cols, cnt_group_cols)

    # Contact numerators: Always count actual contacts (.N), never weight them
    # Population weighting is applied only to denominators (participants)
    # This ensures proper contact rate calculation: actual_contacts / weighted_participants
    message("  Counting contact numerators (unweighted - contacts are observations, not units)")
    contact_numerators_dt <- contacts_expanded_dt[, .(sum_contacts_ij = .N), by = group_by_all_strat_cols]

    # Ensure factor levels are correctly maintained after aggregation
    for (prefix in c("part", "cnt")) {
        for (s_var in stratification_vars) {
            col_name <- paste0(prefix, "_", tolower(s_var), ifelse(s_var == "Age", "_group", ""))
            current_levels <- all_strata_levels[[s_var]]
            if (is.null(current_levels)) stop(paste("Levels for", s_var, "not in all_strata_levels."))

            if (col_name %in% names(contact_numerators_dt)) {
                contact_numerators_dt[, (col_name) := factor(get(col_name), levels = current_levels, ordered = (s_var %in% c("Age", "SES")))]
            }
        }
    }

    message(paste0("  Calculated contact numerators. Rows: ", nrow(contact_numerators_dt)))
    return(contact_numerators_dt)
}

#' Calculate Stratified Mean Contact Matrix with Reciprocity
#'
#' Calculates the mean contact rates (C_ij) between all pairs of demographic strata,
#' applying a population-weighted reciprocity adjustment.
#'
#' @param participant_denominators_dt Data.table with participant counts per stratum (N_i).
#'        Must contain participant stratification columns (e.g., `part_age_group`) and `part_denominator`.
#' @param contact_numerators_dt Data.table with sum of contacts from stratum i to j.
#'        Must contain participant and contact stratification columns and `sum_contacts_ij`.
#' @param aggregated_census_dt Data.table with population counts per stratum (Pop_k).
#'        Must contain stratification columns (named as per `stratification_vars`, e.g., `Age`, `Ethnicity`) and `Population`.
#' @param stratification_vars Character vector of base variable names (e.g., c("Age", "Ethnicity")).
#' @param all_strata_list A list where each element is a factor of all possible levels for a given stratification variable.
#'        The names of the list elements should be the stratification_vars (e.g., list(Age = FINAL_AGE_LEVELS, ...)).
#' @return A data.table with participant stratum columns (`part_var1`, `part_var2`, ...),
#'         contact stratum columns (`cnt_var1`, `cnt_var2`, ...), and `contact_rate` (C_ij).
calculate_mean_contact_matrix_stratified <- function(participant_denominators_dt,
                                                     contact_numerators_dt,
                                                     aggregated_census_dt,
                                                     stratification_vars,
                                                     all_strata_list) {
    message(paste0("Calculating mean contact matrix for stratification: ", paste(stratification_vars, collapse = ", ")))

    stratification_vars <- as.character(stratification_vars)
    part_group_cols <- paste0("part_", tolower(stratification_vars), ifelse(stratification_vars == "Age", "_group", ""))
    cnt_group_cols <- paste0("cnt_", tolower(stratification_vars), ifelse(stratification_vars == "Age", "_group", ""))
    base_strat_cols <- stratification_vars # e.g., c("Age", "SES")

    local_aggregated_census_for_joins_dt <- aggregated_census_dt[, .(Population = sum(Population, na.rm = TRUE)), by = c(base_strat_cols)]

    for (s_var_factor_loop in base_strat_cols) {
        if (s_var_factor_loop %in% names(local_aggregated_census_for_joins_dt) && s_var_factor_loop %in% names(all_strata_list)) {
            is_ordered_var <- (s_var_factor_loop == "Age" || s_var_factor_loop == "SES")
            local_aggregated_census_for_joins_dt[, (s_var_factor_loop) := factor(get(s_var_factor_loop), levels = all_strata_list[[s_var_factor_loop]], ordered = is_ordered_var)]
        }
    }

    # Create a complete grid of all possible strata combinations
    strata_levels_list_part <- lapply(stratification_vars, function(s_var) all_strata_list[[s_var]])
    names(strata_levels_list_part) <- part_group_cols
    all_strata_combinations_part_dt <- setDT(expand.grid(strata_levels_list_part, stringsAsFactors = FALSE))
    for (col in part_group_cols) {
        target_var <- sub("part_", "", sub("_group$", "", col, perl = TRUE))
        # Corrected logic for target_var_cap to match keys in all_strata_list
        target_var_cap <- switch(tolower(target_var),
            "age" = "Age",
            "ses" = "SES",
            "ethnicity" = "Ethnicity",
            "sex" = "Gender",
            "gender" = "Gender",
            stop(paste("Unknown target_var for capitalization:", target_var))
        )
        all_strata_combinations_part_dt[, (col) := factor(get(col), levels = all_strata_list[[target_var_cap]], ordered = (target_var_cap %in% c("Age", "SES")))]
    }

    strata_levels_list_cnt <- lapply(stratification_vars, function(s_var) all_strata_list[[s_var]])
    names(strata_levels_list_cnt) <- cnt_group_cols
    all_strata_combinations_cnt_dt <- setDT(expand.grid(strata_levels_list_cnt, stringsAsFactors = FALSE))
    for (col in cnt_group_cols) {
        target_var <- sub("cnt_", "", sub("_group$", "", col, perl = TRUE))
        # Corrected logic for target_var_cap to match keys in all_strata_list
        target_var_cap <- switch(tolower(target_var),
            "age" = "Age",
            "ses" = "SES",
            "ethnicity" = "Ethnicity",
            "sex" = "Gender",
            "gender" = "Gender",
            stop(paste("Unknown target_var for capitalization:", target_var))
        )
        all_strata_combinations_cnt_dt[, (col) := factor(get(col), levels = all_strata_list[[target_var_cap]], ordered = (target_var_cap %in% c("Age", "SES")))]
    }

    complete_matrix_grid_dt <- CJ_dt(all_strata_combinations_part_dt, all_strata_combinations_cnt_dt)

    # Ensure participant_denominators_dt and contact_numerators_dt columns are factors
    # (This should ideally be guaranteed by upstream functions, but double-check/re-factor)
    for (col in part_group_cols) {
        target_var <- sub("part_", "", sub("_group$", "", col, perl = TRUE))
        target_var_cap <- switch(tolower(target_var),
            "age" = "Age",
            "ses" = "SES",
            "ethnicity" = "Ethnicity",
            "sex" = "Gender",
            "gender" = "Gender",
            stop(paste("Unknown target_var for capitalization:", target_var))
        )
        if (col %in% names(participant_denominators_dt) && !is.factor(participant_denominators_dt[[col]])) {
            participant_denominators_dt[, (col) := factor(get(col), levels = all_strata_list[[target_var_cap]], ordered = (target_var_cap %in% c("Age", "SES")))]
        }
        if (col %in% names(contact_numerators_dt) && !is.factor(contact_numerators_dt[[col]])) {
            contact_numerators_dt[, (col) := factor(get(col), levels = all_strata_list[[target_var_cap]], ordered = (target_var_cap %in% c("Age", "SES")))]
        }
    }
    for (col in cnt_group_cols) {
        target_var <- sub("cnt_", "", sub("_group$", "", col, perl = TRUE))
        target_var_cap <- switch(tolower(target_var),
            "age" = "Age",
            "ses" = "SES",
            "ethnicity" = "Ethnicity",
            "sex" = "Gender",
            "gender" = "Gender",
            stop(paste("Unknown target_var for capitalization:", target_var))
        )
        if (col %in% names(contact_numerators_dt) && !is.factor(contact_numerators_dt[[col]])) {
            contact_numerators_dt[, (col) := factor(get(col), levels = all_strata_list[[target_var_cap]], ordered = (target_var_cap %in% c("Age", "SES")))]
        }
    }

    # Merge contact numerators (sum_contacts_ij)
    mean_contact_matrix_dt <- merge(
        complete_matrix_grid_dt,
        contact_numerators_dt,
        by = c(part_group_cols, cnt_group_cols),
        all.x = TRUE
    )
    mean_contact_matrix_dt[is.na(sum_contacts_ij), sum_contacts_ij := 0]

    # Merge participant denominators (N_i)
    mean_contact_matrix_dt <- merge(
        mean_contact_matrix_dt,
        participant_denominators_dt[, c(part_group_cols, "part_denominator"), with = FALSE],
        by = part_group_cols,
        all.x = TRUE
    )
    mean_contact_matrix_dt[is.na(part_denominator), M_ij_raw := NA_real_]

    # Calculate M_ij = sum_contacts_ij / part_denominator
    mean_contact_matrix_dt[, M_ij_raw := fifelse(part_denominator > 0, sum_contacts_ij / part_denominator, 0)]
    # Set M_ij_raw to NA if part_denominator is NA (no participants in survey for this stratum i)
    mean_contact_matrix_dt[is.na(part_denominator), M_ij_raw := NA_real_]


    # --- Apply Reciprocity: C_ij = (M_ij * P_i + M_ji * P_j) / (2 * P_i) ---

    # 1. Prepare census data for merging P_i and P_j
    # P_i: population of participant's stratum
    # P_j: population of contact's stratum
    census_for_pi <- copy(local_aggregated_census_for_joins_dt) # Use re-aggregated version
    census_for_pj <- copy(local_aggregated_census_for_joins_dt) # Use re-aggregated version

    # Rename census columns to match part_group_cols for P_i merge
    # Original census_for_pi has columns like base_strat_cols (e.g. "Age")
    # We want to rename them to part_group_cols (e.g. "part_age_group")
    pi_cols_old <- character()
    pi_cols_new <- character()
    for (idx in seq_along(base_strat_cols)) {
        s_var <- base_strat_cols[idx] # e.g., "Age"
        part_col <- part_group_cols[idx] # e.g., "part_age_group"
        if (s_var %in% names(census_for_pi)) {
            pi_cols_old <- c(pi_cols_old, s_var)
            pi_cols_new <- c(pi_cols_new, part_col)
        }
    }
    if (length(pi_cols_old) > 0) {
        setnames(census_for_pi, old = pi_cols_old, new = pi_cols_new)
    }
    setnames(census_for_pi, "Population", "P_i")

    # Merge P_i
    # The by columns should be the new names in census_for_pi (i.e., part_group_cols that were successfully renamed)
    merge_by_pi <- intersect(part_group_cols, names(census_for_pi))
    if (length(merge_by_pi) != length(part_group_cols)) {
        warning("Not all part_group_cols are present in census_for_pi after renaming for P_i merge. Check census data structure and stratification_vars.")
    }

    mean_contact_matrix_dt <- merge(
        mean_contact_matrix_dt,
        census_for_pi[, c(merge_by_pi, "P_i"), with = FALSE],
        by = merge_by_pi,
        all.x = TRUE
    )

    # Rename census columns to match cnt_group_cols for P_j merge
    # Original census_for_pj has columns like base_strat_cols (e.g. "Age")
    # We want to rename them to cnt_group_cols (e.g. "cnt_age_group")
    pj_cols_old <- character()
    pj_cols_new <- character()
    for (idx in seq_along(base_strat_cols)) {
        s_var <- base_strat_cols[idx] # e.g., "Age"
        cnt_col <- cnt_group_cols[idx] # e.g., "cnt_age_group"
        if (s_var %in% names(census_for_pj)) {
            pj_cols_old <- c(pj_cols_old, s_var)
            pj_cols_new <- c(pj_cols_new, cnt_col)
        }
    }
    if (length(pj_cols_old) > 0) {
        setnames(census_for_pj, old = pj_cols_old, new = pj_cols_new)
    }
    setnames(census_for_pj, "Population", "P_j")

    # Merge P_j
    # The by columns should be the new names in census_for_pj (i.e., cnt_group_cols that were successfully renamed)
    merge_by_pj <- intersect(cnt_group_cols, names(census_for_pj))
    if (length(merge_by_pj) != length(cnt_group_cols)) {
        warning("Not all cnt_group_cols are present in census_for_pj after renaming for P_j merge. Check census data structure and stratification_vars.")
    }

    mean_contact_matrix_dt <- merge(
        mean_contact_matrix_dt,
        census_for_pj[, c(merge_by_pj, "P_j"), with = FALSE],
        by = merge_by_pj,
        all.x = TRUE
    )

    # Handle cases where P_i or P_j might be NA (stratum not in census) or 0.
    # This will lead to NA or Inf/NaN in C_ij, which should be handled.
    # If P_i is 0 or NA, C_ij is problematic. Assume NGM handles NA contact rates.

    # 2. Create M_ji (mean contacts from j to i)
    # This requires "swapping" part and cnt columns and looking up M_ij_raw
    # Create a "ji" version of the matrix grid for merging
    m_ji_lookup_dt <- mean_contact_matrix_dt[, c(part_group_cols, cnt_group_cols, "M_ij_raw"), with = FALSE]

    # Rename columns for the "ji" perspective: part_ becomes cnt_temp, cnt_ becomes part_temp
    ji_part_cols_temp <- paste0("part_", tolower(stratification_vars), "_temp", ifelse(stratification_vars == "Age", "_group", ""))
    ji_cnt_cols_temp <- paste0("cnt_", tolower(stratification_vars), "_temp", ifelse(stratification_vars == "Age", "_group", ""))

    setnames(m_ji_lookup_dt, old = part_group_cols, new = ji_cnt_cols_temp) # Original i becomes j
    setnames(m_ji_lookup_dt, old = cnt_group_cols, new = ji_part_cols_temp) # Original j becomes i
    setnames(m_ji_lookup_dt, old = "M_ij_raw", new = "M_ji_raw")

    # Now merge this M_ji_raw back to the main mean_contact_matrix_dt
    # The merge keys will be: current part_group_cols must match ji_part_cols_temp,
    # AND current cnt_group_cols must match ji_cnt_cols_temp.
    current_part_to_ji_part_map <- setNames(ji_part_cols_temp, part_group_cols)
    current_cnt_to_ji_cnt_map <- setNames(ji_cnt_cols_temp, cnt_group_cols)

    merge_by_cols_ji <- c(part_group_cols, cnt_group_cols)

    # To avoid name clashes during merge, rename lookup table columns to match current dt's perspective for M_ji
    # M_ji_lookup_dt has (part_temp_age, cnt_temp_age, M_ji_raw)
    # We need to merge where mean_contact_matrix_dt$part_age_group == m_ji_lookup_dt$part_temp_age (as i)
    # AND mean_contact_matrix_dt$cnt_age_group == m_ji_lookup_dt$cnt_temp_age (as j)
    # So, rename m_ji_lookup_dt to match part_group_cols and cnt_group_cols before merge
    m_ji_lookup_dt_renamed <- copy(m_ji_lookup_dt)
    setnames(m_ji_lookup_dt_renamed, old = ji_part_cols_temp, new = part_group_cols)
    setnames(m_ji_lookup_dt_renamed, old = ji_cnt_cols_temp, new = cnt_group_cols)

    # Explicitly set/refresh factor levels on m_ji_lookup_dt_renamed for all join key columns
    # to ensure consistency before the M_ji_raw merge.
    # stratification_vars contains the base names like "SES", "Gender"
    # all_strata_list contains the levels for these base names.
    for (s_var_factor_loop in stratification_vars) {
        # Construct part_ and cnt_ column names from the base stratification variable
        part_col_name_for_factor <- paste0("part_", tolower(s_var_factor_loop), ifelse(s_var_factor_loop == "Age", "_group", ""))
        cnt_col_name_for_factor <- paste0("cnt_", tolower(s_var_factor_loop), ifelse(s_var_factor_loop == "Age", "_group", ""))

        is_ordered_var_for_factor <- (s_var_factor_loop == "Age" || s_var_factor_loop == "SES")

        # Apply to part_ column if it exists in m_ji_lookup_dt_renamed
        if (part_col_name_for_factor %in% names(m_ji_lookup_dt_renamed)) {
            if (s_var_factor_loop %in% names(all_strata_list)) {
                m_ji_lookup_dt_renamed[, (part_col_name_for_factor) := factor(get(part_col_name_for_factor), levels = all_strata_list[[s_var_factor_loop]], ordered = is_ordered_var_for_factor)]
            } else {
                warning(paste0("Levels for '", s_var_factor_loop, "' not found in all_strata_list for column '", part_col_name_for_factor, "' in M_ji_raw factor setting."))
            }
        }

        # Apply to cnt_ column if it exists in m_ji_lookup_dt_renamed
        if (cnt_col_name_for_factor %in% names(m_ji_lookup_dt_renamed)) {
            if (s_var_factor_loop %in% names(all_strata_list)) {
                m_ji_lookup_dt_renamed[, (cnt_col_name_for_factor) := factor(get(cnt_col_name_for_factor), levels = all_strata_list[[s_var_factor_loop]], ordered = is_ordered_var_for_factor)]
            } else {
                warning(paste0("Levels for '", s_var_factor_loop, "' not found in all_strata_list for column '", cnt_col_name_for_factor, "' in M_ji_raw factor setting."))
            }
        }
    }


    mean_contact_matrix_dt <- merge(
        mean_contact_matrix_dt,
        m_ji_lookup_dt_renamed[, c(part_group_cols, cnt_group_cols, "M_ji_raw"), with = FALSE],
        by = c(part_group_cols, cnt_group_cols),
        all.x = TRUE
    )
    # M_ji_raw can be NA if the j->i combination didn't exist or had no survey participants for j.

    # 3. Calculate C_ij (final contact_rate)
    mean_contact_matrix_dt[, contact_rate := NA_real_] # Initialize column

    # More robust calculation using merge to prevent vector recycling/mismatch issues
    # This ensures that we are robustly dividing by the correct population count (P_i)
    # for each stratum i's contact rate calculation.

    # First, ensure M_ij_raw and M_ji_raw are 0 if their respective survey denominators were 0 or NA
    mean_contact_matrix_dt[is.na(M_ij_raw), M_ij_raw := 0]
    mean_contact_matrix_dt[is.na(M_ji_raw), M_ji_raw := 0]

    # PROPER RECIPROCITY CONSTRAINT: Balance contact rates by average rate
    # The goal is to ensure that M_ij ≈ M_ji while respecting the constraint that
    # these represent the same physical contacts from different perspectives.
    #
    # For a truly reciprocal matrix, we want: M_ij * P_i = M_ji * P_j
    # where P_i and P_j are the population sizes.
    #
    # Algorithm:
    # 1. Calculate the symmetric rate as the weighted average of M_ij and M_ji
    # 2. Weight by the inverse of population sizes to account for sampling differences

    # Get population data for both i and j groups
    # P_i is already merged. Get P_j by merging population data on contact group columns
    census_for_pj <- copy(local_aggregated_census_for_joins_dt)

    # Rename census columns to match contact group columns for P_j merge
    pj_cols_old <- character()
    pj_cols_new <- character()
    for (idx in seq_along(base_strat_cols)) {
        s_var <- base_strat_cols[idx] # e.g., "Age"
        cnt_col <- cnt_group_cols[idx] # e.g., "cnt_age_group"
        if (s_var %in% names(census_for_pj)) {
            pj_cols_old <- c(pj_cols_old, s_var)
            pj_cols_new <- c(pj_cols_new, cnt_col)
        }
    }
    if (length(pj_cols_old) > 0) {
        setnames(census_for_pj, old = pj_cols_old, new = pj_cols_new)
    }
    setnames(census_for_pj, "Population", "P_j")

    # Merge P_j onto the matrix if not already done
    if (!"P_j" %in% names(mean_contact_matrix_dt)) {
        merge_by_pj <- intersect(cnt_group_cols, names(census_for_pj))
        mean_contact_matrix_dt <- merge(
            mean_contact_matrix_dt,
            census_for_pj[, c(merge_by_pj, "P_j"), with = FALSE],
            by = merge_by_pj,
            all.x = TRUE
        )
    }

    # Calculate reciprocal contact rate using weighted average
    # CORRECTED: Weight M_ij by P_i and M_ji by P_j for true reciprocity
    mean_contact_matrix_dt[, contact_rate := fifelse(
        !is.na(P_i) & !is.na(P_j) & P_i > 0 & P_j > 0 & !is.na(M_ij_raw) & !is.na(M_ji_raw),
        (M_ij_raw * P_i + M_ji_raw * P_j) / (2 * P_i),
        fifelse(!is.na(M_ij_raw), M_ij_raw, 0) # Fallback to M_ij_raw if reciprocal calc fails
    )]


    # Select and order columns for the flat matrix output
    # Keep relevant intermediate columns for potential debugging if needed, or remove them.
    # For now, keeping M_ij_raw, M_ji_raw, P_i, P_j.
    final_cols <- c(part_group_cols, cnt_group_cols, "part_denominator", "sum_contacts_ij", "M_ij_raw", "P_i", "M_ji_raw", "P_j", "contact_rate")
    mean_contact_matrix_dt <- mean_contact_matrix_dt[, ..final_cols] # Use .. to evaluate final_cols

    message(paste0("Symmetrised mean contact matrix calculated. Rows: ", nrow(mean_contact_matrix_dt)))
    if (any(is.na(mean_contact_matrix_dt$contact_rate))) {
        message(paste0("  Note: ", sum(is.na(mean_contact_matrix_dt$contact_rate)), " NA values in final contact_rate column."))
    }
    if (any(mean_contact_matrix_dt$contact_rate < 0, na.rm = TRUE)) {
        warning("  Warning: Negative values found in final contact_rate column. This is unexpected.")
    }

    return(mean_contact_matrix_dt)
}

#' Calculate Projected Case Share for Stratified Data
#'
#' This function's logic is now confirmed as correct. The dominant eigenvector `w`
#' of the NGM represents the relative distribution of new infections. Therefore, the
#' proportion `w_i` for each stratum `i` is, by definition, the projected share of cases
#' for that stratum. No further calculation involving population size is needed for this metric.
#'
#' @param eigenvector_dt Data.table from `calculate_ngm_stratified`, containing
#'        stratification columns and `Proportion` (of incident cases, i.e., w_i).
#' @param aggregated_census_strat_dt Data.table of census data (not used, kept for API consistency).
#' @param stratification_vars Character vector of base variable names used.
#' @param all_strata_list Named list of levels for each stratification variable.
#' @return A data.table with stratification columns and `ProjectedCaseShare`.
calculate_projected_case_share_stratified <- function(eigenvector_dt,
                                                      aggregated_census_strat_dt,
                                                      stratification_vars,
                                                      all_strata_list) {
    message("Calculating projected case share (from eigenvector)...")
    # The eigenvector `Proportion` (w_i) is the projected share of new cases.
    # The function simply renames the column for clarity in subsequent steps.
    projected_cases_dt <- copy(eigenvector_dt)
    setnames(projected_cases_dt, "Proportion", "ProjectedCaseShare")
    return(projected_cases_dt)
}

#' Calculate Relative Within-Group Burden Compared to a Reference Group
#'
#' Calculates the per-capita infection burden of each demographic group relative
#' to a specified reference group, based on normalized incidence values.
#'
#' @param normalized_incidence_dt Data.table from `calculate_ngm_stratified`,
#'        containing stratification columns and `NormalizedIncidence`.
#' @param stratification_vars Character vector of base variable names used.
#' @param all_strata_list Named list of levels for each stratification variable.
#' @param reference_group_strata A named list where names are stratification variables
#'        (e.g., "Age", "Ethnicity") and values are the specific levels of the
#'        reference group (e.g., list(Age = "20-24", Ethnicity = "White") or list(Age="60-64")).
#' @return A data.table with stratification columns, `NormalizedIncidence`,
#'         and `RelativeBurden`.
calculate_relative_burden_stratified <- function(normalized_incidence_dt,
                                                 stratification_vars,
                                                 all_strata_list,
                                                 reference_group_strata) {
    message(paste0("Calculating relative burden for stratification: ", paste(stratification_vars, collapse = ", ")))

    if (!is.data.table(normalized_incidence_dt) || !("NormalizedIncidence" %in% names(normalized_incidence_dt))) {
        stop("normalized_incidence_dt must be a data.table and contain a 'NormalizedIncidence' column.")
    }
    if (nrow(normalized_incidence_dt) == 0) {
        message("  normalized_incidence_dt is empty. Returning empty relative burden table.")
        empty_dt_list <- lapply(setNames(stratification_vars, stratification_vars), function(v) factor(levels = all_strata_list[[v]]))
        empty_dt_list$NormalizedIncidence <- numeric()
        empty_dt_list$RelativeBurden <- numeric()
        return(as.data.table(empty_dt_list)[0, ])
    }
    if (!all(names(reference_group_strata) %in% stratification_vars)) {
        stop("Names in reference_group_strata must be a subset of stratification_vars.")
    }

    relative_burden_dt <- copy(normalized_incidence_dt)

    # Determine if the new "within-age" comparison logic should be used.
    is_age_stratified <- "Age" %in% stratification_vars
    other_strat_vars <- setdiff(stratification_vars, "Age")

    if (is_age_stratified && length(other_strat_vars) > 0) {
        # New logic: compare within each age group.
        message("  Calculating relative burden within age groups.")

        # Identify the reference levels for the non-age variables from the config.
        reference_spec <- reference_group_strata[names(reference_group_strata) %in% other_strat_vars]
        if (length(reference_spec) == 0) {
            warning("Reference level(s) for non-age variable(s) not specified in reference_group_strata. RelativeBurden will be NA.")
            relative_burden_dt[, RelativeBurden := NA_real_]
            final_cols_early_exit <- c(stratification_vars, "NormalizedIncidence", "RelativeBurden")
            return(relative_burden_dt[, ..final_cols_early_exit])
        }

        # Create a filtering expression to extract reference data for each age group.
        filter_expr <- paste(sapply(names(reference_spec), function(var) {
            paste0(var, " == '", reference_spec[[var]], "'")
        }), collapse = " & ")

        # Create the reference incidence table.
        reference_incidence_dt <- relative_burden_dt[eval(parse(text = filter_expr))]

        if (nrow(reference_incidence_dt) == 0) {
            warning(paste("Could not find any reference data for filter:", filter_expr, ". RelativeBurden will be NA."))
            relative_burden_dt[, RelativeBurden := NA_real_]
        } else {
            # Select Age and the incidence, which becomes the reference incidence for each age group.
            reference_incidence_dt <- reference_incidence_dt[, .SD, .SDcols = c("Age", "NormalizedIncidence")]
            setnames(reference_incidence_dt, "NormalizedIncidence", "ReferenceIncidence")

            # Merge this back to the full table by Age.
            relative_burden_dt <- merge(relative_burden_dt, reference_incidence_dt, by = "Age", all.x = TRUE)

            # Calculate relative burden and handle division by zero.
            relative_burden_dt[, RelativeBurden := fifelse(!is.na(ReferenceIncidence) & ReferenceIncidence > 1e-9, NormalizedIncidence / ReferenceIncidence, NA_real_)]
            relative_burden_dt[, ReferenceIncidence := NULL] # Clean up helper column.
        }
    } else {
        # Old logic: compare to a single, fixed reference group.
        message("  Calculating relative burden against a single reference group.")

        ref_condition_dt <- as.data.table(reference_group_strata)
        norm_inc_copy <- copy(normalized_incidence_dt) # Use a copy for factor manipulation.
        for (s_var in stratification_vars) {
            norm_inc_copy[, (s_var) := factor(get(s_var), levels = all_strata_list[[s_var]])]
            if (s_var %in% names(ref_condition_dt)) {
                if (!ref_condition_dt[[s_var]] %in% all_strata_list[[s_var]]) {
                    stop(paste0("Reference group level '", ref_condition_dt[[s_var]], "' for variable '", s_var, "' is not a valid level."))
                }
                ref_condition_dt[, (s_var) := factor(get(s_var), levels = all_strata_list[[s_var]])]
            }
        }

        reference_burden_value <- NA_real_
        if (length(reference_group_strata) > 0 && nrow(ref_condition_dt) > 0) {
            burden_ref_dt <- merge(norm_inc_copy, ref_condition_dt, by = names(reference_group_strata), all.y = TRUE)
            if (nrow(burden_ref_dt) == 1 && "NormalizedIncidence" %in% names(burden_ref_dt)) {
                reference_burden_value <- burden_ref_dt$NormalizedIncidence[1]
            } else if (nrow(burden_ref_dt) > 1) {
                warning(paste("Multiple matches for reference group strata:", paste(utils::capture.output(print(reference_group_strata)), collapse = " "), ". Using the first match."))
                reference_burden_value <- burden_ref_dt$NormalizedIncidence[1]
            } else {
                ref_group_str_for_warning <- paste(mapply(function(name, val) paste(name, val, sep = ":"), names(reference_group_strata), reference_group_strata), collapse = ", ")
                warning(paste("Reference group (", ref_group_str_for_warning, ") not found or no unique match in normalized_incidence_dt."))
            }
        }

        if (is.na(reference_burden_value)) {
            warning("NormalizedIncidence for the reference group is NA or reference group not found. RelativeBurden will be NA.")
            relative_burden_dt[, RelativeBurden := NA_real_]
        } else if (abs(reference_burden_value) < 1e-9) {
            warning(paste("NormalizedIncidence for the reference group is near zero (", reference_burden_value, "). RelativeBurden might be Inf or NA. Setting to NA."))
            relative_burden_dt[, RelativeBurden := NA_real_]
        } else {
            relative_burden_dt[, RelativeBurden := NormalizedIncidence / reference_burden_value]
        }
    }

    # Final cleanup and return.
    for (s_var in stratification_vars) {
        if (s_var %in% names(relative_burden_dt)) {
            relative_burden_dt[, (s_var) := factor(get(s_var), levels = all_strata_list[[s_var]])]
        }
    }

    final_cols <- c(stratification_vars, "NormalizedIncidence", "RelativeBurden")
    final_cols_present <- intersect(final_cols, names(relative_burden_dt))
    relative_burden_dt <- relative_burden_dt[, ..final_cols_present]

    message(paste0("  Calculated relative burden. Rows: ", nrow(relative_burden_dt)))
    return(relative_burden_dt)
}

# --- NGM Calculation Function ---
#' Calculate Stratified NGM, R0, Eigenvector, and Normalized Incidence
#'
#' Constructs the Next Generation Matrix (NGM) from a contact array,
#' calculates R0, the dominant eigenvector (distribution of incident cases),
#' and the per-capita normalized incidence.
#'
#' @param mean_contact_array A multi-dimensional array of mean contact rates (C_ij),
#'        where the first set of dimensions represent stratum 'i' (participant)
#'        and the second set represent stratum 'j' (contact).
#'        Dimnames must match those in `all_strata_list` for `stratification_vars`.
#' @param aggregated_census_strat_dt A data.table with census population counts
#'        aggregated for the current stratification. Must contain `stratification_vars`
#'        columns and a `Population` column.
#' @param stratification_vars Character vector of base stratification variable names
#'        (e.g., c("Age", "SES")).
#' @param all_strata_list A named list containing vectors of levels for each
#'        stratification variable.
#' @param matrix_name A character string for naming or logging, typically the analysis type.
#' @param q_value Numeric, a constant representing susceptibility/transmissibility. Defaults to 1.
#' @return A list containing:
#'   - `r0` (numeric): The dominant eigenvalue (R0).
#'   - `eigenvector_dt` (data.table): Stratification columns and `Proportion` (normalized dominant right eigenvector, w_i).
#'   - `left_eigenvector_dt` (data.table): Stratification columns and `RelativeContribution` (normalized dominant left eigenvector, v_i).
#'   - `normalized_incidence_dt` (data.table): Stratification columns and `NormalizedIncidence` (I_i).
#'   - `ngm_matrix` (matrix): The constructed NGM (K).
#'   - `error_message` (character): NULL if no error, otherwise the error message.
calculate_ngm_stratified <- function(mean_contact_array,
                                     aggregated_census_strat_dt,
                                     stratification_vars,
                                     all_strata_list,
                                     matrix_name = "UnnamedNGM",
                                     q_value = 1) {
    message(paste0("Calculating NGM for: ", matrix_name))

    if (is.null(mean_contact_array) || !is.array(mean_contact_array) || any(dim(mean_contact_array) == 0)) {
        warning(paste0("NGM calculation for ", matrix_name, " failed: mean_contact_array is NULL, not an array, or has zero dimensions."))
        return(list(r0 = NA_real_, eigenvector_dt = data.table(), normalized_incidence_dt = data.table(), ngm_matrix = NULL, error_message = "Invalid contact array"))
    }

    num_strat_vars <- length(stratification_vars)
    if (length(dim(mean_contact_array)) != 2 * num_strat_vars) {
        stop(paste0("Contact array dimensions (", paste(dim(mean_contact_array), collapse = "x"), ") do not match 2 * number of stratification variables (", 2 * num_strat_vars, ")."))
    }

    # Transpose for correct NGM orientation (K_ij = transmission FROM j TO i)
    # Contact matrix C_ij is contacts of i with j.
    # Transmission from j to i depends on j contacting i, which is C_ji.
    # Thus K is proportional to t(C).
    contact_matrix_for_ngm <- mean_contact_array
    contact_matrix_for_ngm[is.na(contact_matrix_for_ngm)] <- 0

    total_strata_count <- prod(dim(contact_matrix_for_ngm)[1:num_strat_vars])
    if (total_strata_count == 0) {
        warning(paste0("NGM calculation for ", matrix_name, " failed: total_strata_count is zero. Check dimnames and levels."))
        return(list(r0 = NA_real_, eigenvector_dt = data.table(), normalized_incidence_dt = data.table(), ngm_matrix = NULL, error_message = "Total strata count is zero for NGM construction"))
    }

    ngm_matrix_k <- matrix(contact_matrix_for_ngm,
        nrow = total_strata_count,
        ncol = total_strata_count
    )

    # CRITICAL FIX: Transpose for correct NGM orientation (K_ij = transmission FROM j TO i)
    ngm_matrix_k <- t(ngm_matrix_k)

    ngm_matrix_k <- q_value * ngm_matrix_k

    eigen_results <- tryCatch(
        {
            eigen(ngm_matrix_k)
        },
        error = function(e) {
            warning(paste0("Eigen decomposition failed for NGM (", matrix_name, "): ", e$message))
            return(NULL)
        }
    )

    if (is.null(eigen_results)) {
        return(list(r0 = NA_real_, eigenvector_dt = data.table(), normalized_incidence_dt = data.table(), ngm_matrix = ngm_matrix_k, error_message = "Eigen decomposition failed"))
    }

    dominant_idx <- which.max(Re(eigen_results$values))
    r0_val <- Re(eigen_results$values[dominant_idx])

    # --- Right Eigenvector (w): Distribution of Cases ---
    eigenvector_w_raw <- Re(eigen_results$vectors[, dominant_idx])
    if (sum(eigenvector_w_raw) < 0) {
        eigenvector_w_raw <- -eigenvector_w_raw
    }
    eigenvector_w_raw[eigenvector_w_raw < 0] <- 0

    if (sum(eigenvector_w_raw, na.rm = TRUE) != 0) {
        eigenvector_w_normalized <- eigenvector_w_raw / sum(eigenvector_w_raw, na.rm = TRUE)
    } else {
        warning(paste0("Sum of raw right eigenvector w is zero for NGM (", matrix_name, "). Proportions will be NA or 0."))
        eigenvector_w_normalized <- rep(NA_real_, length(eigenvector_w_raw))
    }

    # --- Left Eigenvector (v): Reproductive Value / Contribution to Transmission ---
    # The left eigenvectors of K are the right eigenvectors of t(K)
    left_eigen_results <- tryCatch(
        {
            eigen(t(ngm_matrix_k))
        },
        error = function(e) {
            warning(paste0("Left eigen decomposition failed for NGM (", matrix_name, "): ", e$message))
            return(NULL)
        }
    )

    left_eigenvector_v_normalized <- if (!is.null(left_eigen_results)) {
        left_dominant_idx <- which.max(Re(left_eigen_results$values))
        left_eigenvector_v_raw <- Re(left_eigen_results$vectors[, left_dominant_idx])
        if (sum(left_eigenvector_v_raw) < 0) {
            left_eigenvector_v_raw <- -left_eigenvector_v_raw
        }
        left_eigenvector_v_raw[left_eigenvector_v_raw < 0] <- 0

        if (sum(left_eigenvector_v_raw, na.rm = TRUE) != 0) {
            left_eigenvector_v_raw / sum(left_eigenvector_v_raw, na.rm = TRUE)
        } else {
            warning(paste0("Sum of raw left eigenvector v is zero for NGM (", matrix_name, "). Proportions will be NA or 0."))
            rep(NA_real_, length(left_eigenvector_v_raw))
        }
    } else {
        rep(NA_real_, nrow(ngm_matrix_k))
    }


    strata_dimnames_list <- dimnames(mean_contact_array)[1:num_strat_vars]
    if (any(sapply(strata_dimnames_list, is.null))) {
        warning(paste0("NGM calculation for ", matrix_name, " failed: dimnames for NGM strata are NULL. Check contact array construction."))
        return(list(r0 = r0_val, eigenvector_dt = data.table(), normalized_incidence_dt = data.table(), ngm_matrix = ngm_matrix_k, error_message = "Dimnames for NGM strata are NULL."))
    }

    eigenvector_dt <- setDT(expand.grid(strata_dimnames_list, stringsAsFactors = FALSE))
    if (ncol(eigenvector_dt) != length(stratification_vars)) {
        warning(paste0("Mismatch between columns in eigenvector_dt (", ncol(eigenvector_dt), ") and stratification_vars (", length(stratification_vars), "). Check dimnames of contact_matrix_transposed."))
        # Attempt to name anyway or handle error
        if (ncol(eigenvector_dt) > 0 && length(stratification_vars) >= ncol(eigenvector_dt)) {
            names(eigenvector_dt) <- stratification_vars[1:ncol(eigenvector_dt)]
        } else {
            return(list(r0 = r0_val, eigenvector_dt = data.table(), normalized_incidence_dt = data.table(), ngm_matrix = ngm_matrix_k, error_message = "Column mismatch for eigenvector strata mapping."))
        }
    } else {
        names(eigenvector_dt) <- stratification_vars
    }

    for (s_var in stratification_vars) {
        if (s_var %in% names(eigenvector_dt)) {
            eigenvector_dt[, (s_var) := factor(get(s_var), levels = all_strata_list[[s_var]])]
        } # If s_var not in names due to previous error, this will skip
    }
    eigenvector_dt[, Proportion := eigenvector_w_normalized]

    # Create a separate data.table for the left eigenvector
    left_eigenvector_dt <- copy(eigenvector_dt)
    left_eigenvector_dt[, Proportion := NULL] # Remove the right eigenvector proportion
    left_eigenvector_dt[, RelativeContribution := left_eigenvector_v_normalized]

    census_for_incidence <- copy(aggregated_census_strat_dt)
    for (s_var in stratification_vars) {
        if (s_var %in% names(census_for_incidence)) {
            census_for_incidence[, (s_var) := factor(get(s_var), levels = all_strata_list[[s_var]])]
        } else {
            stop(paste0("Stratification variable '", s_var, "' not found in aggregated_census_strat_dt for NGM calculation."))
        }
    }
    if (!"Population" %in% names(census_for_incidence)) {
        stop("Column 'Population' not found in aggregated_census_strat_dt for NGM calculation.")
    }

    incidence_data <- merge(eigenvector_dt,
        census_for_incidence[, c(stratification_vars, "Population"), with = FALSE],
        by = stratification_vars,
        all.x = TRUE
    )

    incidence_data[, w_div_P := fifelse(!is.na(Population) & Population > 1e-9, Proportion / Population, NA_real_)] # Use 1e-9 to avoid division by very small numbers too

    sum_w_div_P <- sum(incidence_data$w_div_P, na.rm = TRUE)

    if (!is.na(sum_w_div_P) && abs(sum_w_div_P) > 1e-9) {
        incidence_data[, NormalizedIncidence := w_div_P / sum_w_div_P]
    } else {
        warning(paste0("Sum of (w_k / P_k) is NA or near zero for NGM (", matrix_name, "). NormalizedIncidence will be NA or 0."))
        incidence_data[, NormalizedIncidence := NA_real_]
        if (abs(sum_w_div_P) <= 1e-9 && !is.na(sum_w_div_P)) incidence_data[, NormalizedIncidence := 0] # If sum is zero, incidence is zero
    }

    normalized_incidence_dt <- incidence_data[, c(stratification_vars, "NormalizedIncidence"), with = FALSE]

    message(paste0("NGM calculation for ", matrix_name, " complete. R0 = ", round(r0_val, 4)))

    return(list(
        r0 = r0_val,
        eigenvector_dt = eigenvector_dt,
        left_eigenvector_dt = left_eigenvector_dt,
        normalized_incidence_dt = normalized_incidence_dt,
        ngm_matrix = ngm_matrix_k,
        error_message = NULL
    ))
}


# --- Main Workflow Function for a Single Stratified NGM Calculation Set ---

#' Perform a Single Stratified NGM Calculation Set
#'
#' Orchestrates the sequence of operations to calculate NGM and related metrics
#' for a single set of stratification variables and configuration.
#' This function is intended to be called for each analysis type (e.g., Age-Ethnicity)
#' and for each bootstrap iteration if bootstrapping is performed.
#'
#' @param analysis_name (character): A descriptive name (e.g., "Age_Ethnicity").
#' @param iteration_id (integer, or NA if not applicable): For logging, if part of a bootstrap.
#' @param participants_dt_iter data.table, pre-processed participant survey data for this iteration/run.
#' @param contacts_dt_iter data.table, pre-processed contact survey data for this iteration/run.
#' @param census_dt_iter data.table, pre-processed and aggregated census data for this iteration/run.
#' @param stratification_vars (character vector): Variables to stratify by (e.g., c("Age", "Ethnicity")).
#' @param reference_group_strata (named list): For relative burden (e.g., list(Age = "Aged 20 to 24 years")).
#' @param all_strata_levels The global `ALL_STRATA_LEVELS_LIST`.
#'
#' @return A list containing all calculated metrics for this run:
#'   - `analysis_name` (character)
#'   - `iteration_id` (integer, or NA if not applicable)
#'   - `r0` (numeric): The dominant eigenvalue of the NGM.
#'   - `eigenvector_dt` (data.table): Distribution of incident cases (w).
#'   - `left_eigenvector_dt` (data.table): Contribution to transmission (v).
#'   - `normalized_incidence_dt` (data.table): Per-capita normalized incidence (I_i).
#'   - `projected_case_share_dt` (data.table): Projected share of cases.
#'   - `relative_burden_dt` (data.table): Relative within-group burden.
#'   - `mean_contact_matrix_flat_dt` (data.table): The C_ij matrix (flat).
#'   - `mean_contact_array` (array): The C_ij matrix (array).
#'   - `ngm_result` (list): The raw output from `calculate_ngm_stratified`.
#'   - `aggregated_census_dt` (data.table): Census data aggregated for this stratification.
#'   - `error_occurred` (logical): TRUE if an error stopped the full calculation.
#'   - `error_message` (character): The error message if one occurred.
perform_single_stratified_ngm_calculation_set <- function(
    analysis_name,
    iteration_id,
    participants_dt_iter,
    contacts_dt_iter,
    census_dt_iter,
    stratification_vars,
    reference_group_strata,
    reference_group_by_stratum = NULL, # NEW
    all_strata_levels,
    assume_reciprocity = TRUE) {
    log_prefix <- paste0("\n--- Starting NGM Calculation Set: Analysis: '", analysis_name, "', Iteration: ", iteration_id, " ---")
    message(log_prefix)

    # Initialize results list with error defaults
    current_results <- list(
        analysis_name = analysis_name,
        iteration_id = iteration_id,
        r0 = NA_real_,
        eigenvector_dt = data.table(),
        left_eigenvector_dt = data.table(),
        normalized_incidence_dt = data.table(),
        projected_case_share_dt = data.table(),
        relative_burden_dt = data.table(),
        mean_contact_matrix_flat_dt = data.table(),
        mean_contact_array = array(),
        ngm_result = list(),
        aggregated_census_dt = data.table(),
        error_occurred = TRUE, # Default to TRUE
        error_message = "Process not completed"
    )

    tryCatch(
        {
            # 1. Prepare survey columns (factoring, NA removal)
            # Ensure ALL_STRATA_LEVELS_LIST is used here, not individual FINAL_*_LEVELS directly
            survey_cols_prepared <- get_survey_columns_for_stratification(
                participants_dt = participants_dt_iter,
                contacts_dt = contacts_dt_iter,
                stratification_vars = stratification_vars, # Pass the specific s_vars for this analysis
                all_strata_levels = all_strata_levels
            )

            # 2. Simple day-of-week weighting only (avoids demographic weighting complexities)
            # Day of week is never a stratification variable, so this is always safe
            message("Applying day-of-week weighting only (weekday vs weekend sampling bias correction)")
            # Use full participants data here so we always have access to day_week
            population_weights <- create_day_of_week_weights(participants_dt_iter)

            # 3. Calculate participant denominators (with population weighting)
            participant_denominators <- get_participant_denominators_stratified(
                participants_strat_cols = survey_cols_prepared$participants_strat_cols,
                stratification_vars = stratification_vars,
                all_strata_levels = all_strata_levels,
                population_weights = population_weights
            )
            if (nrow(participant_denominators) == 0) {
                stop("Participant denominators table is empty after preparation.")
            }
            current_results$aggregated_census_dt <- census_dt_iter # Store the (already aggregated) census data passed in

            # 4. Calculate contact numerators (with population weighting)
            contact_numerators <- get_contact_numerators_stratified(
                participants_strat_cols = survey_cols_prepared$participants_strat_cols,
                contacts_strat_cols = survey_cols_prepared$contacts_strat_cols,
                stratification_vars = stratification_vars,
                all_strata_levels = all_strata_levels,
                population_weights = population_weights
            )
            # Note: It's possible for contact_numerators to be empty if no contacts exist for the
            #       participant strata that are present. This is not necessarily a fatal error for M_ij_raw calculation
            #       as M_ij_raw would become 0.

            # 5. Aggregate the loaded census data for the specific stratification variables of this analysis.
            aggregated_census_strat <- aggregate_census_population(
                full_census_dt = census_dt_iter, # census_dt_iter is the full data for this config
                stratification_vars = stratification_vars,
                all_strata_levels = all_strata_levels
            )
            current_results$aggregated_census_dt <- aggregated_census_strat # Store the *aggregated* data

            if (nrow(aggregated_census_strat) == 0) {
                stop("Aggregated census table (census_dt_iter) is empty.")
            }

            # 6. Calculate mean contact matrix (C_ij) with reciprocity
            mean_contact_matrix_flat_dt <- calculate_mean_contact_matrix_stratified(
                participant_denominators_dt = participant_denominators,
                contact_numerators_dt = contact_numerators,
                aggregated_census_dt = aggregated_census_strat,
                stratification_vars = stratification_vars,
                all_strata_list = all_strata_levels
            )
            current_results$mean_contact_matrix_flat_dt <- mean_contact_matrix_flat_dt
            if (nrow(mean_contact_matrix_flat_dt) == 0) {
                stop("Mean contact matrix (flat) is empty after calculation.")
            }
            # Check for all NA contact rates
            if (all(is.na(mean_contact_matrix_flat_dt$contact_rate))) {
                stop("All contact rates in the mean contact matrix are NA. Cannot proceed with NGM calculation.")
            }


            # 6. Reshape flat matrix to array
            mean_contact_array_strat <- reshape_flat_matrix_to_array_stratified(
                flat_mean_contact_matrix_dt = mean_contact_matrix_flat_dt,
                stratification_vars = stratification_vars,
                all_strata_list = all_strata_levels
            )
            current_results$mean_contact_array <- mean_contact_array_strat
            if (is.null(mean_contact_array_strat) || any(dim(mean_contact_array_strat) == 0)) {
                stop("Mean contact array reshaping resulted in NULL or zero-dimension array.")
            }

            # 7. Calculate NGM (R0, eigenvector, normalized incidence)
            ngm_outputs <- calculate_ngm_stratified(
                mean_contact_array = mean_contact_array_strat,
                aggregated_census_strat_dt = aggregated_census_strat,
                stratification_vars = stratification_vars,
                all_strata_list = all_strata_levels,
                matrix_name = paste(analysis_name, iteration_id, sep = " - ")
            )
            current_results$ngm_result <- ngm_outputs
            current_results$r0 <- ngm_outputs$r0
            current_results$eigenvector_dt <- ngm_outputs$eigenvector_dt
            current_results$left_eigenvector_dt <- ngm_outputs$left_eigenvector_dt
            current_results$normalized_incidence_dt <- ngm_outputs$normalized_incidence_dt

            if (is.na(ngm_outputs$r0)) {
                message(paste0("  WARNING Iteration ", iteration_id, ": NGM calculation returned NA R0. Error: ", ngm_outputs$error_message))
                # Depending on severity, might stop or allow to continue with NA results
                if (!is.null(ngm_outputs$error_message) && grepl("Eigen decomposition failed|Invalid contact array|Permutation of contact array failed|Total strata count is zero|Dimnames for NGM strata are NULL|Column mismatch for eigenvector strata mapping", ngm_outputs$error_message)) {
                    stop(paste0("Fatal error in NGM calculation: ", ngm_outputs$error_message))
                }
            }


            # 8. Calculate Projected Case Share
            projected_case_share_dt <- calculate_projected_case_share_stratified(
                eigenvector_dt = ngm_outputs$eigenvector_dt,
                aggregated_census_strat_dt = aggregated_census_strat,
                stratification_vars = stratification_vars,
                all_strata_list = all_strata_levels
            )
            current_results$projected_case_share_dt <- projected_case_share_dt


            # 9. Calculate Relative Burden
            if ("NormalizedIncidence" %in% names(ngm_outputs$normalized_incidence_dt) && nrow(ngm_outputs$normalized_incidence_dt) > 0) {
                message("Calculating relative burden for stratification: ", paste(stratification_vars, collapse = ", "))

                stratification_vars_capitalized <- unlist(lapply(stratification_vars, capitalize_first))

                if (!is.null(reference_group_by_stratum) && reference_group_by_stratum %in% stratification_vars) {
                    message("  Calculating relative burden against a reference group within each '", reference_group_by_stratum, "' stratum.")
                    grouping_var_col <- capitalize_first(reference_group_by_stratum)
                    ref_var_name <- setdiff(names(reference_group_strata), tolower(grouping_var_col))
                    if (length(ref_var_name) != 1) stop("When using reference_group_by_stratum, reference_group_strata must contain exactly one variable that is NOT the grouping variable.")
                    ref_val <- reference_group_strata[[ref_var_name]]
                    ref_var_col <- capitalize_first(ref_var_name)

                    reference_incidences <- ngm_outputs$normalized_incidence_dt[get(ref_var_col) == ref_val,
                        .(reference_incidence = NormalizedIncidence),
                        by = grouping_var_col
                    ]
                    relative_burden_dt <- merge(ngm_outputs$normalized_incidence_dt, reference_incidences, by = grouping_var_col, all.x = TRUE)
                    relative_burden_dt[, RelativeBurden := NormalizedIncidence / reference_incidence]
                    relative_burden_dt[, reference_incidence := NULL]
                } else {
                    message("  Calculating relative burden against a single reference group.")
                    ref_group_filter_expr <- paste(sapply(names(reference_group_strata), function(var) {
                        paste0(capitalize_first(var), " == '", reference_group_strata[[var]], "'")
                    }), collapse = " & ")
                    reference_group_incidence_val <- ngm_outputs$normalized_incidence_dt[eval(parse(text = ref_group_filter_expr)), NormalizedIncidence]

                    if (length(reference_group_incidence_val) == 1) {
                        relative_burden_dt <- ngm_outputs$normalized_incidence_dt[, RelativeBurden := NormalizedIncidence / reference_group_incidence_val]
                    } else {
                        warning(paste0("Reference group for relative burden calculation in '", analysis_name, "' did not resolve to a unique stratum. Relative burden will be NA."))
                        relative_burden_dt <- ngm_outputs$normalized_incidence_dt[, RelativeBurden := NA_real_]
                    }
                }
                message("  Calculated relative burden. Rows: ", nrow(relative_burden_dt))
                current_results$relative_burden_dt <- relative_burden_dt
            }

            current_results$error_occurred <- FALSE
            current_results$error_message <- NA_character_
            message(paste0("--- Successfully Completed NGM Calculation Set: '", analysis_name, "', Iteration: ", iteration_id, " ---"))
        },
        error = function(e) {
            message(paste0("!!! ERROR in perform_single_stratified_ngm_calculation_set for Analysis: '", analysis_name, "', Iteration: ", iteration_id, " !!!"))
            message(paste("  Error message: ", e$message))
            # current_results is already initialized with error_occurred = TRUE
            current_results$error_message <- as.character(e$message)
            # Optionally, print stack trace for debugging: print(sys.calls())
        }
    ) # End tryCatch

    return(current_results)
}

#' Symmetrise Mean Contact Matrix
#'
#' Symmetrises the mean contact matrix using population data.
#' This ensures that the total number of contacts from group i to group j equals
#' the total number of contacts from group j to group i.
#'
#' M_ij * N_i = M_ji * N_j, where N is population size.
#'
#' We calculate a symmetric version C_ij = (M_ij * N_i + M_ji * N_j) / 2.
#' The adjusted matrix M'_ij = C_ij / N_i.
#'
#' @param mean_contact_matrix_dt A data.table containing the non-symmetric
#'        mean contacts ('contact_rate') and population sizes ('population_i').
#'        It must have columns for participant strata (e.g., 'part_age_group'),
#'        contact strata (e.g., 'cnt_age_group'), 'contact_rate', and 'population_i'.
#' @param stratification_vars A character vector of stratification variables (e.g., c("Age", "SES")).
#' @return A data.table with the symmetrised mean contact rates.
#' @export
symmetrise_mean_contact_matrix <- function(mean_contact_matrix_dt, stratification_vars) {
    if (!is.data.table(mean_contact_matrix_dt) || nrow(mean_contact_matrix_dt) == 0) {
        warning("Input to symmetrise_mean_contact_matrix is not a valid data.table or is empty.")
        return(data.table())
    }

    # Dynamically create column names
    part_strata_cols <- sapply(stratification_vars, function(v) paste0("part_", tolower(v), ifelse(v == "Age", "_group", "")))
    cnt_strata_cols <- sapply(stratification_vars, function(v) paste0("cnt_", tolower(v), ifelse(v == "Age", "_group", "")))

    # Step 1: Create a copy and rename for merging (M_ji)
    # This creates a data.table with contacts from j's perspective.
    m_ji_dt <- copy(mean_contact_matrix_dt)

    # Rename columns for the merge: part -> cnt and cnt -> part
    # Also rename population_i to population_j for clarity
    setnames(m_ji_dt,
        old = c(part_strata_cols, cnt_strata_cols, "population_i"),
        new = c(cnt_strata_cols, part_strata_cols, "population_j")
    )

    # Select only necessary columns for the merge to avoid conflicts
    m_ji_dt <- m_ji_dt[, c(part_strata_cols, cnt_strata_cols, "contact_rate", "population_j"), with = FALSE]
    setnames(m_ji_dt, "contact_rate", "contact_rate_ji")

    # Step 2: Merge original matrix (M_ij) with the transposed one (M_ji)
    # This aligns rows for i->j with j->i
    merged_dt <- merge(mean_contact_matrix_dt, m_ji_dt, by = c(part_strata_cols, cnt_strata_cols), all = TRUE)

    # Step 3: Calculate the symmetric total contacts (C_ij) and adjusted M'_ij
    # C_ij = (M_ij * N_i + M_ji * N_j) / 2
    # M'_ij = C_ij / N_i
    # Handle NAs in rates and populations: treat as 0 for calculation if the other pair is present
    merged_dt[, contact_rate := ifelse(is.na(contact_rate), 0, contact_rate)]
    merged_dt[, contact_rate_ji := ifelse(is.na(contact_rate_ji), 0, contact_rate_ji)]
    merged_dt[, population_i := ifelse(is.na(population_i), 0, population_i)]
    merged_dt[, population_j := ifelse(is.na(population_j), 0, population_j)]

    merged_dt[, c_ij := (contact_rate * population_i + contact_rate_ji * population_j) / 2]
    merged_dt[, symmetrised_contact_rate := ifelse(population_i > 0, c_ij / population_i, 0)]

    # Final cleanup
    # Keep original strata columns and the new symmetrised rate
    final_cols <- c(part_strata_cols, cnt_strata_cols, "symmetrised_contact_rate")
    symmetrised_dt <- merged_dt[, ..final_cols]
    setnames(symmetrised_dt, "symmetrised_contact_rate", "contact_rate")

    return(symmetrised_dt)
}
