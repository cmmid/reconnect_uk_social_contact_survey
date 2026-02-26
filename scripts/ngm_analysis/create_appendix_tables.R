# Create appendix tables for stratified NGM analysis results
# This script creates tables showing infection shares and risk ratios for
# one-way and two-way stratified analyses

# Load required libraries
library(here)
library(qs)
library(data.table)
library(knitr)
library(dplyr)
library(openxlsx)

# Load the analysis results
results_path <- here::here("results", "stratified_ngm_results_inline", "all_stratified_analyses_summary_for_quarto.qs")
all_analyses_summaries <- qs::qread(results_path)

# Print available analyses to understand structure
cat("Available analyses:\n")
print(names(all_analyses_summaries))

# Function to extract and format results for table
extract_table_data <- function(analysis_name, summary_data) {
    if (!analysis_name %in% names(summary_data)) {
        cat(paste("Analysis", analysis_name, "not found\n"))
        return(NULL)
    }

    analysis_result <- summary_data[[analysis_name]]

    if (is.null(analysis_result) || !is.null(analysis_result$status)) {
        cat(paste("Analysis", analysis_name, "has no valid results\n"))
        return(NULL)
    }

    # Extract projected case share data
    case_share_dt <- analysis_result$projected_case_share_summary_dt

    # Extract relative burden data
    relative_burden_dt <- analysis_result$relative_burden_summary_dt

    # Get stratification variables
    strat_vars <- analysis_result$stratification_vars

    # Merge the datasets
    if (!is.null(case_share_dt) && nrow(case_share_dt) > 0 &&
        !is.null(relative_burden_dt) && nrow(relative_burden_dt) > 0) {
        # Create group identifier for merging
        case_share_dt <- copy(case_share_dt)
        relative_burden_dt <- copy(relative_burden_dt)

        # Create group column for merging
        case_share_dt[, Group := do.call(paste, c(.SD, sep = ", ")), .SDcols = strat_vars]
        relative_burden_dt[, Group := do.call(paste, c(.SD, sep = ", ")), .SDcols = strat_vars]

        # Merge the data
        merged_dt <- merge(
            case_share_dt[, .(Group,
                InfectionShare = ProjectedCaseShare_mean,
                InfectionShare_CI_lower = ProjectedCaseShare_lowerCI,
                InfectionShare_CI_upper = ProjectedCaseShare_upperCI
            )],
            relative_burden_dt[, .(Group,
                RiskRatio = RelativeBurden_mean,
                RiskRatio_CI_lower = RelativeBurden_lowerCI,
                RiskRatio_CI_upper = RelativeBurden_upperCI
            )],
            by = "Group",
            all.x = TRUE
        )

        # Apply analysis-specific filtering
        if (analysis_name == "Age_SES_Stratified_5y") {
            # For Age x NS-SeC: only working-age (20-64) and working NS-SeC categories (1-7)
            working_age_pattern <- "(20-24|25-29|30-34|35-39|40-44|45-49|50-54|55-59|60-64)"
            working_ses_pattern <- ", [1-7]$"
            merged_dt <- merged_dt[
                grepl(working_age_pattern, Group) &
                    grepl(working_ses_pattern, Group) &
                    !is.na(RiskRatio)
            ]
        } else {
            # For other analyses: only filter out unknowns and missing data
            merged_dt <- merged_dt[
                !is.na(RiskRatio) &
                    !grepl("Unknown", Group, ignore.case = TRUE)
            ]
        }

        # Format for display
        merged_dt[, InfectionShare_formatted := sprintf(
            "%.1f%% (%.1f-%.1f)",
            InfectionShare * 100,
            InfectionShare_CI_lower * 100,
            InfectionShare_CI_upper * 100
        )]

        # Handle risk ratio formatting (some may be NA for reference groups)
        merged_dt[!is.na(RiskRatio), RiskRatio_formatted := sprintf(
            "%.2f (%.2f-%.2f)",
            RiskRatio,
            RiskRatio_CI_lower,
            RiskRatio_CI_upper
        )]
        merged_dt[is.na(RiskRatio), RiskRatio_formatted := "Reference"]

        # Add analysis information
        merged_dt[, Analysis := analysis_name]
        merged_dt[, StratificationVars := paste(strat_vars, collapse = " × ")]

        return(merged_dt[, .(Analysis, StratificationVars, Group, InfectionShare_formatted, RiskRatio_formatted)])
    } else {
        cat(paste("No valid data tables found for", analysis_name, "\n"))
        return(NULL)
    }
}

# Define one-way analyses
oneway_analyses <- c("Age_Only_Stratified_5y", "Eth_Only_Stratified", "SES_Only_Stratified", "Gender_Only_Stratified")

# Define two-way analyses
twoway_analyses <- c("Age_Eth_Stratified_5y", "Age_SES_Stratified_5y", "Age_Gender_Stratified_5y", "Eth_SES_Stratified")

# Extract data for one-way analyses
oneway_results <- list()
for (analysis in oneway_analyses) {
    result <- extract_table_data(analysis, all_analyses_summaries)
    if (!is.null(result)) {
        oneway_results[[analysis]] <- result
    }
}

# Extract data for two-way analyses
twoway_results <- list()
for (analysis in twoway_analyses) {
    result <- extract_table_data(analysis, all_analyses_summaries)
    if (!is.null(result)) {
        twoway_results[[analysis]] <- result
    }
}

# Create Excel workbook with multiple sheets
wb <- createWorkbook()

# Sheet 1: One-way analyses
if (length(oneway_results) > 0) {
    oneway_table <- rbindlist(oneway_results)

    # Clean up analysis names for display
    oneway_table[, Analysis_display := case_when(
        Analysis == "Age_Only_Stratified_5y" ~ "Age only",
        Analysis == "Eth_Only_Stratified" ~ "Ethnicity only",
        Analysis == "SES_Only_Stratified" ~ "NS-SeC only",
        Analysis == "Gender_Only_Stratified" ~ "Gender only",
        TRUE ~ Analysis
    )]

    # Print one-way table
    cat("\n=== ONE-WAY STRATIFIED ANALYSIS RESULTS ===\n")
    print(oneway_table[, .(Analysis_display, Group, InfectionShare_formatted, RiskRatio_formatted)])

    # Add one-way results to Excel
    addWorksheet(wb, "One-way Analyses")
    writeData(
        wb, "One-way Analyses",
        oneway_table[, .(
            Analysis = Analysis_display, Group,
            `Infection Share (95% CI)` = InfectionShare_formatted,
            `Risk Ratio (95% CI)` = RiskRatio_formatted
        )]
    )

    # Also save as CSV for backward compatibility
    oneway_output_path <- here::here("results", "oneway_stratified_table.csv")
    fwrite(
        oneway_table[, .(
            Analysis = Analysis_display, Group,
            `Infection Share (95% CI)` = InfectionShare_formatted,
            `Risk Ratio (95% CI)` = RiskRatio_formatted
        )],
        oneway_output_path
    )
    cat(paste("\nOne-way table saved to:", oneway_output_path, "\n"))
}

# Process two-way analyses - create separate sheets for each analysis
if (length(twoway_results) > 0) {
    twoway_table <- rbindlist(twoway_results)

    # Clean up analysis names for display
    twoway_table[, Analysis_display := case_when(
        Analysis == "Age_Eth_Stratified_5y" ~ "Age × Ethnicity",
        Analysis == "Age_SES_Stratified_5y" ~ "Age × NS-SeC",
        Analysis == "Age_Gender_Stratified_5y" ~ "Age × Gender",
        Analysis == "Eth_SES_Stratified" ~ "Ethnicity × NS-SeC",
        TRUE ~ Analysis
    )]

    # Create separate sheets for each two-way analysis
    for (analysis in unique(twoway_table$Analysis_display)) {
        analysis_data <- twoway_table[Analysis_display == analysis]

        # Clean sheet name for Excel (replace special characters)
        sheet_name <- gsub("[×]", "x", analysis)
        sheet_name <- gsub("[^A-Za-z0-9 -]", "", sheet_name)

        addWorksheet(wb, sheet_name)
        writeData(
            wb, sheet_name,
            analysis_data[, .(
                Group,
                `Infection Share (95% CI)` = InfectionShare_formatted,
                `Risk Ratio (95% CI)` = RiskRatio_formatted
            )]
        )

        cat(paste("\nAdded", analysis, "to Excel sheet:", sheet_name, "\n"))
    }

    # Print summary
    cat("\n=== TWO-WAY ANALYSES: TOP 10 BY INFECTION SHARE ===\n")
    for (analysis in unique(twoway_table$Analysis_display)) {
        analysis_data <- twoway_table[Analysis_display == analysis]
        # Sort by infection share (extract numeric value)
        analysis_data[, InfectionShare_numeric := as.numeric(gsub("%.*", "", InfectionShare_formatted))]
        top_10 <- head(analysis_data[order(-InfectionShare_numeric)], 10)

        cat(paste("\n", analysis, "- Top 10 by Infection Share:\n"))
        print(top_10[, .(Group, InfectionShare_formatted)])
    }
}

# Add a summary sheet with metadata
addWorksheet(wb, "README")
readme_content <- data.frame(
    Information = c(
        "Data Source",
        "Analysis Method",
        "Bootstrap Iterations",
        "Reference Groups",
        "Confidence Intervals",
        "Date Generated",
        "",
        "Sheet Contents:",
        "- One-way Analyses",
        "- Age x Ethnicity",
        "- Age x NS-SeC",
        "- Age x Gender",
        "- Ethnicity x NS-SeC",
        "- Contact Matrices (if available)"
    ),
    Description = c(
        "UK Contact Survey stratified next-generation matrix analysis",
        "Bootstrap resampling with stratified NGM calculation",
        "N=1000",
        "White ethnicity, NS-SeC 1, Female gender",
        "95% confidence intervals from bootstrap distribution",
        as.character(Sys.Date()),
        "",
        "",
        "Univariable analyses by demographic group (Age, Ethnicity, NS-SeC, Gender)",
        "Two-way analysis: Age (5-year bands) × Ethnicity",
        "Two-way analysis: Age (5-year bands) × NS-SeC",
        "Two-way analysis: Age (5-year bands) × Gender",
        "Two-way analysis: Ethnicity × NS-SeC (no age stratification)",
        "Raw contact matrices (mixing patterns) for each stratified analysis"
    )
)
writeData(wb, "README", readme_content)

# --- Export mixing pattern tables (contact matrices) ---
# Load cached results to get raw contact matrix data
cat("\n=== EXPORTING MIXING PATTERN TABLES ===\n")
cache_dir <- here::here("cache", "stratified_ngm_cache")

all_analyses <- c(oneway_analyses, twoway_analyses)
mixing_pattern_list <- list()

for (analysis_name in all_analyses) {
    cache_file <- file.path(cache_dir, analysis_name, paste0(analysis_name, "_consolidated_results.qs"))

    if (file.exists(cache_file)) {
        cat(paste("  Loading contact matrix for:", analysis_name, "\n"))
        cached_result <- tryCatch(qs::qread(cache_file), error = function(e) NULL)

        if (!is.null(cached_result) && !is.null(cached_result$mean_contact_matrix_flat_summary_dt)) {
            matrix_dt <- copy(cached_result$mean_contact_matrix_flat_summary_dt)

            # Rename columns for clarity
            if (nrow(matrix_dt) > 0) {
                # Add analysis name column
                matrix_dt[, Analysis := analysis_name]

                # Add to list for combined export
                mixing_pattern_list[[analysis_name]] <- matrix_dt

                # Create sheet name for this analysis
                sheet_name <- paste0("Matrix ", gsub("_Stratified.*", "", analysis_name))
                sheet_name <- gsub("_", " ", sheet_name)
                sheet_name <- substr(sheet_name, 1, 31) # Excel sheet name limit

                # Add sheet to workbook
                addWorksheet(wb, sheet_name)
                writeData(wb, sheet_name, matrix_dt)
                cat(paste("    Added mixing pattern table:", sheet_name, "\n"))
            }
        }
    }
}

# Also save all mixing patterns as a single CSV file
if (length(mixing_pattern_list) > 0) {
    all_mixing_patterns <- rbindlist(mixing_pattern_list, fill = TRUE)
    mixing_csv_path <- here::here("results", "mixing_pattern_tables.csv")
    fwrite(all_mixing_patterns, mixing_csv_path)
    cat(paste("\nMixing pattern tables saved to:", mixing_csv_path, "\n"))
}

# Save Excel file
excel_output_path <- here::here("results", "stratified_ngm_analysis_results.xlsx")
saveWorkbook(wb, excel_output_path, overwrite = TRUE)
cat(paste("\n=== EXCEL FILE CREATED ===\n"))
cat(paste("Complete results saved to:", excel_output_path, "\n"))
cat("Sheets created:\n")
cat("- README (metadata and notes)\n")
cat("- One-way Analyses\n")
cat("- Age x Ethnicity\n")
cat("- Age x NS-SeC\n")
cat("- Age x Gender\n")
cat("- Ethnicity x NS-SeC\n")

cat("\n=== SCRIPT COMPLETED ===\n")
