# =============================================================================
# UNIFIED NGM PLOT GENERATION SCRIPT
# =============================================================================
#
# This single script replaces 5 redundant plotting scripts, reducing code
# duplication and ensuring consistency across all plot types.
#
# REPLACED SCRIPTS:
# - create_combined_plots.R               (272 lines) → Consolidated
# - create_combined_plots_with_overall.R  (329 lines) → Consolidated
# - create_univariable_plots.R            (221 lines) → Consolidated
# - generate_report_plots.R               (219 lines) → Consolidated
# - generate_report_plots_simplified.R    (85 lines)  → Consolidated
#
# TOTAL REDUCTION: ~1,126 lines → 1 script with configurable options
#
# USAGE:
# 1. Modify PLOTS_TO_GENERATE variable below to choose what to generate:
#    - "all"            : Generate all plot types
#    - "report"         : Main plots for final report (RR + infection share)
#    - "univariable"    : Simple demographic plots (age, ethnicity, SES, gender)
#    - "stratified"     : Cross-demographic plots (age×ethnicity, etc.)
#    - "rr_with_overall": Relative risk plots with univariable overlay ribbons
#
# 2. Run: Rscript scripts/ngm_analysis/generate_all_plots.R
#
# OUTPUT DIRECTORIES:
# - report_plots/      : Main figures for publication
# - univariable_plots/ : Simple demographic breakdowns
# - combined_plots/    : Complex stratified analyses
#
# =============================================================================

# --- Configuration Parameters ---
# Modify these variables to control what gets generated
PLOTS_TO_GENERATE <- "all" # Options: "all", "report", "univariable", "stratified", "rr_with_overall"
CUSTOM_OUTPUT_DIR <- NULL # NULL = use default directories
CUSTOM_CACHE_DIR <- NULL # NULL = use default cache directory

# Simple options object for compatibility
opt <- list(
    plots = PLOTS_TO_GENERATE,
    `output-dir` = CUSTOM_OUTPUT_DIR,
    `cache-dir` = CUSTOM_CACHE_DIR
)

# --- Load Libraries ---
message("=== UNIFIED NGM PLOT GENERATION ===")
message("Loading required libraries...")
library(data.table)
library(ggplot2)
library(patchwork)
library(here)
library(qs)
library(dplyr)

# --- Source consolidated functions ---
source(here::here("scripts", "ngm_analysis", "plotting_functions.r"))
source(here::here("scripts", "ngm_analysis", "core_analysis_functions.R"))

# --- Configuration ---
MAIN_CACHE_DIR <- if (is.null(opt$`cache-dir`)) here::here("cache", "stratified_ngm_cache") else opt$`cache-dir`
BASE_OUTPUT_DIR <- here::here("results", "stratified_ngm_output")

# Create base output directory
if (!dir.exists(BASE_OUTPUT_DIR)) {
    dir.create(BASE_OUTPUT_DIR, recursive = TRUE)
}

# --- Helper function to determine output directory ---
get_output_dir <- function(plot_type) {
    if (!is.null(opt$`output-dir`)) {
        return(opt$`output-dir`)
    }

    subdir <- switch(plot_type,
        "report" = "report_plots",
        "univariable" = "univariable_plots",
        "stratified" = "combined_plots",
        "rr_with_overall" = "combined_plots",
        "combined_plots" # default
    )

    output_dir <- file.path(BASE_OUTPUT_DIR, subdir)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    return(output_dir)
}

# --- Generate Report Plots (Main plots for final report) ---
generate_report_plots <- function() {
    message("========================================")
    message("GENERATING REPORT PLOTS")
    message("========================================")

    output_dir <- get_output_dir("report")

    # 1. Generate RR plot with overall ribbons
    message("1. Generating Relative Risk plot with Overall ribbons...")
    source(here::here("scripts", "ngm_analysis", "create_combined_plots_with_overall.R"))

    # Copy RR plot to report directory
    combined_plots_dir <- get_output_dir("rr_with_overall")
    for (fmt in c("png", "pdf", "tif", "eps")) {
        file.copy(
            from = file.path(combined_plots_dir, paste0("combined_risk_ratio_with_overall.", fmt)),
            to = file.path(output_dir, paste0("5_relative_risk_plot.", fmt)),
            overwrite = TRUE
        )
    }

    # 2. Generate infection share plot using consolidated function
    message("2. Generating Infection Share plot...")
    generate_infection_share_report(
        cache_dir = MAIN_CACHE_DIR,
        output_dir = output_dir,
        filename_base = "4_infection_share_plot"
    )

    message("Report plots completed!")
    message("Saved to:", output_dir)
}

# --- Generate Univariable Plots ---
generate_univariable_plots <- function() {
    message("========================================")
    message("GENERATING UNIVARIABLE PLOTS")
    message("========================================")

    output_dir <- get_output_dir("univariable")
    configs <- get_analysis_configs("univariable")
    all_results <- load_analysis_results(configs, MAIN_CACHE_DIR, "univariable")

    # Calculate group counts for layout proportions
    group_counts <- sapply(configs, function(config) {
        if (config$name %in% names(all_results)) {
            res <- all_results[[config$name]]
            if (!is.null(res$relative_contribution_summary_dt)) {
                data <- res$relative_contribution_summary_dt
                if (!is.null(config$filter_func)) {
                    data <- config$filter_func(data)
                }
                return(nrow(data))
            }
        }
        return(3) # fallback
    })

    col_widths <- c(max(group_counts[c(1, 3)]), max(group_counts[c(2, 4)]))

    # Generate infection share plots
    message("Generating univariable infection share plots...")
    transmission_plots <- list()

    for (config in configs) {
        if (config$name %in% names(all_results)) {
            res <- all_results[[config$name]]

            if (!is.null(res$projected_case_share_summary_dt)) {
                plot_data <- res$projected_case_share_summary_dt
                if (!is.null(config$filter_func)) {
                    plot_data <- config$filter_func(plot_data)
                }

                if (!is.null(config$levels)) {
                    plot_data[, (config$var) := factor(get(config$var), levels = config$levels)]
                }

                p <- create_demographic_plot(
                    data_dt = plot_data,
                    value_col = "ProjectedCaseShare_mean",
                    ci_lower_col = "ProjectedCaseShare_lowerCI",
                    ci_upper_col = "ProjectedCaseShare_upperCI",
                    x_var = config$var,
                    plot_title = config$title,
                    y_label = if (config$title == "A) By Age") "Projected Share of Infections" else NULL,
                    plot_type = "bars"
                )

                if (!is.null(config$labels)) {
                    p <- p + scale_x_discrete(labels = config$labels)
                }

                transmission_plots[[config$name]] <- p
            }
        }
    }

    # Combine and save
    if (length(transmission_plots) == 4) {
        combined_transmission <- create_combined_plot(
            transmission_plots,
            layout = "grid",
            widths = col_widths
        )

        save_plot_files(
            combined_transmission,
            file.path(output_dir, "univariable_projected_infection_share"),
            width = 12, height = 10,
            message_text = "Saved univariable Projected Share of Infections plot to:"
        )
    }

    # Generate relative risk plots
    message("Generating univariable relative risk plots...")
    risk_ratio_plots <- list()

    for (config in configs) {
        if (config$name %in% names(all_results)) {
            res <- all_results[[config$name]]

            if (!is.null(res$relative_burden_summary_dt)) {
                plot_data <- res$relative_burden_summary_dt
                if (!is.null(config$filter_func)) {
                    plot_data <- config$filter_func(plot_data)
                }

                if (!is.null(config$levels)) {
                    plot_data[, (config$var) := factor(get(config$var), levels = config$levels)]
                }

                ref_val <- res$reference_group_strata[[config$var]]
                plot_data[, is_reference := get(config$var) == ref_val]

                # Use the same shared colour scheme as create_demographic_plot
                fill_label <- if (config$var == "SES") "NS-SeC" else tools::toTitleCase(config$var)
                set2 <- RColorBrewer::brewer.pal(8, "Set2")
                shared_values <- c(
                    "Age"                     = set2[1],
                    "Ethnicity"               = set2[2],
                    "NS-SeC"                  = set2[3],
                    "Gender"                  = set2[4],
                    "NS-SeC and Ethnicity" = set2[5]
                )

                p <- ggplot(plot_data, aes(x = .data[[config$var]], y = RelativeBurden_mean)) +
                    geom_pointrange(
                        data = plot_data[is_reference == FALSE],
                        aes(
                            ymin = RelativeBurden_lowerCI, ymax = RelativeBurden_upperCI,
                            color = fill_label
                        ),
                        size = 0.8
                    ) +
                    scale_color_manual(values = shared_values, name = NULL) +
                    geom_pointrange(
                        data = plot_data[is_reference == TRUE],
                        aes(ymin = RelativeBurden_lowerCI, ymax = RelativeBurden_upperCI),
                        color = "red", shape = 4, size = 1.2
                    ) +
                    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
                    labs(
                        title = config$title,
                        subtitle = format_reference_group(res$reference_group_strata),
                        x = NULL,
                        y = if (config$title == "A) By Age") "Relative Risk of Infection (vs Reference)" else NULL,
                        colour = NULL
                    ) +
                    get_common_theme() +
                    scale_y_log10(labels = number_format(accuracy = 0.1))

                if (!is.null(config$labels)) {
                    p <- p + scale_x_discrete(labels = config$labels)
                }

                risk_ratio_plots[[config$name]] <- p
            }
        }
    }

    # Combine and save
    if (length(risk_ratio_plots) == 4) {
        combined_risk_ratio <- create_combined_plot(
            risk_ratio_plots,
            layout = "grid",
            widths = col_widths
        )

        save_plot_files(
            combined_risk_ratio,
            file.path(output_dir, "univariable_risk_ratio"),
            width = 12, height = 10,
            message_text = "Saved univariable Relative Risk of Infection plot to:"
        )
    }

    message("Univariable plots completed!")
    message("Saved to:", output_dir)
}

# --- Generate Stratified Plots (without overall ribbons) ---
generate_stratified_plots <- function() {
    message("========================================")
    message("GENERATING STRATIFIED PLOTS")
    message("========================================")

    output_dir <- get_output_dir("stratified")

    # Generate infection share plots using consolidated function
    message("Generating stratified infection share plots...")
    plots <- generate_infection_share_plots(MAIN_CACHE_DIR, output_dir)

    if (length(plots) >= 4) {
        configs <- get_analysis_configs("stratified")
        all_results <- load_analysis_results(configs, MAIN_CACHE_DIR, "stratified")
        facet_counts <- calculate_facet_counts(all_results, configs, "projected_case_share_summary_dt", FALSE)

        combined_plot <- create_report_layout(plots, facet_counts)

        save_plot_files(
            combined_plot,
            file.path(output_dir, "combined_projected_infection_share"),
            width = 20, height = 14,
            message_text = "Saved combined Projected Share of Infections plot to:"
        )
    }

    # Generate RR plots (basic, without overall ribbons) using consolidated config
    message("Generating stratified relative risk plots...")
    configs <- get_analysis_configs("stratified")
    all_results <- load_analysis_results(configs, MAIN_CACHE_DIR, "stratified")

    plots <- list()
    for (config in configs) {
        if (config$name %in% names(all_results)) {
            res <- all_results[[config$name]]
            if (!is.null(res$relative_burden_summary_dt) && nrow(res$relative_burden_summary_dt) > 0) {
                plot_data <- copy(res$relative_burden_summary_dt)
                if (!is.null(config$filter_func)) {
                    plot_data <- config$filter_func(plot_data)
                }
                # Determine axes
                x_variable <- config$x_var %||% "Age"
                facet_var <- config$facet_var %||% NULL
                # Build plot
                p <- create_demographic_plot(
                    data_dt = plot_data,
                    value_col = "RelativeBurden_mean",
                    ci_lower_col = if ("RelativeBurden_lowerCI" %in% names(plot_data)) "RelativeBurden_lowerCI" else NULL,
                    ci_upper_col = if ("RelativeBurden_upperCI" %in% names(plot_data)) "RelativeBurden_upperCI" else NULL,
                    x_var = x_variable,
                    facet_var = facet_var,
                    plot_title = paste0(config$title %||% paste("By", paste(c(x_variable, facet_var), collapse = " × "))),
                    y_label = "Relative Risk of Infection (vs Reference)",
                    plot_type = "pointrange",
                    use_log_scale = TRUE
                )
                plots[[config$name]] <- p
            }
        }
    }

    if (length(plots) > 0) {
        combined <- create_combined_plot(plots, layout = "grid")
        save_plot_files(
            combined,
            file.path(output_dir, "combined_risk_ratio_basic"),
            width = 20, height = 14,
            message_text = "Saved combined stratified Relative Risk plot to:"
        )
    }

    message("Stratified plots completed!")
    message("Saved to:", output_dir)
}

# --- Generate RR Plots with Overall Ribbons ---
generate_rr_with_overall <- function() {
    message("========================================")
    message("GENERATING RR PLOTS WITH OVERALL RIBBONS")
    message("========================================")

    # This uses the existing comprehensive script
    source(here::here("scripts", "ngm_analysis", "create_combined_plots_with_overall.R"))

    message("RR plots with overall ribbons completed!")
    message("Saved to:", get_output_dir("rr_with_overall"))
}

# --- Main execution logic ---
plots_to_generate <- tolower(opt$plots)

if (plots_to_generate == "all") {
    message("Generating ALL plot types...")
    generate_report_plots()
    generate_univariable_plots()
    generate_stratified_plots()
    generate_rr_with_overall()
} else if (plots_to_generate == "report") {
    generate_report_plots()
} else if (plots_to_generate == "univariable") {
    generate_univariable_plots()
} else if (plots_to_generate == "stratified") {
    generate_stratified_plots()
} else if (plots_to_generate == "rr_with_overall") {
    generate_rr_with_overall()
} else {
    stop("Invalid --plots option. Choose: 'all', 'report', 'univariable', 'stratified', or 'rr_with_overall'")
}

message("========================================")
message("UNIFIED PLOT GENERATION COMPLETE!")
message("========================================")
message("Generated plots for:", plots_to_generate)
message("Check output directories under:", BASE_OUTPUT_DIR)
