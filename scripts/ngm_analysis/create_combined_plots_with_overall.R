# Combined RR Plots with "Overall" Category
# Creates RR plots that include univariable results as "Overall" within each facet

# --- Load Libraries ---
message("Loading required libraries...")
library(data.table)
library(ggplot2)
library(patchwork)
library(here)
library(qs)
library(dplyr)

# --- Source helper functions ---
source(here::here("scripts", "ngm_analysis", "plotting_functions.r"))
source(here::here("scripts", "ngm_analysis", "core_analysis_functions.R"))

# --- Helper functions to reduce repetition ---

# Exclude reference groups from data
exclude_reference_groups <- function(data_dt, config, reference_group_strata) {
    # Exclude age reference groups (for age-stratified analyses)
    if (!is.null(reference_group_strata$Age)) {
        data_dt <- data_dt[Age != reference_group_strata$Age]
    }

    # Exclude facet variable reference groups
    if (config$facet_var == "Ethnicity" && !is.null(reference_group_strata$Ethnicity)) {
        data_dt <- data_dt[Ethnicity != reference_group_strata$Ethnicity]
    } else if (config$facet_var == "SES" && !is.null(reference_group_strata$SES)) {
        data_dt <- data_dt[SES != reference_group_strata$SES]
    } else if (config$facet_var == "Gender" && !is.null(reference_group_strata$Gender)) {
        data_dt <- data_dt[Gender != reference_group_strata$Gender]
    }

    # For Eth_SES_Stratified, also exclude reference ethnicity groups
    if (config$name == "Eth_SES_Stratified" && !is.null(reference_group_strata$Ethnicity)) {
        data_dt <- data_dt[Ethnicity != reference_group_strata$Ethnicity]
    }

    return(data_dt)
}

# Exclude reference groups for univariable data
exclude_univariable_references <- function(data_dt, univar_var, reference_group_strata) {
    if (univar_var == "Ethnicity" && !is.null(reference_group_strata$Ethnicity)) {
        data_dt <- data_dt[Ethnicity != reference_group_strata$Ethnicity]
    } else if (univar_var == "SES" && !is.null(reference_group_strata$SES)) {
        data_dt <- data_dt[SES != reference_group_strata$SES]
    } else if (univar_var == "Gender" && !is.null(reference_group_strata$Gender)) {
        data_dt <- data_dt[Gender != reference_group_strata$Gender]
    }
    return(data_dt)
}

# Prepare stratified data (filter, exclude references, filter positive values)
prepare_stratified_data <- function(data_dt, config, reference_group_strata) {
    # Filter positive values for log scale
    data_dt <- data_dt[RelativeBurden_mean > 0 & RelativeBurden_lowerCI > 0]

    # Apply filtering if specified
    if (!is.null(config$filter_func)) {
        data_dt <- config$filter_func(data_dt)
    }

    # Exclude reference categories
    data_dt <- exclude_reference_groups(data_dt, config, reference_group_strata)

    return(data_dt)
}

# Prepare univariable data (apply filtering, exclude references)
prepare_univariable_data <- function(data_dt, config, univar_var, reference_group_strata) {
    # Apply filtering if specified
    if (!is.null(config$filter_func)) {
        data_dt <- config$filter_func(data_dt)
    }

    # Exclude reference categories
    data_dt <- exclude_univariable_references(data_dt, univar_var, reference_group_strata)

    return(data_dt)
}

# --- Configuration ---
MAIN_CACHE_DIR <- here::here("cache", "stratified_ngm_cache")
MAIN_OUTPUT_DIR <- here::here("results", "stratified_ngm_output", "combined_plots")

if (!dir.exists(MAIN_OUTPUT_DIR)) {
    dir.create(MAIN_OUTPUT_DIR, recursive = TRUE)
}

# Define which analyses to include
ANALYSIS_CONFIG <- list(
    list(
        name = "Age_Eth_Stratified_5y",
        univariable_name = "Eth_Only_Stratified",
        facet_var = "Ethnicity",
        title = "A) By Age & Ethnicity"
    ),
    list(
        name = "Age_SES_Stratified_5y",
        univariable_name = "SES_Only_Stratified",
        facet_var = "SES",
        title = "C) By Age & NS-SeC",
        filter_func = filter_ses_data,
        facet_labels = get_ses_labels()
    ),
    list(
        name = "Age_Gender_Stratified_5y",
        univariable_name = "Gender_Only_Stratified",
        facet_var = "Gender",
        title = "B) By Age & Gender",
        filter_func = filter_gender_data
    ),
    list(
        name = "Eth_SES_Stratified",
        univariable_name = "Eth_Only_Stratified",
        facet_var = "Ethnicity",
        title = "D) By NS-SeC & Ethnicity",
        filter_func = filter_eth_ses_data,
        x_axis_labels = get_ses_labels() # For SES labels on x-axis
    )
)

# --- Load Stratified Data ---
message("Loading stratified results data...")
stratified_results <- list()
for (config in ANALYSIS_CONFIG) {
    file_path <- file.path(MAIN_CACHE_DIR, config$name, paste0(config$name, "_consolidated_results.qs"))
    if (file.exists(file_path)) {
        message(paste("  Loading stratified:", config$name))
        stratified_results[[config$name]] <- qs::qread(file_path)
    } else {
        warning(paste("Could not find stratified results for:", config$name))
    }
}

# --- Load Univariable Data ---
message("Loading univariable results data...")
univariable_results <- list()
for (config in ANALYSIS_CONFIG) {
    file_path <- file.path(MAIN_CACHE_DIR, config$univariable_name, paste0(config$univariable_name, "_consolidated_results.qs"))
    if (file.exists(file_path)) {
        message(paste("  Loading univariable:", config$univariable_name))
        univariable_results[[config$univariable_name]] <- qs::qread(file_path)
    } else {
        warning(paste("Could not find univariable results for:", config$univariable_name))
    }
}

# --- Generate Combined RR Plots with Overall ---
message("Generating Combined RR plots with Overall category...")
combined_rr_overall_plots <- list()

for (i in seq_along(ANALYSIS_CONFIG)) {
    config <- ANALYSIS_CONFIG[[i]]

    if (config$name %in% names(stratified_results) &&
        config$univariable_name %in% names(univariable_results)) {
        strat_res <- stratified_results[[config$name]]
        univ_res <- univariable_results[[config$univariable_name]]

        if (!is.null(strat_res$relative_burden_summary_dt) &&
            !is.null(univ_res$relative_burden_summary_dt)) {
            # Get and prepare stratified data
            strat_data <- prepare_stratified_data(
                strat_res$relative_burden_summary_dt,
                config,
                strat_res$reference_group_strata
            )

            # For Eth_SES_Stratified, the univariable is by Ethnicity (for the ribbon)
            univar_var <- if (config$name == "Eth_SES_Stratified") "Ethnicity" else config$facet_var

            # Get and prepare univariable data
            univ_data <- prepare_univariable_data(
                univ_res$relative_burden_summary_dt,
                config,
                univar_var,
                univ_res$reference_group_strata
            )

            # Create title and subtitle separately
            title_text <- config$title
            subtitle_text <- format_reference_group(strat_res$reference_group_strata)

            # Check if both datasets have data
            if (nrow(strat_data) > 0 && nrow(univ_data) > 0) {
                # Determine x-axis variable
                x_var <- if (config$name == "Eth_SES_Stratified") "SES" else "Age"

                # Create combined plot
                p <- create_combined_rr_with_overall(
                    stratified_data = strat_data,
                    univariable_data = univ_data,
                    facet_var = config$facet_var,
                    title_text = title_text,
                    subtitle_text = subtitle_text,
                    facet_labels = config$facet_labels,
                    x_var = x_var,
                    x_axis_labels = config$x_axis_labels
                )

                if (!is.null(p)) {
                    combined_rr_overall_plots[[length(combined_rr_overall_plots) + 1]] <- p
                }
            } else {
                message(paste("Insufficient data for combined plot:", config$name, "- skipping"))
            }
        } else {
            message(paste("Missing relative_burden_summary_dt for:", config$name, "or", config$univariable_name))
        }
    }
}

# --- Save Combined Plots ---
if (length(combined_rr_overall_plots) > 0) {
    # Calculate proportions for layout (accounting for removed reference categories)
    facet_counts <- sapply(ANALYSIS_CONFIG, function(config) {
        if (config$name %in% names(stratified_results)) {
            res <- stratified_results[[config$name]]
            if (!is.null(res$relative_burden_summary_dt)) {
                data <- res$relative_burden_summary_dt

                # Apply same filtering as used in the actual plots
                if (!is.null(config$filter_func)) {
                    data <- config$filter_func(data)
                }

                # Exclude reference categories (same logic as in plot generation)
                data <- exclude_reference_groups(data, config, res$reference_group_strata)

                return(length(unique(data[[config$facet_var]])))
            }
        }
        return(3) # fallback
    })

    # Custom vertical layout: Age&Eth + Age&Gender on top row, Age&SES on second row, Eth&SES on bottom
    if (length(combined_rr_overall_plots) >= 4) {
        # Identify plots: Age&Eth=1, Age&SES=2, Age&Gender=3, Eth&SES=4
        p_age_eth <- combined_rr_overall_plots[[1]] # A) By Age & Ethnicity
        p_age_ses <- combined_rr_overall_plots[[2]] # B) By Age & NS-SeC
        p_age_gender <- combined_rr_overall_plots[[3]] # C) By Age & Gender
        p_eth_ses <- combined_rr_overall_plots[[4]] # D) By Ethnicity & NS-SeC

        # Remove y-axis label from Age & Gender (right side of top row)
        p_age_gender_no_y <- p_age_gender + theme(axis.title.y = element_blank())

        # Get facet counts for proportional widths
        age_eth_facets <- facet_counts[1] # Ethnicity facets
        age_gender_facets <- facet_counts[3] # Gender facets

        # Create top row (Age & Ethnicity + Age & Gender)
        top_row <- wrap_plots(p_age_eth, p_age_gender_no_y,
            nrow = 1,
            widths = c(age_eth_facets, age_gender_facets)
        ) + plot_layout(guides = "collect")

        # Create custom vertical layout
        combined_rr_overall_vertical <- wrap_plots(
            top_row, # Row 1: Age & Ethnicity + Age & Gender
            p_age_ses, # Row 2: Age & NS-SeC (full width)
            p_eth_ses, # Row 3: Ethnicity & NS-SeC (full width)
            ncol = 1
        ) +
            plot_layout(guides = "collect") +
            add_caption_annotation() &
            theme(legend.position = "bottom", legend.direction = "horizontal")
    } else {
        # Fallback to simple vertical layout if fewer than 4 plots
        combined_rr_overall_vertical <- create_combined_plot(
            combined_rr_overall_plots,
            layout = "vertical"
        ) +
            add_caption_annotation()
    }

    save_plot_files(
        combined_rr_overall_vertical,
        file.path(MAIN_OUTPUT_DIR, "combined_risk_ratio_with_overall"),
        width = 20, height = 21,
        dpi = 300,
        formats = c("png", "pdf", "tif", "eps"),
        message_text = "Saved combined RR plot with Overall category to:"
    )

    # 2x2 Grid layout
    if (length(combined_rr_overall_plots) >= 4) {
        # Remove y-axis labels from right plots
        p1 <- combined_rr_overall_plots[[1]] # Top-left: Age & Ethnicity
        p2 <- combined_rr_overall_plots[[2]] + theme(axis.title.y = element_blank()) # Top-right: Age & NS-SeC
        p3 <- combined_rr_overall_plots[[3]] # Bottom-left: Age & Gender
        p4 <- combined_rr_overall_plots[[4]] + theme(axis.title.y = element_blank()) # Bottom-right: Ethnicity & NS-SeC

        # Create 2x2 layout
        combined_grid <- wrap_plots(p1, p2, p3, p4, ncol = 2) +
            plot_layout(guides = "collect") +
            add_titled_caption_annotation("Relative Risk of Infection by Demographic Group (Including Overall)")

        save_plot_files(
            combined_grid,
            file.path(MAIN_OUTPUT_DIR, "combined_risk_ratio_with_overall_2x2"),
            width = 20, height = 13,
            message_text = "Saved 2x2 Grid Layout RR plot with Overall to:"
        )
    }

    # Horizontal layout (for backward compatibility)
    combined_rr_overall_plots_wide <- combined_rr_overall_plots
    for (i in 2:length(combined_rr_overall_plots_wide)) {
        combined_rr_overall_plots_wide[[i]] <- combined_rr_overall_plots_wide[[i]] +
            theme(axis.title.y = element_blank())
    }

    combined_rr_overall_horizontal <- create_combined_plot(
        combined_rr_overall_plots_wide,
        layout = "horizontal",
        widths = facet_counts
    ) +
        add_caption_annotation()

    save_plot_files(
        combined_rr_overall_horizontal,
        file.path(MAIN_OUTPUT_DIR, "combined_risk_ratio_with_overall_wide"),
        width = 36, height = 8,
        message_text = "Saved WIDE combined RR plot with Overall category to:"
    )
}

message("Combined plot generation with Overall category finished.")
