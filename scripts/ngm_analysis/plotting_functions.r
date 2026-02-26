# Unified Plotting Functions for NGM Analysis
# This file contains all plotting functions used across the NGM analysis pipeline
# Combines functions from plotting_helpers.R, plotting_functions.r, and plotting_functions_new.r

library(ggplot2)
library(data.table)
library(scales)
library(patchwork)
library(forcats)
library(RColorBrewer)

# Global variables to avoid R CMD check notes
globalVariables(c(
    "SES", "Gender", "Stratum", "RelativeBurden_mean", "RelativeBurden_lowerCI",
    "RelativeBurden_upperCI", "Age", "is_overall", ".x", "ymin", "ymax", "y_center",
    "Ethnicity"
))

# --- Standardized plot saving function ---
#' Save plot in multiple formats (PNG, PDF, TIF, EPS)
#' @param plot The ggplot or patchwork plot object
#' @param filename_base Base filename without extension
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution for raster formats (default 300)
#' @param formats Character vector of formats to save ("png", "pdf", "tif", "eps")
#' @param message_text Optional message to display
save_plot_files <- function(plot, filename_base, width = 10, height = 7, dpi = 300,
                            formats = c("png", "pdf"), message_text = "Saved plot to:") {
    for (fmt in formats) {
        filename <- paste0(filename_base, ".", fmt)

        if (fmt == "png") {
            ggsave(filename,
                plot = plot, width = width, height = height,
                dpi = dpi, bg = "white", limitsize = FALSE
            )
        } else if (fmt == "pdf") {
            ggsave(filename,
                plot = plot, width = width, height = height,
                dpi = dpi, bg = "white", limitsize = FALSE
            )
        } else if (fmt == "tif" || fmt == "tiff") {
            ggsave(filename,
                plot = plot, width = width, height = height,
                dpi = dpi, device = "tiff", compression = "lzw",
                bg = "white", limitsize = FALSE
            )
        } else if (fmt == "eps") {
            ggsave(filename,
                plot = plot, width = width, height = height,
                device = cairo_ps, bg = "white", limitsize = FALSE
            )
        }
    }

    message(paste(message_text, paste0(filename_base, ".{", paste(formats, collapse = ","), "}")))
}

# --- Common plot elements (defined once) ---
RIBBON_CAPTION_TEXT <- "Coloured ribbons shows univariable (overall) result"

# Standard y-axis scale for log plots
get_log_y_scale <- function(accuracy = 0.1, use_scientific = FALSE) {
    scale_y_log10(
        breaks = scales::log_breaks(n = 7),
        labels = if (use_scientific) scales::trans_format("log10", scales::math_format(10^.x)) else number_format(accuracy = accuracy)
    )
}

# Standard x-axis scale for log plots
get_log_x_scale <- function(decimal_labels = FALSE) {
    scale_x_log10(
        breaks = scales::log_breaks(n = 5),
        labels = if (decimal_labels) scales::number_format(accuracy = 0.01) else scales::trans_format("log10", scales::math_format(10^.x))
    )
}

# Standard caption theme
get_caption_theme <- function() {
    theme(plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))
}

# Standard plot annotation with caption
add_caption_annotation <- function() {
    plot_annotation(
        caption = RIBBON_CAPTION_TEXT,
        theme = get_caption_theme()
    )
}

# Plot annotation with title and caption (for 2x2 grid)
add_titled_caption_annotation <- function(title_text) {
    plot_annotation(
        title = title_text,
        caption = RIBBON_CAPTION_TEXT,
        theme = theme(
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50")
        )
    )
}

# --- Common plotting configuration ---
get_common_theme <- function() {
    theme_bw(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = "gray90", linewidth = 0.2),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black", linewidth = 0.5),
            axis.ticks = element_line(colour = "black", linewidth = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.margin = margin(t = 0, b = 0),
            strip.text = element_text(face = "bold", size = 11),
            strip.background = element_blank()
        )
}

# --- Color palette assignment for different demographics ---
get_demographic_palette <- function(var_name, factor_levels) {
    # Map demographic variables to specific colors from Set2 palette
    set2_colors <- RColorBrewer::brewer.pal(8, "Set2")

    color_map <- list(
        "Age" = set2_colors[1], # First color from Set2
        "Ethnicity" = set2_colors[2], # Second color from Set2
        "SES" = set2_colors[3], # Third color from Set2
        "Gender" = set2_colors[4], # Fourth color from Set2
        "Overall" = set2_colors[5] # Fifth color from Set2
    )

    # Get the color for this demographic, default to first Set2 color if not found
    demographic_color <- color_map[[var_name]]
    if (is.null(demographic_color)) {
        demographic_color <- set2_colors[1]
    }

    # Create named vector with the same color for all levels
    colors <- rep(demographic_color, length(factor_levels))
    names(colors) <- factor_levels

    return(colors)
}

# --- SES labels for plots ---
get_ses_labels <- function() {
    c(
        `1` = "1. Higher Managerial/\nProfessional",
        `2` = "2. Lower Managerial/\nProfessional",
        `3` = "3. Intermediate",
        `4` = "4. Small Employers",
        `5` = "5. Lower Supervisory/\nTechnical",
        `6` = "6. Semi-routine",
        `7` = "7. Routine",
        "Retired" = "Retired",
        "Student" = "Student",
        "Under 17" = "Under 17",
        "Unemployed" = "Unemployed"
    )
}

# --- Helper to format reference groups ---
format_reference_group <- function(ref_group_list) {
    if (is.null(ref_group_list) || length(ref_group_list) == 0 || !is.list(ref_group_list)) {
        return("N/A")
    }
    ref_items <- lapply(names(ref_group_list), function(name) {
        value <- ref_group_list[[name]]

        # Special handling for NS-SeC references
        if (tolower(name) == "ses" && value %in% names(get_ses_labels())) {
            # Get the full label and clean up line breaks for display
            full_label <- get_ses_labels()[[value]]
            full_label <- gsub("\n", " ", full_label) # Replace line breaks with spaces
            return(full_label)
        } else {
            return(value)
        }
    })
    paste("Reference group:", paste(ref_items, collapse = ", "))
}

# --- Generic function to create a single demographic plot ---
create_demographic_plot <- function(data_dt,
                                    value_col,
                                    ci_lower_col = NULL,
                                    ci_upper_col = NULL,
                                    x_var = "Age",
                                    facet_var = NULL,
                                    plot_title = "",
                                    y_label = "",
                                    plot_type = c("bars", "pointrange"),
                                    use_log_scale = FALSE,
                                    facet_labels = NULL) {
    plot_type <- match.arg(plot_type)
    common_theme <- get_common_theme()

    # Basic plot setup
    p <- ggplot(data_dt, aes(x = .data[[x_var]], y = .data[[value_col]]))

    # Derive a single descriptive label for this plot's aesthetic.
    # Mapping fill/colour to a CONSTANT string (not a data column) gives exactly
    # one legend entry per subplot with no sub-categories.  Using the same shared
    # scale_fill/color_manual across every subplot lets patchwork
    # collect all entries into one clean legend.
    fill_label <- if (!is.null(facet_var)) {
        if ((x_var == "SES" && facet_var == "Ethnicity") ||
            (x_var == "Ethnicity" && facet_var == "SES")) {
            "NS-SeC and Ethnicity"
        } else if (x_var == "Age" && facet_var == "SES") {
            "Age and NS-SeC"
        } else if (x_var == "Age") {
            paste0("Age and ", tools::toTitleCase(facet_var))
        } else if (facet_var == "SES") {
            "NS-SeC"
        } else {
            tools::toTitleCase(facet_var)
        }
    } else {
        if (x_var == "SES") "NS-SeC" else tools::toTitleCase(x_var)
    }

    # Shared colour map — identical definition in every subplot so patchwork
    # can merge all fill/colour guides into a single legend.
    set2 <- RColorBrewer::brewer.pal(8, "Set2")
    shared_values <- c(
        "Age" = set2[1],
        "Age and Ethnicity" = set2[2],
        "Age and NS-SeC" = set2[3],
        "Age and Gender" = set2[4],
        "NS-SeC and Ethnicity" = set2[5]
    )

    # Add geom based on plot type
    if (plot_type == "bars") {
        p <- p + geom_col(aes(fill = fill_label), alpha = 0.8) +
            scale_fill_manual(values = shared_values, name = NULL)
        if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
            p <- p + geom_errorbar(aes(ymin = .data[[ci_lower_col]], ymax = .data[[ci_upper_col]]),
                width = 0.25, color = "gray30"
            )
        }
    } else if (plot_type == "pointrange") {
        if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
            p <- p + geom_pointrange(
                aes(
                    ymin = .data[[ci_lower_col]], ymax = .data[[ci_upper_col]],
                    color = fill_label
                ),
                size = 0.8
            ) +
                scale_color_manual(values = shared_values, name = NULL)
        } else {
            p <- p + geom_point(aes(color = fill_label), size = 2) +
                scale_color_manual(values = shared_values, name = NULL)
        }
    }

    # Add faceting if specified
    if (!is.null(facet_var)) {
        if (!is.null(facet_labels)) {
            p <- p + facet_wrap(as.formula(paste("~", facet_var)),
                nrow = 1, scales = "free_x",
                labeller = as_labeller(facet_labels)
            )
        } else {
            p <- p + facet_wrap(as.formula(paste("~", facet_var)), nrow = 1, scales = "free_x")
        }
    }

    p <- p +
        labs(title = plot_title, x = NULL, y = y_label, fill = NULL, colour = NULL) +
        common_theme +
        guides(
            fill = guide_legend(title = NULL, nrow = 1),
            colour = guide_legend(title = NULL, nrow = 1)
        )

    # Add log scale if needed
    if (use_log_scale) {
        p <- p +
            geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
            get_log_y_scale()
    } else {
        # Use appropriate scale based on plot type
        if (plot_type == "bars") {
            p <- p + scale_y_continuous(labels = percent_format(accuracy = 0.1))
        } else {
            p <- p + scale_y_continuous(labels = number_format(accuracy = 0.01))
        }
    }

    return(p)
}

# --- Function to filter SES data consistently ---
filter_ses_data <- function(data_dt, exclude_categories = c("Unknown", "Retired", "Under 17")) {
    if ("SES" %in% names(data_dt)) {
        filtered_dt <- data_dt[!SES %in% exclude_categories]
        # If this dataset also includes Age (e.g., Age × NS-SeC),
        # remove child age bands so minors do not appear under NS-SeC 1–7.
        if ("Age" %in% names(filtered_dt)) {
            child_age_bands <- c("0-4", "5-9", "10-14", "15-19")
            filtered_dt <- filtered_dt[!Age %in% child_age_bands]
        }
        return(filtered_dt)
    }
    return(data_dt)
}

# --- Function to filter gender data consistently ---
filter_gender_data <- function(data_dt, exclude_categories = c("Unknown")) {
    if ("Gender" %in% names(data_dt)) {
        return(data_dt[!Gender %in% exclude_categories])
    }
    return(data_dt)
}

# --- Function to filter ethnicity and SES data consistently ---
filter_eth_ses_data <- function(data_dt, exclude_categories = c("Unknown", "Retired", "Under 17")) {
    # Filter out NA, Unknown, and other excluded categories from both Ethnicity and SES
    filtered_dt <- data_dt

    if ("Ethnicity" %in% names(data_dt)) {
        filtered_dt <- filtered_dt[!is.na(Ethnicity) & Ethnicity != "Unknown" & Ethnicity != ""]
    }

    if ("SES" %in% names(data_dt)) {
        filtered_dt <- filtered_dt[!is.na(SES) & !SES %in% exclude_categories & SES != ""]
    }

    return(filtered_dt)
}

# --- Function to create combined plot layouts ---
create_combined_plot <- function(plot_list,
                                 layout = "vertical",
                                 widths = NULL) {
    if (layout == "vertical") {
        combined <- wrap_plots(plot_list, ncol = 1)
    } else if (layout == "horizontal") {
        combined <- wrap_plots(plot_list, nrow = 1, widths = widths)
    } else if (layout == "grid") {
        combined <- wrap_plots(plot_list, ncol = 2, widths = widths)
    }

    combined <- combined + plot_layout(guides = "collect")
    return(combined)
}


#' Generate and Save Stratified NGM Plots
#'
#' Creates and saves a set of standardized plots for stratified NGM results.
#' This includes a "Top N" point-range plot for highlighting key strata and a
#' "Full Distribution" vertical bar plot for a comprehensive view.
#'
#' @param data_dt A data.table containing the data to plot.
#' @param value_col The name of the column with the main values to plot.
#' @param lower_ci_col Name of the lower confidence interval column.
#' @param upper_ci_col Name of the upper confidence interval column.
#' @param stratification_vars Character vector of stratification variables.
#' @param all_strata_list A named list of levels for each stratification variable.
#' @param top_n_count Integer, how many top strata to show in the "Top N" plot.
#' @param plot_title_stem A character string for the plot title (e.g., "Normalized Incidence by").
#' @param plot_subtitle_stem A character string for the subtitle (e.g., reference group info).
#' @param x_axis_label The label for the value axis in the plots.
#' @param output_dir Directory path to save plots.
#' @param output_filename_base_suffix A suffix for the output plot filenames.
#' @param ngm_approach_label Label indicating if it's a bootstrap or single run.
#' @param y_scale_labels_format A ggplot2 scales function for formatting the value axis labels (e.g., `scales::percent_format()`).
#' @param use_log_value_axis Logical, whether to use a log scale for the value axis.
#' @param filter_positive_value_for_log Logical, if using log scale, should non-positive values be filtered out.
#' @param log_axis_labels_decimal Logical, for log axis, use decimal (0.01) or scientific (10^-2) labels.
#' @return Invisibly returns NULL. Plots are saved to disk.
generate_stratified_ngm_plots <- function(data_dt,
                                          value_col,
                                          lower_ci_col = NULL,
                                          upper_ci_col = NULL,
                                          stratification_vars,
                                          all_strata_list,
                                          top_n_count = 20,
                                          plot_title_stem = "Value by",
                                          plot_subtitle_stem = "",
                                          x_axis_label = "Value",
                                          output_dir = ".",
                                          output_filename_base_suffix = "plot",
                                          ngm_approach_label = "(Bootstrap N=100)",
                                          y_scale_labels_format = scales::number_format(),
                                          use_log_value_axis = FALSE,
                                          filter_positive_value_for_log = FALSE,
                                          log_axis_labels_decimal = FALSE) {
    if (is.null(data_dt) || nrow(data_dt) == 0) {
        message(paste("  Skipping plot generation for", output_filename_base_suffix, "as data is NULL or empty."))
        return(invisible(NULL))
    }
    message(paste0(" -> Generating NGM plots for: ", plot_title_stem, " ", ngm_approach_label, " Stratification: ", paste(stratification_vars, collapse = ", ")))

    plot_data <- copy(data_dt)
    add_cis <- !is.null(lower_ci_col) && !is.null(upper_ci_col) &&
        lower_ci_col %in% names(plot_data) && upper_ci_col %in% names(plot_data)

    if (use_log_value_axis && filter_positive_value_for_log) {
        initial_rows <- nrow(plot_data)
        plot_data <- plot_data[get(value_col) > 0 & get(lower_ci_col) > 0 & get(upper_ci_col) > 0]
        if (nrow(plot_data) < initial_rows) {
            message(paste0("  Applying positive value filter for log scale on column: '", value_col, "'. Rows reduced from ", initial_rows, " to ", nrow(plot_data), "."))
        }
    }

    plot_title <- paste(plot_title_stem, ngm_approach_label)
    plot_subtitle <- plot_subtitle_stem

    common_theme <- theme_bw(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = "gray90", linewidth = 0.2),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black", size = 0.5),
            axis.ticks = element_line(colour = "black", size = 0.5)
        )

    # --- Create plot for Top N (as Point-Range Plot) ---
    if (top_n_count > 0 && nrow(plot_data) > top_n_count) {
        plot_data_top_n <- copy(plot_data)
        plot_data_top_n[, Stratum := do.call(paste, c(.SD, sep = " | ")), .SDcols = stratification_vars]
        plot_data_top_n <- plot_data_top_n[order(-get(value_col))][1:top_n_count]
        plot_data_top_n[, Stratum := forcats::fct_reorder(Stratum, get(value_col))]

        plot_title_top_n <- paste("Top", top_n_count, plot_title_stem, ngm_approach_label)

        p_top_n <- ggplot(plot_data_top_n, aes(y = Stratum, x = .data[[value_col]])) +
            geom_point(size = 2.5)

        if (add_cis) {
            p_top_n <- p_top_n +
                geom_linerange(aes(xmin = .data[[lower_ci_col]], xmax = .data[[upper_ci_col]]), color = "gray30")
        }
        if (use_log_value_axis) {
            p_top_n <- p_top_n + geom_vline(xintercept = 1, linetype = "dashed", color = "black")
        }

        p_top_n <- p_top_n +
            labs(
                title = plot_title_top_n,
                subtitle = plot_subtitle,
                y = NULL,
                x = x_axis_label
            ) +
            common_theme

        if (use_log_value_axis) {
            p_top_n <- p_top_n + get_log_x_scale(log_axis_labels_decimal)
        } else {
            p_top_n <- p_top_n + scale_x_continuous(labels = y_scale_labels_format)
        }

        plot_top_n_filename_base <- file.path(output_dir, paste0("top_n_", output_filename_base_suffix))
        ggsave(paste0(plot_top_n_filename_base, ".png"), plot = p_top_n, width = 10, height = 7, dpi = 600, bg = "white")
        ggsave(paste0(plot_top_n_filename_base, ".pdf"), plot = p_top_n, width = 10, height = 7, dpi = 600, bg = "white")
        message(paste("  -> Top N plot saved to:", paste0(plot_top_n_filename_base, ".png/.pdf")))
    }

    # --- Create plot for Full Distribution ---
    plot_data_full <- copy(plot_data)
    x_var <- stratification_vars[1]

    # Detect if this is a risk ratio plot (use pointrange) or absolute measure (use bars)
    is_risk_ratio <- grepl("burden|ratio", value_col, ignore.case = TRUE) ||
        grepl("burden|ratio", x_axis_label, ignore.case = TRUE) ||
        use_log_value_axis # Log scale usually indicates ratios

    # Get demographic-specific color palette
    # For stratified plots, use the first stratification variable for color palette
    palette_var_full <- stratification_vars[1]
    x_levels_full <- unique(plot_data_full[[x_var]])
    color_palette_full <- get_demographic_palette(palette_var_full, x_levels_full)

    if (is_risk_ratio && add_cis) {
        # Use pointrange for risk ratios
        p_full_dist <- ggplot(plot_data_full, aes(x = .data[[x_var]], y = .data[[value_col]])) +
            geom_pointrange(
                aes(
                    ymin = .data[[lower_ci_col]], ymax = .data[[upper_ci_col]],
                    color = .data[[x_var]]
                ),
                size = 0.8
            ) +
            scale_color_manual(values = color_palette_full)
    } else {
        # Use bars for absolute measures (transmission contribution, case share, etc.)
        p_full_dist <- ggplot(plot_data_full, aes(x = .data[[x_var]], y = .data[[value_col]])) +
            geom_col(aes(fill = .data[[x_var]]), alpha = 0.8) +
            scale_fill_manual(values = color_palette_full)

        if (add_cis) {
            p_full_dist <- p_full_dist +
                geom_errorbar(aes(ymin = .data[[lower_ci_col]], ymax = .data[[upper_ci_col]]),
                    width = 0.25, color = "gray30"
                )
        }
    }
    if (use_log_value_axis) {
        p_full_dist <- p_full_dist + geom_hline(yintercept = 1, linetype = "dashed", color = "black")
    }
    if (length(stratification_vars) > 2) {
        facet_var_cols <- stratification_vars[2]
        facet_var_rows <- stratification_vars[3]
        p_full_dist <- p_full_dist +
            facet_grid(rows = vars(.data[[facet_var_rows]]), cols = vars(.data[[facet_var_cols]]), scales = "free_x")
    } else if (length(stratification_vars) == 2) {
        facet_var <- stratification_vars[2]
        p_full_dist <- p_full_dist +
            facet_wrap(vars(.data[[facet_var]]), nrow = 1, scales = "free_x")
    }

    plot_title_full_dist <- paste("Full Distribution:", plot_title_stem, ngm_approach_label)
    p_full_dist <- p_full_dist +
        labs(
            title = plot_title_full_dist,
            subtitle = plot_subtitle,
            x = NULL,
            y = x_axis_label,
            fill = tools::toTitleCase(x_var)
        ) +
        common_theme +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        ) +
        guides(
            fill = guide_legend(title = tools::toTitleCase(x_var), nrow = 1),
            colour = guide_legend(title = tools::toTitleCase(x_var), nrow = 1)
        )

    if (use_log_value_axis) {
        p_full_dist <- p_full_dist + get_log_y_scale(
            accuracy = if (log_axis_labels_decimal) 0.01 else 0.1,
            use_scientific = !log_axis_labels_decimal
        )
    } else {
        p_full_dist <- p_full_dist + scale_y_continuous(labels = y_scale_labels_format)
    }

    plot_full_dist_filename_base <- file.path(output_dir, paste0("full_dist_", output_filename_base_suffix))

    # Adjust plot dimensions based on faceting
    if (length(stratification_vars) > 2) {
        # For grid facets
        n_facets_rows <- length(all_strata_list[[stratification_vars[3]]])
        n_facets_cols <- length(all_strata_list[[stratification_vars[2]]])
        plot_height <- max(7, 2 + n_facets_rows * 3)
        plot_width <- max(12, 4 + n_facets_cols * 2.5) # Make it wide
    } else if (length(stratification_vars) == 2) {
        # For horizontal facets (nrow=1)
        plot_height <- 7
        n_facets <- length(all_strata_list[[stratification_vars[2]]])
        plot_width <- max(12, 4 + n_facets * 2.5) # Make it wide
    } else {
        # For a single plot
        plot_height <- 7
        plot_width <- 8
    }

    ggsave(paste0(plot_full_dist_filename_base, ".png"), plot = p_full_dist, width = plot_width, height = plot_height, dpi = 600, bg = "white", limitsize = FALSE)
    ggsave(paste0(plot_full_dist_filename_base, ".pdf"), plot = p_full_dist, width = plot_width, height = plot_height, dpi = 600, bg = "white", limitsize = FALSE)
    message(paste("  -> Full distribution plot saved to:", paste0(plot_full_dist_filename_base, ".png/.pdf")))
}

# --- Function to create combined RR plots with "Overall" ribbon ---
create_combined_rr_with_overall <- function(stratified_data, univariable_data, facet_var,
                                            title_text, subtitle_text = "", facet_labels = NULL, x_var = "Age", x_axis_labels = NULL) {
    # Ensure both datasets have the required columns
    if (is.null(stratified_data) || is.null(univariable_data)) {
        warning("Missing data for combined plot")
        return(NULL)
    }

    # Filter positive values for log scale
    stratified_data <- stratified_data[RelativeBurden_mean > 0 & RelativeBurden_lowerCI > 0]
    univariable_data <- univariable_data[RelativeBurden_mean > 0 & RelativeBurden_lowerCI > 0]

    # Create ribbon data for univariable results across all facets
    # Show ribbon for all analyses
    show_ribbon <- TRUE

    ribbon_data <- NULL
    if (show_ribbon) {
        facet_levels <- unique(stratified_data[[facet_var]])
        ribbon_data_list <- list()

        for (level in facet_levels) {
            # Get univariable result for this demographic level
            univ_result <- univariable_data[get(facet_var) == level]
            if (nrow(univ_result) > 0) {
                ribbon_temp <- data.table(
                    facet_level = level,
                    ymin = univ_result$RelativeBurden_lowerCI[1],
                    ymax = univ_result$RelativeBurden_upperCI[1],
                    y_center = univ_result$RelativeBurden_mean[1]
                )
                setnames(ribbon_temp, "facet_level", facet_var)
                ribbon_data_list[[length(ribbon_data_list) + 1]] <- ribbon_temp
            }
        }
        ribbon_data <- rbindlist(ribbon_data_list, fill = TRUE)
    }

    # Derive the same single-label colour scheme used by create_demographic_plot
    # so the legend entry matches across all plot types.
    fill_label <- if ((x_var == "SES" && facet_var == "Ethnicity") ||
        (x_var == "Ethnicity" && facet_var == "SES")) {
        "NS-SeC and Ethnicity"
    } else if (x_var == "Age" && facet_var == "SES") {
        "Age and NS-SeC"
    } else if (x_var == "Age") {
        paste0("Age and ", tools::toTitleCase(facet_var))
    } else if (facet_var == "SES") {
        "NS-SeC"
    } else {
        tools::toTitleCase(facet_var)
    }

    set2_colors <- RColorBrewer::brewer.pal(8, "Set2")
    shared_values <- c(
        "Age"                 = set2_colors[1],
        "Age and Ethnicity"   = set2_colors[2],
        "Age and NS-SeC"      = set2_colors[3],
        "Age and Gender"      = set2_colors[4],
        "NS-SeC and Ethnicity" = set2_colors[5]
    )

    # Ribbon colour matches the fill_label colour
    ribbon_color <- shared_values[[fill_label]]
    if (is.null(ribbon_color)) ribbon_color <- set2_colors[1]

    # Create the plot
    p <- ggplot(stratified_data, aes(x = .data[[x_var]], y = RelativeBurden_mean)) +
        # Add reference line first so it appears behind other elements
        geom_hline(yintercept = 1, linetype = "dashed", color = "black")

    # Add ribbon elements only if ribbon should be shown
    if (show_ribbon && !is.null(ribbon_data)) {
        p <- p +
            # Add ribbon for univariable results (horizontal across each facet)
            geom_rect(
                data = ribbon_data,
                aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
                alpha = 0.2, fill = ribbon_color, inherit.aes = FALSE
            ) +
            # Add center line for univariable results
            geom_segment(
                data = ribbon_data,
                aes(x = -Inf, xend = Inf, y = y_center, yend = y_center),
                linetype = "solid", color = ribbon_color, linewidth = 0.8, inherit.aes = FALSE
            )
    }

    p <- p +
        geom_pointrange(
            aes(
                ymin = RelativeBurden_lowerCI,
                ymax = RelativeBurden_upperCI,
                color = fill_label
            ),
            size = 0.8
        ) +
        scale_color_manual(values = shared_values, name = NULL) +
        {
            if (!is.null(facet_labels)) {
                facet_wrap(vars(.data[[facet_var]]),
                    nrow = 1, scales = "free_x",
                    labeller = as_labeller(facet_labels)
                )
            } else {
                facet_wrap(vars(.data[[facet_var]]), nrow = 1, scales = "free_x")
            }
        } +
        labs(
            title = title_text,
            subtitle = subtitle_text,
            x = if (x_var == "SES") "NS-SeC" else tools::toTitleCase(x_var),
            y = "Relative Risk of Infection (vs Reference)",
            colour = NULL
        ) +
        get_common_theme() +
        get_log_y_scale() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        ) +
        guides(colour = guide_legend(title = NULL, nrow = 1))

    # Add x-axis labels if provided
    if (!is.null(x_axis_labels)) {
        p <- p + scale_x_discrete(labels = x_axis_labels)
    }

    # Caption will be added at patchwork level, not individual plots

    return(p)
}

# ===================================================================
# CONSOLIDATED PLOTTING SYSTEM
# ===================================================================
# This section provides centralized configuration and functions to eliminate
# repetition across plotting scripts and ensure consistency

# --- CENTRAL CONFIGURATION SYSTEM ---

#' Get standardized analysis configurations for all plot types
#' @param analysis_type Type of analysis: "univariable", "stratified", "stratified_with_overall", "report"
#' @return List of analysis configurations
get_analysis_configs <- function(analysis_type = "stratified") {
    # Base configurations used across different analysis types
    base_configs <- list(
        univariable = list(
            list(
                name = "Age_Only_Stratified_5y", var = "Age", title = "A) By Age",
                levels = c(
                    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"
                )
            ),
            list(name = "Eth_Only_Stratified", var = "Ethnicity", title = "B) By Ethnicity"),
            list(
                name = "SES_Only_Stratified", var = "SES", title = "C) By NS-SeC",
                filter_func = filter_ses_data, labels = get_ses_labels()
            ),
            list(
                name = "Gender_Only_Stratified", var = "Gender", title = "D) By Gender",
                filter_func = filter_gender_data
            )
        ),
        stratified = list(
            list(name = "Age_Eth_Stratified_5y", facet_var = "Ethnicity", title = "A) By Age & Ethnicity"),
            list(
                name = "Age_SES_Stratified_5y", facet_var = "SES", title = "C) By Age & NS-SeC",
                facet_labels = get_ses_labels(), filter_func = filter_ses_data
            ),
            list(
                name = "Age_Gender_Stratified_5y", facet_var = "Gender", title = "B) By Age & Gender",
                filter_func = filter_gender_data
            ),
            list(
                name = "Eth_SES_Stratified", facet_var = "Ethnicity", title = "D) By NS-SeC & Ethnicity",
                x_var = "SES", labels = get_ses_labels(), filter_func = filter_eth_ses_data
            )
        )
    )

    # Add configurations for stratified_with_overall (includes univariable mappings)
    base_configs$stratified_with_overall <- lapply(base_configs$stratified, function(config) {
        # Add univariable mapping based on analysis type
        if (config$name == "Age_Eth_Stratified_5y") {
            config$univariable_name <- "Eth_Only_Stratified"
        } else if (config$name == "Age_SES_Stratified_5y") {
            config$univariable_name <- "SES_Only_Stratified"
        } else if (config$name == "Age_Gender_Stratified_5y") {
            config$univariable_name <- "Gender_Only_Stratified"
        } else if (config$name == "Eth_SES_Stratified") {
            config$univariable_name <- "Eth_Only_Stratified"
            config$title <- "D) By NS-SeC & Ethnicity" # Different title for this analysis
            config$x_axis_labels <- get_ses_labels() # For SES labels on x-axis
        }
        return(config)
    })

    # Report configurations (simplified subset for final report plots)
    base_configs$report <- base_configs$stratified

    return(base_configs[[analysis_type]])
}

# --- CONSOLIDATED DATA LOADING ---

#' Load analysis results with consistent error handling
#' @param configs List of analysis configurations
#' @param cache_dir Directory containing cached results
#' @param analysis_type Type for logging ("stratified", "univariable", etc.)
#' @return Named list of loaded results
load_analysis_results <- function(configs, cache_dir, analysis_type = "analysis") {
    results <- list()

    message(paste("Loading", analysis_type, "results data..."))

    for (config in configs) {
        file_path <- file.path(cache_dir, config$name, paste0(config$name, "_consolidated_results.qs"))

        if (file.exists(file_path)) {
            message(paste("  Loading:", config$name))
            results[[config$name]] <- qs::qread(file_path)
        } else {
            warning(paste("Could not find results for:", config$name))
        }
    }

    return(results)
}

# --- IMPROVED COLOR ASSIGNMENT ---

#' Get consistent color assignment based on plot type and variables
#' This ensures all plots use the same color logic
#' @param x_variable Variable on x-axis
#' @param facet_var Variable used for faceting
#' @param config Analysis configuration object
#' @param x_levels Levels of the x variable
#' @return Named vector of colors
get_consistent_plot_colors <- function(x_variable, facet_var = NULL, config = NULL, x_levels) {
    # Special case: Ethnicity x NS-SeC analysis always uses green
    if (!is.null(config) && !is.null(facet_var)) {
        if ((x_variable == "SES" && facet_var == "Ethnicity") ||
            (x_variable == "Ethnicity" && facet_var == "SES")) {
            # Use green for ethnicity-SES interactions
            set2_colors <- RColorBrewer::brewer.pal(8, "Set2")
            color_palette <- rep(set2_colors[5], length(x_levels)) # Green
            names(color_palette) <- x_levels
            return(color_palette)
        }
    }

    # Standard case: use facet_var for color assignment (consistent with RR plots)
    color_var <- if (!is.null(facet_var)) facet_var else x_variable
    return(get_demographic_palette(color_var, x_levels))
}

# --- ADVANCED LAYOUT FUNCTIONS ---

#' Create the standard report layout with age analyses on top
#' This function creates the consistent layout used in reports:
#' Row 1: Age & Ethnicity + Age & Gender (proportional widths)
#' Row 2: Age & SES (full width)
#' Row 3: Ethnicity & SES (full width)
#' @param plots Named list of plots: age_eth, age_ses, age_gender, eth_ses
#' @param facet_counts Named vector of facet counts for proportional widths
#' @return Combined plot using patchwork
create_report_layout <- function(plots, facet_counts = NULL) {
    required_plots <- c("age_eth", "age_gender", "age_ses", "eth_ses")
    if (!all(required_plots %in% names(plots))) {
        stop("Missing required plots. Need: ", paste(required_plots, collapse = ", "))
    }

    # Calculate proportional widths if not provided
    if (is.null(facet_counts)) {
        # Default reasonable proportions
        age_eth_facets <- 4 # Typically 4 ethnicity groups
        age_gender_facets <- 2 # Typically 2 gender groups
    } else {
        age_eth_facets <- facet_counts[["age_eth"]] %||% 4
        age_gender_facets <- facet_counts[["age_gender"]] %||% 2
    }

    # Remove y-axis label from right plot in top row
    plots$age_gender_no_y <- plots$age_gender + theme(axis.title.y = element_blank())

    # Create top row with proportional widths
    top_row <- wrap_plots(
        plots$age_eth, plots$age_gender_no_y,
        nrow = 1,
        widths = c(age_eth_facets, age_gender_facets)
    )

    # Create final layout
    combined_plot <- wrap_plots(
        top_row, # Row 1: Age & Ethnicity + Age & Gender
        plots$age_ses, # Row 2: Age & NS-SeC
        plots$eth_ses, # Row 3: Ethnicity & NS-SeC
        ncol = 1
    ) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom", legend.direction = "horizontal")

    return(combined_plot)
}

#' Calculate facet counts for layout proportions
#' @param results_list Named list of analysis results
#' @param configs List of analysis configurations
#' @param data_type Type of data to count ("projected_case_share_summary_dt" or "relative_burden_summary_dt")
#' @param apply_filtering Whether to apply filtering (for RR plots with reference exclusion)
#' @return Named vector of facet counts
calculate_facet_counts <- function(results_list, configs, data_type = "projected_case_share_summary_dt", apply_filtering = FALSE) {
    facet_counts <- sapply(configs, function(config) {
        if (config$name %in% names(results_list)) {
            res <- results_list[[config$name]]
            if (!is.null(res[[data_type]])) {
                data <- res[[data_type]]

                # Apply filtering if specified
                if (apply_filtering && !is.null(config$filter_func)) {
                    data <- config$filter_func(data)
                }

                # For RR plots, also exclude reference groups if apply_filtering = TRUE
                if (apply_filtering && data_type == "relative_burden_summary_dt") {
                    data <- exclude_reference_groups_from_data(data, config, res$reference_group_strata)
                }

                return(length(unique(data[[config$facet_var]])))
            }
        }
        return(3) # fallback
    })

    # Convert to named vector with meaningful names
    names(facet_counts) <- sapply(configs, function(config) {
        if (grepl("Age_Eth", config$name)) {
            return("age_eth")
        }
        if (grepl("Age_SES", config$name)) {
            return("age_ses")
        }
        if (grepl("Age_Gender", config$name)) {
            return("age_gender")
        }
        if (grepl("Eth_SES", config$name)) {
            return("eth_ses")
        }
        return("other")
    })

    return(facet_counts)
}

# Helper function to exclude reference groups from data
exclude_reference_groups_from_data <- function(data_dt, config, reference_group_strata) {
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

# --- HIGH-LEVEL PLOT GENERATION FUNCTIONS ---

#' Generate infection share plots using consistent configuration
#' @param cache_dir Cache directory path
#' @param output_dir Output directory path
#' @param analysis_configs Optional custom configs (uses standard if NULL)
#' @return List of generated plots
generate_infection_share_plots <- function(cache_dir, output_dir, analysis_configs = NULL) {
    if (is.null(analysis_configs)) {
        analysis_configs <- get_analysis_configs("stratified")
    }

    # Load data
    all_results <- load_analysis_results(analysis_configs, cache_dir, "infection share")

    # Generate individual plots
    plots <- list()
    for (i in seq_along(analysis_configs)) {
        config <- analysis_configs[[i]]

        if (config$name %in% names(all_results)) {
            result <- all_results[[config$name]]

            if (!is.null(result$projected_case_share_summary_dt)) {
                # Apply filtering
                plot_data <- copy(result$projected_case_share_summary_dt)
                if (!is.null(config$filter_func)) {
                    plot_data <- config$filter_func(plot_data)
                }

                # Determine variables
                x_variable <- config$x_var %||% "Age"

                # create_demographic_plot handles color/legend internally now
                p <- create_demographic_plot(
                    data_dt = plot_data,
                    value_col = "ProjectedCaseShare_mean",
                    ci_lower_col = "ProjectedCaseShare_lowerCI",
                    ci_upper_col = "ProjectedCaseShare_upperCI",
                    x_var = x_variable,
                    facet_var = config$facet_var,
                    plot_title = config$title,
                    y_label = "Projected Share of Infections",
                    plot_type = "bars",
                    facet_labels = config$facet_labels
                )

                # Add x-axis labels if specified
                if (!is.null(config$labels) && x_variable == "SES") {
                    p <- p + scale_x_discrete(labels = config$labels)
                }

                # Store with meaningful name
                plot_name <- if (grepl("Age_Eth", config$name)) {
                    "age_eth"
                } else if (grepl("Age_SES", config$name)) {
                    "age_ses"
                } else if (grepl("Age_Gender", config$name)) {
                    "age_gender"
                } else if (grepl("Eth_SES", config$name)) {
                    "eth_ses"
                } else {
                    paste0("plot_", i)
                }
                plots[[plot_name]] <- p
            }
        }
    }

    return(plots)
}

#' Generate and save the standard infection share report plot
#' @param cache_dir Cache directory path
#' @param output_dir Output directory path
#' @param filename_base Base filename (without extension)
#' @return Combined plot object
generate_infection_share_report <- function(cache_dir, output_dir, filename_base = "2_infection_share_plot") {
    # Generate individual plots
    plots <- generate_infection_share_plots(cache_dir, output_dir)

    if (length(plots) >= 4) {
        # Calculate facet counts for proportional layout
        configs <- get_analysis_configs("stratified")
        all_results <- load_analysis_results(configs, cache_dir, "facet counting")
        facet_counts <- calculate_facet_counts(all_results, configs, "projected_case_share_summary_dt", FALSE)

        # Create report layout
        combined_plot <- create_report_layout(plots, facet_counts)

        # Save files in high-res formats
        save_plot_files(
            combined_plot,
            file.path(output_dir, filename_base),
            width = 20, height = 21,
            dpi = 300,
            formats = c("png", "pdf", "tif", "eps"),
            message_text = "Saved combined Projected Share of Infections plot to:"
        )

        return(combined_plot)
    } else {
        stop("Insufficient plots generated for report layout")
    }
}

# Helper function for null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
