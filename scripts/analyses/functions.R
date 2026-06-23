library(readr)
"%notin%" <- Negate("%in%")

#### DATA ANALYSIS ####

last_monday <- function(dates) {
  output <- c()
  for (date in dates) {
    move_back <- 7 - which(weekdays(seq.Date(
      from = as.Date(date) - 6,
      to = as.Date(date),
      by = 1
    )) == "Monday")
    monday <- as.Date(date) - move_back
    if (!weekdays(monday) == "Monday") {
      warning("Monday move wrong")
    }
    output <- c(output, monday)
  }
  as.Date(output)
}

na_to_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
max_100 <- function(x) {
  ifelse(x > max_n_contacts, max_n_contacts, x)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

format_label <- function(var) {
  if (grepl("sec_", var)) {
    var <- gsub("^p_", "Participant ", var)
    var <- gsub("^c_", "Contact ", var)
    var <- gsub("sec", "NS-SEC", var)
    var <- gsub("_input", "", var)
    return(gsub("_", " ", var))
  } else {
    var <- gsub("^p_", "Participant ", var)
    var <- gsub("^c_", "Contact ", var)
    return(gsub("_", " ", var))
  }
}

reorder_factors <- function(data, var_name) {
  if (var_name == "Time of day") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c(
        paste0(c(12, 1:11), "AM"),
        paste0(c(12, 1:11), "PM")
      ))
    )
  }
  if (var_name == "Participant's ethnicity") {
    data$Category_short <- factor(data$Category_short,
      levels = (c("Prefer not to say", "Other", "Mixed", "Black", "Asian", "White"))
    )
  }
  if (var_name == "Day of Week") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c(
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday", "Sunday"
      ))
    )
  }
  if (var_name == "Participant's household income (aged 18+)") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c(
        "Less than £20,000", "£20,000 - £39,999",
        "£40,000 - £59,999", "£60,000 - £100,000",
        "Over £100,000"
      ))
    )
  }
  if (var_name == "Participant's highest qualification (aged 18+)") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c(
        "Child (Not Applic.)", "No qualifications", "Level 1 (1-4 GCSEs, O-levels\n(any), NVQ level 1, etc.)",
        "Level 2 (5+ GCSEs, O-levels\n(passes), NVQ level 2, etc.)", "Apprenticeship",
        "Level 3 (A-level, BTEC, NVQ\nlevel 3, etc.)", "Level 4+ (University degree and above)",
        "Other"
      ))
    )
  }
  if (var_name == "Participant's employment status (aged 18+)") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c(
        "Employed full-time (35+ hours per week)", "Employed part-time", "Self-employed full time", "Self-employed part time",
        "Unemployed (currently looking for work)", "Unemployed (not currently looking for work)",
        "Looking after home or family", "Retired",
        "Long-term sick or disabled", "Other", "Student", "Child (Not Applic.)"
      ))
    )
  }
  if (var_name == "Urban/Rural") {
    data$Category_short <- factor(data$Category_short,
      levels = rev(c("Urban", "Rural"))
    )
  }

  data
}


## dataset imputation function ##

fcn_impute <- function(
    data_input, # dataset which needs filling in, e.g. part, contacts
    var, # name of column to fill in missing values
    dependent_vars = NULL # names of columns to create a dependent distribution
    ) {
  data <- copy(data.table(data_input))

  # check is in colnames
  if (!var %in% colnames(data)) {
    stop(paste0("Var '", var, "' not in data column names"))
  }

  # Determine the *actual* base variable name for logic checks
  actual_generalised_var <- NULL
  if (var == "part_ses" || var == "cnt_ses") {
    actual_generalised_var <- "ses"
  } else if (var == "part_ethnicity" || var == "cnt_ethnicity") {
    actual_generalised_var <- "ethnicity"
  } else if (var == "p_sec_input" || var == "c_sec_input") {
    actual_generalised_var <- "sec_input"
  }
  # Add more mappings here if needed for other variables

  # Check if the variable is one we are set up to impute
  allowed_base_vars <- c("ethnicity", "sec_input", "ses")
  # Ensure %notin% is defined (should be from start of script)
  if (!exists("%notin%", mode = "function")) {
    `%notin%` <- Negate(`%in%`)
    message("Defined %notin% within fcn_impute.")
  }
  if (is.null(actual_generalised_var) || (actual_generalised_var %notin% allowed_base_vars)) {
    stop(paste0("Not set up for imputation for this var: '", var, "'"))
  }

  # Set unusable values to NA based on the *actual* generalised var name
  # Note: This logic might need adjustment based on expected values for each specific var
  if (actual_generalised_var == "ethnicity") {
    # Handle "Prefer not to say" for ethnicity
    if (any(data[[var]] %like% "Prefer", na.rm = TRUE)) {
      # Use data.table syntax for efficiency
      rows_to_update <- data[, .I[get(var) %like% "Prefer"]]
      if (length(rows_to_update) > 0) {
        data[rows_to_update, (var) := NA]
        message(paste0("Set ", length(rows_to_update), " 'Prefer not to say' to NA for variable: ", var))
      }
    }
  }
  if (actual_generalised_var == "ses") { # ADDED THIS BLOCK
    # Handle "Unknown" for SES
    if (any(data[[var]] %like% "Unknown", na.rm = TRUE)) {
      rows_to_update_unknown_ses <- data[, .I[get(var) %like% "Unknown"]]
      if (length(rows_to_update_unknown_ses) > 0) {
        data[rows_to_update_unknown_ses, (var) := NA]
        message(paste0("Set ", length(rows_to_update_unknown_ses), " 'Unknown' SES to NA for variable: ", var))
      }
    }
  }
  # Original code handled "Unknown" specifically for "sec_input".
  # Commenting this out as the main script filters "Unknown" SES later,
  # and it's unclear if "Unknown" is a valid level for the 'part_ses' column.
  # if (actual_generalised_var == "sec_input") {
  #   # Handle "Unknown" for NS-SEC
  #    rows_to_update_unknown <- data[, .I[get(var) %like% "Unknown"]]
  #    if(length(rows_to_update_unknown) > 0) {
  #      data[rows_to_update_unknown, (var) := NA]
  #      message(paste0("Set ", length(rows_to_update_unknown), " 'Unknown' to NA for variable: ", var))
  #   }
  # }

  # --- Imputation Logic Start ---
  rows_to_impute_idx <- which(is.na(data[[var]]))
  n_to_change <- length(rows_to_impute_idx)

  # If no NAs after potential NA conversion, end
  if (n_to_change == 0) {
    cat("0 changes needed for", var, "\n")
    return(data)
  }

  # Check dependent vars exist and handle NAs in them for rows needing imputation
  if (!is.null(dependent_vars)) {
    missing_dependent_vars <- setdiff(dependent_vars, names(data))
    if (length(missing_dependent_vars) > 0) {
      stop(paste("Dependent variable(s) not found in data:", paste(missing_dependent_vars, collapse = ", ")))
    }
    # Check for NAs in dependent variables among rows needing imputation
    na_in_dependent <- apply(data[rows_to_impute_idx, ..dependent_vars], 1, anyNA)
    if (any(na_in_dependent)) {
      warning(paste("Some dependent variables have NA values for rows where '", var, "' needs imputation. These rows might not be imputed correctly."))
      # Consider filtering: rows_to_impute_idx <- rows_to_impute_idx[!na_in_dependent]
      # Consider filtering: n_to_change <- length(rows_to_impute_idx)
      # if (n_to_change == 0) { cat("0 rows eligible for imputation after NA dependent check for", var, "\n"); return(data) }
    }
  } else {
    dependent_vars <- character(0) # Ensure it's an empty character vector if NULL
  }

  # How is the variable distributed in the non-NA data?
  data_non_na <- data[!is.na(get(var))]
  if (nrow(data_non_na) == 0) {
    stop(paste("No non-NA values found for variable '", var, "' to base imputation on."))
  }

  # Calculate distribution(s)
  if (length(dependent_vars) == 0) {
    # Calculate overall distribution
    data_distr_dt <- data_non_na[, .N, by = var]
    data_distr_dt[, prop_var_in_dep_cat := N / sum(N)]
    distr_values <- data_distr_dt[[var]]
    distr_probs <- data_distr_dt$prop_var_in_dep_cat
  } else {
    # Calculate distribution dependent on grouping variables
    safe_grouping_vars <- c(var, dependent_vars)
    data_distr_dt <- data_non_na[, .(n_var_dep = .N), by = safe_grouping_vars]
    dep_totals_dt <- data_non_na[, .(n_dep = .N), by = dependent_vars]
    setkeyv(data_distr_dt, dependent_vars)
    setkeyv(dep_totals_dt, dependent_vars)
    data_distr_dt <- dep_totals_dt[data_distr_dt, nomatch = 0]
    data_distr_dt[, prop_var_in_dep_cat := n_var_dep / n_dep]

    # Check if all combinations needing imputation have a distribution
    informative_data_combos <- unique(data[rows_to_impute_idx, ..dependent_vars])
    # Check if any dependent_vars are all NA for rows needing imputation
    if (nrow(informative_data_combos) > 0 && any(sapply(informative_data_combos, function(col) all(is.na(col))))) {
      warning(paste("At least one dependent variable is all NA for the rows needing imputation in '", var, "'. Imputation might fail for these rows."))
    }
    setkeyv(informative_data_combos, dependent_vars)
    available_distr_combos <- unique(data_distr_dt[, ..dependent_vars])
    setkeyv(available_distr_combos, dependent_vars)

    # Find combinations needing imputation but lacking a distribution
    # Using data.table's anti-join feature: X[!Y, on=.(cols)]
    missing_distr_combos <- informative_data_combos[!available_distr_combos, on = dependent_vars] # Use var names directly for 'on'

    if (nrow(missing_distr_combos) > 0) {
      warning("No distribution found for some combinations of dependent variables needed for imputation. Affected rows will likely remain NA.")
      # print("Combinations missing distribution:") # Optional: too verbose?
      # print(missing_distr_combos)
    }
  }

  n_actually_imputed <- 0 # Initialize counter
  # Perform imputation
  if (length(dependent_vars) == 0) {
    # Impute based on overall distribution
    if (length(distr_values) > 0 && sum(distr_probs, na.rm = TRUE) > 0) {
      sampled_values <- sample(
        x = distr_values,
        size = n_to_change,
        prob = distr_probs,
        replace = TRUE
      )
      data[rows_to_impute_idx, (var) := sampled_values]
      n_actually_imputed <- n_to_change # All eligible rows imputed
    } else {
      warning(paste("Cannot perform imputation for '", var, "': No valid overall distribution found."))
    }
  } else {
    # Impute based on dependent distributions
    # Use data.table join to apply imputation efficiently
    # Create a table of combinations needing imputation
    impute_targets <- data[rows_to_impute_idx, c("part_id", dependent_vars), with = FALSE] # MODIFIED: Use part_id instead of p_id
    setkeyv(impute_targets, dependent_vars)
    setkeyv(data_distr_dt, dependent_vars)

    # Join distributions to the targets
    impute_info <- data_distr_dt[impute_targets, allow.cartesian = TRUE] # Gets distribution for each p_id needing imputation

    # Sample for each group defined by p_id (effectively, sample 1 for each row)
    # Need to handle cases where a combination has no distribution (impute_info rows will have NA for distr columns)
    # Perform sampling only where distribution is available
    impute_info_valid <- impute_info[!is.na(prop_var_in_dep_cat) & prop_var_in_dep_cat > 0]

    if (nrow(impute_info_valid) > 0) {
      # Group by the original combination (represented by p_id here)
      # Sample one value for each p_id based on its specific group's distribution
      # Ensure probabilities sum to 1 within each group after filtering
      impute_info_valid[, prob_sum := sum(prop_var_in_dep_cat), by = part_id] # MODIFIED: Use part_id
      impute_info_valid[, prob_norm := prop_var_in_dep_cat / prob_sum]

      # Check for NA probabilities after normalization (should not happen if filtered correctly)
      if (anyNA(impute_info_valid$prob_norm)) {
        warning("NA probabilities encountered during dependent imputation normalization for ", var, ". Check distributions.")
        impute_info_valid <- impute_info_valid[!is.na(prob_norm)]
      }

      if (nrow(impute_info_valid) > 0) {
        sampled_vals_dt <- impute_info_valid[, .(SampledValue = sample(get(var), size = 1, replace = TRUE, prob = prob_norm)), by = part_id] # MODIFIED: Use part_id
        # Update the main data table using the sampled values
        setkey(data, part_id) # MODIFIED: Use part_id
        setkey(sampled_vals_dt, part_id) # MODIFIED: Use part_id
        data[sampled_vals_dt, (var) := i.SampledValue]
        n_actually_imputed <- nrow(sampled_vals_dt)
      } else {
        n_actually_imputed <- 0
      }

      if (n_actually_imputed < n_to_change) {
        warning(paste("Only imputed", n_actually_imputed, "out of", n_to_change, "eligible rows for", var, "due to missing/invalid distributions for some combinations."))
      }
    } else {
      warning(paste("No valid distributions found for any combinations needing imputation for '", var, "'. No rows imputed."))
    }
  }


  # check NAs remaining
  n_na_after <- sum(is.na(data[[var]])) # Check NA count *in the original column* again
  if (n_na_after > 0) {
    # Only warn if the number of NAs is unexpected (i.e., not explained by missing distributions)
    expected_na <- length(rows_to_impute_idx) - n_actually_imputed
    if (n_na_after > expected_na) {
      warning(paste(n_na_after, "NAs remaining after imputation for", var, "(expected", expected_na, "due to missing distributions)"))
    }
  }

  cat(n_actually_imputed, "rows imputed for", var, "\n")
  return(data)
}

## dataset imputation function ##

fcn_impute_raw_counts <- function(
    data_input, # dataset which needs filling in, e.g. raw_counts
    var, # name of variable which is being imputed (c_ethnicity or c_sec_input)
    dependent_vars = NULL # names of columns to create a dependent distribution 
){
  
  data_in <- copy(data.table(data_input))
  
  generalised_var <- gsub("^p_", "", gsub("^c_", "", var))
  
  # check set up for var
  if(generalised_var %notin% c('ethnicity', 'sec_input')){stop('Not set up for this var')}
  
  # define column to be distributed elsewhere
  if(generalised_var == 'ethnicity'){key_col <- 'cag_Prefer not to say'}
  if(generalised_var == 'sec_input'){key_col <- 'cag_Unknown'}
  
  # if no NAs, end
  if(sum(data_in[,get(key_col)]) == 0){
    cat('0 changes made')
    return(data_in)
  }else{
    n_changes <- sum(data_in[,get(key_col)]) 
    rows_to_change <- data_in[get(key_col) > 0, row_id]
  }
  
  grouping_vars <- c(var, dependent_vars)
  
  # how is the variable of interest distributed in the non-NA data?
  distribution_var_vec <- c(dependent_vars, colnames(data_in)[grepl('cag', colnames(data_in))])
  distribution_var_vec <- distribution_var_vec[!distribution_var_vec == key_col]
  data_distr <- data_in[, ..distribution_var_vec][, lapply(.SD, mean), by = dependent_vars]
  data_distr[, tot := rowSums(data_distr[, 2:ncol(data_distr)])]
  data_distr_l <- melt.data.table(data_distr, id.vars = c(dependent_vars, 'tot'))[, prop := value/tot]
  
  if(length(dependent_vars) == 0){
    
    vars_to_replace <- data_distr_l$variable
    var_col_nums <- which(colnames(data_in) %in% vars_to_replace)
    key_col_num <- which(colnames(data_in) == key_col)
    all_cols <- c(var_col_nums, key_col_num)
    
    for(i_row in rows_to_change){
      
      n_not_imputing <- sum(data_in[i_row, ..vars_to_replace])
      n_to_impute <- data_in[i_row, get(key_col)]
      samples <- table(sample(x = data_distr_l$variable, 
                        size = n_to_impute,
                        prob = data_distr_l$prop,
                        replace = T))
      
      # add the imputed observations
      for(col_name in vars_to_replace){
        col_num <- which(colnames(data_in) == col_name)
        data_in[i_row, col_num] <- data_in[i_row, ..col_num] + unname(samples[names(samples) == col_name])
      }
      # remove the original imputed observations
      data_in[i_row, key_col_num] <- 0
      
      # check the number of observations is correct
      if(sum(data_in[i_row, ..all_cols]) != n_not_imputing + n_to_impute){warning(paste0("Number of observations doesn't add up (i_row = ", i_row))}
      
    }
  }else{
    
    vars_to_replace <- unique(data_distr_l$variable)
    var_col_nums <- which(colnames(data_in) %in% vars_to_replace)
    key_col_num <- which(colnames(data_in) == key_col)
    all_cols <- c(var_col_nums, key_col_num)
    
    for(i_row in rows_to_change){
      
      # filter data_distr_l to apply to participant
      data_distr_l_filt <- copy(data_distr_l)
      for(dep_var in dependent_vars){
        filter <- data_in[i_row, get(dep_var)]
        data_distr_l_filt <- data_distr_l_filt[get(dep_var) == filter]
      }
      
      n_not_imputing <- sum(data_in[i_row, ..vars_to_replace])
      n_to_impute <- data_in[i_row, get(key_col)]
      samples <- table(sample(x = data_distr_l_filt$variable, 
                              size = n_to_impute,
                              prob = data_distr_l_filt$prop,
                              replace = T))
      
      # add the imputed observations
      for(col_name in vars_to_replace){
        col_num <- which(colnames(data_in) == col_name)
        data_in[i_row, col_num] <- data_in[i_row, ..col_num] + unname(samples[names(samples) == col_name])
      }
      # remove the original imputed observations
      data_in[i_row, key_col_num] <- 0
      
      # check the number of observations is correct
      if(sum(data_in[i_row, ..all_cols]) != n_not_imputing + n_to_impute){warning(cat("Number of observations doesn't add up (i_row = ", i_row, sep = ''))}
      
    }
  }
  
  # check no NAs remaining
  if(sum(data_in[,get(key_col)]) > 0){
    warning(paste0("Still some observations left in ", key_col))
  }
  
  # cat(length(rows_to_change), ' changes made to ', var, ' (', n_changes, ' total changes)\n', sep = '')
  return(data_in)
  
}



#### CLEAN DATA ####

# Define processing function for participants
process_participants <- function(data,
                                 children_input = F) {
  
  n_contacts <- data %>% 
    select(id, additional_contacts, contains('_age')) %>% 
    select(!contains('m')) %>% 
    mutate_at(vars(contains("_age")), as.character) %>%
    mutate_at(vars(contains("_age")), as.numeric) %>% 
    pivot_longer(!c(id,p_age,additional_contacts)) %>% 
    filter(!is.na(value)) %>% group_by(id, additional_contacts) %>% count() %>% ungroup() %>% 
    mutate(additional_contacts = as.character(additional_contacts)) %>% 
    complete(id = data$id, fill = list(additional_contacts = 'I did not have any contacts', n = 0)) %>% 
    rename(p_id = id)
  
  data_la <- data %>%
    select(id, contains("_la.")) %>%
    pivot_longer(!id, values_drop_na = T) %>%
    rename(p_la = value) %>%
    select(id, p_la)

  data_pcd <- data %>%
    select(id, "p_pcd.2") %>%
    setnames("p_pcd.2", "pcd1") %>%
    left_join(urban_rural_assignments, by = "pcd1")

  # if unassigned, some are missing the final character of first half of postcode
  # (common for london postcodes)
  # or 4-digit postcodes where only the first 3 are the first half of the postcode

  urban_rural_assignments_test <- urban_rural_assignments %>%
    rename(postcode = pcd1) %>%
    mutate(
      pcd_char2 = substr(postcode, 1, 2),
      pcd_char3 = substr(postcode, 1, 3)
    )

  for (i in 1:nrow(data_pcd)) {
    if (is.na(data_pcd$urban_rural[i])) {
      if (nchar(data_pcd$pcd1[i]) == 2 &
        nrow(urban_rural_assignments_test %>% filter(pcd_char2 == data_pcd$pcd1[i])) > 0 &
        n_distinct((urban_rural_assignments_test %>% filter(pcd_char2 == data_pcd$pcd1[i]))$urban_rural) == 1) {
        data_pcd$urban_rural[i] <- (urban_rural_assignments_test %>% filter(pcd_char2 == data_pcd$pcd1[i]))$urban_rural[1]
      } else {
        if (nchar(data_pcd$pcd1[i]) == 3 &
          nrow(urban_rural_assignments_test %>% filter(pcd_char3 == data_pcd$pcd1[i])) > 0 &
          n_distinct((urban_rural_assignments_test %>% filter(pcd_char3 == data_pcd$pcd1[i]))$urban_rural) == 1) {
          data_pcd$urban_rural[i] <- (urban_rural_assignments_test %>% filter(pcd_char3 == data_pcd$pcd1[i]))$urban_rural[1]
        } else {
          if (nchar(data_pcd$pcd1[i]) == 4 &
            nrow(urban_rural_assignments_test %>% filter(postcode == substr(data_pcd$pcd1[i], 1, 3))) == 1) {
            data_pcd$urban_rural[i] <- (urban_rural_assignments_test %>% filter(postcode == substr(data_pcd$pcd1[i], 1, 3)))$urban_rural[1]
          }
        }
      }
    }
  }

  data_pcd <- data_pcd %>% rename(p_urban_rural = urban_rural)

  data <- data %>%
    left_join(data_la, by = "id") %>%
    left_join(data_pcd, by = "id") %>%
    mutate(c_contact_date = case_when(
      contact_today == "Today" ~ as.Date(p2_date_calc),
      contact_today == "Yesterday" ~ as.Date(p2_date_calc) - 1,
      grepl("Other", contact_today) ~ as.Date(contact_date_o)
    )) %>%
    select("p_id" = id, starts_with("p"), starts_with("q_vac"), household_members, c_contact_date, starts_with("add_"), -contains(".")) %>%
    haven::as_factor() %>%
    mutate_at(vars(contains("add_")), na_to_zero) %>%
    mutate_at(vars(contains("add_")), max_100) %>%
    mutate(
      p_age = as.numeric(as.character(p_age)),
      c_day_of_week = wday(c_contact_date, label = TRUE, abbr = FALSE),
      household_members_max8 = as.factor(cut(household_members + 1, breaks = c(0:7, Inf), labels = c(as.character(1:7), "8+")))
    ) %>%
    mutate(
      p_age_group = cut(p_age,
        breaks = age_breaks,
        labels = age_labels,
        right = F
      ),
      p_gender = suppressWarnings(fct_collapse(p_gender,
        Male = c("Male", "Boy"),
        Female = c("Female", "Girl"),
        other_level = c("Other")
      )),
      p_hiqual = suppressWarnings(fct_collapse(p_hiqual,
        `Apprenticeship` = "Apprenticeship",
        `Level 1 (1-4 GCSEs, O-levels (any), NVQ level 1, etc.)` = "Level 1",
        `Level 2 (5+ GCSEs, O-levels (passes), NVQ level 2, etc.)` = "Level 2",
        `Level 3 (A-level, BTEC, NVQ level 3, etc.)` = "Level 3",
        `Level 4+ (University degree and above)` = "Level 4+",
        Other = "Other",
        other_level = "No qualifications"
      )),
      p_ethnicity = fct_collapse(p_ethnicity,
        White = "White",
        Mixed = "Mixed/multiple ethnic groups",
        Asian = "Asian/Asian British",
        Black = c("Black/African/Caribbean/Black British"),
        Other = "Other ethnic group",
        `Prefer not to say` = "Prefer not to say",
      ),
      p_ethnicity = fct_relevel(
        p_ethnicity,
        "White", "Black", "Asian", "Mixed", "Other", "Prefer not to say"
      ),
      day_week = case_when(
        c_day_of_week %like% "Mon|Tue|Wed|Thur|Fri" ~ "weekday",
        c_day_of_week %like% "Sat|Sun" ~ "weekend",
        T ~ NA
      ),
      p_termtime = case_when(
        c_contact_date < as.Date("21-12-2024", format = "%d-%m-%Y") ~ "termtime",
        c_contact_date %in% seq.Date(as.Date("21-12-2024", format = "%d-%m-%Y"),
          as.Date("05-01-2025", format = "%d-%m-%Y"),
          by = 1
        ) ~ "xmas_holiday",
        !grepl("Wales", p_country) & c_contact_date %in% seq.Date(as.Date("06-01-2025", format = "%d-%m-%Y"),
          as.Date("14-02-2025", format = "%d-%m-%Y"),
          by = 1
        ) ~ "termtime",
        !grepl("Wales", p_country) & c_contact_date %in% seq.Date(as.Date("15-02-2025", format = "%d-%m-%Y"),
          as.Date("23-02-2025", format = "%d-%m-%Y"),
          by = 1
        ) ~ "halfterm",
        !grepl("Wales", p_country) & c_contact_date > as.Date("23-02-2025", format = "%d-%m-%Y") ~ "termtime",
        grepl("Wales", p_country) & c_contact_date %in% seq.Date(as.Date("06-01-2025", format = "%d-%m-%Y"),
          as.Date("21-02-2025", format = "%d-%m-%Y"),
          by = 1
        ) ~ "termtime",
        grepl("Wales", p_country) & c_contact_date %in% seq.Date(as.Date("22-02-2025", format = "%d-%m-%Y"),
          as.Date("02-03-2025", format = "%d-%m-%Y"),
          by = 1
        ) ~ "halfterm",
        grepl("Wales", p_country) & c_contact_date > as.Date("02-03-2025", format = "%d-%m-%Y") ~ "termtime",
        T ~ NA
      ),
      p_broad_age = case_when(
        p_age < 18 ~ "Child",
        p_age %in% 18:64 ~ "Adult",
        p_age > 64 ~ "Elderly"
      ),
      p_broad_hols = case_when(
        p_termtime == "termtime" ~ "Term time",
        is.na(p_termtime) ~ NA,
        T ~ "Holiday"
      )
    )
  if (children_input == F) {
    data <- data %>%
      mutate(
        p2_visit_work = case_when(
          (p2_volunteering == "Yes" &
            (p2_work_office == "Checked" | p2_work_client == "Checked" | p2_work_other == "Checked")) ~ "Checked",
          T ~ "Unchecked"
        )
      ) %>%
      rename(
        add_u18_work = add_u18_1,
        add_u18_school = add_u18_2,
        add_u18_other = add_u18_3,
        add_18_64_work = add_18_64_1,
        add_18_64_school = add_18_64_2,
        add_18_64_other = add_18_64_3,
        add_65_work = add_65_1,
        add_65_school = add_65_2,
        add_65_other = add_65_3
      ) %>%
      mutate(p_income = paste0(p_income_1, p_income_2)) %>%
      mutate(p_income = gsub("NA", "", p_income)) %>%
      mutate(p_income = gsub("Prefer not to say", "", p_income)) %>%
      mutate(p_income = gsub("Dont know", "", p_income)) %>%
      mutate(p_income = fct_collapse(p_income,
        `Less than £20,000` = c("£10,000 - £14,999", "£15,000 - £19,999", "Less than £10,000", "Less than £20,000"),
        `£20,000 - £39,999` = c("£20,000 - £29,999", "£30,000 - £39,999", "Between £20,000 and £39,999"),
        `£40,000 - £59,999` = c("£40,000 - £49,999", "£50,000 - £59,999", "Between £40,000 and £59,999"),
        `£60,000 - £100,000` = c(
          "£60,000 - £69,999", "£70,000 - £79,999",
          "£80,000 - £89,999", "£90,000 - £99,999",
          "£60,000 or more"
        ),
        `Over £100,000` = c("£100,000 - £149,999", "£150,000 - £199,999", "£200,000 or more")
      )) %>%
      mutate(p_income = fct_relevel(p_income, c(
        "Less than £20,000", "£20,000 - £39,999", "£40,000 - £59,999",
        "£60,000 - £100,000", "Over £100,000"
      )))

    # save ids of teachers
    teachers_id <<- (data %>% filter(tolower(p_job) %like% "teacher|learning support|classroom|athemat|teaching|pupil" &
      !grepl("pilates|dance|swim|online", p_job, ignore.case = T)))$p_id

    data <- data %>%
      mutate(
        add_u18_work = case_when(
          p_id %in% teachers_id ~ 0,
          T ~ add_u18_work
        ),
        add_18_64_work = case_when(
          p_id %in% teachers_id ~ 0,
          T ~ add_18_64_work
        ),
        add_65_work = case_when(
          p_id %in% teachers_id ~ 0,
          T ~ add_65_work
        ),
        add_u18_school = case_when(
          p_id %in% teachers_id ~ add_u18_school + add_u18_work,
          T ~ add_u18_school
        ),
        add_18_64_school = case_when(
          p_id %in% teachers_id ~ add_18_64_school + add_18_64_work,
          T ~ add_18_64_school
        ),
        add_65_school = case_when(
          p_id %in% teachers_id ~ add_65_school + add_65_work,
          T ~ add_65_school
        )
      )

    data <- data %>% mutate(
      p_any_risk = case_when(
        p_risk_1 == "Checked" | p_risk_2 == "Checked" |
          p_risk_3 == "Checked" | p_risk_4 == "Checked" |
          p_risk_5 == "Checked" | p_risk_6 == "Checked" |
          p_risk_7 == "Checked" | p_risk_8 == "Checked" |
          p_risk_9 == "Checked" ~ "Yes",
        T ~ "No"
      ),
      p_any_symp = case_when(
        p_symp_1 == "Checked" | p_symp_2 == "Checked" |
          p_symp_3 == "Checked" | p_symp_4 == "Checked" |
          p_symp_5 == "Checked" | p_symp_6 == "Checked" |
          p_symp_7 == "Checked" | p_symp_8 == "Checked" |
          p_symp_9 == "Checked" | p_symp_10 == "Checked" |
          p_symp_11 == "Checked" | p_symp_12 == "Checked" ~ "Yes",
        T ~ "No"
      )
    )
  } else {
    data <- data %>%
      rename(
        add_u18_school = add_u18_1, # this will be assumed to be same age group
        add_u18_other = add_u18_2,
        add_18_64_school = add_18_64_1,
        add_18_64_other = add_18_64_2,
        add_65_school = add_65_1,
        add_65_other = add_65_2
      ) %>%
      mutate(
        add_u18_work = 0,
        add_18_64_work = 0,
        add_65_work = 0
      )
  }
  
  data <- data %>% 
    mutate(large_n = add_u18_other + add_u18_work + add_u18_school + 
             add_18_64_other + add_18_64_work + add_18_64_school +
             add_65_other + add_65_work + add_65_school)
  
  ## remove large_n / _other if equal to n_contacts
  data <- data %>% left_join(n_contacts, by = 'p_id') %>% 
    mutate(eq = (n == large_n & additional_contacts %like% 'everyone')) %>% 
    mutate(large_n = case_when(eq ~ 0, T ~ large_n),
           add_u18_other = case_when(eq ~ 0, T ~ add_u18_other),
           add_18_64_other = case_when(eq ~ 0, T ~ add_18_64_other),
           add_65_other = case_when(eq ~ 0, T ~ add_65_other)) %>% 
    select(!n)

  data
}

# Define processing function for contacts
process_contacts <- function(data,
                             children_input = F # locations coded in a different order for children
) {
  data <- data %>%
    mutate(c_contact_date = case_when(
      contact_today == "Today" ~ as.Date(p2_date_calc),
      contact_today == "Yesterday" ~ as.Date(p2_date_calc) - 1,
      grepl("Other", contact_today) ~ as.Date(contact_date_o)
    )) %>%
    select("p_id" = id, s1_date, p2_complete, starts_with("c"), -c(starts_with("con_"))) %>%
    mutate_at(vars(contains("_age")), as.character) %>%
    mutate_at(vars(contains("_age")), as.numeric) %>%
    mutate_at(vars(contains("_sex")), as.character) %>%
    mutate_at(vars(contains("_emp")), as.character) %>%
    mutate_at(vars(contains("_ethn")), as.character) %>%
    mutate_at(vars(contains("_relationship")), as.character) %>%
    mutate_at(vars(contains("_freq")), as.character) %>%
    mutate_at(vars(contains("_physical")), as.character) %>%
    mutate_at(vars(contains("_tod")), as.character) %>%
    mutate_at(vars(contains("_time")), as.character) %>%
    pivot_longer(
      cols = starts_with("c") & matches("^c\\d+"),
      names_to = c("c_id", ".value"),
      names_pattern = "c(\\d+)_(.*)",
      values_drop_na = TRUE
    ) %>%
    mutate(c_age = age) %>% 
    drop_na(c_age) %>%
    select(-age) %>%
    mutate(c_age_group = cut(c_age,
      breaks = age_breaks,
      labels = age_labels,
      right = F
    ))

  data <- data %>%
    mutate(c_location = case_when(
      locn_1 == "Checked" ~ "Home",
      locn_6 == "Checked" ~ "School",
      locn_3 == "Checked" ~ "Work",
      T ~ "Other"
    )) %>%
    mutate(c_location_long = case_when(
      locn_1 == "Checked" ~ "Home",
      locn_6 == "Checked" ~ "School",
      locn_3 == "Checked" ~ "Work",
      locn_4 == "Checked" ~ "Transport",
      (locn_2 == "Checked" | locn_7 == "Checked" | locn_8 == "Checked" |
        locn_9 == "Checked" | locn_11 == "Checked") ~ "Leisure",
      T ~ "Other"
    )) %>%
    mutate(c_location_long_long = case_when(
      locn_1 == "Checked" ~ "Home",
      locn_6 == "Checked" ~ "School",
      locn_3 == "Checked" ~ "Work",
      locn_2 == "Checked" ~ "Other house",
      locn_4 == "Checked" ~ "Public transport",
      locn_5 == "Checked" ~ "Private transport",
      locn_7 == "Checked" ~ "Retail venue",
      locn_8 == "Checked" ~ "Hospitality venue",
      locn_9 == "Checked" ~ "Sports venue",
      locn_10 == "Checked" ~ "Healthcare setting",
      locn_11 == "Checked" ~ "Personal care establishment",
      locn_12 == "Checked" ~ "Outside",
      locn_13 == "Checked" ~ "Place of worship",
      T ~ "Other"
    ))

  # ADULT locn names:
  # 1 My house
  # 2 Their or someone else's house
  # 3 My workplace
  # 4 Any form of public transport (e.g. bus, train, taxis)
  # 5 Any form of private transport (e.g. car, excluding taxis)
  # 6 School, pre-school, nursery, or university
  # 7 Retail venue (e.g. supermarket, clothes shop, pharmacy)
  # 8 Hospitality venue (e.g. restaurant, pub, café)
  # 9 Sports venue (e.g. gym or cricket club)
  # 10 Healthcare setting (e.g. GP, dentist, hospital)
  # 11 Personal care establishment (e.g. hair dresser, barber, nail salon)
  # 12 Outside (e.g. in a park, street, nature trail)
  # 13 Place of worship (e.g. church, mosque)
  # 14 Somewhere else

  if (children_input == F) {
    data <- data %>%
      mutate(
        c_location = case_when(
          p_id %in% teachers_id & c_location == "Work" ~ "School",
          T ~ c_location
        ),
        c_location_long = case_when(
          p_id %in% teachers_id & c_location_long == "Work" ~ "School",
          T ~ c_location_long
        )
      )
  } else {
    data <- data %>%
      mutate(emp = case_when(
        grepl("Student", emp) ~ "Student (at school or in higher education)",
        T ~ emp
      ))
  }

  data <- data %>%
    mutate(c_day_of_week = wday(c_contact_date, label = TRUE, abbr = FALSE)) %>%
    select(!contains("locn")) %>%
    rename_with(.cols = c("sex":"time"), ~ paste0("c_", .x))
  # rename_with(.cols= c("sex":"tod"), ~paste0("c_", .x)) # while 'time' isn't in dataset for now

  data
}

household_processing <- function(data) {
  out <- data %>%
    select(id, starts_with("m")) %>%
    rename(p_id = id) %>%
    haven::as_factor() %>%
    pivot_longer(starts_with("m"),
      names_to = "name", values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    na.omit() %>%
    mutate(household_member = as.numeric(gsub("\\D", "", name)), information = gsub("m|_|[0-9]+", "", name)) %>%
    select(p_id, household_member, information, value) %>%
    pivot_wider(names_from = information, values_from = value) %>%
    filter(!is.na(age)) %>%
    group_by(p_id) %>%
    mutate(n_household_members = n()) %>%
    ungroup()

  if ("ethnicity" %in% colnames(out)) {
    out <- out %>% select(!ethnicity)
  }
  if ("job" %in% colnames(out)) {
    out <- out %>% select(!job)
  }

  out <- out %>% rename_with(.cols = !c(p_id, household_member, n_household_members), ~ paste0("h_", .x))

  out
}

#### PARTICIPANT ANALYSIS ####

# Define a function to summarize variables
summarize_var <- function(x) {
  if (is.numeric(x)) {
    # If the variable is numeric, calculate the mean and standard deviation
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    tibble(mean = mean_x, sd = sd_x) %>%
      mutate(estimate = paste0(round(mean_x, 1), " (", round(sd_x, 1), ")"))
  } else {
    if (length(setdiff(unique(x), c("Unknown", "Unemployed", "Retired", "Student", "Under 17", as.character(1:7)))) == 0) {
      x %>%
        factor() %>%
        fct_count(sort = FALSE, prop = TRUE) %>%
        filter(f %in% as.character(1:7)) %>%
        mutate(p = p / sum(p)) %>%
        mutate(
          n_percent = scales::percent(n / sum(n), accuracy = 0.1),
          p_percent = scales::percent(p, accuracy = 0.1)
        ) %>%
        select(f, n, n_percent) %>%
        mutate(estimate = case_when(
          n < censor_low_val ~ paste0("<", censor_low_val),
          T ~ paste0(n_percent, " (", n, ")")
        ))
    } else {
      x %>%
        factor() %>%
        fct_count(sort = FALSE, prop = TRUE) %>%
        mutate(
          n_percent = scales::percent(n / sum(n), accuracy = 0.1),
          p_percent = scales::percent(p, accuracy = 0.1)
        ) %>%
        select(f, n, n_percent) %>%
        mutate(estimate = case_when(
          n < censor_low_val ~ paste0("<", censor_low_val),
          T ~ paste0(n_percent, " (", n, ")")
        ))
    }
  }
}

#### WEIGHTING ####

weight_participants <- function(part = part,
                                age_structure = NULL,
                                age_structure_gendered = NULL,
                                ethnicity_structure = NULL,
                                age_structure_adult_child = NULL,
                                eth_age_sex_structure = eth_age_sex,
                                weighting = NULL,
                                group_vars = NULL,
                                truncation_percentile = c(0.05, 0.95) # c(0,1)
) {
  # align p_gender case
  part <- part %>% mutate(p_gender = tolower(p_gender))
  eth_age_sex_structure <- eth_age_sex_structure %>%
    mutate(p_gender = tolower(p_gender)) %>%
    select(!value)

  # add dataframe for day_week
  if ("day_week" %in% weighting) {
    day_week_structure <- data.table(
      day_week = c("weekday", "weekend"),
      proportion = c(5 / 7, 2 / 7)
    )
    if ("c_day_of_week" %in% group_vars) {
      stop("Can't weight by day_week and group by c_day_of_week")
    }
  }

  # specify which of age/gender/ethnicity are being weighted for
  age_gender_vec <- weighting[weighting %like% "age|gender|ethn"]

  # age_gender_vec plus p_adult_child if this is in the group_vars vector and p_age_group is in weighting vec
  a_g_e_grouping_vars <- if ("p_adult_child" %in% group_vars & "p_age_group" %in% weighting) {
    c(age_gender_vec, "p_adult_child")
  } else {
    age_gender_vec
  }

  a_g_e_df <- if (length(age_gender_vec) > 0) {
    eth_age_sex_structure %>%
      group_by(!!!syms(a_g_e_grouping_vars)) %>%
      summarise(proportion = sum(proportion))
  } else {
    data.frame()
  }

  # weight by age and/or gender within specified groups
  part_age <- part %>%
    group_by(!!!syms(unique(c(group_vars, age_gender_vec)))) %>%
    summarise(sample_totals = n()) %>%
    ungroup() %>%
    group_by(!!!syms(group_vars)) %>%
    mutate(group_total = sum(sample_totals))

  if (nrow(a_g_e_df) > 0) {
    part_age <- part_age %>% left_join(a_g_e_df, by = (a_g_e_grouping_vars))

    not_in_prop_groups <- part_age %>%
      group_by(!!!syms(group_vars)) %>%
      summarise(sum_in = sum(proportion, na.rm = T)) %>%
      ungroup()

    for (i in 1:nrow(part_age)) {
      if (is.na(part_age$proportion[i]) & "p_gender" %in% age_gender_vec) {
        if ((is.na(part_age$p_gender[i]) | part_age$p_gender[i] == "other")) {
          if ("p_ethnicity" %in% age_gender_vec) {
            if (part_age$p_ethnicity[i] %like% "Prefer") {
              if ("p_age_group" %in% a_g_e_grouping_vars) {
                part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(
                  a_g_e_df %>% group_by(p_age_group) %>%
                    summarise(proportion = sum(proportion)) %>%
                    left_join(part_age %>% group_by(!!!syms(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn|gender"])) %>%
                      summarise(sample_totals = sum(sample_totals))),
                  by = a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn|gender"],
                  unmatched = "ignore"
                ))
              }
            }
          } else {
            part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(
              a_g_e_df %>% group_by(!!!syms(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "gender"])) %>%
                summarise(proportion = sum(proportion)) %>%
                left_join(part_age %>% group_by(!!!syms(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "gender"])) %>%
                  summarise(sample_totals = sum(sample_totals))),
              by = a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "gender"],
              unmatched = "ignore"
            ))
          }
        }
      } else {
        if (is.na(part_age$proportion[i]) & "p_ethnicity" %in% age_gender_vec) {
          if (part_age$p_ethnicity[i] %like% "Prefer" & length(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn"]) > 0) {
            part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(
              a_g_e_df %>% group_by(!!!syms(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn"])) %>%
                summarise(proportion = sum(proportion)) %>%
                left_join(part_age %>% group_by(!!!syms(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn"])) %>%
                  summarise(sample_totals = sum(sample_totals))),
              by = a_g_e_grouping_vars[!a_g_e_grouping_vars %like% "ethn"],
              unmatched = "ignore"
            ))
          }
        }
      }
    }

    # scale up proportions to account for missing categories
    if (length(group_vars) > 0) {
      part_age <- part_age %>%
        left_join(not_in_prop_groups, by = group_vars) %>%
        mutate(proportion = proportion / sum_in) %>%
        select(!sum_in)
    } else {
      part_age <- part_age %>%
        mutate(sum_in = not_in_prop_groups$sum_in[1]) %>%
        mutate(proportion = proportion / sum_in) %>%
        select(!sum_in)
    }
  } else {
    part_age <- part_age %>% mutate(proportion = NA)
  }

  part_age <- part_age %>% mutate(
    sample_prop = sample_totals / nrow(part),
    post_strat_weight = case_when(
      is.na(proportion) ~ 1,
      T ~ proportion / sample_prop
    )
  )


  weighted_data <- part %>%
    select(p_id, !!!(group_vars), !!!syms(weighting))
  if (length(age_gender_vec) == 0) {
    weighted_data <- weighted_data %>% mutate(post_strat_weight = 1)
  } else {
    weighted_data <- weighted_data %>%
      left_join(unique(part_age %>% select(!!!(group_vars), !!!syms(age_gender_vec), post_strat_weight)), by = unique(c(group_vars, age_gender_vec)))
  }

  if ("day_week" %in% weighting) {
    weighted_data <- weighted_data %>%
      left_join(
        day_week_structure %>% left_join(
          part %>% group_by(day_week, !!!syms(group_vars)) %>%
            summarise(sample_tot = n()) %>% ungroup() %>%
            group_by(!!!syms(group_vars)) %>% mutate(pop = sum(sample_tot)) %>%
            mutate(sample_proportion = sample_tot / pop) %>% ungroup(),
          by = "day_week"
        ) %>%
          mutate(weight_mult = proportion / sample_proportion) %>% select(day_week, !!!syms(group_vars), weight_mult),
        by = c("day_week", group_vars)
      ) %>%
      mutate(post_strat_weight = case_when(
        is.na(day_week) ~ post_strat_weight,
        T ~ post_strat_weight * weight_mult
      )) %>%
      select(!weight_mult)
  }

  # truncate the weights that fall above the 99th percentile or below the 1% percentile
  percs <- quantile(weighted_data$post_strat_weight, truncation_percentile)
  if (n_distinct(weighted_data$post_strat_weight) >= 20) {
    weighted_data <- weighted_data %>%
      mutate(post_strat_weight = case_when(
        post_strat_weight < percs[1] ~ percs[1],
        post_strat_weight > percs[2] ~ percs[2],
        T ~ post_strat_weight
      ))
  }

  weighted_data
}

weight_participants_old <- function(part = part,
                                    age_structure = age_structure_fine,
                                    age_structure_gendered = NULL,
                                    ethnicity_structure = NULL,
                                    age_structure_adult_child = NULL,
                                    weighting = NULL,
                                    group_vars = NULL,
                                    truncation_percentile = c(0, 1) # c(0.01, 0.99)
) {
  # align p_gender case
  part <- part %>% mutate(p_gender = tolower(p_gender))

  # check all relevant dataframes are provided
  if ("p_age_group" %in% weighting & is.null(age_structure_gendered)) {
    stop("Must weight by age")
  }
  if ("p_gender" %in% weighting & is.null(age_structure_gendered)) {
    stop("Needs gender-stratified age structure")
  }
  if ("p_ethnicity" %in% weighting & is.null(ethnicity_structure)) {
    stop("Needs ethnicity structure")
  }

  # add dataframe for day_week
  if ("day_week" %in% weighting) {
    day_week_structure <- data.table(
      day_week = c("weekday", "weekend"),
      proportion = c(5 / 7, 2 / 7)
    )
    if ("c_day_of_week" %in% group_vars) {
      stop("Can't weight by day_week and group by c_day_of_week")
    }
  }

  # specify which of age/gender are being weighted for
  age_gender_vec <- weighting[grepl("age", weighting) | grepl("gender", weighting)]
  if (length(age_gender_vec) == 0) {
    age_gender_vec <- c("")
  }

  age_gender_df <- if (length(age_gender_vec) == 2 & "p_adult_child" %in% group_vars) {
    age_structure_adult_child
  } else {
    if (length(age_gender_vec) == 2) {
      age_structure_gendered
    } else {
      if (age_gender_vec == c("p_age_group") & "p_adult_child" %in% group_vars) {
        age_structure_adult_child %>%
          group_by(p_age_group, p_adult_child) %>%
          summarise(proportion = sum(proportion))
      } else {
        if (age_gender_vec == c("p_age_group")) {
          age_structure
        } else {
          if (age_gender_vec == c("p_gender")) {
            age_structure_gendered %>%
              select("p_gender", "proportion") %>%
              group_by(p_gender) %>%
              summarise(proportion = sum(proportion))
          } else {
            data.frame()
          }
        }
      }
    }
  }

  age_gender_vec_2 <- if (length(age_gender_vec) == 2 & "p_adult_child" %in% group_vars) {
    c(age_gender_vec, "p_adult_child")
  } else {
    age_gender_vec
  }

  # weight by age and/or gender within specified groups
  part_ag <- part %>%
    group_by(!!!syms(unique(c(group_vars, "p_age_group", "p_gender")))) %>%
    summarise(sample_totals_ag = n()) %>%
    ungroup() %>%
    group_by(!!!syms(unique(c(group_vars, "p_age_group")))) %>%
    mutate(sample_totals_a = sum(sample_totals_ag)) %>%
    ungroup() %>%
    group_by(!!!syms(unique(c(group_vars, "p_gender")))) %>%
    mutate(sample_totals_g = sum(sample_totals_ag)) %>%
    ungroup() %>%
    group_by(!!!syms(group_vars)) %>%
    mutate(sample_totals_group = sum(sample_totals_ag)) %>%
    ungroup()

  if (nrow(age_gender_df) > 0) {
    part_ag <- part_ag %>%
      left_join(age_gender_df, by = (age_gender_vec_2)) %>%
      select(!starts_with("n"))
  } else {
    part_ag <- part_ag %>% mutate(proportion = NA)
  }

  if (length(age_gender_vec) == 2) {
    part_ag <- part_ag %>% mutate(sample_totals = case_when(
      is.na(proportion) ~ sample_totals_a,
      T ~ sample_totals_ag
    ))
  } else {
    if (age_gender_vec == c("p_gender")) {
      part_ag <- part_ag %>% mutate(sample_totals = case_when(
        is.na(proportion) ~ sample_totals_group,
        T ~ sample_totals_g
      ))
    } else {
      if (age_gender_vec == c("p_age_group")) {
        part_ag <- part_ag %>% mutate(sample_totals = case_when(
          is.na(proportion) ~ sample_totals_group,
          T ~ sample_totals_a
        ))
      } else {
        part_ag <- part_ag %>% mutate(sample_totals = sample_totals_group)
      }
    }
  }

  if (is.na(sum(part_ag$proportion))) { # this only occurs if p_gender in weighting and exists p_gender=='other'/NA
    if ("p_age_group" %in% age_gender_vec) {
      part_ag <- part_ag %>%
        left_join(age_structure_fine %>% rename(proportion.y = proportion), by = "p_age_group") %>%
        mutate(proportion = case_when(
          is.na(proportion) ~ proportion.y,
          T ~ proportion
        )) %>%
        select(!c("n", "proportion.y"))
    }
  }

  part_ag <- part_ag %>% mutate(
    sample_prop = sample_totals / sample_totals_group,
    post_strat_weight = case_when(
      is.na(proportion) ~ 1,
      T ~ proportion / sample_prop
    )
  )

  weighted_data <- part %>%
    select(p_id, !!!(group_vars), !!!syms(weighting))
  if (age_gender_vec[1] == "") {
    weighted_data <- weighted_data %>% mutate(post_strat_weight = 1)
  } else {
    weighted_data <- weighted_data %>%
      left_join(unique(part_ag %>% select(!!!(group_vars), !!!syms(age_gender_vec), post_strat_weight)), by = c(group_vars, age_gender_vec))
  }

  ## weighting by other variables
  for (var in weighting[weighting %notin% age_gender_vec]) {
    population_df <- get(paste0(gsub("p_", "", var), "_structure"))

    sample_props <- part %>%
      group_by(!!sym(var), !!!syms(group_vars)) %>%
      summarise(sample_totals = n()) %>%
      ungroup() %>%
      group_by(!!!syms(group_vars)) %>%
      mutate(sample_totals_group = sum(sample_totals)) %>%
      ungroup() %>%
      group_by(!!sym(var)) %>%
      mutate(sample_totals_var_prop = sum(sample_totals) / nrow(part)) %>%
      ungroup() %>%
      mutate(sample_prop = sample_totals / sample_totals_group)

    not_na <- sum(sample_props %>% filter(!!sym(var) %in% unname(unlist(population_df[, ..var]))) %>% select(sample_totals_var_prop)) /
      ifelse(length(group_vars) == 0, 1, nrow(unique(sample_props[, group_vars])))

    sample_props <- sample_props %>%
      left_join(population_df, by = var) %>%
      mutate(post_strat_weight_update = case_when(
        is.na(proportion) ~ 1,
        T ~ (not_na * proportion / sample_prop)
      ))

    weighted_data <- weighted_data %>%
      left_join(sample_props, by = c(var, group_vars)) %>%
      mutate(post_strat_weight = post_strat_weight * post_strat_weight_update) %>%
      select(p_id, all_of(group_vars), all_of(weighting), post_strat_weight)
  }

  # truncate the weights that fall above the 99th percentile or below the 1% percentile
  percs <- quantile(weighted_data$post_strat_weight, truncation_percentile)
  weighted_data <- weighted_data %>%
    mutate(post_strat_weight = case_when(
      post_strat_weight < percs[1] ~ percs[1],
      post_strat_weight > percs[2] ~ percs[2],
      T ~ post_strat_weight
    ))

  weighted_data
}


#### NUMBER OF CONTACTS ####

# Define the bootstrap_analysis function
bootstrap_analysis <- function(contacts = contacts,
                               part = part,
                               age_structure = age_structure_fine,
                               age_structure_gendered = age_sex_strata,
                               ethnicity_structure = ons_ethnicity,
                               age_structure_adult_child = age_structure_adult_child,
                               variable_name = NULL,
                               is_participant_attribute = FALSE,
                               weights = NULL,
                               R = 1000,
                               conf_level = 0.95) {
  par_vars <- variable_name[is_participant_attribute]
  non_par_vars <- variable_name[!is_participant_attribute]

  if (length(weights) >= 1) {
    group_var <- NULL
    if (sum(is_participant_attribute) > 0) {
      group_var <- variable_name
    }
    weighted_all <- part %>%
      select(p_id) %>%
      left_join(
        weight_participants(part,
          age_structure,
          age_structure_gendered,
          ethnicity_structure,
          age_structure_adult_child,
          weighting = weights,
          group_vars = group_var[is_participant_attribute]
        ) %>%
          select(p_id, post_strat_weight),
        by = "p_id"
      )
  } else {
    weighted_all <- part %>%
      group_by(p_id) %>%
      summarise(post_strat_weight = 1)
  }

  # Join contacts and participant data
  data <- part %>%
    left_join(contacts %>% select(-c(c_contact_date, c_day_of_week)), by = "p_id") %>%
    mutate(p_gender = tolower(p_gender))

  if (FALSE %notin% is_participant_attribute) {
    if (is.null(variable_name)) {
      # Total contacts analysis with age adjustment
      weighted_data <- data %>%
        mutate(zero_contacts = is.na(c_id)) %>%
        # Group by participant ID and age group, count contacts
        group_by(p_id, zero_contacts) %>%
        summarise(n_contacts = n(), .groups = "drop") %>%
        mutate(n_contacts = ifelse(zero_contacts, 0, n_contacts)) %>%
        select(!zero_contacts) %>%
        # Add large group contacts
        left_join(part %>% select(p_id, large_n), by = "p_id") %>%
        mutate(
          n_contacts = n_contacts + large_n,
          n_contacts = ifelse(n_contacts > max_n_contacts, max_n_contacts, n_contacts)
        ) %>%
        # Add weighting
        left_join(weighted_all, by = c("p_id"))

      # Define bootstrap function
      boot_mean <- function(data, indices) mean(data[indices])

      # Perform bootstrap
      boot_res <- boot(weighted_data$n_contacts, boot_mean, R = R, weights = weighted_data$post_strat_weight)
      ci <- boot.ci(boot_res, type = "perc")

      # Prepare results
      results <- tibble(
        Category = "Total",
        n = n_distinct(weighted_data$p_id),
        mean = mean(boot_res$t),
        lower_ci = ci$percent[4],
        upper_ci = ci$percent[5]
      )
    } else {
      # Stratified analysis
      weighted_data <- data %>%
        mutate(zero_contacts = is.na(c_id)) %>%
        # Group by participant ID and age group, count contacts
        group_by(p_id, !!!syms(variable_name), zero_contacts) %>%
        summarise(n_contacts = n(), .groups = "drop") %>%
        mutate(n_contacts = ifelse(zero_contacts, 0, n_contacts)) %>%
        select(!zero_contacts) %>%
        # Add large group contacts
        left_join(part %>% select(p_id, large_n), by = "p_id") %>%
        mutate(n_contacts = n_contacts + large_n) %>%
        mutate(n_contacts = case_when(n_contacts > max_n_contacts ~ max_n_contacts, T ~ n_contacts)) %>%
        # Add weighting
        left_join(weighted_all, by = c("p_id")) %>%
        # Group by specified variable
        group_by(!!!syms(variable_name)) %>%
        summarise(
          n = n_distinct(p_id),
          n_unique = length(unique(n_contacts)),
          unique_val = list(unique(n_contacts)),
          total_contacts = list(n_contacts),
          weights = list(post_strat_weight),
          .groups = "drop"
        )

      weighted_data_mult <- weighted_data %>%
        filter(n_unique > 1) %>%
        select(!unique_val)
      weighted_data_one <- weighted_data %>% filter(n_unique == 1)

      weighted_data_l <- weighted_data_mult %>% purrr::transpose()
      boot_res <- map(
        weighted_data_l,
        ~ boot(
          data = .x$total_contacts,
          function(d, i) mean(d[i]),
          R = R,
          weights = .x$weights
        )
      )

      mean_in <- map(boot_res, ~ mean(.x$t))
      ci <- map(boot_res, ~ boot.ci(., type = "perc")$percent[4:5])
      # ci originally used 'basic' bootstrap method (i.e. mean \pm 1.96*SD),
      # but this produces negative values when SD is large, e.g. in a low-participation
      # category where one participant has high 'large_n'
      # now using 'percent' method (i.e. take 2.5 and 97.5 percentiles)
      lower_ci <- map_dbl(ci, ~ .[1])
      upper_ci <- map_dbl(ci, ~ .[2])

      results <- weighted_data_mult %>%
        select(!!!syms(variable_name), n) %>%
        mutate(
          mean = unlist(mean_in),
          lower_ci = unlist(lower_ci),
          upper_ci = unlist(upper_ci)
        )
      results <- rbind(
        results,
        weighted_data_one %>% select(!!!syms(variable_name), n, unique_val) %>%
          mutate(
            mean = unlist(unique_val),
            lower_ci = unlist(unique_val),
            upper_ci = unlist(unique_val)
          ) %>% select(!unique_val)
      )
    }
  } else {
    # Non-participant attribute analysis
    summary_data <- if (is.null(variable_name)) {
      # If no variable specified, count contacts per participant
      data %>%
        mutate(zero_contacts = is.na(c_id)) %>%
        group_by(p_id, zero_contacts) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(count = case_when(zero_contacts ~ 0, T ~ count)) %>%
        select(!zero_contacts) %>%
        left_join(weighted_all, by = c("p_id"))
    } else {
      if (length(non_par_vars) == 1 & "c_location" %in% non_par_vars) { # add in large_n
        variable_name_add <- c(variable_name, colnames(data)[grepl("add_", colnames(data))])
        long_par_vars <- c(par_vars, colnames(data)[grepl("add_", colnames(data))])
        data %>%
          mutate(zero_contacts = is.na(c_id)) %>%
          group_by(p_id, !!!syms(variable_name_add), zero_contacts) %>%
          summarise(count = n(), .groups = "drop") %>%
          mutate(count = case_when(zero_contacts ~ 0, T ~ count)) %>%
          select(!zero_contacts) %>%
          complete(c_location, nesting(p_id, !!!syms(long_par_vars)), fill = list(count = 0)) %>%
          drop_na() %>%
          mutate(count = case_when(
            c_location == "Work" ~ count + add_u18_work + add_18_64_work + add_65_work,
            c_location == "School" ~ count + add_u18_school + add_18_64_school + add_65_school,
            c_location == "Other" ~ count + add_u18_other + add_18_64_other + add_65_other,
            T ~ count
          )) %>%
          mutate(count = case_when(count > max_n_contacts ~ max_n_contacts, T ~ count)) %>%
          select(!starts_with("add_")) %>%
          left_join(weighted_all, by = c("p_id")) # %>%
        # filter(!is.na(!!!syms(variable_name)),
        # !(nchar(as.character(!!!syms(variable_name)))==0)) # fixing '' in time
      } else {
        # Count contacts per participant and specified variable
        data %>%
          mutate(zero_contacts = is.na(c_id)) %>%
          group_by(!!!syms(variable_name), p_id, zero_contacts) %>%
          summarise(count = n(), .groups = "drop") %>%
          drop_na() %>%
          mutate(count = case_when((zero_contacts == T) ~ 0, T ~ count)) %>%
          select(!zero_contacts) %>%
          mutate(count = case_when(count > max_n_contacts ~ max_n_contacts, T ~ count)) %>%
          complete(part %>% select(p_id, !!!syms(par_vars)), !!!syms(non_par_vars), fill = list(count = 0)) %>%
          # filter(!is.na(!!!syms(variable_name)),
          # !(nchar(as.character(!!!syms(variable_name)))==0)) %>%
          left_join(weighted_all, by = c("p_id")) # fixing '' in time
      }
    }

    # Calculate results
    weighted_data <- summary_data %>%
      group_by(!!!syms(variable_name)) %>%
      summarise(
        n = n_distinct(p_id),
        weights = list(post_strat_weight),
        n_unique = length(unique(count)),
        unique_val = list(unique(count)),
        total_contacts = list(count),
        .groups = "drop"
      )

    weighted_data_mult <- weighted_data %>%
      filter(n_unique > 1) %>%
      select(!unique_val)
    weighted_data_one <- weighted_data %>% filter(n_unique == 1)

    weighted_data_l <- weighted_data_mult %>% purrr::transpose()
    boot_res <- map(
      weighted_data_l,
      ~ boot(
        data = .x$total_contacts,
        function(d, i) mean(d[i]),
        R = R,
        weights = .x$weights
      )
    )

    mean_in <- map(boot_res, ~ mean(.x$t))
    ci <- map(boot_res, ~ boot.ci(., type = "perc")$percent[4:5])
    # ci originally used 'basic' bootstrap method (i.e. mean \pm 1.96*SD),
    # but this produces negative values when SD is large, e.g. in a low-participation
    # category where one participant has high 'large_n'
    # now using 'percent' method (i.e. take 2.5 and 97.5 percentiles)
    lower_ci <- map_dbl(ci, ~ .[1])
    upper_ci <- map_dbl(ci, ~ .[2])

    results <- weighted_data_mult %>%
      select(!!!syms(variable_name), n) %>%
      mutate(
        mean = unlist(mean_in),
        lower_ci = unlist(lower_ci),
        upper_ci = unlist(upper_ci)
      )
    results <- rbind(
      results,
      weighted_data_one %>% select(!!!syms(variable_name), n, unique_val) %>%
        mutate(
          mean = unlist(unique_val),
          lower_ci = unlist(unique_val),
          upper_ci = unlist(unique_val)
        ) %>% select(!unique_val)
    )
  }

  # Rename the grouping variable to "Category" if it exists
  if (!is.null(variable_name) & length(variable_name) == 1) {
    results <- results %>% rename_with(~"Category", all_of(variable_name))

    # Round numeric results and sort by Category
    results %>%
      mutate(across(c(mean, lower_ci, upper_ci), ~ round(., 2))) %>%
      arrange(Category)
  } else {
    results %>%
      mutate(across(c(mean, lower_ci, upper_ci), ~ round(., 2)))
  }
}


plot_contact_data <- function(data, variable,
                              ci_var, max_upper, upper_fixed_at) {
  filtered_data <- data %>%
    filter(Variable == variable) %>%
    select(Variable, Category_short, mean, lower_ci, upper_ci)

  filtered_data <- reorder_factors(filtered_data, variable)

  filtered_data <- filtered_data %>%
    mutate(over_max_upper_ci = case_when(
      upper_ci > max_upper ~ T,
      T ~ F
    )) %>%
    mutate(upper_ci = case_when(
      upper_ci > max_upper ~ max_upper,
      T ~ upper_ci
    ))

  filtered_data %>%
    ggplot(aes(
      x = Category_short, y = mean, ymin = lower_ci, ymax = upper_ci,
      col = is.na(lower_ci)
    )) +
    scale_color_manual(values = c("black", "darkgrey")) +
    geom_pointrange(aes(x = Category_short, ymin = lower_ci, ymax = upper_ci)) +
    geom_arrowsegment(
      data = filtered_data %>% filter(over_max_upper_ci == T),
      aes(x = Category_short, xend = Category_short, y = mean, yend = upper_ci),
      arrows = arrow(type = "closed", length = unit(0.2, "cm"))
    ) +
    coord_flip() +
    ggtitle(variable) +
    # facet_grid(Variable~., scales = "free",space='free') +
    # facet_wrap(~Variable, scales = "free") +
    labs(x = NULL, y = "Mean with 95% CI") +
    theme_bw() +
    ylim(c(0, upper_fixed_at)) +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
}


#### COMPARING TWO MEANS ####

bootstrap_compare <- function(
    part_in = part,
    contacts_in = contacts,
    age_structure = age_structure_fine,
    age_structure_gendered = age_sex_strata,
    ethnicity_structure = ons_ethnicity,
    age_structure_ac = age_structure_adult_child,
    group_name = NULL,
    ratio_var = NULL,
    is_participant_attribute = FALSE,
    weights = c("p_age_group", "p_gender", "p_gender", "p_ethnicity"),
    R = 1000,
    conf_level = 0.95,
    neat = F,
    summary_var = "mean") {
  if (length(weights) >= 1) {
    group_var <- NULL
    if (is_participant_attribute) {
      group_var <- c(group_name, ratio_var)
    }
    weighted_all <- part_in %>%
      select(p_id) %>%
      left_join(
        weight_participants(part_in,
          age_structure,
          age_structure_gendered,
          ethnicity_structure,
          age_structure_ac,
          weighting = weights,
          group_vars = group_var
        ) %>%
          select(p_id, post_strat_weight),
        by = "p_id"
      )
  } else {
    weighted_all <- part_in %>%
      group_by(p_id) %>%
      summarise(post_strat_weight = 1)
  }

  # Join contacts and participant data
  data <- part_in %>%
    left_join(contacts_in %>% select(-c(c_contact_date, c_day_of_week)), by = "p_id") %>%
    mutate(p_gender = tolower(p_gender))

  # Stratified analysis
  weighted_data <- data %>%
    mutate(zero_contacts = is.na(c_id)) %>%
    # Group by participant ID and age group, count contacts
    group_by(p_id, !!!syms(group_name), !!!syms(ratio_var), zero_contacts) %>%
    summarise(n_contacts = n(), .groups = "drop") %>%
    mutate(n_contacts = ifelse(zero_contacts, 0, n_contacts)) %>%
    select(!zero_contacts) %>%
    # Add large group contacts
    left_join(part %>% select(p_id, large_n), by = "p_id") %>%
    mutate(n_contacts = n_contacts + large_n) %>%
    mutate(n_contacts = ifelse(n_contacts > max_n_contacts, max_n_contacts, n_contacts)) %>%
    # Add weighting
    left_join(weighted_all, by = c("p_id")) %>%
    # Group by specified variable
    group_by(!!!syms(group_name), !!!syms(ratio_var)) %>%
    summarise(
      n = n_distinct(p_id),
      n_unique = length(unique(n_contacts)),
      total_contacts = list(n_contacts),
      weights = list(post_strat_weight),
      .groups = "drop"
    ) %>%
    # Remove any groups with only 1 observation
    filter(n_unique > 1) %>%
    drop_na()

  weighted_data_l <- weighted_data %>% purrr::transpose()
  boot_res <- map(
    weighted_data_l,
    ~ boot(
      data = .x$total_contacts,
      function(d, i) mean(d[i]),
      R = R,
      weights = .x$weights
    )
  )

  if (length(group_name) == 1 & length(ratio_var) == 1) {
    if (nrow(unique(part[, group_name])) == 2) {
      values <- list(c(boot_res[[1]]$t), c(boot_res[[2]]$t), c(boot_res[[3]]$t), c(boot_res[[4]]$t))
      rrs <- list(
        c(boot_res[[1]]$t) / c(boot_res[[2]]$t),
        c(boot_res[[2]]$t) / c(boot_res[[1]]$t),
        c(boot_res[[3]]$t) / c(boot_res[[4]]$t),
        c(boot_res[[4]]$t) / c(boot_res[[3]]$t)
      )
      diffs <- list(
        c(boot_res[[1]]$t) - c(boot_res[[2]]$t),
        c(boot_res[[2]]$t) - c(boot_res[[1]]$t),
        c(boot_res[[3]]$t) - c(boot_res[[4]]$t),
        c(boot_res[[4]]$t) - c(boot_res[[3]]$t)
      )
    } else {
      if (nrow(unique(part[, group_name])) == 3) {
        values <- list(
          c(boot_res[[1]]$t), c(boot_res[[2]]$t), c(boot_res[[3]]$t), c(boot_res[[4]]$t),
          c(boot_res[[5]]$t), c(boot_res[[6]]$t)
        )
        rrs <- list(
          c(boot_res[[1]]$t) / c(boot_res[[2]]$t),
          c(boot_res[[2]]$t) / c(boot_res[[1]]$t),
          c(boot_res[[3]]$t) / c(boot_res[[4]]$t),
          c(boot_res[[4]]$t) / c(boot_res[[3]]$t),
          c(boot_res[[5]]$t) / c(boot_res[[6]]$t),
          c(boot_res[[6]]$t) / c(boot_res[[5]]$t)
        )
        diffs <- list(
          c(boot_res[[1]]$t) - c(boot_res[[2]]$t),
          c(boot_res[[2]]$t) - c(boot_res[[1]]$t),
          c(boot_res[[3]]$t) - c(boot_res[[4]]$t),
          c(boot_res[[4]]$t) - c(boot_res[[3]]$t),
          c(boot_res[[5]]$t) - c(boot_res[[6]]$t),
          c(boot_res[[6]]$t) - c(boot_res[[5]]$t)
        )
      } else {
        stop("Not set up for groups of more than 3 yet.")
      }
    }
  } else {
    if (is.null(group_name)) {
      values <- list(c(boot_res[[1]]$t), c(boot_res[[2]]$t))
      rrs <- list(
        c(boot_res[[1]]$t) / c(boot_res[[2]]$t),
        c(boot_res[[2]]$t) / c(boot_res[[1]]$t)
      )
      diffs <- list(
        c(boot_res[[1]]$t) - c(boot_res[[2]]$t),
        c(boot_res[[2]]$t) - c(boot_res[[1]]$t)
      )
    }
  }

  results <- data.frame(
    (weighted_data %>% select(!!!syms(group_name), !!!syms(ratio_var)))
  )

  if (length(group_name) == 1 & length(ratio_var) == 1) {
    colnames(results) <- c("grouping_var", "ratio_var")
  } else {
    if (is.null(group_name)) {
      colnames(results) <- c("ratio_var")
    }
  }

  if (neat == T) {
    for (data_name in c("values", "rrs", "diffs")) {
      med_in <- map(get(data_name), ~ get(summary_var)(.x))
      ci <- map(get(data_name), ~ unname(quantile(.x, c(0.025, 0.975))))
      lower_ci <- map_dbl(ci, ~ .[1])
      upper_ci <- map_dbl(ci, ~ .[2])

      names <- colnames(results)
      results <- results %>% mutate(
        paste = paste0(
          format(round(unlist(med_in), 2), nsmall = 2), " (",
          trimws(format(round(unlist(lower_ci), 2), nsmall = 2)), ", ",
          trimws(format(round(unlist(upper_ci), 2), nsmall = 2)), ")"
        )
      )
      colnames(results) <- c(names, paste0("estimate", "_", data_name))
    }
  } else {
    for (data_name in c("values", "rrs", "diffs")) {
      med_in <- map(get(data_name), ~ get(summary_var)(.x))
      ci <- map(get(data_name), ~ unname(quantile(.x, c(0.025, 0.975))))
      lower_ci <- map_dbl(ci, ~ .[1])
      upper_ci <- map_dbl(ci, ~ .[2])

      names <- colnames(results)
      results <- results %>% mutate(
        median = unlist(med_in),
        lower_ci = unlist(lower_ci),
        upper_ci = unlist(upper_ci)
      )
      colnames(results) <- c(names, paste0(c(summary_var, "lower_ci", "upper_ci"), "_", data_name))
    }
  }

  results
}

#### DENOMINATOR-SPECIFIC CONTACT ANALYSIS ####

# Define the bootstrap_analysis function
bootstrap_analysis_denom <- function(contacts = contacts,
                                     part = part,
                                     households = households,
                                     numerator_variable,
                                     numerator_value,
                                     denominator_variable,
                                     denominator_value,
                                     R = 1000,
                                     conf_level = 0.95) {
  numerator_df <- case_when(
    substr(numerator_variable, 1, 1) == "c" ~ "contacts",
    T ~ "stop"
  )
  if (numerator_df == "stop") {
    stop("Wrong numerator?")
  }
  numerator_df <- get(numerator_df)

  denominator_df <- case_when(
    substr(denominator_variable[1], 1, 1) == "c" ~ "contacts",
    substr(denominator_variable[1], 1, 1) == "p" ~ "part",
    substr(denominator_variable[1], 1, 1) == "h" ~ "households"
  )
  denominator_df <- get(denominator_df)

  if (length(denominator_variable) == 1) {
    denominator_df <- denominator_df %>%
      filter(!!as.symbol(denominator_variable) %in% denominator_value)
  } else {
    ids_df <- denominator_df[, denominator_variable]
    ids <- c()
    for (i in 1:nrow(ids_df)) {
      for (var in denominator_variable) {
        if (!is.na(ids_df[i, var]) & ids_df[i, var] == denominator_value) {
          ids <- c(ids, i)
        }
      }
    }
    ids <- unique(ids)
    denominator_df <- denominator_df %>% slice(ids)
  }

  # Join contacts and participant data
  data <- numerator_df %>% filter(p_id %in% denominator_df$p_id)

  summary_data <- data %>%
    group_by(p_id, !!!syms(numerator_variable)) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!!as.symbol(numerator_variable) %in% numerator_value) %>%
    complete(p_id = unique(denominator_df$p_id), fill = list(count = 0))

  # Calculate results
  results <- summary_data %>%
    summarise(
      n = n_distinct(p_id),
      counts = list(count),
      .groups = "drop"
    ) %>%
    # Perform bootstrap for each group
    mutate(
      boot_res = map(counts, ~ boot(., function(d, i) mean(d[i]), R = R)),
      mean = map_dbl(boot_res, ~ .$t0),
      ci = map(boot_res, ~ boot.ci(., type = "perc")$percent[4:5]),
      lower_ci = map_dbl(ci, ~ .[1]),
      upper_ci = map_dbl(ci, ~ .[2])
    ) %>%
    select(-boot_res, -ci, -counts)

  # Round numeric results and sort by Category
  results %>%
    mutate(across(c(mean, lower_ci, upper_ci), ~ round(., 2)))
}

#### DEGREE DISTRIBUTIONS ####

plot_deg_dist <- function(part, contacts, chars, variable_id, prob = T) {
  variable <- chars$variable[variable_id]
  variable_name <- chars$name[variable_id]
  participant_var <- chars$participant_var[variable_id]

  if (participant_var) {
    data <- part %>%
      left_join(contacts, by = "p_id") %>%
      mutate(zero_contacts = is.na(c_id)) %>%
      # removing individuals where is.na(variable)
      filter(!is.na(!!sym(variable))) %>%
      # Group by participant ID and variable, count contacts
      group_by(p_id, !!sym(variable), zero_contacts) %>%
      summarise(n_contacts = n(), .groups = "drop") %>%
      mutate(n_contacts = ifelse(zero_contacts, 0, n_contacts)) %>%
      select(!zero_contacts) %>%
      # Add large group contacts
      left_join(part %>% select(p_id, large_n), by = "p_id") %>%
      mutate(n_contacts = n_contacts + large_n) %>%
      mutate(n_contacts = ifelse(n_contacts > max_n_contacts_dd, max_n_contacts_dd, n_contacts))
  } else {
    if (variable == "c_location") {
      data <- part %>%
        left_join(contacts, by = "p_id") %>%
        # removing individuals where is.na(variable)
        filter(!is.na(!!sym(variable))) %>%
        # Group by participant ID and variable, count contacts
        group_by(
          p_id, !!sym(variable), add_18_64_school, add_18_64_work, add_18_64_other, add_u18_work, add_u18_other, add_u18_school,
          add_65_other, add_65_school, add_65_work
        ) %>%
        summarise(n_contacts = n(), .groups = "drop") %>%
        # Add large group contacts
        mutate(n_contacts = case_when(
          c_location == "Home" ~ n_contacts,
          c_location == "School" ~ n_contacts + add_u18_school + add_18_64_school + add_65_school,
          c_location == "Work" ~ n_contacts + add_u18_work + add_18_64_work + add_65_work,
          c_location == "Other" ~ n_contacts + add_u18_other + add_18_64_other + add_65_other
        )) %>%
        select(!starts_with("add_")) %>%
        mutate(n_contacts = ifelse(n_contacts > max_n_contacts_dd, max_n_contacts_dd, n_contacts))
    } else {
      data <- part %>%
        left_join(contacts, by = "p_id") %>%
        # removing individuals where is.na(variable)
        filter(!is.na(!!sym(variable))) %>%
        # Group by participant ID and variable, count contacts
        group_by(p_id, !!sym(variable)) %>%
        summarise(n_contacts = n(), .groups = "drop") %>%
        mutate(n_contacts = ifelse(n_contacts > max_n_contacts_dd, max_n_contacts_dd, n_contacts))
    }
  }

  hist_plot <- data %>%
    ggplot(aes(x = n_contacts, fill = !!sym(variable))) +
    geom_histogram(bins = 100) +
    theme_minimal() +
    labs(fill = paste(variable_name)) +
    # scale_fill_brewer(palette = 'Set1') +
    xlab("Contact degree") +
    ylab("Density")
  hist_plot
  ggsave(
    here::here(
      "results", survey,
      "degree_distributions",
      "histograms",
      paste0(variable, "_histogram", ".png")
    ),
    width = 7, height = 5, dpi = 600, bg = "white"
  )

  if (prob) {
    plot <- data %>%
      group_by(!!sym(variable)) %>%
      mutate(n_participants = length(unique(p_id))) %>%
      ungroup() %>%
      group_by(n_contacts, !!sym(variable), n_participants) %>%
      summarise(people_w_n_contacts = length(unique(p_id))) %>%
      arrange(desc(n_contacts)) %>%
      group_by(!!sym(variable), n_participants) %>%
      mutate(
        cumul_n = cumsum(people_w_n_contacts),
        prob_n = cumul_n / n_participants
      ) %>%
      filter(n_contacts > 0) %>% # for log scale
      ggplot(aes(x = n_contacts, y = prob_n, col = !!sym(variable))) +
      geom_line(lwd = 0.8) +
      geom_point() +
      theme_minimal() +
      labs(col = paste(variable_name)) +
      scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
      scale_y_log10(breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)) +
      xlab("Contact degree") +
      ylab("Proportion of participants")
  } else {
    plot <- data %>%
      group_by(n_contacts, !!sym(variable)) %>%
      summarise(people_w_n_contacts = length(unique(p_id))) %>%
      arrange(desc(n_contacts)) %>%
      group_by(!!sym(variable)) %>%
      mutate(cumul_n = cumsum(people_w_n_contacts)) %>%
      filter(n_contacts > 0) %>% # for log scale
      ggplot(aes(x = n_contacts, y = cumul_n, col = !!sym(variable))) +
      geom_line(lwd = 0.8) +
      geom_point() +
      theme_minimal() +
      labs(col = paste(variable_name)) +
      scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
      scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000)) +
      xlab("Contact degree") +
      ylab("Participants")
  }
  plot
  ggsave(
    here::here(
      "results", survey,
      "degree_distributions",
      paste(ifelse(prob, "probability", "total")),
      paste0(variable, ifelse(prob, "_prob", "_total"), ".png")
    ),
    width = 7, height = 6, dpi = 600, bg = "white"
  )
  plot
}

#### CONTACT MATRICES ####

create_contact_matrices <- function(participant_data, contact_data,
                                    age_structure = age_structure_fine,
                                    age_structure_gendered = age_sex_strata,
                                    ethnicity_structure = ons_ethnicity,
                                    age_structure_adult_child = age_structure_adult_child,
                                    participant_var, contact_var, large_n_input = T,
                                    weighting_vec = c("p_age_group", "p_gender", "p_ethnicity", "day_week"),
                                    polymod_weights = polymod_wts) {
  if (participant_var %in% weighting_vec) {
    weighting_vec <- weighting_vec[weighting_vec != participant_var]
  }

  if (length(weighting_vec) >= 1) {
    weighted_all <- part %>%
      select(p_id) %>%
      left_join(
        weight_participants(participant_data,
          age_structure,
          age_structure_gendered,
          ethnicity_structure,
          age_structure_adult_child,
          weighting = weighting_vec,
          group_vars = participant_var
        ) %>%
          select(p_id, post_strat_weight),
        by = "p_id"
      )
  } else {
    weighted_all <- part %>%
      group_by(p_id) %>%
      summarise(post_strat_weight = 1)
  }

  # Determine which variables to select from participant_data
  select_vars <- unique(c("p_id", "p_age_group", weighting_vec, participant_var))

  if (contact_var == "c_age_group" & large_n_input) {
    select_vars <- c(select_vars, colnames(participant_data)[grepl("add_", colnames(participant_data))])
  }

  # Calculate contacts for each combination and location
  contact_counts <- contact_data %>%
    filter(p_id %in% participant_data$p_id) %>% # in case contact_data/participant_data are filtered, e.g. by gender
    left_join(participant_data %>% filter(p_id %in% contact_data$p_id) %>% select(!!!syms(select_vars)), by = "p_id") %>%
    group_by(!!!syms(select_vars), !!sym(contact_var), c_location) %>%
    summarise(n_contacts = n(), .groups = "drop")

  add_data <- participant_data %>%
    filter(p_id %notin% contact_data$p_id) %>%
    select(!!!syms(select_vars)) %>%
    mutate(
      c_age_group = unname(unlist(contact_data[1, contact_var])),
      c_location = "Other", n_contacts = 0
    )
  colnames(add_data) <- c(select_vars, contact_var, "c_location", "n_contacts")
  contact_counts <- rbind(
    contact_counts,
    add_data
  ) %>% # filling in those not in contact data (ie zero contacts)
    complete(nesting(!!!syms(select_vars)), !!sym(contact_var), c_location, fill = list(n_contacts = 0))

  # age_vals_ch <- age_breaks[is.finite(age_breaks) & age_breaks<21]
  # age_labels_ch <- c(paste0(c(0, age_vals_ch[1:length(age_vals_ch)-1]), '-', c(age_vals_ch-1)), paste0(age_vals_ch[length(age_vals_ch)], '+'))
  # age_labels_ch <- age_labels_ch[1:(length(age_labels_ch)-1)]
  #
  # age_vals_el <- age_breaks[is.finite(age_breaks) & age_breaks > 65]
  # age_labels_el <- c(paste0(c(65, age_vals_el[1:length(age_vals_el)-1]), '-', c(age_vals_el-1)), paste0(age_vals_el[length(age_vals_el)], '+'))

  ## turning 'add_' columns into actual contacts, weighting by setting-specific age distribution in polymod
  if (contact_var == "c_age_group" & large_n_input) {
    contact_counts <- contact_counts %>%
      left_join(
        polymod_weights %>%
          mutate(c_location = firstup(c_location)) %>%
          pivot_wider(names_from = broad_age_group, values_from = prob),
        by = c("p_age_group", "c_age_group", "c_location")
      )

    contact_counts <- contact_counts %>%
      mutate(n_contacts = case_when(
        c_location == "Home" ~ n_contacts,
        c_location == "Work" ~ n_contacts +
          add_u18_work * `0-17` + add_18_64_work * `18-64` + add_65_work * `65+`,
        c_location == "School" ~ n_contacts +
          add_u18_school * `0-17` + add_18_64_school * `18-64` + add_65_school * `65+`,
        c_location == "Other" ~ n_contacts +
          add_u18_other * `0-17` + add_18_64_other * `18-64` + add_65_other * `65+`,
        T ~ n_contacts
      ))

    contact_counts <- contact_counts %>%
      select(!c(starts_with("add_"), `0-17`, `18-64`, `65+`))

    select_vars <- select_vars[!grepl("add_", select_vars)]
  }

  # Calculate total contacts across all locations
  total_contacts <- contact_counts %>%
    group_by(!!!syms(select_vars), !!sym(contact_var)) %>%
    summarise(n_contacts = sum(n_contacts), .groups = "drop") %>%
    mutate(c_location = "Total")

  # Combine location-specific and total contacts
  all_contacts <- bind_rows(contact_counts, total_contacts)
  # all_contacts <- all_contacts %>%
  #   mutate(p_gender = case_when(is.na(p_gender) ~ 'Other',
  #                               T ~ p_gender)) %>%
  #   mutate(age_gender = paste0(p_age_group,'_',tolower(p_gender)))

  # Calculate survey weights
  weighted_contact_matrix <- all_contacts %>%
    left_join(weighted_all, by = c("p_id"))

  # Create survey design object
  svy_design <- svydesign(
    ids = ~p_id,
    strata = ~p_age_group,
    weights = ~post_strat_weight,
    data = weighted_contact_matrix
  )

  weighted_contacts <- svyby(
    formula = ~n_contacts,
    by = as.formula(paste0("~c_location + ", participant_var, " + ", contact_var)),
    design = svy_design,
    FUN = svymean,
    na.rm = TRUE,
    vartype = "ci"
  )

  if (contact_var == "c_age_group") {
    weighted_contacts$c_age_group <- factor(weighted_contacts$c_age_group,
      levels = age_labels
    )
  }

  return(weighted_contacts)
}

plot_contact_matrix <- function(data, location_type, participant_var, contact_var, uncertainty = F) {
  filtered_data <- data %>%
    filter(c_location %in% location_type) %>%
    filter(
      !(!!sym(participant_var) %like% "Prefer not to say"),
      !(!!sym(contact_var) %like% "Prefer not to say")
    )

  if (participant_var == "p_age_group") {
    filtered_data$p_age_group <- factor(filtered_data$p_age_group, levels = age_labels)
  }
  if (contact_var == "c_age_group") {
    filtered_data$c_age_group <- factor(filtered_data$c_age_group, levels = age_labels)
  }
  if (participant_var == "p_ethnicity") {
    filtered_data$p_ethnicity <- factor(filtered_data$p_ethnicity, levels = rev(c("Other", "Mixed", "Black", "Asian", "White")))
  }
  if (contact_var == "c_ethnicity") {
    filtered_data$c_ethnicity <- factor(filtered_data$c_ethnicity, levels = rev(c("Other", "Mixed", "Black", "Asian", "White")))
  }
  if (participant_var == "p_sec_input") {
    filtered_data$p_sec_input <- fct_recode(filtered_data$p_sec_input,
      "1. Higher managerial, administrative\nand professional occupations" = "1",
      "2. Lower managerial, administrative\nand professional occupations" = "2",
      "3. Intermediate occupations" = "3",
      "4. Small employers and\nown account workers" = "4",
      "5. Lower supervisory and\ntechnical occupations" = "5",
      "6. Semi-routine occupations" = "6",
      "7. Routine occupations" = "7"
    )

    filtered_data$p_sec_input <- forcats::fct_relevel(
      filtered_data$p_sec_input,
      "1. Higher managerial, administrative\nand professional occupations",
      "2. Lower managerial, administrative\nand professional occupations",
      "3. Intermediate occupations",
      "4. Small employers and\nown account workers",
      "5. Lower supervisory and\ntechnical occupations",
      "6. Semi-routine occupations",
      "7. Routine occupations", "Student", "Under 17", "Retired", "Unemployed", "Unknown"
    )
  }
  if (contact_var == "c_sec_input") {
    filtered_data$c_sec_input <- fct_recode(filtered_data$c_sec_input,
      "1. Higher managerial, administrative\nand professional occupations" = "1",
      "2. Lower managerial, administrative\nand professional occupations" = "2",
      "3. Intermediate occupations" = "3",
      "4. Small employers and\nown account workers" = "4",
      "5. Lower supervisory and\ntechnical occupations" = "5",
      "6. Semi-routine occupations" = "6",
      "7. Routine occupations" = "7"
    )

    filtered_data$c_sec_input <- forcats::fct_relevel(
      filtered_data$c_sec_input,
      "1. Higher managerial, administrative\nand professional occupations",
      "2. Lower managerial, administrative\nand professional occupations",
      "3. Intermediate occupations",
      "4. Small employers and\nown account workers",
      "5. Lower supervisory and\ntechnical occupations",
      "6. Semi-routine occupations",
      "7. Routine occupations", "Student", "Under 17", "Unemployed", "Unknown"
    )
  }

  if (uncertainty == F) {
    ggplot(data = filtered_data, mapping = aes(x = !!sym(participant_var), y = !!sym(contact_var), fill = n_contacts)) +
      geom_tile() +
      geom_text(mapping = aes(label = sprintf("%.2f", n_contacts)), color = "white", size = 2) +
      scale_fill_viridis_c(
        name = (paste0("Mean daily contacts")),
        trans = "pseudo_log", option = "D"
      ) +
      labs(
        title = paste0(ifelse(location_type == "Total", "", location_type)),
        x = format_label(participant_var),
        y = format_label(contact_var)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_equal()
  } else {
    ggplot(data = filtered_data, mapping = aes(x = !!sym(participant_var), y = !!sym(contact_var), fill = ci_u - ci_l)) +
      geom_tile() +
      geom_text(mapping = aes(label = paste0("(", round(ci_l, 2), ", ", round(ci_u, 2), ")")), color = "white", size = 2) +
      scale_fill_viridis_c(
        name = (paste0("Width of 95% confidence interval")),
        trans = "pseudo_log", option = "A"
      ) +
      labs(
        title = paste0(ifelse(location_type == "Total", "", paste0(location_type, ", 95% confidence interval"))),
        x = format_label(participant_var),
        y = format_label(contact_var)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_equal()
  }
}

create_plots <- function(weighted_contacts, participant_var, contact_var, output_prefix, print_plots = FALSE, uncertainty = F) {
  locations <- c("Total", "Home", "Work", "School", "Other")

  matrix_plots <- map(
    .x = locations,
    .f = ~ plot_contact_matrix(
      data = weighted_contacts,
      location_type = .x,
      participant_var = participant_var,
      contact_var = contact_var,
      uncertainty = uncertainty
    )
  )

  layout <- "
  AABC
  AADE
  "

  combined_plot <- patchwork::wrap_plots(matrix_plots,
    design = layout,
    plot_layout(guides = "collect")
  )

  if (print_plots) print(matrix_plots)

  combined_plot
}

create_and_save_plots <- function(weighted_contacts, participant_var, contact_var, output_prefix, print_plots = FALSE, uncertainty = F) {
  locations <- c("Total", "Home", "Work", "School", "Other")

  # Create base directory for this set of contact matrices
  base_dir <- here::here("results", survey, output_prefix, ifelse(uncertainty, "uncertainty", "contacts"))
  dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

  # Create subdirectories for PNG and PDF
  png_dir <- file.path(base_dir, "png")
  pdf_dir <- file.path(base_dir, "pdf")
  dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

  matrix_plots <- map(
    .x = locations,
    .f = ~ plot_contact_matrix(
      data = weighted_contacts,
      location_type = .x,
      participant_var = participant_var,
      contact_var = contact_var,
      uncertainty = uncertainty
    )
  )

  layout <- "
  AABC
  AADE
  "

  combined_plot <- patchwork::wrap_plots(matrix_plots,
    design = layout,
    plot_layout(guides = "collect")
  )

  if (print_plots) print(matrix_plots)

  # Save combined plot as PNG
  ggsave(
    filename = file.path(png_dir, paste0(output_prefix, "_combined.png")),
    plot = combined_plot,
    width = 30,
    height = 15,
    dpi = 600,
    bg = "white"
  )

  # Save combined plot as PDF
  ggsave(
    filename = file.path(pdf_dir, paste0(output_prefix, "_combined.pdf")),
    plot = combined_plot,
    width = 30,
    height = 15,
    device = "pdf",
    bg = "white"
  )

  # Save individual plots as PNG and PDF
  walk2(
    .x = matrix_plots,
    .y = locations,
    .f = function(plot, location) {
      # Save as PNG
      ggsave(
        filename = file.path(png_dir, paste0(output_prefix, "_", tolower(location), ".png")),
        plot = plot,
        width = 10,
        height = 8,
        dpi = 600,
        bg = "white"
      )

      # Save as PDF
      ggsave(
        filename = file.path(pdf_dir, paste0(output_prefix, "_", tolower(location), ".pdf")),
        plot = plot,
        width = 10,
        height = 8,
        device = "pdf",
        bg = "white"
      )
    }
  )
}
