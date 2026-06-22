
#### NEGATIVE BINOMIAL MODEL FUNCTIONS #### 

# Function to get weights for large_n contacts from Polymod setting-specific matrices
polymod_weights <- function(
    broad_ages = c(18,65),
    fine_ages = age_vals,
    age_struc = age_structure_fine,
    locations = c('total','work','school','other')
){
  
  # add 0 to lower bounds if not there, same with 18
  fine_ages <- sort(unique(c(0, fine_ages)))
  age_labels <- c(paste0(c(fine_ages[1:length(fine_ages)-1]), '-', c(fine_ages[2:length(fine_ages)]-1)), paste0(fine_ages[length(fine_ages)], '+'))
  
  # broad ages labels 
  broad_ages <- sort(unique(c(0, broad_ages)))
  broad_age_labels <- c(paste0(c(broad_ages[1:length(broad_ages)-1]), '-', c(broad_ages[2:length(broad_ages)]-1)), paste0(broad_ages[length(broad_ages)], '+'))
  
  # overlaps between fine and broad age groups 
  overlaps <- CJ(age_labels, broad_age_labels, overlap = F)
  for(i in 1:nrow(overlaps)){
    broad_vec <- suppressWarnings(case_when(grepl('[+]', overlaps[i, broad_age_labels]) ~ 
                                              c(as.numeric(gsub('[+]','',overlaps[i, broad_age_labels])), 120),
                                            T ~ as.numeric(unlist(strsplit(overlaps[i, broad_age_labels],'-')))))
    c_vec <- suppressWarnings(case_when(grepl('[+]', overlaps[i, age_labels]) ~ 
                                          c(as.numeric(gsub('[+]','',overlaps[i, age_labels])), 120),
                                        T ~ as.numeric(unlist(strsplit(overlaps[i, age_labels],'-')))))
    
    if(length(intersect(c(broad_vec[1]:broad_vec[2]), c(c_vec[1]:c_vec[2]))) > 0){
      overlaps[i, overlap := T]
    }
  }
  
  # switch between age label styles (ukscs vs polymod)
  age_to_pm_lab <- function(chars){
    out <- c()
    for(char in chars){
      if(char == paste0(max(fine_ages), '+')){
        out <- c(out, paste0('[',max(fine_ages), ',Inf)'))
      }else{
        vals <- as.numeric(unlist(strsplit(char, '-')))
        val <- paste0('[',vals[1], ',', vals[2] + 1,')')
        out <- c(out, val)
      }
    }
    out
  }
  
  # contact matrices list
  matrices <- list()
  
  # per capita location-specific contact matrix, scaled up to 2025 population
  for(i in 1:length(locations)){
    location <- locations[i]
    filter_locn <- list(a = 1)
    names(filter_locn) <- paste0('cnt_', location)
    
    if(location == 'other'){
      out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                        per.capita = TRUE,
                                        filter = list(cnt_transport = 1), 
                                        missing.participant.age = 'remove',
                                        missing.contact.age = 'remove')$matrix.per.capita
      out <- out + socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                        per.capita = TRUE,
                                        filter = list(cnt_leisure = 1), 
                                        missing.participant.age = 'remove',
                                        missing.contact.age = 'remove')$matrix.per.capita
      out <- out + socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                        per.capita = TRUE,
                                        filter = list(cnt_otherplace = 1), 
                                        missing.participant.age = 'remove',
                                        missing.contact.age = 'remove')$matrix.per.capita
    }else{
      if(location=='total'){
        out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                          per.capita = TRUE,
                                          missing.participant.age = 'remove',
                                          missing.contact.age = 'remove')$matrix.per.capita  
      }else{
        out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                          per.capita = TRUE,
                                          filter = filter_locn, 
                                          missing.participant.age = 'remove',
                                          missing.contact.age = 'remove')$matrix.per.capita    
      }
    }
    
    # scale up to 2022 population 
    for(age in 1:nrow(age_struc)){
      out[, age] <- out[, age]*age_struc$n[age]
    }
    
    matrices[[i]] <- out
  }
  
  names(matrices) <- locations
  
  # polymod weights for each p_age_group, c_age_group, c_location
  pmw <- data.table(CJ(p_age_group = age_labels, c_age_group = age_labels, 
                       broad_age_group = broad_age_labels, c_location = locations, prob = -8, sorted=F))
  
  for(i in 1:nrow(pmw)){
    # if no overlap, prob = 0
    if(overlaps[age_labels == pmw[i, c_age_group] & broad_age_labels == pmw[i, broad_age_group], overlap] == F){
      pmw[i, prob := 0]
    }else{
      matr <- data.table(matrices[[pmw[i, c_location]]]) # location-specfic matrix
      row <- matr[which(age_labels == pmw[i, p_age_group]), ] # participant's age row
      ages_to_subset <- overlaps[broad_age_labels == pmw[i, broad_age_group] & overlap == T, age_labels]
      ages_to_subset_pm <- age_to_pm_lab(ages_to_subset)
      subset_row <- row[,..ages_to_subset_pm] # contacts with feasible ages
      if(rowSums(subset_row) == 0){ # if all 0, allocated according to population size 2025
        filt_age <- data.table(age_struc[age_struc$p_age_group %in% ages_to_subset, ])
        # scale 15-19 value by 60% if broad age group is 0-17
        if(pmw[i, broad_age_group] == '0-17' & any(names(subset_row) == "[15,20)")){
          filt_age[p_age_group == '15-19', n := 0.6*n]
          pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
        }else{
          # scale 15-19 value by 40% if broad age group is 18-64
          if(pmw[i, broad_age_group] == '18-64' & any(names(subset_row) == "[15,20)")){
            filt_age[p_age_group == '15-19', n := 0.4*n]
            pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
          }else{
            pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
          }
        }
      }else{
        col <- age_to_pm_lab(pmw[i, c_age_group])
        # scale 15-19 value by 60% if broad age group is 0-17
        if(pmw[i, broad_age_group] == '0-17' & any(names(subset_row) == "[15,20)")){
          subset_row[, '[15,20)'] <- 0.6*subset_row[, '[15,20)']
          suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
        }else{
          # scale 15-19 value by 40% if broad age group is 18-64
          if(pmw[i, broad_age_group] == '18-64' & any(names(subset_row) == "[15,20)")){
            subset_row[, '[15,20)'] <- 0.4*subset_row[, '[15,20)']
            suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
          }else{
            suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
          }
        }
      }
    }
  }
  
  # pmw$p_age_group <- factor(pmw$p_age_group, levels = age_labels); pmw$c_age_group <- factor(pmw$c_age_group, levels = age_labels)
  # pmw %>% filter(broad_age_group=='0-17') %>%
  #   ggplot() + geom_line(aes(x=c_age_group, y=prob, group=interaction(c_location,broad_age_group), col=c_location),lwd=0.8) + facet_wrap(.~p_age_group)
  
  # check all appropriate sums are 1
  test <- pmw %>% group_by(p_age_group, broad_age_group, c_location) %>% summarise(s=sum(prob)) 
  if(!all.equal(test$s, rep(1,nrow(test)))){warning('Some sums not 1')}
  
  pmw
}

# Log-likelihood function for negative binomial
nb_loglik <- function(x, par, trunc_num) {
  k <- par[["k"]]
  mean <- par[["mu"]]
  ll <- rep(NA_real_, length(x))
  ll[x < trunc_num] <- dnbinom(x[x < trunc_num], mu = mean, size = 1/k, log = TRUE)
  ll[x >= trunc_num] <- dnbinom(trunc_num, mu = mean, size = 1/k, log = TRUE)
  return(-sum(ll))
}

# Function to estimate negative binomial mu from counts data
nbinom_optim <- function(counts_input, 
                         param = 'mu', trunc) {
  
  bs_optim <- function(counts_){
    if(sum(counts_) == 0){
      return(0)
    }else{
      m <- mean(counts_)
      v <- var(counts_)
      outs = optim(c(mu = m, k = (v - m)/m^2), lower = c(mu = 1e-5, k = 1e-5), nb_loglik, x = counts_, trunc_num = trunc, method = "L-BFGS-B")
      return(as.numeric(outs$par[param]))
    }
  }
  
  if(sum(counts_input != 0)){
    outs_mat = apply(counts_input, FUN = bs_optim, MARGIN = 1)
    outs_mat
  } else{
    return(rep(0,nrow(counts_input)))
  }
}

raw_counts <- function(
    part_in,
    cont_in,
    participant_var,
    contact_var,
    large_n_input = T,
    location_inclusion = T
){
  
  if(is.null(contact_var)){
    cont_in <- cont_in %>% mutate(c_null = 'NULL')
    contact_var <- 'c_null'
  }
  
  # Determine which variables to select from part_in
  select_vars <- unique(c('p_id', participant_var))
  
  if((length(contact_var) == 1 & 'c_age_group' %in% contact_var & large_n_input) |
     (length(contact_var) == 1 & 'c_location' %in% contact_var & large_n_input)){
    select_vars <- c(select_vars, colnames(part_in)[grepl('add_', colnames(part_in))])
  }else{
    if(large_n_input & length(contact_var) == 1){
      if(contact_var=='c_null'){
        select_vars <- c(select_vars, 'large_n')
      }
    }
  }
  
  if(location_inclusion == T){
    contact_var_orig <- contact_var
    contact_var <- unique(c(contact_var, 'c_location'))
  }else{contact_var_orig <- contact_var}
  
  # in case joining vars (apart from p_id) are in both part_in and cont_in
  for(var_i in select_vars[select_vars != 'p_id']){
    cont_in <- cont_in %>% select(!starts_with(var_i))
  }
  
  # Calculate contacts for each combination and location
  contact_counts <- cont_in %>% filter(p_id %in% part_in$p_id) %>%  # in case cont_in/part_in are filtered, e.g. by gender
    left_join(part_in %>% filter(p_id %in% cont_in$p_id) %>% select(!!!syms(select_vars)), by = "p_id") %>%
    group_by(!!!syms(select_vars), !!!syms(contact_var)) %>%
    summarise(n_contacts = n(), .groups = "drop")  
  
  # drop any participants which contain any NAs for contact_var
  contact_counts <- contact_counts %>% drop_na(!!!syms(contact_var))
  
  # adding back in individuals with 0 contacts
  add_data <- part_in %>% filter(p_id %notin% cont_in$p_id) %>% select(!!!syms(select_vars)) 
  names <- colnames(add_data)
  for(var in contact_var){
    add_data <- add_data %>% mutate(var = unname(unlist(cont_in[1,var])))
    colnames(add_data) <- c(names, var)
    names <- colnames(add_data)
  }
  add_data <- add_data %>% mutate(n_contacts = 0)
  colnames(add_data) <- c(select_vars, contact_var, 'n_contacts')
  contact_counts <- rbind(contact_counts,
                          add_data) %>% 
    complete(nesting(!!!syms(select_vars)), !!!syms(contact_var), fill = list(n_contacts = 0)) 
  
  if(location_inclusion == T){
    # Calculate total contacts across all locations
    total_contacts <- contact_counts %>%
      group_by(!!!syms(select_vars), !!!syms(contact_var[contact_var != 'c_location'])) %>%
      summarise(n_contacts = sum(n_contacts), .groups = "drop") %>%
      mutate(c_location = "Total") 
    
    # Combine location-specific and total contacts
    all_contacts <- bind_rows(contact_counts, total_contacts)
  }else{
    all_contacts <- contact_counts
  }
  
  # pivot_wider
  all_contacts_w <- all_contacts %>% pivot_wider(
    names_from = !!sym(contact_var_orig),
    values_from = n_contacts,
    names_glue = sprintf('cag_{%s}', contact_var_orig),
  ) %>% select(!!!syms(select_vars), contains('c_location'), starts_with('cag'), starts_with('add_'))
    
  # set maximum (right-truncation)
  all_contacts_w <- all_contacts_w %>% 
    mutate(across(starts_with('add_'), max_100))
  
  all_contacts_w
  
}


add_large_n <- function(vec, p_age_in, locn_in, large_weights_loc_in){
  
  p_age_num <- which(age_labels == p_age_in)
  # print(length(vec))

  # u18 contacts
  if(vec[1] > 0){

      probs <- large_weights_loc_in[broad_age_group %like% '0-17',]$prob
      samps <- sample(1:length(probs),
                      size = vec[1],
                      replace = T,
                      prob = probs)
      
      occurrence_vec <- sapply(1:length(probs), FUN = function(x) length(samps[samps==x]))
      
      vec <- vec + c(rep(0,3), occurrence_vec)

  }
  

  # add other contacts proportional to polymod weights

  # 18-64
  if(vec[2] > 0){
    probs <- large_weights_loc_in[broad_age_group %like% '18-64',]$prob
    samps <- sample(1:length(probs),
                    size = vec[2],
                    replace = T,
                    prob = probs)
    
    occurrence_vec <- sapply(1:length(probs), FUN = function(x) length(samps[samps==x]))
    
    vec <- vec + c(rep(0,3), occurrence_vec)
  }

  # 65+
  if(vec[3] > 0){
    probs <- large_weights_loc_in[broad_age_group %like% '65+',]$prob
    samps <- sample(1:length(probs),
                    size = vec[3],
                    replace = T,
                    prob = probs)
    
    occurrence_vec <- sapply(1:length(probs), FUN = function(x) length(samps[samps==x]))
    
    vec <- vec + c(rep(0,3), occurrence_vec)
  }
  
  vec

}

# function to bootstrap sample from participants
bootstrap_sample <- function(
  data,
  locn, 
  p_var,
  c_var,
  bs,
  large_n_input,
  large_weights = NULL,
  trunc_in,
  impute_contact_in,
  impute_vars_in
){
  
  locn <- tolower(locn)
  
  if(is.null(large_weights) & large_n_input == T){
    stop('Needs large_weights')
  }
  
  data <- data[tolower(data$c_location) == locn,]
  
  # add all of 'add_' columns for 'add_XXX_total'
  if(sum(grepl('add',colnames(data))) > 0){
    data <- data %>%  mutate(
      add_u18_total = add_u18_school + add_u18_work + add_u18_other,
      add_18_64_total = add_18_64_school + add_18_64_work + add_18_64_other,
      add_65_total = add_65_school + add_65_work + add_65_other
    )  
  }
  
  data <- data %>% select(p_id, !!sym(p_var), post_strat_weight, c_location, contains(tolower(locn)), starts_with('cag'))
  
  p_vec <- unname(unlist(unique(data %>% select(!!sym(p_var)))))
  c_vec <- gsub('cag_','',unique(colnames(data %>% select(starts_with('cag')))))
  
  # main output
  bs_out <- CJ(c_location = locn, bs_index = 1:bs, p_var = p_vec, c_var = c_vec, mu = -8, k = -8)
  
  cat(format_label(p_var), 'x', format_label(c_var), 'x', firstup(locn))
  
  # progress bar 
  pb <- txtProgressBar(min = 0, max = bs*length(p_vec), initial = 0, style=3); step_i <- 0 
  
  # selected participants for each group
  for(p in p_vec){
    
    # print(p)
    
    counts <- list()
    
    # will contain count values for each possible contact's group
    for(c in 1:ncol(data %>% select(starts_with('cag')))){
      n_col <- n_distinct((data %>% filter(!!sym(p_var) == p))$p_id)
      counts[[c]] <- data.table(matrix(0, ncol = n_col))
    }
    names(counts) <- c_vec
    
    data <- data.table(data)
    data <- data[, row_id := 1:nrow(data)] # = p_id for adults but not for children
    
    # bootstrapping
    for(bs_i in 1:bs){
      
      # impute if impute_contact_in = T
      if(impute_contact_in){
        data_imputed <- fcn_impute_raw_counts(data,
                                              c_var,
                                              impute_vars_in)
      }else{
        data_imputed <- copy(data)
      }
      
      # p_ids to sample from
      ids_df <- unique(data_imputed[get(p_var) == p, c('p_id', 'row_id', 'post_strat_weight')])
      row_samples <- sample(ids_df$row_id, replace = T, prob = ids_df$post_strat_weight)
      
      # merging sampled data
      occurrences <- rbind(data.table(table(row_samples)), 
                           data.table(row_samples = (1:nrow(data_imputed))[!(1:nrow(data_imputed)) %in% row_samples],
                                      N = 0
                           ))
      setnames(occurrences, 'row_samples', 'row_id'); occurrences[, row_id := as.numeric(row_id)]
      data_add <- data_imputed[occurrences, on = 'row_id']
      counts_all <- data_add[rep(seq_len(nrow(data_add)), N), ]
      counts_all[, c('row_id','N') := NULL]
      
      # uniquely identify rows (about to become columns)
      counts_all[, p_id_row := paste0(p_id, '_', 1:nrow(counts_all))]
      
      # add large_n in at random, if large_n_input == T
      if(large_n_input & length(c_var == 1) & 'c_age_group' %in% c_var & locn %in% large_weights$c_location){
        
        large_weights_loc <- large_weights %>% filter(c_location == locn, !!sym(p_var) == p)
        
        # transpose so can vectorise across columns
        t_counts_all <- dcast.data.table(melt.data.table(counts_all, 
                                       id.vars = c('p_id','p_id_row',p_var,'post_strat_weight','c_location'))[,c('p_id_row','variable','value')],
                                  variable ~ p_id_row, value.var = 'value')
        t_counts_all[, variable := NULL]
        
        # make back into numeric
        t_counts_all <- t_counts_all[, lapply(.SD, as.numeric)]
        
        # apply function add_large_n
        t_counts_all <- t_counts_all[, lapply(.SD, add_large_n, 
                                                      p_age_in = p, locn_in = locn,
                                                      large_weights_loc_in = large_weights_loc)]
        
        # make back into original form
        counts_all2 <- dcast.data.table(melt.data.table(cbind(t_counts_all, variable = colnames(counts_all)[grepl('cag|add', colnames(counts_all))]), 
                                   id.vars = 'variable', variable.name = 'p_id_row'),
                                  p_id_row ~ variable, value.var = 'value')
        
        vec_colnames <- !grepl('cag|add', colnames(counts_all))
        counts_all2 <- counts_all2[counts_all[, ..vec_colnames], on = 'p_id_row']
        
        add_vec <- which(!colnames(counts_all2) %like% 'add')
        counts_all <- counts_all2[, ..add_vec]
        
      }
      
      # move this data into `counts`
      for(c in c_vec){
        
        if(bs_i == 1){
          counts[[c]] <- matrix(counts_all[, get(paste0('cag_', c))], nrow=1)
        }else{
          counts[[c]] <- rbind(counts[[c]], matrix(counts_all[, get(paste0('cag_', c))], nrow=1))  
        }
        
      }
      
      # if(bs_i %% 50 == 0){print(bs_i)}
      
      setTxtProgressBar(pb,step_i); step_i <- step_i + 1
      
    }
    
    # fit negative binomial
    for(c in c_vec){
      
      mu_out <- nbinom_optim(counts[[c]], 'mu', trunc = trunc_in)
      k_out <- nbinom_optim(counts[[c]], 'k', trunc = trunc_in)
      
      bs_out[p_var == p & c_var == c, mu := mu_out]
      bs_out[p_var == p & c_var == c, k := k_out]
      
    }
    
  }
  
  close(pb)
  
  bs_out
}


# function to bootstrap sample from participants
bootstrap_sample_one_dim <- function(
    data,
    p_var,
    c_var,
    bs,
    large_n_input,
    trunc_in
){
  
  data <- data %>% drop_na()
  
  # filter out participant categories if only one participant
  if(!is.null(p_var)){
    data <- data %>% group_by(!!!syms(p_var)) %>% 
      mutate(nrow=n()) %>% ungroup() %>% filter(nrow > 1) %>% select(!nrow)  
  }
  
  # add all of 'add_' columns into location totals
  if(is.null(p_var) & !is.null(c_var) & sum(grepl('add',colnames(data))) == 9){
    if(c_var == 'c_location'){
      data <- data %>% mutate(
        cag_School = cag_School + add_u18_school + add_18_64_school + add_65_school,
        cag_Work = cag_Work + add_u18_work + add_18_64_work + add_65_work,
        cag_Other = cag_Other + add_u18_other + add_18_64_other + add_65_other
      )    
    }
  }
  
  if(!is.null(p_var)){
    data <- data %>% select(p_id, !!!syms(p_var), post_strat_weight, contains('large_n'), starts_with('cag'))
  }else{
    data <- data %>% select(p_id, post_strat_weight, contains('large_n'), starts_with('cag'))
  }
  
  data <- data.table(data)
  
  p_vec <- if(!is.null(p_var)){
    if(length(p_var) == 1){
      unname(unlist(unique(data[,get(p_var)])))
    }else{
      unique(data[,..p_var])
    }}else{
    'NULL'
  }
  c_vec <- gsub('cag_','',unique(colnames(data)[grepl('cag_',colnames(data))]))
  
  # main output
  bs_out <- if(is.null(p_var) | length(p_var) == 1){CJ(bs_index = 1:bs, p_var = p_vec, c_var = c_vec, N = -8, mu = -8, k = -8)}else{
    CJ(bs_index = 1:bs, p_var_1 = unlist(unique(p_vec[,1])), p_var_2 = unlist(unique(p_vec[,2])), c_var = c_vec, N = -8, mu = -8, k = -8)
  }
  
  # if total
  if(is.null(p_var) & is.null(c_var)){
    
    # progress bar 
    pb <- txtProgressBar(min = 0, max = bs, initial = 0, style=3); step_i <- 0 
    
    setnames(data, 'cag_NULL', 'c_var', skip_absent=TRUE)
    
    # print(p)
    n_col <- n_distinct(data$p_id)
    counts <- data.table(matrix(0, ncol = n_col))
    
    bs_out[,N := n_col]
    
    # bootstrapping
    for(bs_i in 1:bs){
      
      # p_ids to sample from
      data <- data[, row_id := 1:nrow(data)] # = p_id for adults but not for children
      ids_df <- unique(data[, c('p_id', 'row_id', 'post_strat_weight')])
      row_samples <- sample(ids_df$row_id, replace = T, prob = ids_df$post_strat_weight)
      
      # merging sampled data
      occurrences <- rbind(data.table(table(row_samples)), 
                           data.table(row_samples = (1:nrow(data))[!(1:nrow(data)) %in% row_samples],
                                      N = 0
                           ))
      setnames(occurrences, 'row_samples', 'row_id'); occurrences[, row_id := as.numeric(row_id)]
      data_add <- data[occurrences, on = 'row_id']
      counts_all <- data_add[rep(seq_len(nrow(data_add)), N), ]
      counts_all[, c('row_id','N') := NULL]
      
      # add large_n in, if large_n_input == T
      if(large_n_input){
        counts_all[, c_var := c_var + large_n][, large_n := NULL]
      }
      
      # move this data into `counts`
      if(bs_i == 1){
        counts <- matrix(unlist(counts_all[, 'c_var']), nrow=1)
      }else{
        counts <- rbind(counts, matrix(unlist(counts_all[, 'c_var']), nrow=1))  
      }
      
      # if(bs_i %% 50 == 0){print(bs_i)}
      
      setTxtProgressBar(pb,step_i); step_i <- step_i + 1
      
    }
    
    # fit negative binomial
    mu_out <- nbinom_optim(counts, 'mu', trunc = trunc_in)
    k_out <- nbinom_optim(counts, 'k', trunc = trunc_in)
    
    bs_out[,mu := mu_out]
    bs_out[,k := k_out]
    
  }
  # if participant attribute
  if(!is.null(p_var) & is.null(c_var)){
    
    if(length(p_var) == 1){
      # progress bar 
      pb <- txtProgressBar(min = 0, max = bs*length(p_vec), initial = 0, style=3); step_i <- 0 
      
      setnames(data, 'cag_NULL', 'c_var', skip_absent=TRUE)
      
      for(p in p_vec){
        # print(p)
        n_col <- n_distinct((data[get(p_var) == p, p_id]))
        counts <- data.table(matrix(0, ncol = n_col))
        
        bs_out[p_var == p, N := n_col]
        
        # bootstrapping
        for(bs_i in 1:bs){
          
          # p_ids to sample from
          data <- data[, row_id := 1:nrow(data)] # = p_id for adults but not for children
          ids_df <- unique(data[get(p_var) == p, c('p_id', 'row_id', 'post_strat_weight')])
          row_samples <- sample(ids_df$row_id, replace = T, prob = ids_df$post_strat_weight)
          
          # merging sampled data
          occurrences <- rbind(data.table(table(row_samples)), 
                               data.table(row_samples = (1:nrow(data))[!(1:nrow(data)) %in% row_samples],
                                          N = 0
                               ))
          setnames(occurrences, 'row_samples', 'row_id'); occurrences[, row_id := as.numeric(row_id)]
          data_add <- data[occurrences, on = 'row_id']
          counts_all <- data_add[rep(seq_len(nrow(data_add)), N), ]
          counts_all[, c('row_id','N') := NULL]
          
          # add large_n in, if large_n_input == T
          if(large_n_input){
            counts_all[, c_var := c_var + large_n][, large_n := NULL]
          }
          
          # move this data into `counts`
          if(bs_i == 1){
            counts <- matrix(unlist(counts_all[, 'c_var']), nrow=1)
          }else{
            counts <- rbind(counts, matrix(unlist(counts_all[, 'c_var']), nrow=1))  
          }
          
          # if(bs_i %% 50 == 0){print(bs_i)}
          
          setTxtProgressBar(pb,step_i); step_i <- step_i + 1
          
        }
        
        # fit negative binomial
        mu_out <- nbinom_optim(counts, 'mu', trunc = trunc_in)
        k_out <- nbinom_optim(counts, 'k', trunc = trunc_in)
        
        bs_out[p_var == p, mu := mu_out]
        bs_out[p_var == p, k := k_out]
        
      }
    }
    
    if(length(p_var) == 2){
      
      # progress bar 
      pb <- txtProgressBar(min = 0, max = bs*nrow(p_vec), initial = 0, style=3); step_i <- 0 
      
      setnames(data, 'cag_NULL', 'c_var', skip_absent=TRUE)
      
      var1 <- colnames(p_vec)[1]; var2 <- colnames(p_vec)[2]
      
      for(p_row in 1:nrow(p_vec)){
        
        var1_val <- unname(unlist(p_vec[p_row,..var1])); var2_val <- unname(unlist(p_vec[p_row,..var2]))
        
        n_col <- n_distinct(data[which(data[,..var1] == var1_val & data[,..var2] == var2_val)])
        counts <- data.table(matrix(0, ncol = n_col))
        
        bs_out[p_var_1 == var1_val & p_var_2 == var2_val, N := n_col]
        
        # bootstrapping
        for(bs_i in 1:bs){
          
          # p_ids to sample from
          data <- data[, row_id := 1:nrow(data)] # = p_id for adults but not for children
          ids_df <- unique(data[which(data[,..var1] == var1_val & data[,..var2] == var2_val), c('p_id', 'row_id', 'post_strat_weight')])
          row_samples <- sample(ids_df$row_id, replace = T, prob = ids_df$post_strat_weight)
          
          # merging sampled data
          occurrences <- rbind(data.table(table(row_samples)), 
                               data.table(row_samples = (1:nrow(data))[!(1:nrow(data)) %in% row_samples],
                                          N = 0
                               ))
          setnames(occurrences, 'row_samples', 'row_id'); occurrences[, row_id := as.numeric(row_id)]
          data_add <- data[occurrences, on = 'row_id']
          counts_all <- data_add[rep(seq_len(nrow(data_add)), N), ]
          counts_all[, c('row_id','N') := NULL]
          
          # add large_n in, if large_n_input == T
          if(large_n_input){
            counts_all[, c_var := c_var + large_n][, large_n := NULL]
          }
          
          # move this data into `counts`
          if(bs_i == 1){
            counts <- matrix(unlist(counts_all[, 'c_var']), nrow=1)
          }else{
            counts <- rbind(counts, matrix(unlist(counts_all[, 'c_var']), nrow=1))  
          }
          
          # if(bs_i %% 50 == 0){print(bs_i)}
          
          setTxtProgressBar(pb,step_i); step_i <- step_i + 1
          
        }
        
        # fit negative binomial
        mu_out <- nbinom_optim(counts, 'mu', trunc = trunc_in)
        k_out <- nbinom_optim(counts, 'k', trunc = trunc_in)
        
        bs_out[p_var_1 == var1_val & p_var_2 == var2_val, mu := mu_out]
        bs_out[p_var_1 == var1_val & p_var_2 == var2_val, k := k_out]
        
      }
    }
    
  }
  # if contact attribute
  if(is.null(p_var) & !is.null(c_var)){
    
    # progress bar 
    pb <- txtProgressBar(min = 0, max = 2*(bs*length(c_vec)), initial = 0, style=3); step_i <- 0 
    
    counts <- list()
    
    # will contain count values for each possible contact's group
    n_col <- n_distinct(data$p_id)
    
    bs_out[,N := n_col]
    
    # bootstrapping
    for(bs_i in 1:bs){
      
      # p_ids to sample from
      data <- data[, row_id := 1:nrow(data)] # = p_id for adults but not for children
      ids_df <- data[, c('p_id', 'row_id', 'post_strat_weight')]
      row_samples <- sample(ids_df$row_id, replace = T, prob = ids_df$post_strat_weight)
      
      # merging sampled data
      occurrences <- rbind(data.table(table(row_samples)), 
                           data.table(row_samples = (1:nrow(data))[!(1:nrow(data)) %in% row_samples],
                                      N = 0
                           ))
      setnames(occurrences, 'row_samples', 'row_id'); occurrences[, row_id := as.numeric(row_id)]
      data_add <- data[occurrences, on = 'row_id']
      counts_all <- data_add[rep(seq_len(nrow(data_add)), N), ]
      counts_all[, c('row_id','N') := NULL]
      
      # move this data into `counts`
      for(c in c_vec){
        
        if(bs_i == 1){
          counts[[c]] <- matrix(counts_all[, get(paste0('cag_', c))], nrow=1)
        }else{
          counts[[c]] <- rbind(counts[[c]], matrix(counts_all[, get(paste0('cag_', c))], nrow=1))  
        }
      
        setTxtProgressBar(pb,step_i); step_i <- step_i + 1  
      }
      
    }
    
    step_i <- step_i + bs
    
    # fit negative binomial
    for(c in c_vec){
      
      mu_out <- nbinom_optim(counts[[c]], 'mu', trunc = trunc_in)
      k_out <- nbinom_optim(counts[[c]], 'k', trunc = trunc_in)
      
      bs_out[c_var == c, mu := mu_out]
      bs_out[c_var == c, k := k_out]
      
      setTxtProgressBar(pb,step_i); step_i <- step_i + bs
    }
    
  }
  
  close(pb)
  
  bs_out
  
}


# main fitting function - for mean contacts
nb_bootstrap_analysis <- function(
    participant_data,
    contact_data,
    location_filter = NULL,
    participant_var,
    contact_var,
    n_bootstrap = 1000,
    trunc,
    age_structure = age_structure_fine,
    age_structure_gendered = age_sex_strata,
    ethnicity_structure = ons_ethnicity,
    age_structure_ac = age_structure_adult_child,
    large_n_input = T,
    weighting_vec = c('p_age_group','p_gender','p_ethnicity','day_week'),
    save = T
){
  
  weighting_vec <- weighting_vec[weighting_vec %notin% participant_var]
  
  weighted_all <- if(length(weighting_vec)>=1){
    part %>% select(p_id) %>% 
      left_join(weight_participants(part, 
                                    age_structure, 
                                    age_structure_gendered,
                                    ethnicity_structure, 
                                    age_structure_ac,
                                    weighting = weighting_vec,
                                    group_vars = participant_var) %>% 
                  select(p_id, post_strat_weight),
                by = 'p_id')
  }else{
    part %>% group_by(p_id) %>% summarise(post_strat_weight = 1)
  }
  
  if(!is.null(location_filter)){
    contact_data <- contact_data %>% filter(c_location == location)
    not_location_cols <- colnames(participant_data)[grepl('add',colnames(participant_data))]
    location_cols <- not_location_cols[grepl(tolower(location), not_location_cols)]
    not_location_cols <- not_location_cols[!grepl(tolower(location), not_location_cols)]
    participant_data[, not_location_cols] <- 0
    participant_data$large_n <- rowSums(participant_data[, location_cols])
  }
  
  # summarise contact data
  raw_counts_matr <- raw_counts(
    part_in = participant_data,
    cont_in = contact_data,
    participant_var,
    contact_var,
    large_n_input = large_n_input,
    location_inclusion = F
  )
  
  # Add survey weights
  weighted_contact_data <- raw_counts_matr %>%
    left_join(weighted_all, by = c('p_id')) 
  
  weighted_bs <- bootstrap_sample_one_dim(
    data = weighted_contact_data,
    p_var = participant_var,
    c_var = contact_var,
    bs = n_bootstrap, 
    large_n_input,
    trunc_in = trunc
  )
  
  if(save == T){
    saveRDS(weighted_bs, here::here('results',survey,'neg_bin','one_dim',paste0('nb_',gsub('p_','',participant_var),'.rds')))  
  }
  
  weighted_bs
  
}

# main fitting function
nb_matrix_fit <- function(
    participant_data,
    contact_data,
    participant_var,
    contact_var,
    n_bootstrap = 1000,
    trunc,
    polymod_weighting,
    age_structure = age_structure_fine,
    age_structure_gendered = age_sex_strata,
    ethnicity_structure = ons_ethnicity,
    age_structure_ac = age_structure_adult_child,
    large_n_input = T,
    weighting_vec = c('p_age_group','p_gender','p_ethnicity','day_week'),
    locations,
    save = T,
    impute_contact = F, # is the contact_var being imputed in the bootstrapping? (only usable for c_ethnicity and c_sec_input)
    impute_vars = NULL # what are the dependent variables for the imputation?
    ){
  
  if(impute_contact & contact_var %notin% c('c_ethnicity','c_sec_input')){stop("Can't impute for this contact variable.")}
  
  weighting_vec <- weighting_vec[weighting_vec %notin% participant_var]

  weighted_all <- if(length(weighting_vec)>=1){
    participant_data %>% select(p_id) %>% 
      left_join(weight_participants(participant_data, 
                                    age_structure, 
                                    age_structure_gendered,
                                    ethnicity_structure, 
                                    age_structure_ac,
                                    weighting = weighting_vec,
                                    group_vars = participant_var) %>% 
                  select(p_id, post_strat_weight),
                by = 'p_id')
  }else{
    participant_data %>% group_by(p_id) %>% summarise(post_strat_weight = 1)
  }
  
  if(is.null(contact_var)){
    contact_data <- contact_data %>% mutate(c_null = 1)
    contact_var <- 'c_null'
  }
  
  # summarise contact data
  raw_counts_matr <- raw_counts(
    part_in = participant_data,
    cont_in = contact_data,
    participant_var,
    contact_var,
    large_n_input = large_n_input
  )
  
  # add imputation dependent variables if using
  if(impute_contact & !is.null(impute_vars)){
    # throw error if impute_vars not in colnames(participant_data)
    if(length(setdiff(impute_vars, colnames(participant_data))) > 0){stop('Not set up for impute_vars not in participant_data')}
    # add if not already there
    impute_vars_add <- impute_vars[!impute_vars == participant_var]
    raw_counts_matr <- raw_counts_matr %>% left_join(participant_data %>% select(p_id, !!!syms(impute_vars_add)), by = 'p_id')  
  }
  
  # Add survey weights
  weighted_contact_data <- raw_counts_matr %>%
    left_join(weighted_all, by = c('p_id')) 
  
  bs_parallel <- function(location){
    bootstrap_sample(
      data = weighted_contact_data,
      locn = location,
      p_var = participant_var,
      c_var = contact_var,
      bs = n_bootstrap, 
      large_n_input,
      large_weights = polymod_weighting,
      trunc_in = trunc,
      impute_contact_in = impute_contact,
      impute_vars_in = impute_vars
    )
  }
  
  weighted_bs <- mclapply(locations, bs_parallel, mc.cores = length(locations))

  names(weighted_bs) <- locations
  
  if(save == T){
    saveRDS(weighted_bs, here::here('results',survey,'neg_bin',paste0('nb_',gsub('p_','',participant_var),'.rds')))  
  }
  
  weighted_bs
  
}

# Function to normalise with respect to underlying age structure

normalise_matrix <- function(matrix,
                             p_var_name,
                             population_dt,
                             per_capita = F){
  
  # check appropriate columns exist
  if(!'proportion' %in% colnames(population_dt) | 
     ! p_var_name %in% colnames(population_dt)){
    stop('Key columns missing from population_dt')
  }
  
  population_dt <- population_dt %>% select(!!sym(p_var_name), proportion)
  colnames(population_dt) <- c('p_var','proportion')
  
  scaled_matrix_1 <- matrix %>% arrange(bs_index, p_var) %>% 
    left_join(population_dt, by='p_var') %>% mutate(scaled_mu = mu*proportion) %>% 
    select(c_location, bs_index, p_var, c_var, mu, proportion, scaled_mu)
  
  transposed_scaled_matrix_1 <- scaled_matrix_1 %>% select('c_location', 'bs_index', 'p_var', 'c_var', 'scaled_mu')
  colnames(transposed_scaled_matrix_1) <- c('c_location', 'bs_index', 'c_var', 'p_var', 'scaled_mu_t')
  
  scaled_matrix_2 <- scaled_matrix_1 %>% left_join(transposed_scaled_matrix_1, by=c('c_location', 'bs_index', 'c_var', 'p_var')) %>% 
    mutate(mean_scaled_mu = (scaled_mu + scaled_mu_t)/2) %>% mutate(unscaled_mean_scaled_mu = mean_scaled_mu/proportion)
  
  matrix_out <- scaled_matrix_2 %>% select(c_location, bs_index, p_var, c_var, unscaled_mean_scaled_mu) %>% 
    setnames('unscaled_mean_scaled_mu', 'mu')
  
  if(per_capita == T){
    matrix_out %>% arrange(bs_index, p_var) %>% 
      left_join(population_dt %>% rename('c_var' = 'p_var'), by='c_var') %>% mutate(scaled_mu = mu/(proportion*sum(age_sex_strata$n))) %>% 
      select(c_location, bs_index, p_var, c_var, scaled_mu) %>% rename('mu' = 'scaled_mu')
  }else{
    matrix_out
  }
}

# fcn to calculate assortativity of contact matrices

## INPUTS: 
## M x M contact matrix, beta
## M-dimensional vector of probability proportions, n
## M-dimensional vector for ordering groups

fcn_assortativity <- function(
    beta,
    n_input,
    order,
    ci = 0.95
){
  
  # ##
  # beta = nb_age_group_norm$Total
  # n_input = age_structure_fine
  # order = age_labels
  # ##
  
  n <- copy(n_input) # leave input
  
  if('p_var' %notin% colnames(n)){
    p_var_col <- which(substr(colnames(n), 1, 2) == 'p_')
    setnames(n, colnames(n)[p_var_col], 'p_var')
  }
  
  n <- n %>% select('p_var','proportion')
  
  bs_vars <- c()
  if('bs_index' %in% colnames(beta)){
    if(n_distinct(beta$bs_index) > 1){
      bs_vars <- c('bs_index')
    }
  }
  
  selection_vars <- c('p_var','c_var','mu')
  
  beta_thin <- beta %>% select(!!!syms(bs_vars), !!!syms(selection_vars))
  
  if(!is.null(bs_vars)){
    if(nrow(beta_thin) != n_distinct(beta_thin$bs_index)*n_distinct(beta_thin$p_var)*n_distinct(beta_thin$c_var)){
      stop('Contact matrix defined by index other than bs.')
    }
  }else{
    if(nrow(beta_thin) != n_distinct(beta_thin$p_var)*n_distinct(beta_thin$c_var)){
      stop('Contact matrix defined by index other than bs.')
    }
  }
  
  beta_hat <- beta_thin %>% left_join(n, by = 'p_var') %>% mutate(beta_hat_var = mu*proportion)
  
  beta_hat$p_var <- factor(beta_hat$p_var, levels = order)
  beta_hat$c_var <- factor(beta_hat$c_var, levels = order)
  
  B <- beta_hat %>% left_join(beta_hat %>% group_by(!!!syms(bs_vars), c_var) %>% summarise(sum_beta_hat_var = sum(beta_hat_var)),
                              by = c(bs_vars,'c_var')) %>% 
    mutate(B_var = beta_hat_var/sum_beta_hat_var)
  
  # B %>% group_by(p_var, c_var) %>% summarise(mean_B = mean(B_var)) %>% 
  #   ggplot() + geom_tile(aes(x = p_var, y = c_var, fill = mean_B)) +
  #   theme_minimal() + geom_text(aes(x = p_var, y = c_var, label = round(mean_B, 2)), col = 'white') +
  #   labs(x = 'Participant', y = 'Contact', fill = 'Proportion of\ninfections') + theme(text = element_text(size = 14))
  
  Q <- B %>% filter(p_var == c_var) %>% group_by(!!!syms(bs_vars)) %>% 
    summarise(sum_Bii = sum(B_var)) %>% mutate(Q_var = (sum_Bii - 1)/(n_distinct(beta_thin$p_var) - 1)) %>% 
    select(!!!syms(bs_vars), Q_var) %>% filter(!is.na(Q_var))
  
  if(!is.null(bs_vars)){
    q <- data.table(bs_index = unique(beta$bs_index),
                    q_var = 0)
    
    for(bs in q$bs_index){
      
      matrix <- beta_hat %>% filter(bs_index == bs) %>% arrange(p_var, c_var) %>% 
        select(p_var, c_var, mu) %>% pivot_wider(id_cols = p_var, names_from = c_var, values_from = mu) %>% 
        select(!p_var)
      
      matrix <- as.matrix(matrix)
      
      eigenvalues <- eigen(matrix)$values
      
      q[bs_index == bs, q_var := eigenvalues[2]/eigenvalues[1]]
      
    }
    
  }else{
    
    matrix <- beta_hat %>% arrange(p_var, c_var) %>% 
      select(p_var, c_var, mu) %>% pivot_wider(id_cols = p_var, names_from = c_var, values_from = mu) %>% 
      select(!p_var)
    
    matrix <- as.matrix(matrix)
    
    eigenvalues <- eigen(matrix)$values
    
    q <- eigenvalues[2]/eigenvalues[1]
    
  }
  
  if(!is.null(bs_vars)){
    
    return(data.table(
      variable = c('Q','q'),
      mean = c(mean(Q$Q_var), mean(q$q_var)),
      lower_ci = c(quantile(Q$Q_var, (1 - ci)/2), quantile(q$q_var, (1 - ci)/2)),
      upper_ci = c(quantile(Q$Q_var, 1 - (1 - ci)/2), quantile(q$q_var, 1 - (1 - ci)/2))
    ))
    
  }else{
    
    return(data.table(
      variable = c('Q','q'),
      value = c(Q$Q_var, q)
    ))
    
  }
  
}


# Function to plot matrix

plot_mu_matrix <- function(matrix_data,
                           p_var_name, c_var_name,
                           variable,
                           uncertainty = F,
                           text_lab = T) {
  
  text_lab <- (firstup(matrix_data$c_location[1]) == 'Total' | p_var_name != 'p_age_group')
  
  if(p_var_name == 'p_age_group'){
    matrix_data$p_var <- factor(matrix_data$p_var,
                                      levels = age_labels)
  }
  if(c_var_name == 'c_age_group'){
    matrix_data$c_var <- factor(matrix_data$c_var,
                                levels = age_labels)
  }
  if(p_var_name == 'p_ethnicity'){
    matrix_data <- matrix_data[!p_var %like% 'refer']
    matrix_data$p_var <- factor(matrix_data$p_var, levels = rev(c('Other','Mixed','Black','Asian','White')))
  }
  if(c_var_name == 'c_ethnicity'){
    matrix_data <- matrix_data[!c_var %like% 'refer']
    matrix_data$c_var <- factor(matrix_data$c_var, levels = rev(c('Other','Mixed','Black','Asian','White')))
  }
  if(p_var_name == 'p_sec_input'){
    matrix_data$p_var <- factor(matrix_data$p_var, levels = c(1:7, 'Under 17','Student','Retired','Unemployed','Unknown'))
    matrix_data$p_var <- fct_recode(matrix_data$p_var, "1. Higher managerial, administrative\nand professional occupations" = '1',
                                    "2. Lower managerial, administrative\nand professional occupations" = '2',
                                    "3. Intermediate occupations" = '3',
                                    "4. Small employers and\nown account workers" = '4',
                                    "5. Lower supervisory and\ntechnical occupations" = '5',
                                    "6. Semi-routine occupations" = '6',
                                    "7. Routine occupations" = '7')
  }
  if(c_var_name == 'c_sec_input'){
    matrix_data$c_var <- factor(matrix_data$c_var, levels = c(1:7, 'Under 17','Student','Retired','Unemployed','Unknown'))
    matrix_data$c_var <- fct_recode(matrix_data$c_var, "1. Higher managerial, administrative\nand professional occupations" = '1',
                                    "2. Lower managerial, administrative\nand professional occupations" = '2',
                                    "3. Intermediate occupations" = '3',
                                    "4. Small employers and\nown account workers" = '4',
                                    "5. Lower supervisory and\ntechnical occupations" = '5',
                                    "6. Semi-routine occupations" = '6',
                                    "7. Routine occupations" = '7')
  }
  
  font_size <- if(p_var_name == 'p_sec_input' & firstup(matrix_data$c_location[1]) == 'Total'){11}else{
    if(p_var_name == 'p_sec_input' & firstup(matrix_data$c_location[1]) != 'Total'){6}else{14}
  }
  
  label_size <- if(p_var_name=='p_sec_input'&firstup(matrix_data$c_location[1])!='Total'){2}else{
    if(p_var_name=='p_sec_input'){4}else{
      if(p_var_name=='p_age_group'){4}else{
        if(p_var_name=='p_ethnicity'){5}
      }
    }
  }
  
  max_val <- max((matrix_data %>% group_by(p_var, c_var) %>% 
                    summarise(mean = mean(!!sym(variable))))$mean)
  
  
  if(uncertainty == F){
    if(text_lab == T){
      
      matrix_data %>% group_by(p_var, c_var) %>% 
        summarise(mean = mean(!!sym(variable))) %>% 
        ggplot() + 
        geom_tile(aes(x=p_var, y=c_var, fill=mean)) + 
        geom_text(aes(x=p_var, y=c_var,
                      label = sprintf("%.1f", mean), 
                      color = (mean < 0.6*max_val)), size = label_size) +
        scale_fill_viridis_c(name = ifelse(variable=='mu', "Mean of negative\nbinomial distribution",
                                           "Size of negative\nbinomial distribution"),
                             trans = "pseudo_log", option=ifelse(variable=='mu','D','B'),
                             limits = c(0, max_val)) +
        scale_color_manual(values = c('black','white')) + 
        guides(color="none") +
        labs(title = firstup(matrix_data$c_location[1]),
             x = format_label(p_var_name),
             y = format_label(c_var_name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = font_size)) +
        coord_equal()
    }else{
      matrix_data %>% group_by(p_var, c_var) %>% 
        summarise(mean = mean(!!sym(variable))) %>% 
        ggplot() + 
        geom_tile(aes(x=p_var, y=c_var, fill=mean)) + 
        scale_fill_viridis_c(name = ifelse(variable=='mu', "Mean",
                                           "Size"),
                             trans = "pseudo_log", option=ifelse(variable=='mu','D','B'),
                             limits = c(0, max_val)) +
        labs(title = firstup(matrix_data$c_location[1]),
             x = format_label(p_var_name),
             y = format_label(c_var_name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = font_size)) +
        coord_equal()
    }
  }else{
    matrix_data %>% group_by(p_var, c_var) %>% 
      summarise(width = quantile(!!sym(variable), 0.975) - quantile(!!sym(variable), 0.025)) %>% 
      ggplot() + 
      geom_tile(aes(x=p_var, y=c_var, fill=width)) + 
      geom_text(aes(x=p_var, y=c_var, 
                    label = sprintf("%.1f", width)), color = "white", size = 2) +
      scale_fill_viridis_c(name = ifelse(variable=='mu', "Width of\nconfidence interval (mean)",
                                         "Width of\nconfidence interval (size)"),
                           trans = "pseudo_log", option = ifelse(variable=='mu','A','C'),
                           limits = c(0, max_val)) +
      labs(title = firstup(matrix_data$c_location[1]),
           x = format_label(p_var_name),
           y = format_label(c_var_name)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size = font_size)) +
      coord_equal()
  }
  
}

# Function to plot and save matrix
plot_and_save_mu_matrix <- function(matrix_data, 
                                    p_var, c_var,
                                    variable = 'mu', 
                                    uncertainty = F,
                                    text_lab_in = T) {
  
  folder <- gsub('p_', '', p_var)
  
  matrix_plots <- map(.x = matrix_data, 
                      .f = ~plot_mu_matrix(.x,
                                           p_var, c_var,
                                           variable,
                                           uncertainty,
                                           text_lab_in))
  
  map(.x = names(matrix_plots), 
      .f = ~ggsave(here::here('results',survey,'neg_bin',folder,variable,
                              ifelse(uncertainty,'uncertainty','contacts'),
                              paste0('nb_plot_',tolower(.x), ifelse(uncertainty,'_ui',''),'.png')),
                   matrix_plots[[.x]],
                   width = 8,
                   height = 8,
                   dpi = 600,
                   bg = "white"))
  
  layout <- "
  AABC
  AADE
  "
  
  plot_nb <- patchwork::wrap_plots(matrix_plots, design = layout,
                        plot_layout(guides = 'collect')) + 
                        plot_annotation(tag_levels = 'a',
                                        tag_prefix = '(',
                                        tag_suffix = ')'); plot_nb
  
  ggsave(here::here('results',survey,'neg_bin',folder,variable,
                    ifelse(uncertainty,'uncertainty','contacts'),
                    paste0('nb_plot_combined', ifelse(uncertainty,'_ui',''),'.png')),
         plot_nb,
         width = 20,
         height = 9,
         dpi = 800,
         bg = "white")
  
  plot_nb
  
}


# Function to plot bootstrapped data
plot_nb_contact_data <- function(data, variable, 
                                 ci_var, max_upper, upper_fixed_at) {
  
  filtered_data <- data %>% filter(Variable == variable) %>% 
    select(Variable, Category_short, N, !!sym(ci_var), bs_index) 
  
  colnames(filtered_data) <- c('Variable', 'Category_short', 'N', 'ci_var', 'bs_index')
  
  filtered_data <- filtered_data %>% 
      group_by(Variable, Category_short, N) %>% 
      summarise(mean = mean(ci_var), 
                lower_ci = quantile(ci_var, 0.025), 
                upper_ci = quantile(ci_var, 0.975))
  
  filtered_data <- reorder_factors(filtered_data, variable)
  
  filtered_data <- filtered_data %>% 
    mutate(over_max_upper_ci = case_when(upper_ci > max_upper ~ T,
                                        T ~ F)) %>% 
    mutate(upper_ci = case_when(upper_ci > max_upper ~ max_upper,
                                T ~ upper_ci))
  
  filtered_data %>% 
      ggplot(aes(x = Category_short, y = mean, ymin = lower_ci, ymax = upper_ci)) +
      geom_pointrange(aes(x = Category_short, ymin=lower_ci, ymax=upper_ci)) +
      geom_arrowsegment(data = filtered_data %>% filter(over_max_upper_ci == T),
                        aes(x = Category_short, xend = Category_short, y = mean, yend = upper_ci), 
                        arrows = arrow(type = 'closed', length=unit(0.2,'cm'))) +
      coord_flip() + 
      ggtitle(variable) + 
      labs(x = NULL, y = "Mean with 95% CI") +
      theme_bw() + 
      ylim(c(0, upper_fixed_at)) +
      theme(text = element_text(size = 15),
            legend.position='none') 
    
}

plot_nb_contact_data_locn <- function(data, variable, 
                                      ci_var, max_upper, upper_fixed_at) {
  
  filtered_data <- data %>% filter(Variable == variable) %>% 
    select(Variable, c_location, Category_short, N, !!sym(ci_var), bs_index) 
  
  colnames(filtered_data) <- c('Variable', 'c_location', 'Category_short', 'N', 'ci_var', 'bs_index')
  
  filtered_data <- filtered_data %>% 
    group_by(Variable, c_location, Category_short, N) %>% 
    summarise(mean = mean(ci_var), 
              lower_ci = quantile(ci_var, 0.025), 
              upper_ci = quantile(ci_var, 0.975))
  
  filtered_data <- reorder_factors(filtered_data, variable)
  
  filtered_data <- filtered_data %>% 
    mutate(over_max_upper_ci = case_when(upper_ci > max_upper ~ T,
                                         T ~ F)) %>% 
    mutate(upper_ci = case_when(upper_ci > max_upper ~ max_upper,
                                T ~ upper_ci))
  
  filtered_data %>% 
    ggplot(aes(x = Category_short, y = mean, ymin = lower_ci, ymax = upper_ci, col = c_location)) +
    geom_pointrange(aes(x = Category_short, ymin=lower_ci, ymax=upper_ci),
                    position = position_dodge(width = 0.7)) +
    coord_flip() + 
    ggtitle(variable) + 
    scale_color_brewer(palette = 'Set1',guide = guide_legend(reverse = TRUE)) + 
    labs(x = NULL, y = "Mean with 95% CI") +
    theme_bw() + labs(color = 'Contact location') + 
    ylim(c(0, upper_fixed_at)) +
    theme(text = element_text(size = 15)) 
  
}













