
## reformatting survey data ##

part <- reconnect_participant %>% 
  rename(p_id = part_id,
         p_age_group = part_age_group,
         p_adult_child = part_adult_child,
         p_gender = part_gender,
         p_ethnicity = part_ethnicity,
         p_sec_input = part_ses) %>% 
  mutate(p_gender = case_when(p_gender == 'F' ~ 'Female',
                              p_gender == 'M' ~ 'Male',
                              T ~ NA),
         p_age_group = cut(part_age_exact,
                           breaks = age_breaks,
                           labels = age_labels,
                           right = F))

contacts <- reconnect_contact %>% 
  rename(c_id = cont_id,
         p_id = part_id,
         c_location = cnt_location,
         c_age_group = cnt_age_group,
         c_sex = cnt_gender,
         c_ethnicity = cnt_ethnicity,
         c_sec_input = cnt_ses) %>% 
  mutate(c_sex = case_when(c_sex == 'F' ~ 'Female',
                           c_sex == 'M' ~ 'Male',
                           T ~ NA),
         c_age_group = cut(cnt_age_exact,
                           breaks = age_breaks,
                           labels = age_labels,
                           right = F))

# if results folder doesn't exist, create
if(!file.exists('results')){dir.create('results')}

###################
#### AGE GROUP ####
###################

calc_matrices <- c(F, F, F)

if(calc_matrices[1]){
  nb_age_group <- nb_matrix_fit(
    participant_data = part,
    contact_data = contacts,
    participant_var = "p_age_group",
    contact_var = "c_age_group",
    n_bootstrap = 1000,
    trunc = max_n_contacts,
    polymod_weighting = polymod_wts,
    weighting_vec = c('p_gender','p_ethnicity','day_week'),
    locations = c("Total", "Home", "Work", "School", "Other")
  )
}else{
  nb_age_group <- readRDS(here::here('results',survey,'neg_bin','nb_age_group.rds'))
}

cat('\nAge done, n bootstraps = ', n_distinct(nb_age_group[[1]]$bs_index), '\n', sep = '')

# normalise wrt population structure
nb_age_group_norm <- map(
  .x = nb_age_group,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = 'p_age_group',
    population_dt = age_structure_fine
  )
)

age_assortativity <- map(
  .x = nb_age_group_norm,
  .f = ~fcn_assortativity(
    beta = .x,
    n_input = age_structure_fine,
    order = age_labels
  )
)
names(age_assortativity) <- names(nb_age_group_norm)
age_assortativity_dt <- rbindlist(age_assortativity, idcol = 'location')
age_assortativity_dt[, grouping := 'Age group']

age_matrices <- plot_and_save_mu_matrix(nb_age_group_norm,
                                        p_var = 'p_age_group',
                                        c_var = 'c_age_group',
                                        text_lab_in = F)

plot_and_save_mu_matrix(nb_age_group_norm,
                        p_var = 'p_age_group',
                        c_var = 'c_age_group',
                        uncertainty = T)

plot_and_save_mu_matrix(nb_age_group,
                         variable = 'k',
                         p_var = 'p_age_group',
                         c_var = 'c_age_group')

##################
### ETHNICITY ####
##################

if(calc_matrices[2]){
  nb_ethnicity <- nb_matrix_fit(
    participant_data = part,
    contact_data = contacts,
    participant_var = "p_ethnicity",
    contact_var = "c_ethnicity",
    n_bootstrap = 1000,
    trunc = max_n_contacts,
    polymod_weighting = polymod_wts,
    weighting_vec = c('p_gender','p_age_group','day_week'),
    locations = c("Total", "Home", "Work", "School", "Other"),
    save = T,
    impute_contact = T,
    impute_vars = c('p_ethnicity')
  )
}else{
  nb_ethnicity <- readRDS(here::here('results',survey,'neg_bin','nb_ethnicity.rds'))  
}

cat('\nEthnicity done, n bootstraps = ', n_distinct(nb_ethnicity[[1]]$bs_index), '\n', sep = '')

# normalise wrt population structure
nb_ethnicity_norm <- map(
  .x = nb_ethnicity,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = 'p_ethnicity',
    population_dt = ons_ethnicity
  ) %>% filter(!p_var %like% 'Prefer', !c_var %like% 'Prefer')
)

ethn_assortativity <- map(
  .x = nb_ethnicity_norm,
  .f = ~fcn_assortativity(
    beta = .x,
    n_input = ons_ethnicity,
    order = arrange(ons_ethnicity, desc(ons_ethnicity$proportion))$p_ethnicity
  )
)
names(ethn_assortativity) <- names(nb_ethnicity_norm)
ethn_assortativity_dt <- rbindlist(ethn_assortativity, idcol = 'location')
ethn_assortativity_dt[, grouping := 'Ethnicity']

nb_ethnicity_norm_pc <- map(
  .x = nb_ethnicity,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = 'p_ethnicity',
    population_dt = ons_ethnicity,
    per_capita = T
  ) %>% mutate(mu = mu*1e6)
)

ethnicity_matrices_pc <- plot_and_save_mu_matrix(nb_ethnicity_norm_pc,
                                                 p_var = 'p_ethnicity',
                                                 c_var = 'c_ethnicity')

ethnicity_matrices <- plot_and_save_mu_matrix(nb_ethnicity_norm,
                        p_var = 'p_ethnicity',
                        c_var = 'c_ethnicity',
                        text_lab_in = F)

plot_and_save_mu_matrix(nb_ethnicity_norm,
                        p_var = 'p_ethnicity',
                        c_var = 'c_ethnicity',
                        uncertainty = T)

plot_and_save_mu_matrix(nb_ethnicity,
                        variable = 'k',
                        p_var = 'p_ethnicity',
                        c_var = 'c_ethnicity')


#############
#### SES ####
#############

if(calc_matrices[3]){
  nb_sec_input <- nb_matrix_fit(
    participant_data = part,
    contact_data = contacts,
    participant_var = "p_sec_input",
    contact_var = "c_sec_input",
    n_bootstrap = 1000,
    trunc = max_n_contacts,
    polymod_weighting = polymod_wts,
    large_n_input = F,
    weighting_vec = c('day_week'),
    locations = c("Total", "Home", "Work", "School", "Other"),
    save = T,
    impute_contact = T,
    impute_vars = c('p_sec_input')
  )
}else{
  nb_sec_input <- readRDS(here::here('results',survey,'neg_bin','nb_sec_input.rds'))  
}

cat('\nNS-SEC done, n bootstraps = ', n_distinct(nb_sec_input[[1]]$bs_index), '\n', sep = '')

sec_props <- copy(nssec_census) %>%
  mutate(nssec_class = as.character(nssec_class)) %>%
  setnames('nssec_class', 'p_sec_input')

ses_assortativity <- map(
  .x = nb_sec_input,
  .f = ~fcn_assortativity(
    beta = .x %>% filter(p_var %in% as.character(1:7), c_var %in% as.character(1:7)),
    n_input = sec_props,
    order = as.character(1:7)
  )
)

names(ses_assortativity) <- names(nb_sec_input)
ses_assortativity_dt <- rbindlist(ses_assortativity, idcol = 'location')
ses_assortativity_dt[, grouping := 'NS-SEC']

sec_matrices <- plot_and_save_mu_matrix(nb_sec_input,
                        p_var = 'p_sec_input',
                        c_var = 'c_sec_input',
                        text_lab_in = F)
plot_and_save_mu_matrix(nb_sec_input,
                        p_var = 'p_sec_input',
                        c_var = 'c_sec_input',
                        uncertainty = T)
plot_and_save_mu_matrix(nb_sec_input,
                        variable = 'k',
                        p_var = 'p_sec_input',
                        c_var = 'c_sec_input')

## patchwork figure

matrs_fig <- (age_matrices | ethnicity_matrices[[1]] + sec_matrices[[1]]) + 
  plot_layout(nrow=2) + plot_annotation(tag_levels = c('A'),
                                        tag_prefix = '',
                                        tag_suffix = ')'); matrs_fig

ggsave(
  filename = file.path("results", "contact_matrs.pdf"),
  plot = matrs_fig,
  width = 20,
  height = 18,
  device = "pdf",
  bg = "white"
)

ggsave(
  filename = file.path("results", "contact_matrs.png"),
  plot = matrs_fig,
  width = 20,
  height = 18,
  device = "png",
  bg = "white"
)

## save matrix elements

save_matrices_values <- rbind(cbind(rbindlist(nb_age_group_norm), var = 'Age groups'),
                              cbind(rbindlist(nb_ethnicity_norm), var = 'Ethnicity groups'),
                              cbind(rbindlist(nb_sec_input)[, k := NULL], var = 'NS-SEC classes'))

save_matrices_values <- save_matrices_values %>%
  drop_na() %>% # remove 'Prefer not to say' ethnicity
  group_by(var,c_location,p_var,c_var) %>%
  summarise(mean = mean(mu),
            lower = quantile(mu, 0.025),
            upper = quantile(mu, 0.975))

save_matrices_values <- save_matrices_values %>%
  mutate(c_location = firstup(c_location)) %>%
  mutate(neat_out = paste0(round(mean, 2), ' (', round(lower, 2), ' - ', round(upper, 2), ')')) %>%
  select(! c(mean, lower, upper))

save_matrices_values_w <- save_matrices_values %>%
  pivot_wider(names_from = c_location, values_from = neat_out)

save_matrices_values_w$p_var <- factor(save_matrices_values_w$p_var,
                                       levels = c(age_labels, 'White','Asian','Black','Mixed','Other',
                                                  as.character(1:7), 'Under 17','Student','Retired','Unemployed','Unknown'))

save_matrices_values_w$c_var <- factor(save_matrices_values_w$c_var,
                                       levels = c(age_labels, 'White','Asian','Black','Mixed','Other',
                                                  as.character(1:7), 'Under 17','Student','Retired','Unemployed','Unknown'))


save_matrices_values_w <- save_matrices_values_w %>% arrange(p_var, c_var) %>%
  filter(! (var == 'NS-SEC classes' & (c_var == 'Unknown' | p_var == 'Unknown'))) %>% 
  select(var, p_var, c_var, Total, Home, Work, School, Other)

colnames(save_matrices_values_w) <- c('Stratifying variable','Participant variable','Contact variable','All settings','Home','Work','School','Other')

write_xlsx(save_matrices_values_w,
          here::here('results','contact_matrix_values.xlsx'))


### SAVE ASSORTATIVITY

assortativity <- rbind(
  age_assortativity_dt,
  ethn_assortativity_dt,
  ses_assortativity_dt
)

write_xlsx(assortativity, here::here('results','assortativity.xlsx'))

assortativity$variable <- factor(assortativity$variable, levels = c('Q','q'))
assortativity$location <- factor(assortativity$location, levels = c('Total','Home','Work','School','Other'))

assortativity %>% filter(variable == 'Q') %>%
  ggplot() +
  geom_errorbar(aes(x = location, ymin = lower_ci, ymax = upper_ci, group = location),
                position = position_dodge(width = 0.90), width = 0.3, lwd = 0.8) +
  geom_point(aes(x = location, y = mean, group = location),
             position = position_dodge(width = 0.90), size = 2) + ylim(c(0,NA)) +
  theme_bw() + facet_grid(. ~ grouping, scales = 'fixed') +
  # theme(legend.position = 'none') +
  scale_color_brewer(palette = 'Paired') + xlab('Contact setting') + ylab('Assortativity')

ggsave(here::here('results','assortativity_filt.png'), width = 10, height = 4)







