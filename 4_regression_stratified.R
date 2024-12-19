# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Test associations stratified by system size
# LATEST REVISION: 2024-11-12 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Start here:
# source("1_combine_process.R")

# for regression:
library(lme4)             # glmer() 
# help(lme4)

# for cleaning outputs of regression model runs:
library(gtools)           # stars.pval()
library(broom.mixed) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces the results reported in Table 4. We evaluated associations
# between contaminant detection (any of the target contaminants) and the set of 
# explanatory variables (county served demographics, PWS characteristics, wastewater, 
# and any TRI facilities), after stratifying by system size. Overall, small systems face 
# greater challenges in obtaining and maintaining water quality relative to 
# large systems. In addition, small systems tend to serve more rural communities. 
# We tested whether associations were consistent across models after stratification, 
# or whether differences emerge based on system size.
# 
# The script uses a logistic mixed-effect model from the lme4 package. Similar to 
# the main regression script ("4__regressions_main.R"), this script creates a nested
# data frame with a list-column and applies a function to obtain odds ratios (95% CIs), 
# standard errors, and p-values. 

# Start ---

nested_df4strat <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>% 
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
               names_to = "outcome_name", 
               values_to = "outcome_value") %>%
  group_by(outcome_name, size) %>%
  nest() 

nested_df4strat2 <- nested_df4strat %>%
  filter(outcome_name == "det_any")

nested_df4strat3 <- nested_df4strat2 %>%
  mutate(my_formula = paste0(
    "outcome_value ~ ", 
    "perc_hisp_any + perc_black_nohisp + mdi_rate + perc_urban + ", 
    "pws_type + n_samples + ", 
    "adj_wwtp_flow + n_fac_any_bin + ", 
    "(1|state)" 
  ))

# Create a function that conducts adjusted logistic mixed-effects models. 
# Use tidy() to save from the broom.mixed package to clean model results as a 
# tidy data frame object. Calculate odds ratios and 95% CI.
# This will be used to loop over a list-column of a nested data frame.
# As of 6/26/24, we removed percent change formatting. 
# Note- same function as in "4__regressions_main.R"

run_log2 <- function(dat, formula){
  lme4::glmer(formula = formula,
              data = dat, 
              family = binomial) %>% 
    broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

# Apply the function "run_log2()" over the list-column in the nested 
# data frame. This may take a while to run! (Estimated: 1.5 mins). 

stratified_results <- nested_df4strat3 %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$outcome_value)))) %>%
  mutate(model_results = 
           map(data,
               ~run_log2(dat = ., 
                         formula = my_formula))) %>%
  unnest(model_results) 

# Visual inspection 
# stratified_results %>%
#   select(outcome_name, term, n, p.value, estimate, conf.low, conf.high) #%>%
# # view()

# Clean the output. Remove intercepts. Format odds ratios and the 95% CIs. 

stratified_results2 <- stratified_results %>% 
  filter(!str_detect(term, "Intercept")) %>%
  select(-data) %>%
  mutate(estimate = format(round(estimate, 2), nsmall = 2), 
         conf.low = format(round(conf.low, 2), nsmall = 2),
         conf.high = format(round(conf.high, 2), nsmall = 2), 
         estimate_edit = paste0(estimate, " (", conf.low, ", ", conf.high, ")"), 
         p_star = gtools::stars.pval(p.value), 
         p_format = 
           format.pval(p.value, eps = 0.001, nsmall = 2, digits = 2)
  )

# Tidy the outputs by ordering the predictors according to how it appeared 
# in the paper. 

TableOrder3 <-   c("perc_hisp_any", 
                   "perc_black_nohisp", 
                   "mdi_rate",
                   "perc_urban", 
                   "size",
                   "sizeL",
                   "pws_type",
                   "pws_typeGW",
                   "pws_typeMX", 
                   "n_samples", 
                   "adj_wwtp_flow",
                   "n_fac_any_bin",
                   "n_fac_diox_bin",
                   "n_fac_chlor_solv_bin", 
                   "n_fac_cfc_bin",
                   "src_epa_present_bin",
                   "n_MFTA_airport_bin", 
                   
                   # for adjusted regressions:
                   "n_fac_any_bin1",
                   "n_fac_diox_bin1",
                   "n_fac_chlor_solv_bin1",
                   "n_fac_cfc_bin1",
                   "src_epa_present_bin1",
                   "n_MFTA_airport_bin1",
                   
                   # for supplementary regressions:
                   "perc_hmown", "perc_pov_ppl", "perc_uninsur")

stratified_results3 <- stratified_results2 %>%
  select(outcome_name, size, term, n, estimate_edit, p_format, p_star) %>%
  mutate(term = factor(term, levels = TableOrder3)) %>%
  arrange(term)

stratified_results_export <- stratified_results3 %>%
  pivot_wider(
    id_cols =  term,
    names_from = c(size, outcome_name, n),
    values_from = c(estimate_edit, p_format, p_star),
    names_glue = "{size}_{outcome_name}_{n}_{.value}", 
    names_vary = "slowest",
    names_sort = TRUE,
  ) 

# Save progress. 

# write.csv(
#   stratified_results_export,
#   paste0(
#     "results/Size Stratified Adjusted Logistic Results_",
#     Sys.Date(),
#     ".csv"
#   )
# )

# Archived ---- 

# base_formula2 <- paste(
#   "~ perc_hisp_any + perc_black_nohisp + mdi_rate +",
#   "perc_urban + pws_type + n_samples + adj_wwtp_flow",
#   collapse = " "
# )
# 
# nested_df4strat2 <- nested_df4strat %>%
#   mutate(my_formula = paste("value", base_formula2)) %>%
#   mutate(my_formula = case_when(str_detect(name,"any") ~ paste(my_formula, "+ n_fac_any_bin"), 
#                                 str_detect(name, "diox") ~ paste(my_formula, "+ n_fac_diox_bin"), 
#                                 str_detect(name, "dca") ~ paste(my_formula, "+ n_fac_chlor_solv_bin"), 
#                                 str_detect(name, "hcfc") ~ paste(my_formula, "+ n_fac_cfc_bin"), 
#                                 str_detect(name, "pfas") ~ paste(my_formula, "+  n_MFTA_airport_bin + src_epa_present_bin"), 
#                                 TRUE ~ "9999")) %>%
#   {stopifnot(nrow(filter(., my_formula == "9999"))==0); .;} %>%
#   mutate(my_formula = paste(my_formula, " + (1|state)")) #%>%
# # mutate(my_formula = if_else(
# #   name == "det_dca", 
# #   str_remove(my_formula, "size \\+ "), 
# #   my_formula
# # ))
# 
# nested_data2_ready <- nested_df4strat2 %>% 
#   filter(name == "det_any") 
# # filter(name %in% c("det_any", "viol_any", "det_diox", "det_dca", "det_hcfc", "det_pfas"))
# 
# # Visual check that the formulas make sense with the outcomes
# nested_data2_ready %>%
#   distinct(name, my_formula)

# ## function
# run_log2 <- function(dat, my_formula){
#   lme4::glmer(my_formula, data = dat, family = binomial) %>% 
#     broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
#     mutate(estimate_perc = 100*(estimate - 1), 
#            estimate_perc_low = 100*(conf.low - 1), 
#            estimate_perc_hi = 100*(conf.high - 1), 
#            fmt_perc_change = paste0(format(round(estimate_perc, 1), nsmall = 1), 
#                                     " (", format(round(estimate_perc_low, 1), nsmall = 1), 
#                                     ", ", format(round(estimate_perc_hi, 1), nsmall = 1), 
#                                     ")"), 
#            p_stars = gtools::stars.pval(p.value))
# }
# 
# #+ This takes approximately 3 minutes to run entirely.
# #+ temp_results is what happens after running ALL the regression models (list of 
# #+ data still in df). Do not view().
# temp_results <- temp %>%
#   filter(name != "viol_dca") %>% 
#   filter(name != "det_dca") %>% # this outcome was throwing an error - investigate separately
#   mutate(model_results = map(data, 
#                              ~run_log2(dat = ., 
#                                        my_formula = my_formula))) %>%
#   unnest(model_results)
# 
# colnames(temp_results)
# 
# ## formatting to wider and ordering columns
# stratified_results <- temp_results %>%
#   filter(!str_detect(term, "(Intercept)")) %>%
#   # mutate(p.value = format(round(p.value, 3), nsmall = 1)) %>% # formatting p.value
#   select(-estimate) %>%
#   rename(estimate = fmt_perc_change,, p.stars = p_stars) %>%
#   pivot_wider(id_cols = c(term), 
#               names_from = c(name, size),
#               values_from = c(estimate, p.value, p.stars), 
#               names_sort = TRUE, 
#               names_glue = "{name}_{size}_{.value}") %>%
#   #colnames() %>%
#   select(term, 
#          starts_with("det_any_L"), 
#          starts_with("det_any_S"), 
#          starts_with("viol_any_L"), # in this order in table
#          starts_with("viol_any_S"), # in this order in table
#          starts_with("det_diox_L"), 
#          starts_with("det_diox_S"), 
#          starts_with("det_hcfc_L"), 
#          starts_with("det_hcfc_S"), 
#          starts_with("det_dca_L"), 
#          starts_with("det_dca_S"), 
#          starts_with("det_pfas_L"), 
#          starts_with("det_pfas_S"), 
#          # not needed:
#          # starts_with("viol_diox"), 
#          # starts_with("viol_dca"), 
#          # starts_with("viol_pfas")
#          )
# 
# ## more formatting 
# stratified_results2 <- stratified_results %>%
#   mutate_at(vars(matches("_estimate")), ~custom_trimws(.)) %>%
#   mutate_at(vars(matches("_estimate")), ~str_replace(., c("NA|Inf|\\+"), as.character(NA))) %>%
#   mutate_at(vars(matches("_estimate")), ~str_replace(., c(".{30,}"), "--")) %>%
#   mutate_at(vars(matches("_p.value")), ~ifelse(. < 0.001, "<0.001", round(., 5))) %>%
#   mutate(term = factor(term, levels = TableOrder_vec)) %>%
#   arrange(term)
# 
# flex_basic <- function(dat){
#   # dat = data.frame object 
#   dat %>% 
#     flextable() %>%
#     separate_header(split = "_") %>%
#     set_table_properties(layout = "autofit") %>%
#     theme_box()
#   
# }
# 
# tempfile(fileext = ".docx")
# tempdir()
# 
# 
# pr_sect_default <- prop_section(
#   page_size = page_size(
#     orient = 'landscape', 
#     width = 10, height = 12), 
#   type = "continuous", 
# )
# 
# 
# stratified_results2 %>%
#   select(term, starts_with("det_any"), starts_with("viol_any")) %>%
#   flex_basic() %>%
#   save_as_docx(path = "outputs/stratified results.docx", 
#                pr_section = pr_sect_default
#                )
# 
# final_table3 %>%
#   mutate(det_any_freq = round(det_any_freq, 1), 
#          exceed_any_freq = round(det_any_freq, 1), 
#          pop_served = round(pop_served, 0)) %>% 
#   flextable() %>%
#   theme_box() %>%
#   colformat_num(big.mark = " ", decimal.mark = ".", na_str = "N/A") %>%
#   set_table_properties(layout = "autofit") %>%
#   save_as_docx(path = "outputs/study characteristics.docx", 
#                pr_section = pr_sect_default
#   )
# 
# bleh3 %>%
#   mutate(det_freq_samp = round(det_freq_samp, 1), 
#          exc_freq_samp = round(exc_freq_samp, 1), 
#          det_freq_sys = round(det_freq_sys, 1), 
#          exc_freq_sys = round(exc_freq_sys, 1)) %>%
#   flextable() %>%
#   theme_box() %>%
#   autofit()
# 
# crude_results1 %>%
#   mutate_at(vars(matches("p.value")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
#   select(term, starts_with("det_"), starts_with("viol_any")) %>%
#   flex_basic()
#   #save_as_docx(path = "outputs/stratified results.docx", 
#                pr_section = pr_sect_default
#   )
# 
# adjusted_results2 %>%
#   mutate_at(vars(matches("p.value")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
#   select(pred, starts_with("det_"), starts_with("viol_any")) %>%
#   flex_basic()
# 
# means3 %>%
#   mutate_at(vars(matches("p_")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
#   mutate_if(is.numeric, ~round(., 2)) %>%
#   flex_basic()
# 
# # https://davidgohel.github.io/flextable/reference/save_as_docx.html