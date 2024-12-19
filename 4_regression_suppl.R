# DATE STARTED: 2023-09-17
# AUTHOR: Aaron Maruzzo
# PURPOSE: Conduct sensitivity analyses 
# LATEST REVISION: 2024-11-12 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Start here (if not already run):
# source("1_combine_process.R")
# source("4__regressions_main.R")

library(tidyverse)
library(lme4)
library(broom.mixed)
library(gtools) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produces regression tables included in the supplement. 
# Fewer comments in this script since its organization is similar to "4__regressions_main.R"

# == Sensitivity check 1: with and without point source terms == 
# Conducts logistic mixed-effect models without certain point 
# source terms and wastewater. The aim was to compare how robust associations 
# were between the outcome and explanatory variables with and without 
# facilities and wastewater as explanatory variables. Since facility 
# siting could be driven by race/ethnicity and SES, including point sources 
# as terms in the model may be attenuating the relationship between demographics
# and contaminant detections. This model was compared with results from the 
# adjusted model. 

# == Sensitivity check 2: regressions with different SES variables (not MDI) == 
# The second section conducts logistic mixed-effect models using socioeconomic 
# (SES) variables other than percent deprived (or MDI). 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# With and without source terms ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare a nested data frame.

nested_data4supp <- dat_clean %>%
  select(-viol_dca, -viol_diox, -viol_pfas) %>%
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
               names_to =  "outcome_name", 
               values_to = "outcome_value") %>%
  mutate(add_source = "N") %>%
  group_by(outcome_name, add_source) %>%
  nest() 

# Define a base formula.
# Include terms for county-level demographic variables and water system characteristics. 
# Exclude point source terms. 
# Include a state intercept term at the end of the equation.

base_formula <- paste(
  "outcome_value ~ perc_hisp_any + perc_black_nohisp + mdi_rate +",
  "perc_urban + size + pws_type + n_samples + (1|state)",
  collapse = " "
)

nested_data4supp2 <- nested_data4supp %>%
  mutate(my_formula = base_formula)

# check equation
# nested_data4supp2 %>%
#   select(-data) %>%
#   view()

# Note- same function as in "4__regressions_main.R"
# Create a function that conducts *adjusted* logistic models
# Calculate odds ratios and 95% CI.
# This will be used to loop over a list-column of a nested data frame.

run_log2 <- function(dat, formula){
  lme4::glmer(formula = formula,
              data = dat, 
              family = binomial) %>% 
    broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

# Apply the function "run_log2()" over the list-column in the nested 
# data frame. This may take a while to run. (Estimated: 1.5 mins). 

suppl1_reg_results <- nested_data4supp2 %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$value)))) %>%
  mutate(model_results = 
           map(data,
               ~run_log2(dat = ., 
                         formula = my_formula))) %>%
  unnest(model_results) 

# Clean the output. Remove intercepts. Format odds ratios and the 95% CIs. 

suppl1_reg_results2 <- suppl1_reg_results %>% 
  filter(!str_detect(term, "Intercept")) %>%
  select(-data) %>%
  mutate(estimate = format(round(estimate, 2), nsmall = 2), 
         conf.low = format(round(conf.low, 2), nsmall = 2),
         conf.high = format(round(conf.high, 2), nsmall = 2), 
         estimate_edit = paste0(estimate, " (", conf.low, ", ", conf.high, ")"), 
         p_star = stars.pval(p.value), 
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

suppl1_reg_results3 <- suppl1_reg_results2 %>%
  select(outcome_name, add_source, term, n, estimate_edit, p_format, p_star) %>%
  mutate(term = factor(term, levels = TableOrder3)) %>%
  arrange(term)

suppl1_reg_results4 <- suppl1_reg_results3 %>%
  pivot_wider(
    id_cols =  c(term, add_source),
    names_from = outcome_name,
    values_from = c(estimate_edit, p_format, p_star),
    names_glue = "{outcome_name}_{.value}"
  ) %>%
  select(
    term, 
    add_source,
    starts_with("det_any"),
    starts_with("viol_any"),
    starts_with("det_diox"),
    starts_with("det_dca"),
    starts_with("det_hcfc"),
    starts_with("det_pfas")
  )

# Adjusted model results from Table 3 in paper
adj_res_to_merge <- adjusted_results_export %>% mutate(add_source = "Y")

# Bind main result and supplemental result together
suppl1_reg_results5 <- rbind(suppl1_reg_results4, adj_res_to_merge)
suppl1_reg_results_export <- suppl1_reg_results5 %>% arrange(term, add_source)

# Save progress. 

# write.csv(suppl1_reg_results_export,
#           paste0("results/SuppTable. Adj model results with & without source terms_",
#                  Sys.Date(),
#                  ".csv")
#           )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other socioeconomic indicators ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(dat_clean)[grepl("perc", colnames(dat_clean))]

mdiFormula <- paste("~ perc_hisp_any + perc_black_nohisp + mdi_rate +", 
                      "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                      collapse = " ")
povFormula <- paste("~ perc_hisp_any + perc_black_nohisp + perc_pov_ppl +", 
                    "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                    collapse = " ")
insuranceFormula <- paste("~ perc_hisp_any + perc_black_nohisp + perc_uninsur +", 
                    "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                    collapse = " ")
hmownFormula <- paste("~ perc_hisp_any + perc_black_nohisp + perc_hmown +", 
                    "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                    collapse = " ")
combinedFormula <- paste("~ perc_hisp_any + perc_black_nohisp + perc_pov_ppl + perc_uninsur + perc_hmown + ", 
                      "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                      collapse = " ")

data_for_univarSES_reg <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L")))

## Run the models separated by outcome (6 outcomes in total)

# This may take a few minutes to run (Estimate: 1 min)
suppl2_reg_results_det_any <- bind_rows(
  glmer(paste("det_any", povFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
)

# This may take a few minutes to run (Estimate: 1 min)
suppl2_reg_results_viol_any <- bind_rows(
  glmer(paste("viol_any", povFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("viol_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("viol_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
)

# This may take a few minutes to run (Estimate: 1 min)
suppl2_reg_results_det_diox <- bind_rows(
  glmer(paste("det_diox", povFormula, "n_fac_diox_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_diox", insuranceFormula, "n_fac_diox_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_diox", hmownFormula, "n_fac_diox_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

# This may take a few minutes to run (Estimate: 1 min)
# Note: the formula was modified to remove size as a covariate in these models.
suppl2_reg_results_det_dca <- bind_rows(
  glmer(paste("det_dca", povFormula, "n_fac_chlor_solv_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_dca", insuranceFormula, "n_fac_chlor_solv_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_dca", hmownFormula, "n_fac_chlor_solv_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

# This may take a few minutes to run (Estimate: 1 min)
suppl2_reg_results_det_hcfc <- bind_rows(
  glmer(paste("det_hcfc", povFormula, "n_fac_cfc_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_hcfc", insuranceFormula, "n_fac_cfc_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_hcfc", hmownFormula, "n_fac_cfc_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

# This may take a few minutes to run (Estimate: 1 min)
suppl2_reg_results_det_pfas <- bind_rows(
  glmer(paste("det_pfas", povFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_pfas", insuranceFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_pfas", hmownFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

## Combine results into one big dataset
suppl2_reg_results_all <- bind_rows(
  suppl2_reg_results_det_any %>% mutate(name = "det_any"),
  suppl2_reg_results_viol_any %>% mutate(name = "viol_any"),
  suppl2_reg_results_det_diox %>% mutate(name = "det_diox"),
  suppl2_reg_results_det_dca %>% mutate(name = "det_dca"),
  suppl2_reg_results_det_hcfc %>% mutate(name = "det_hcfc"),
  suppl2_reg_results_det_pfas %>% mutate(name = "det_pfas")
)

# Convert from dataframe to nested dataframe
suppl2_reg_results_all <- suppl2_reg_results_all %>%
  group_by(model_run) %>%
  nest()

# Visual inspection
suppl2_reg_results_all %>% pull(data)

## Clean outputs 

clean3 <- function(dat){
  dat %>% 
    filter(!str_detect(term, "Intercept")) %>%
    mutate(estimate = format(round(estimate, 2), nsmall = 2), 
           conf.low = format(round(conf.low, 2), nsmall = 2),
           conf.high = format(round(conf.high, 2), nsmall = 2), 
           estimate_edit = paste0(estimate, " (", conf.low, ", ", conf.high, ")"), 
           p_star = stars.pval(p.value), 
           p_format = 
             format.pval(p.value, eps = 0.001, nsmall = 2, digits = 2)
    )
}

suppl2_reg_results_tidy <- suppl2_reg_results_all %>%
  mutate(data = map(data, function(x){
    x %>% 
      mutate(estimate = if_else(estimate > 1000, as.numeric(NA), estimate), 
             conf.low = if_else(conf.low > 1000, as.numeric(NA), conf.low), 
             conf.high = if_else(conf.high > 1000, as.numeric(NA), conf.high))
  })) %>%
  mutate(clean_out = map(data, ~clean3(.))) %>%
  unnest(clean_out) %>%
  select(model_run, term, name, estimate_edit, p_star, p_format)

suppl2_reg_results_export <- suppl2_reg_results_tidy %>%
  pivot_wider(
    id_cols =  c(model_run, term),
    names_from = c(name),
    values_from = c(estimate_edit, p_format, p_star),
    names_glue = "{name}_{.value}", 
    names_vary = "slowest",
    names_sort = TRUE,
  ) %>%
  select(
    model_run, 
    term,
    starts_with("det_any"),
    starts_with("viol_any"),
    starts_with("det_diox"),
    starts_with("det_dca"),
    starts_with("det_hcfc"),
    starts_with("det_pfas")
  )

# Save progress.

# write.csv(
#   suppl2_reg_results_export,
#   paste0("results/SuppTable. Adj model results diff SES variables_",
#   Sys.Date(), ".csv"
#   )
# )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  ARCHIVE ----
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write.csv(d_list2[[1]],
#           paste0("outputs/", Sys.Date(), " - reg results pov.csv"))
# write.csv(d_list2[[2]],
#           paste0("outputs/", Sys.Date(), " - reg results uninsur.csv"))
# write.csv(d_list2[[3]],
#           paste0("outputs/", Sys.Date(), " - reg results hmown.csv"))
# 
# 
# suppl2_reg_results_det_any <- bind_rows(
#   glmer(paste("det_any", povFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
#     tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
#     mutate(model_run = "poverty"), 
#   glmer(paste("det_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
#     tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
#     mutate(model_run = "insurance status"), 
#   glmer(paste("det_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
#     tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
#     mutate(model_run = "homeownership"), 
#   glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = data_for_univarSES_reg, family = 'binomial') %>%
#     tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
#     mutate(model_run = "combined uniSES"), 
# )
# 
# suppl2_reg_results_det_any
# 
# d_list2 <- lapply(d_list, function(x) 
#   x %>% 
#     filter(str_detect(term, "Intercept", negate = TRUE)) %>%
#     select(term, name, p.value, estimate, conf.low, conf.high) %>% 
#     mutate(perc_change = 100*(estimate - 1), 
#            perc_change_LOW = 100*(conf.low - 1),
#            perc_change_UPP = 100*(conf.high - 1)) %>%
#     mutate_at(vars(matches("perc")), round, 1) %>%
#     mutate_at(vars(matches("perc")), formatC, format = "g") %>%
#     mutate(fmt_perc_change = 
#              paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
#     mutate(p_stars = stars.pval(p.value)) %>%
#     # mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
#     #                            p.value >= 0.001 & p.value < 0.05 ~ "*", 
#     #                            p.value >= 0.05 & p.value < 0.10 ~ "+", 
#     #                            p.value >= 0.10 ~ " ", 
#     #                            TRUE ~ "n.s.")) %>%
#     mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
#                             paste(round(p.value, 2)))) %>%
#     select(term, name,
#            fmt_perc_change,
#            p_stars, p_clean) %>%
#     mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#     mutate(fmt_perc_change = paste0(fmt_perc_change, " ", p_stars)) %>% 
#     arrange(term) %>%
#     pivot_wider(id_cols = term, 
#                 names_from = name, 
#                 values_from = fmt_perc_change)
# )
# 
# ###### Plot of demographic coefficients in adjusted models with different SES measures
# 
# #+ This produces a facet_wrap figure of percent change estimates for four primary
# #+ community demographics: the SES variable of interest, % Hispanic, % NH Black,
# #+ % urbanicity. The top header columns are the outcomes any det, any viol, 1,4-d, 
# #+ 1,1-DCE, HCFC-22, and PFAS. X-axis is percent change estimate; y-axis is the 
# #+ SES indicators. 
# 
# ggplot(suppl2_reg_results_det_any %>%
#          filter(model_run != "combined uniSES") %>%
#          filter(term %in% c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
#                             "perc_pov_ppl", "perc_uninsur", "perc_hmown")) %>%
#          mutate(term2 = ifelse(term %in% c("perc_pov_ppl", "perc_uninsur", "perc_hmown", "mdi_rate"), 
#                                "perc_ses", term)) %>%
#          mutate(term2 = factor(term2, 
#                                levels = c("perc_hisp_any", "perc_black_nohisp", "perc_urban", "perc_ses"), 
#                                labels = c("(a) Percent Hispanic", 
#                                           "(b) Percent non-Hispanic Black", 
#                                           "(c) Percent urban households", 
#                                           "(d) Percent SES variable"
#                                ))) %>%
#          mutate(model_run = factor(model_run, 
#                                    levels = c("mdi", "poverty", "insurance status", "homeownership"), 
#                                    labels = c("Percent deprived", "Percent poverty", "Percent uninsured", "Percent homeownership"))) %>%
#          mutate(perc_change = 100*(estimate - 1), 
#                 perc_change_LOW = 100*(conf.low - 1),
#                 perc_change_UPP = 100*(conf.high - 1)) %>%
#          mutate(stars = stars.pval(p.value)),
#        # mutate(stars = case_when(p.value < 0.001 ~ "**", 
#        #                          p.value >= 0.001 & p.value < 0.05 ~ "*", 
#        #                          p.value >= 0.05 & p.value < 0.10 ~ "+", 
#        #                          TRUE ~ "")), 
#        aes(x = model_run, y = perc_change)) +
#   geom_pointrange(aes(ymin = perc_change_LOW, ymax = perc_change_UPP)) + 
#   geom_text(aes(label = stars), nudge_y = 2) + 
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
#   geom_hline(yintercept = 0) + 
#   facet_wrap(~term2) + 
#   labs(x = "", y = "Percent change (95% CI)") +
#   theme_bw() +
#   theme(text = element_text(size = 15), 
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# ggsave(filename = paste0("outputs/", Sys.Date(), "- Different SES variables.pdf"),
#        height = 6, width = 6)

# 
# # colnames(dat_clean)
# 
# base_formula
# 
# noSources <- dat_clean %>%
#   
#   mutate(size = factor(size, levels = c("S", "L"))) %>% # temporary. delete this after factoring in r script source0.
#   
#   # Pivot outcomes
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
#   group_by(name) %>%
#   nest() %>%
#   
#   # Add base formula 
#   mutate(my_formula = paste("value", base_formula)) %>%
#   
#   # Add state intercept 
#   mutate(my_formula = paste(my_formula, "(1|state)"))
# noSources
# 
# #+ This takes approximately 3 minutes to run entirely.
# run_log2 <- function(dat, my_formula){
#   lme4::glmer(my_formula, data = dat, family = binomial) %>% 
#     broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# }
# 
# noSources_reg_results <- noSources %>%
#   filter(name != "viol_dca") %>% #viol_dca was producing an error - investigate separately.
#   mutate(n = map_dbl(data, ~sum(!is.na(.$value)))) %>%
#   mutate(model_results = map(data, 
#                              ~run_log2(dat = ., 
#                                        my_formula = my_formula))) %>%
#   unnest(model_results)
# 
# ## cleaning
# TableOrder_vec2 <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
#                      "size", "sizeL",
#                      "pws_type",  "pws_typeGW", "pws_typeMX", 
#                      "n_samples", "adj_wwtp_flow", "n_fac_any_bin",
#                      "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
#                      "src_epa_present_bin", "n_MFTA_airport_bin", 
#                      
#                      # for adjusted regressions:
#                      paste0(c("n_fac_any_bin",
#                               "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
#                               "src_epa_present_bin", "n_MFTA_airport_bin"), "1"),
#                      
#                      "perc_hmown", "perc_pov_ppl", "perc_uninsur")
# 
# noSources_reg_results_clean <- noSources_reg_results %>% 
#   filter(str_detect(term, "Intercept", negate = TRUE)) %>%
#   select(name, term, n, p.value, estimate, conf.low, conf.high) %>% 
#   mutate(perc_change = 100*(estimate - 1), 
#          perc_change_LOW = 100*(conf.low - 1),
#          perc_change_UPP = 100*(conf.high - 1)) %>%
#   mutate_at(vars(matches("perc")), round, 1) %>%
#   mutate_at(vars(matches("perc")), formatC, format = "g") %>%
#   mutate(fmt_perc_change = 
#            paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
#   mutate(p_stars = stars.pval(p.value)) %>%
#   # mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
#   #                            p.value >= 0.001 & p.value < 0.05 ~ "*", 
#   #                            p.value >= 0.05 & p.value < 0.10 ~ "+", 
#   #                            p.value >= 0.10 ~ " ", 
#   #                            TRUE ~ "n.s.")) %>%
#   mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
#                           paste(round(p.value, 2)))) %>%
#   select(name, term, n, 
#          #starts_with("perc_change"), 
#          fmt_perc_change, 
#          p_stars, p_clean) %>%
#   mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#   arrange(term)
# noSources_reg_results_clean
# 
# # create a tidier df to merge with dataframe adj_results_clean
# adj_results_clean
# noSources_reg_results_clean
# 
# # quick cleaning function 
# tidy_output <- function(dat){
#   dat %>% 
#     mutate(fmt_perc_change = paste0(fmt_perc_change, " ", p_stars)) %>%
#     distinct(name, term, fmt_perc_change)
# }
# 
# noSources_reg_results_export <- bind_rows(
#   adj_results_clean %>% tidy_output() %>% mutate(src_term = "Y"),
#   noSources_reg_results_clean %>% tidy_output() %>% mutate(src_term = "N")
# ) %>%
#   arrange(name, term) %>%
#   pivot_wider(id_cols = c(term, src_term), 
#               names_from = name, 
#               values_from = fmt_perc_change) %>%
#   select(term, src_term, det_any, viol_any, 
#          det_diox, det_dca, det_hcfc, det_pfas)
# 
# # write.csv(noSources_reg_results_export,
# #           paste0("outputs/", Sys.Date(), " - adj results with & without source terms.csv"))


# d_nested <- d %>% group_by(model_run) %>% nest()
# d_test<-d_nested$data[1] %>% as.data.frame()
# d_test %>% 
#   filter(str_detect(term, "Intercept", negate = TRUE)) %>%
#   select(term, p.value, estimate, conf.low, conf.high) %>% 
#   mutate(perc_change = 100*(estimate - 1), 
#          perc_change_LOW = 100*(conf.low - 1),
#          perc_change_UPP = 100*(conf.high - 1)) %>%
#   mutate_at(vars(matches("perc")), round, 1) %>%
#   mutate_at(vars(matches("perc")), formatC, format = "g") %>%
#   mutate(fmt_perc_change = 
#            paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
#   mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
#                              p.value >= 0.001 & p.value < 0.05 ~ "*", 
#                              p.value >= 0.05 & p.value < 0.10 ~ "+", 
#                              p.value >= 0.10 ~ " ", 
#                              TRUE ~ "n.s.")) %>%
#   mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
#                           paste(round(p.value, 2)))) %>%
#   select(term,  
#          #starts_with("perc_change"), 
#          fmt_perc_change, 
#          p_stars, p_clean) %>%
#   mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#   arrange(term) 
# 
# clean2 <- function(dat){
#   dat %>% 
#     filter(str_detect(term, "Intercept", negate = TRUE)) %>%
#     select(term,  p.value, estimate, conf.low, conf.high) %>% 
#     mutate(perc_change = 100*(estimate - 1), 
#            perc_change_LOW = 100*(conf.low - 1),
#            perc_change_UPP = 100*(conf.high - 1)) %>%
#     mutate_at(vars(matches("perc")), round, 1) %>%
#     mutate_at(vars(matches("perc")), formatC, format = "g") %>%
#     mutate(fmt_perc_change = 
#              paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
#     mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
#                                p.value >= 0.001 & p.value < 0.05 ~ "*", 
#                                p.value >= 0.05 & p.value < 0.10 ~ "+", 
#                                p.value >= 0.10 ~ " ", 
#                                TRUE ~ "n.s.")) %>%
#     mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
#                             paste(round(p.value, 2)))) %>%
#     # select(term,  
#     #        #starts_with("perc_change"), 
#     #        fmt_perc_change, 
#     #        p_stars, p_clean) %>%
#     mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#     arrange(term) 
#     
# }
# 
# d %>% filter(model_run == "mdi") %>% clean2()  
# d %>% filter(model_run == "poverty") %>% clean2()  
# d %>% filter(model_run == "insurance status") %>% clean2()  
# d %>% filter(model_run == "homeownership") %>% clean2()  
# d %>% filter(model_run == "combined uniSES") %>% clean2()
# 
# d_nested %>% pull(data)
# 
# %>%
#   select(term,  
#          #starts_with("perc_change"), 
#          fmt_perc_change, 
#          p_stars, p_clean)
# temp <- dat_clean %>%
#   
#   mutate(size = factor(size, levels = c("S", "L"))) %>% # temporary. delete this after factoring in r script source0.
#   
#   # Pivot outcomes
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
#   group_by(name) %>%
#   nest() %>%
#   
#   # Add base formula 
#   mutate(my_formula = paste("value", base_formula)) %>%
#   
#   mutate(my_formula = case_when(str_detect(name,"any") ~ paste(my_formula, "+ n_fac_any_bin"), 
#                                 TRUE ~ "9999")) %>%
#   {stopifnot(nrow(filter(., my_formula == "9999"))==0); .;} %>%
#   
#   # Add state intercept 
#   mutate(my_formula = paste(my_formula, " + (1|state)"))
# 
# 
# #+ first attempt led to an error because of the outcome viol_dca. 
# #+   viol_dca is not a meaningful outcome in these tables since only PWSID exceeded the DCA 
# #+   HRL.
# dat_clean %>% filter(viol_dca == 1) %>% pull(PWSID) # only one: IL2010300
# #+ Viol_DCA is not meaningful as an outcome. There was only one sample in 
# #+ one PWSID that exceeded the DCA health reference level.
# 
# #+ This takes approximately 3 minutes to run entirely.
# adjusted_results <- temp %>%
#   filter(name != "viol_dca") %>% #viol_dca was producing an error - investigate separately.
#   mutate(n = map_dbl(data, ~sum(!is.na(.$value)))) %>%
#   mutate(model_results = map(data, 
#                              ~run_log2(dat = ., 
#                                        my_formula = my_formula))) %>%
#   unnest(model_results)
# 
# ## cleaning
# TableOrder_vec2 <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
#                      "size", "sizeL",
#                      "pws_type",  "pws_typeGW", "pws_typeMX", 
#                      "n_samples", "adj_wwtp_flow", "n_fac_any_bin",
#                      "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
#                      "src_epa_present_bin", "n_MFTA_airport_bin", 
#                      
#                      # for adjusted regressions:
#                      paste0(c("n_fac_any_bin",
#                               "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
#                               "src_epa_present_bin", "n_MFTA_airport_bin"), "1"),
#                      
#                      "perc_hmown", "perc_pov_ppl", "perc_uninsur")
# 
# adj_results_clean <- adjusted_results %>% 
#   filter(str_detect(term, "Intercept", negate = TRUE)) %>%
#   select(name, term, n, p.value, estimate, conf.low, conf.high) %>% 
#   mutate(perc_change = 100*(estimate - 1), 
#          perc_change_LOW = 100*(conf.low - 1),
#          perc_change_UPP = 100*(conf.high - 1)) %>%
#   mutate_at(vars(matches("perc")), round, 1) %>%
#   mutate_at(vars(matches("perc")), formatC, format = "g") %>%
#   mutate(fmt_perc_change = 
#            paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
#   mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
#                              p.value >= 0.001 & p.value < 0.05 ~ "*", 
#                              p.value >= 0.05 & p.value < 0.10 ~ "+", 
#                              p.value >= 0.10 ~ " ", 
#                              TRUE ~ "n.s.")) %>%
#   mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
#                           paste(round(p.value, 2)))) %>%
#   select(name, term, n, 
#          #starts_with("perc_change"), 
#          fmt_perc_change, 
#          p_stars, p_clean) %>%
#   mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#   arrange(term)
