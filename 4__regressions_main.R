# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Conduct crude and adjusted regressions (Table 3 in paper)
# LATEST REVISION: 2024-11-12 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Beginning of regression modeling

# Start here (if not already run):
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

# This script produced the primary regressions reported in the paper (Table 3). 
# 
# == Crude models == 
# Tested associations between contamination and several variables among 4815 PWSs.
# 
# == Explanatory variables (sometimes called predictors) == 
# 15 predictors: Included county-level demographics, water system 
# characteristics, wastewater, and the presence of industrial and commercial
# sources of unregulated contaminants. 
#
# Demographic terms (X1-X4)
# X1 = % Hispanic 
# X2 = % non-Hispanic Black 
# X3 = % urban 
# X4 = % deprived 
# 
# System characteristics (X5-X8)
# X5 = system size (large or small, ref: small) 
# X6 = GW (ref: SW)
# X7 = MIX (ref: SW)
# X7 = Number of samples 
# 
# Point sources (X9-X15)      
# X9 = Wastewater flow (million L per km2) 
# X10 = relevant TRI facility present (yes or no, ref: no) 
# X11 = 1,4-dioxane TRI facility present (yes or no, ref: no)
# X12 = chlorinated solvent TRI facility present (yes or no, ref: no)
# X13 = chlorofluorocarbon TRI facility present (yes or no, ref: no)
# X14 = major industrial PFAS facility present (yes or no, ref: no)
# X15 = AFFF-certified airports of military fire-training areas present (yes or no, ref: no)

# == Outcome variables ==
# Six outcomes: detection of any target unregulated contaminant, 
# detection of 1,4-dioxane only, detection of 1,1-DCA only, detection of HCFC-22 only, 
# detection of PFAS only, and exceedance of any health-reference concentration for 
# 1,4-dioxane, 1,1-DCA, PFOA, and PFOS.
#
# Y1 = detected any target chem 
# Y2-Y5 = detection of either 1,4-d; HCFC-22; 1,1-DCA; PFAS 
# Y6 = exceeded a health reference level

# Explanatory and outcome variables were defined in series 1 scripts in the repo. 
# See scripts for more info. 

# == Adjusted models == 
# Logistic mixed-effects models
# Tested associations between contamination and a combination of variables among 4815 PWSs, 
# typically accounting for multiple effects combined.
# Each adjusted model included the same set of terms for demographics and system characteristics.
# Pollution source terms, by contrast, were outcome-dependent
# State random intercepts included to account for state clustering

# == Pollution terms == 
# For models of system detection of >=1 target contaminant and exceeding >=1 health benchmark, 
# terms for wastewater flow and the presence of any TRI facility were included only.
# For chemical-specific detections, use wastewater flow and a set of
# terms corresponding to each source (eg, 
# for 1,4-dioxane, we used TRI facilities that reported 1,4-dioxane emissions, and for 
# PFAS, we used the set of point sources associated with PFAS).

# == Sensitivity testing == 
# Tested the sensitivity of MDI by comparing with: 
# X16 = % homeownership
# X17 = % uninsured
# X18 = % poverty

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Crude logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# == Overview note == 
# This section divides the crude logistic models into a set of continuous 
# predictors and a set of categorical predictors. 

# Create a function that conducts crude logistic models
# Calculate odds ratios and 95% CI.
# 6/26/24 change-removed percent change format, so reported associations were in ORs. 
# This will be used to loop over a list-column of a nested data frame.

run_log1 <- function(dat){
  glm(outcome ~ predictor, data = dat, family = 'binomial') %>% 
    broom::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) # %>%
    # mutate(estimate_perc = 100*(estimate - 1), 
    #        estimate_perc_low = 100*(conf.low - 1), 
    #        estimate_perc_hi = 100*(conf.high - 1), 
    #        fmt_perc_change = paste0(format(round(estimate_perc, 1), nsmall = 1), 
    #                                 " (", format(round(estimate_perc_low, 1), nsmall = 1), 
    #                                 ", ", format(round(estimate_perc_hi, 1), nsmall = 1), 
    #                                 ")", sep = " ")) 
    # p_stars = gtools::stars.pval(p.value))
}

# Define continuous predictors
# Included other (non-MDI) SES variables here for sensitivity testing. 

continuous_pred <- c(
  "perc_hisp_any", 
  "perc_black_nohisp",
  "mdi_rate",
  "perc_urban",
  "n_samples",
  "adj_wwtp_flow", 
  "perc_hmown", 
  "perc_uninsur",
  "perc_pov_ppl"
)

# Prepare a nested df to iterate the custom regression function 
# over the list-column. 
# Pivot the outcome variables and predictor variables separately, then remove 
# unnecessary columns and nest. Count the number of systems in each data frame. 

continuous_nested_df4crude <- dat_clean %>%
  select(-viol_dca, -viol_diox, -viol_pfas) %>%
  pivot_longer(
    cols = c(starts_with("det_"), starts_with("viol_")),
    names_to = "name",
    values_to = "outcome"
  ) %>%
  pivot_longer(
    cols = all_of(continuous_pred),
    names_to = "pred",
    values_to = "predictor"
  ) %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~ sum(!is.na(.$outcome))))

# visual check:
continuous_nested_df4crude

# Check for any missing data
stopifnot(
  continuous_nested_df4crude %>%
  mutate(num_NA = map_dbl(data, ~sum(is.na(.$predictor)))) %>%
  pull(num_NA) %>%
  unique() == 0)
# good 

# * Run simple logistic model ----------------

# Run the function over the list-column. This may take a few minutes to run.
# Note: repeated warning messages appear for the number of samples. 
# Message: glm.fit: fitted probabilities numerically 0 or 1 occurred.
# This suggested complete separation of cases.

crude_results_cont <- continuous_nested_df4crude %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)

# checking for consistency with running the model by hand
# glm(det_any ~ n_samples, data=dat_clean, family='binomial')
# glm(det_any ~ perc_hisp_any, data=dat_clean, family='binomial')
# glm(det_any ~ adj_wwtp_flow, data=dat_clean, family='binomial')

# * Categorical predictors ----

# Repeat the same process as above, but for categorical predictors. 

categorical_predictors <- c("pws_type", 
                            "size", 
                            "n_fac_any_bin", 
                            "n_fac_diox_bin",
                            "n_fac_chlor_solv_bin", 
                            "n_fac_cfc_bin", 
                            "src_epa_present_bin", 
                            "n_MFTA_airport_bin")

# check that all these predictors are coded as categorical variables
# dat_clean %>% select(all_of(categorical_predictors)) %>% str()

categorical_nested_df4crude <- dat_clean %>%
  select(-viol_dca, -viol_diox, -viol_pfas) %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>%
  pivot_longer(
    cols = c(starts_with("det_"), starts_with("viol_")),
    names_to = "name",
    values_to = "outcome"
  ) %>%
  pivot_longer(
    cols = all_of(categorical_predictors),
    names_to = "pred",
    values_to = "predictor"
  ) %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~ sum(!is.na(.$outcome))))

# visual check:
# categorical_nested_df4crude

# * Run simple logistic model ----------------

# Run the function over the list-column. This may take a few minutes to run.
# Note: repeated warning messages appear for models using system size as the predictor and 
# the outcome was detection of 1,1-DCA. 
# Message: glm.fit: fitted probabilities numerically 0 or 1 occurred.
# This suggested complete separation of cases. This is true because only large
# water systems detected 1,1-DCA. Excluded these results in the final table.

crude_results_cat <- categorical_nested_df4crude %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)

# visual check:
crude_results_cat

# * Combine and clean outputs --------------------------------------------------

# Clean the outputs of the model runs from above. Define a function to 
# clean model outputs. 

clean2 <- function(dat){
  dat %>% 
    filter(!str_detect(term, "Intercept")) %>%
    mutate(estimate = format(round(estimate, 2), nsmall = 2), 
           conf.low = format(round(conf.low, 2), nsmall = 2),
           conf.high = format(round(conf.high, 2), nsmall = 2), 
           estimate_edit = paste0(estimate, " (", conf.low, ", ", conf.high, ")"), 
           p_star = gtools::stars.pval(p.value), 
           p_format = 
           format.pval(p.value, eps = 0.001, nsmall = 2, digits = 2)
    )
}

# Apply cleaning function and combine. The resulting data frame 
# called "crude_results_all" removed the intercept values, and reported 
# coefficients (and 95% CIs), standard errors, p-values, and the original 
# names of variables. More formatting was needed to prepare for table for export.

crcont <- clean2(crude_results_cont) %>% select(-data)
crcat <- clean2(crude_results_cat) %>% select(-data)
crude_results_all <- bind_rows(crcat, crcont) 
crude_results_all

# Tidy the outputs by ordering the predictors according to how it appears 
# in the paper. 

TableOrder <-  c("perc_hisp_any",
                 "perc_black_nohisp",
                 "mdi_rate",
                 "perc_urban",
                 "size",
                 "pws_type",
                 "n_samples",
                 "adj_wwtp_flow",
                 "n_fac_any_bin",
                 "n_fac_diox_bin",
                 "n_fac_chlor_solv_bin",
                 "n_fac_cfc_bin",
                 "src_epa_present_bin",
                 "n_MFTA_airport_bin",
                 "perc_hmown",
                 "perc_pov_ppl",
                 "perc_uninsur"
)

crude_results_tidy <- crude_results_all %>%
  mutate(pred = factor(pred, levels = TableOrder)) %>%
  arrange(pred)
crude_results_tidy

# Prepare to export. Will need to sort columns manually after exporting.
# Names are ok and identifiable as is.

crude_results_export <- crude_results_tidy %>%
  pivot_wider(id_cols = c(pred, term), 
              names_from = c(name),
              values_from = c(estimate_edit, 
                              p_format, 
                              p_star),
              names_glue = "{name}_{.value}") %>%
  select(pred, 
         term, 
         starts_with("det_any"), 
         starts_with("viol_any"), 
         starts_with("det_diox"), 
         starts_with("det_dca"), 
         starts_with("det_hcfc"), 
         starts_with("det_pfas")
         )

# Visual inspection
crude_results_export

### SAVE OUTPUTS HERE:
# write.csv(crude_results_export,
#           paste0("results/Table 3. Crude Results_",
#                  Sys.Date(), ".csv"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Adjusted mixed-effects logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# == Overview note == 
# First, we defined specific regression equations for each outcome. Later we 
# used the lme4 and broom.mixed packages to create a function that does
# the regression modeling based on the defined equations. Similar to the 
# section above, it then applies the custom function over a nested df and 
# pulls the ORs and 95% CIs results and combines these results
# into an export-ready table.

# Prepare a nested data frame. Defined small systems as reference for system size. 
# Pivot the outcome variable columns only, then nest.

nested_df4adj <- dat_clean %>%
  select(-viol_dca, -viol_diox, -viol_pfas) %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>% 
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
               names_to = "outcome_name", 
               values_to = "outcome_value") %>%
  group_by(outcome_name) %>%
  nest() 

# check
stopifnot(nrow(nested_df4adj)==6)

# Define a base formula.
# Include terms for county-level demographic variables, water system characteristics, and wastewater flow. 
# Exclude point source terms (except for wastewater flow). 
# The base formula was adjusted for each model according to the outcome. 
# Include a state intercept term at the end of the equation.

base_formula <- paste(
  "~ perc_hisp_any + perc_black_nohisp + mdi_rate +",
  "perc_urban + size + pws_type + n_samples + adj_wwtp_flow",
  collapse = "")

nested_df4adj2 <- nested_df4adj %>%
  mutate(my_formula = paste("outcome_value", base_formula)) %>%
  mutate(my_formula = case_when(
    str_detect(outcome_name, "any")  ~ paste(my_formula, "+ n_fac_any_bin"), 
    str_detect(outcome_name, "diox") ~ paste(my_formula, "+ n_fac_diox_bin"), 
    str_detect(outcome_name, "dca")  ~ paste(my_formula, "+ n_fac_chlor_solv_bin"), 
    str_detect(outcome_name, "hcfc") ~ paste(my_formula, "+ n_fac_cfc_bin"), 
    str_detect(outcome_name, "pfas") ~ paste(my_formula, "+  n_MFTA_airport_bin + src_epa_present_bin"), 
    TRUE ~ "oops")) %>%
  {stopifnot(nrow(filter(., my_formula == "oops"))==0); .;} %>%
  mutate(my_formula = paste(my_formula, " + (1|state)")) 

# Visual check that the formulas make sense with the outcomes and no oops
# nested_df4adj2 %>%
#   distinct(outcome_name, my_formula) #%>%
#   #view()

# * Function and run adjusted model -------------------------------------------

# Required: lme4 and broom.mixed packages.

run_log2 <- function(dat, formula){
  lme4::glmer(formula = formula,
              data = dat, 
              family = binomial) %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

# Apply the function "run_log2()" over the list-column in the nested 
# data frame. This may take a while to run! (Estimated: 2 mins). 
# Note that warning messages appear from 1,1-DCA model because system size 
# was included as a variable
# PFAS and HCFC-22 models failed to converge 

adjusted_results <- nested_df4adj2 %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$outcome_value)))) %>%
  mutate(model_results = 
           map(data,
               ~run_log2(dat = ., formula = my_formula))) %>%
  unnest(model_results) 

# Visual inspection 
# adjusted_results %>%
#   select(outcome_name, term, n, p.value, estimate, conf.low, conf.high) #%>%
#   #view()

# * Clean outputs --------------------------------------------------

# Remove intercepts. Clean ORs and 95%CI.

adjusted_results2 <- adjusted_results %>% 
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

# Tidy outputs by ordering the predictors according to how it appeared 
# in the paper. 

TableOrder2 <- 
  c("perc_hisp_any", 
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
    
    # unique names used as terms in adjusted regressions:
    "n_fac_any_bin1",
    "n_fac_diox_bin1",
    "n_fac_chlor_solv_bin1",
    "n_fac_cfc_bin1",
    "src_epa_present_bin1",
    "n_MFTA_airport_bin1",
    
    # move these terms to the end of the list (used for sensitivity check)
    "perc_hmown", 
    "perc_pov_ppl", 
    "perc_uninsur")

adjusted_results3 <- adjusted_results2 %>%
  select(outcome_name, term, n, estimate_edit, p_format, p_star) %>%
  mutate(term = factor(term, levels = TableOrder2)) %>%
  arrange(term)

adjusted_results_export <- adjusted_results3 %>%
  pivot_wider(
    id_cols =  term,
    names_from = outcome_name,
    values_from = c(estimate_edit, p_format, p_star),
    names_glue = "{outcome_name}_{.value}"
  ) %>%
  select(
    term,
    starts_with("det_any"),
    starts_with("viol_any"),
    starts_with("det_diox"),
    starts_with("det_dca"),
    starts_with("det_hcfc"),
    starts_with("det_pfas")
  )

# Save progress. 

# write.csv(adjusted_results_export,
#           paste0("results/Table 4. Adjusted Logistic Results_",
#                  Sys.Date(),
#                  ".csv")
#           )

## ARCHIVE ----------------------------------------


## playing with flextable

# library(ftExtra)
# library(flextable)
# crude_results_export %>%
#   flextable() %>%
#   separate_header(split = "_") %>%
#   theme_vanilla()

# rabbit hole - beware:
# crude_results_export %>%
#   select(pred, ends_with('star'), ends_with('estimate')) %>%
#   pivot_longer(ends_with("star")) %>%
#   pivot_longer(ends_with('estimate'), names_to = "remove", values_to = "value2") %>%
#   select(-remove) %>%
#   ggplot(aes(x = pred, y = name, fill = value)) +
#   geom_tile(color = 'black') +
#   geom_text(aes(label = value2), size = 5) + 
#   scale_fill_brewer(type = "seq", palette = "BrGn")
#
# # generic column cleaning name function
# 
# custom_trimws <- function(column){
#   column <- gsub("\\s*,\\s*", ", ", column)
#   column <- gsub("(?<=\\()\\s+|\\s+(?=\\))", "", column, perl = TRUE)
#   column <- gsub("\\s+\\(", " (", column)
#   #gsub("[[:space:]](?=[^()]*\\))", "", column, perl = TRUE)
# }
# 
## Archived code:
# adj_results <- adjusted_results %>% select(name, term, n, fmt_perc_change, p.value)
# adj_results
# 
# adj_results <- adj_results  %>%
#   # add stars
#   mutate(p_stars = ) 
# 
# adj_results_clean <- adj_results %>%
#   mutate(fmt_perc_change = custom_trimws(fmt_perc_change)) %>%
#   mutate(fmt_perc_change = str_replace(fmt_perc_change, c("NA|Inf"), as.character(NA))) %>%
#   mutate(p.value = ifelse(p.value < 0.001, "<0.001", paste(round(p.value, 3)))) %>%
#   mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#   arrange(term)
# adj_results_clean
## Format output as a result table 
# Pivot wider
# adjusted_results2 %>%
#   flextable() %>%
#   theme_vanilla()
# 
# dat_clean %>%
#   select(PWSID, perc_black_nohisp, det_any, det_dca) %>%
#   filter(is.na(det_dca))
# 
# mean(dat_clean$perc_black_nohisp)
#+ possible explanation for why %Black is associated with det DCA, but not det any. 
#+ 8 PWSs did not sample for DCA, and most were serving counties with higher
#+ average %Black.

# unique(ucmr3_raw$Contaminant)
# ucmr3_raw %>%
#   filter(PWSID %in% (dat_clean %>%
#            select(PWSID, det_any, det_dca) %>%
#            filter(is.na(det_dca)) %>% 
#            pull(PWSID) )) %>%
#   filter(Contaminant == "1,1-dichloroethane")
#   count(det_any, det_dca)

