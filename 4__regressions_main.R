# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Conduct crude and adjusted regressions (Table 3 in paper)
# LATEST REVISION: 2024-11-12 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# for regression:
library(lme4)             # glmer() 
# help(lme4)

# for cleaning outputs of regression model runs:
library(gtools)           # stars.pval()
library(broom.mixed) 

# Start here:
source("1_combine_process.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script produced the primary regressions reported in the paper (Table 3). 
# Crude logistic models were used to evaluate associations between 
# contaminant detects among US public water systems in the UCMR3 and 
# county-level demographics, water system characteristics, and 
# the presence of industrial sources of unregulated contaminants. There were
# five primary outcomes of interest: detection of any target unregulated contaminant, 
# detection of 1,4-dioxane only, detection of 1,1-DCA only, detection of HCFC-22 only, 
# detection of PFAS only, and exceedance of any health-reference concentration for 
# 1,4-dioxane, 1,1-DCA, PFOA, and PFOS. Outcomes and explanatory variables were pre-defined 
# and processed in series 1 scripts in the repo. See those scripts for additional info. 
# 
# Adjusted models were logistic mixed-effects models using the lme4 package. 
# Adjusted models included the same set of demographics and system characteristics 
# into a single model for each outcome of interest. Pollution source terms, by contrast, 
# were outcome-dependent. For models of detection of >=1 target contaminant and of 
# exceeding >=1 health benchmark, we used wastewater flow and the presence of any 
# TRI facility as point source terms. For models of chemical-specific detections, 
# we used wastewater flow and a set of terms corresponding to each source (for example, 
# for 1,4-dioxane, we used TRI facilities that reported 1,4-dioxane emissions, and for 
# PFAS, we used the set of point sources reported in Hu et al. 2016).
#
# A state-specific random effect was added to each equation to account for some
# clustering of demographics by state and to account for
# the systems within the same state being more similar to each other than to 
# systems in other states. 
# 
# Six outcomes -- 
#
# Y1 = detected any target chem 
# Y2-Y5 = detection of either 1,4-d; HCFC-22; 1,1-DCA; PFAS 
# Y6 = exceeded a health reference level
# 
# Explanatory variables (predictors) --
# 
# Demographic terms (X1-X4)
# X1 = % Hispanic 
# X2 = % non-Hispanic Black 
# X3 = % urban 
# X4 = % deprived 
# 
# System characteristics (X5-X7)
# X5 = system size (large or small, ref: small) 
# X6 = source water (GW, MIX, or SW, ref: SW)
# X7 = Number of samples 
# 
# Point sources* (X8-X14*)      *model specific
# X8 = Wastewater effluent flow (million L per km2) 
# X9 = Any TRI facility (yes or no, ref: no) 
# X10-14 = Any relevant pollution sources (3 types of TRI facility, MFTA, airport)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Crude logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Create a function that conducts simple logistic models. Use tidy() to save 
# model results as a tidy data frame object. Calculate odds ratios and 95% CI.
# This will be used to loop over a list-column of a nested data frame.
# As of 6/26/24, we removed percent change formatting. 

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

# * Continuous predictors ----

# Define continuous predictors. Included SES variables used for sensitivity 
# checks (ie percent homeownership, percent uninsured, and percent poverty). 

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

# Prepare a nested data frame to iterate a regression function over it. 
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
  select(PWSID, outcome_name, pred, outcome, predictor) %>%
  group_by(outcome_name, pred) %>%
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

# glm(det_any ~ n_samples, data=dat_clean, family='binomial')

# visual check:
# crude_results_cont

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

# check
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
  select(PWSID, outcome_name, pred, outcome, predictor) %>%
  group_by(outcome_name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~ sum(!is.na(.$outcome))))

# visual check:
categorical_nested_df4crude

# * Run simple logistic model ----------------

# Run the function over the list-column. This may take a few minutes to run.
# Note: repeated warning messages appear for models using system size as the predictor and 
# the outcome was detection of 1,1-DCA. 
# Message: glm.fit: fitted probabilities numerically 0 or 1 occurred.
# This suggested complete separation of cases. This is true because only large
# water systems detected 1,1-DCA.

crude_results_cat <- categorical_nested_df4crude %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)

# visual check:
crude_results_cat

# * Combine and clean outputs --------------------------------------------------

# Clean the outputs of the model runs from above. Define a function to 
# clean model outputs. Then merge together. The resulting data frame object 
# called "crude_results_all" removes the intercept values, and reports 
# the coefficient (and 95% CIs), standard error, p-value, and the original 
# names of variables. More formatting was needed to prepare for table for export.

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

# Apply function and combine. 

crcont <- clean2(crude_results_cont) %>% select(-data)
crcat <- clean2(crude_results_cat) %>% select(-data)
crude_results_all <- bind_rows(crcat, crcont) 
crude_results_all

# Tidy the outputs by ordering the predictors according to how it was shown 
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

# Prepare to export. Will need to sort the columns manually after exporting (OK as is).
# Names are also ok as is.

crude_results_export <- crude_results_tidy %>%
  pivot_wider(id_cols = c(pred, term), 
              names_from = c(outcome_name),
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
         starts_with("det_pfas"), 
         # starts_with("viol_diox"), 
         # starts_with("viol_dca"),  
         # starts_with("viol_pfas")
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

# Prepare a nested data frame. Pivot the outcome variable columns only, then 
# nest.

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
# 
# The base formula is adjusted for each model based on the outcome being modeled. 
# Adjust the base formula to include contaminant-specific sources, which
# vary by outcome (e.g., 1,4-dioxane detect = [base formula] + any 1-4d facility).
# Remove system size as a variable in the 1,1-DCA regression.
# 
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

# Visual check that the formulas make sense with the outcomes
nested_df4adj2 %>%
  distinct(outcome_name, my_formula) #%>%
  #view()

# * Function and run adjusted model -------------------------------------------

# Create a function that conducts adjusted logistic mixed-effects models. 
# Use tidy() to save from the broom.mixed package to clean model results as a 
# tidy data frame object. Calculate odds ratios and 95% CI.
# This will be used to loop over a list-column of a nested data frame.
# As of 6/26/24, we removed percent change formatting. 

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
# was included as a variable. The models for PFAS and HCFC-22 failed to converge. 

adjusted_results <- nested_df4adj2 %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$outcome_value)))) %>%
  mutate(model_results = 
           map(data,
               ~run_log2(dat = ., formula = my_formula))) %>%
  unnest(model_results) 

# Visual inspection 
adjusted_results %>%
  select(outcome_name, term, n, p.value, estimate, conf.low, conf.high) #%>%
  #view()

# * Combine and clean outputs --------------------------------------------------

# Clean the outputs of the results, then merge together. Add p-value stars.

# Order the explantory variables (predictors):
TableOrder_vec2 <- 
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
    
    # for adjusted regressions:
    "n_fac_any_bin1",
    "n_fac_diox_bin1",
    "n_fac_chlor_solv_bin1",
    "n_fac_cfc_bin1",
    "src_epa_present_bin1",
    "n_MFTA_airport_bin1",
    
    # for supplementary regressions:
    "perc_hmown", "perc_pov_ppl", "perc_uninsur")

# Clean coefficients and upper and lower bound results 
# Had challenges rounding the p-values, will do in Excel

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

# Order the table, almost ready to export 

adjusted_results3 <- adjusted_results2 %>%
  select(outcome_name, term, n, estimate_edit, p_format, p_star) %>%
  mutate(term = factor(term, levels = TableOrder_vec2)) %>%
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

# SAVE HERE: 
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

