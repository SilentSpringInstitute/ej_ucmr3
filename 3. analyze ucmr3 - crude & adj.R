library(gtools) # for stars.pval
library(lme4) # for glmer() (mixed effects model)

# If starting from here, run below:
# source("2. create main datasets.R")

# generic column cleaning name function

custom_trimws <- function(column){
  column <- gsub("\\s*,\\s*", ", ", column)
  column <- gsub("(?<=\\()\\s+|\\s+(?=\\))", "", column, perl = TRUE)
  column <- gsub("\\s+\\(", " (", column)
  #gsub("[[:space:]](?=[^()]*\\))", "", column, perl = TRUE)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 3. Crude logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data source: processed UCMR 3 data 
#   dat_clean 
# 
# Six outcomes: 
# Y1 = detected any target chem 
# Y2 ... Y5 = detection of either 1,4-d; HCFC-22; 1,1-DCA; PFAS (mutually exclusive)
# Y6 = exceeded a health reference level (guidance value)
# 
# Explanatory variables (predictors):
# X1 = % Hispanic 
# X2 = % non-Hispanic Black 
# X3 = % urban 
# X4 = % deprived (MDI rate) 
# X4 = system size (large or small, ref: small) 
# X5 = source water (GW, MIX, or SW, ref: SW)
# X6 = Number of samples 
# X7 = Wastewater effluent flow (million L per km2) 
# X8 = Any TRI facility (yes or no, ref: no) 
# X9...X13 = Any relevant pollution source (3 types of TRI facility, MFTA, airport)

# Crude logistic model function
# 6/26/24 - remove percent change formatting
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
  #p_stars = gtools::stars.pval(p.value))
}

## Continuous predictors
predictor_columns <- c(
  "perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban",
  "n_samples", "adj_wwtp_flow", "perc_hmown", "perc_uninsur",
  "perc_pov_ppl"
)

## Prepare a nested dataframe to iterate a regression function over it. 
## Focus on continuous predictors only. 
## Pivot the outcome variables and predictor variables separately, then remove 
## unnecessary columns and nest. 
## Count the number of systems in each dataframe. Is sample total in regression.

crude_nested_df <- dat_clean %>%
  pivot_longer(
    cols = c(starts_with("det_"), starts_with("viol_")),
    names_to = "name",
    values_to = "outcome"
  ) %>%
  pivot_longer(
    cols = all_of(predictor_columns),
    names_to = "pred",
    values_to = "predictor"
  ) %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~ sum(!is.na(.$outcome))))

# visual check:
crude_nested_df

# These lines run the simple regression models over the data listed in column "data."
# This may take a few minutes to run!
crude_results_cont <- crude_nested_df %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)

# visual check:
crude_results_cont

# colnames(dat_clean)

## Repeat the process for categorical variables. 
## Pivot the outcome variables and predictor variables separately, remove 
## unnecessary columns, nest, then count the number of systems in each dataframe. 
## The number is the sample used for regression.

predictor_columns <- c("pws_type", "size", "n_fac_any_bin", 
                       "n_fac_diox_bin", "n_fac_chlor_solv_bin", 
                       "n_fac_cfc_bin", "src_epa_present_bin", 
                       "n_MFTA_airport_bin")

crude_nested_df <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>%
  pivot_longer(
    cols = c(starts_with("det_"), starts_with("viol_")),
    names_to = "name",
    values_to = "outcome"
  ) %>%
  pivot_longer(
    cols = all_of(predictor_columns),
    names_to = "pred",
    values_to = "predictor"
  ) %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~ sum(!is.na(.$outcome))))

# visual check:
crude_nested_df

# These lines run the simple regression models over the data listed in column "data."
# This may take a few minutes to run!

crude_results_cat <- crude_nested_df %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)

# visual check:
crude_results_cat

## Clean the outputs of the results, then merge together. Add p-value stars.

# clean2() function is simple (should be loaded)
clean2 <- function(dat){
  dat %>%
    filter(term != "(Intercept)") %>%
    select(name, pred, n, term, estimate, p.value)
}

# Order the explantory variables (predictors):
TableOrder_vec <-
  c(
    "perc_hisp_any",
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

# Clean and merge
crcat <- clean2(crude_results_cat) 
crcont <- clean2(crude_results_cont)
crude_results <- bind_rows(crcat, crcont) 

# Visual inspection
crude_results

# Tidy formatting
crude_results_tidy <- crude_results %>%
  mutate(p_stars = stars.pval(p.value)) %>% 
  mutate(p.value = ifelse(
    p.value < 0.001, 
    "<0.001", 
    paste(round(p.value, 2)
          )
    )
  ) %>%
  mutate(pred = factor(pred, levels = TableOrder_vec)) %>%
  mutate(estimate = round(estimate, 2)) %>%
  # mutate(
  #   fmt_perc_change = custom_trimws(fmt_perc_change),
  #   fmt_perc_change = str_replace(fmt_perc_change, "NA|Inf|\\+", as.character(NA))
  # ) %>%
  rename(estimate = estimate, p.value = p.value, star = p_stars) %>%
  arrange(pred)

# Visual inspection
crude_results_tidy

# Prepare to export
# Order the columns manually
# Only 1 PWS exceeded the DCA HRL, so it's not a meaningful outcome.

crude_results_export <- crude_results_tidy %>%
  pivot_wider(id_cols = c(pred, term), 
              names_from = c(name),
              values_from = c(estimate, p.value, star),
              names_glue = "{name}_{.value}") %>%
  select(pred, term, 
         starts_with("det_any"), 
         starts_with("viol_any"), 
         starts_with("det_diox"), 
         starts_with("det_dca"), 
         starts_with("det_hcfc"), 
         starts_with("det_pfas"), 
         starts_with("viol_diox"), 
         #starts_with("viol_dca"),  
         starts_with("viol_pfas"))

# Visual inspection
crude_results_export

### SAVE OUTPUTS HERE:
# write.csv(crude_results_export, paste0("results/Table 3. Crude Results_", Sys.Date(), ".csv"))

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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 4. Adjusted logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data source: processed UCMR 3 data 
#   dat_clean 
# 
# Six outcomes: 
# Y1 = detected any target chem 
# Y2 ... Y5 = detection of either 1,4-d; HCFC-22; 1,1-DCA; PFAS (mutually exclusive)
# Y6 = exceeded a health reference level (guidance value)
# 
# Explanatory variables (predictors):
# X1 = % Hispanic 
# X2 = % non-Hispanic Black 
# X3 = % urban 
# X4 = % deprived (SES) 
# X4 = system size (large or small, ref: small) 
# X5 = source water (GW, MIX, or SW, ref: SW)
# X6 = Number of samples 
# X7 = Wastewater effluent flow (million L per km2) 
# X8 = Any TRI facility (yes or no, ref: no) 
# X9...X13 = Any relevant pollution source (3 types of TRI facility, MFTA, airport)

## Prepare a nested dataframe to run over with the regression models.
# Pivot the OUTCOME variables ONLY, then nest.

nested_data_for_adjreg <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>% 
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
  group_by(name) %>%
  nest() 

# Define a base formula (main equation, which includes sociodemographic variables,
# system characteristics, and wastewater, but excludes contaminant-specific sources).
# Adjust the base formula to include contaminant-specific sources, which
# vary by outcome (e.g., 1,4-dioxane detect = [base formula] + any 1-4d facility).
# Remove system size as a variable in the 1,1-DCA regression 
# Add a state intercept term at the end of the equation.

base_formula <- paste(
  "~ perc_hisp_any + perc_black_nohisp + mdi_rate +",
  "perc_urban + size + pws_type + n_samples + adj_wwtp_flow",
  collapse = " "
)

nested_data_add_form <- nested_data_for_adjreg %>%
  mutate(my_formula = paste("value", base_formula)) %>%
  mutate(my_formula = case_when(str_detect(name,"any") ~ paste(my_formula, "+ n_fac_any_bin"), 
                                str_detect(name, "diox") ~ paste(my_formula, "+ n_fac_diox_bin"), 
                                str_detect(name, "dca") ~ paste(my_formula, "+ n_fac_chlor_solv_bin"), 
                                str_detect(name, "hcfc") ~ paste(my_formula, "+ n_fac_cfc_bin"), 
                                str_detect(name, "pfas") ~ paste(my_formula, "+  n_MFTA_airport_bin + src_epa_present_bin"), 
                                TRUE ~ "9999")) %>%
  {stopifnot(nrow(filter(., my_formula == "9999"))==0); .;} %>%
  mutate(my_formula = paste(my_formula, " + (1|state)")) %>%
  mutate(my_formula = if_else(
    name == "det_dca", 
    str_remove(my_formula, "size \\+ "), 
    my_formula
  ))
  
## Filter the nested data to include the outcomes of interest only. 
# Note: Only 1 PWS (PWSID: IL2010300) had a sample of 1,1-DCA with conc > health guidance value. 

nested_data_ready <- nested_data_add_form %>% 
  filter(name %in% c("det_any", "viol_any", "det_diox", "det_dca", "det_hcfc", "det_pfas"))

# Visual check that the formulas make sense with the outcomes
nested_data_ready %>%
  distinct(name, my_formula)

## Apply the multiple logistic mixed effect function over the nested data. 

# Mixed effects model for adjusted regression 
# uses lme4 package and glmer() function 
# Fit a generalized linear mixed-effects model (GLMM).
# Both fixed effects and random effects are specified via the model formula.
# uses broom.mixed package and tidy() function to clean outputs

run_log2 <- function(dat, formula){
  lme4::glmer(formula = formula,
              data = dat, 
              family = binomial) %>% 
    broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

# This may take a while to run! (Estimated: 2 mins)

adjusted_results <- nested_data_ready %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$value)))) %>%
  mutate(model_results = 
           map(data,
               ~run_log2(dat = ., 
                         formula = my_formula))) %>%
  unnest(model_results) 

# Visual inspection 
adjusted_results %>%
  select(name, term, n, p.value, estimate, conf.low, conf.high) #%>%
  # view()

## Clean the outputs of the results, then merge together. Add p-value stars.

# Order the explantory variables (predictors):
TableOrder_vec2 <- 
  c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
    "size", "sizeL",
    "pws_type",  "pws_typeGW", "pws_typeMX", 
    "n_samples", "adj_wwtp_flow", "n_fac_any_bin",
    "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
    "src_epa_present_bin", "n_MFTA_airport_bin", 
    
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

# colnames(adjusted_results)

adj_res_clean_estimates <- adjusted_results %>% 
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

# Order the table, almost ready to export 

adj_res_clean_tidy <- adj_res_clean_estimates %>%
  select(name, term, n, estimate_edit, p_format, p_star) %>%
  mutate(term = factor(term, levels = TableOrder_vec2)) %>%
  arrange(term)

adjusted_results_export <- adj_res_clean_tidy %>%
  pivot_wider(
    id_cols =  term,
    names_from = name,
    values_from = c(estimate_edit, p_format, p_star),
    names_glue = "{name}_{.value}"
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

