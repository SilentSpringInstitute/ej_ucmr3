library(gtools) # for stars.pval

# If starting from here, run below:
# source("2. create main datasets.R")

## functions

custom_crude_model <- function(dat, predictor_columns){
  
  results <- list()
  
  for (col_name in predictor_columns){
    formula_obj <- as.formula(paste("test ~ ", col_name))
    
    # Logistic 
    model <- glm(formula_obj, data = dat, family = 'binomial')
    
    # store
    
    results[[col_name]] <- summary(model)
  }
  
  return(results)
}

## crude regression function
run_log1 <- function(dat){
  glm(outcome ~ predictor, data = dat, family = 'binomial') %>% 
    broom::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(estimate_perc = 100*(estimate - 1), 
           estimate_perc_low = 100*(conf.low - 1), 
           estimate_perc_hi = 100*(conf.high - 1), 
           fmt_perc_change = paste0(format(round(estimate_perc, 1), nsmall = 1), 
                                    " (", format(round(estimate_perc_low, 1), nsmall = 1), 
                                    ", ", format(round(estimate_perc_hi, 1), nsmall = 1), 
                                    ")", sep = " ")) 
  #p_stars = gtools::stars.pval(p.value))
}

## state mixed effects model for adjusted regression
run_log2 <- function(dat, my_formula){
  lme4::glmer(my_formula, data = dat, family = binomial) %>% 
    broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

clean2 <- function(dat){
  dat %>%
    filter(term != "(Intercept)") %>%
    select(name, pred, n, term, fmt_perc_change, p.value)
}

custom_trimws <- function(column){
  column <- gsub("\\s*,\\s*", ", ", column)
  column <- gsub("(?<=\\()\\s+|\\s+(?=\\))", "", column, perl = TRUE)
  column <- gsub("\\s+\\(", " (", column)
  #gsub("[[:space:]](?=[^()]*\\))", "", column, perl = TRUE)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 3. Crude logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

temp <- dat_clean %>%
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")))



#+ This method makes tinier versions of the df in a nested df across 
#+ each outcome and predictor category of interest

colnames(dat_clean)

predictor_columns <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
                       "n_samples", "adj_wwtp_flow", "perc_hmown", "perc_uninsur", 
                       "perc_pov_ppl")

crude_nested_df <- dat_clean %>%
  # Pivot outcomes
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
               names_to = "name", values_to = "outcome") %>%
  # Pivot predictors
  pivot_longer(cols = all_of(predictor_columns), names_to = "pred", values_to = "predictor") %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$outcome))))
crude_nested_df

## test
# temp <- crude_nested_df$data[[1]]
# run_log1(temp)


crude_results_cont <- crude_nested_df %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)
crude_results_cont

# colnames(dat_clean)

predictor_columns <- c("pws_type", "size", "n_fac_any_bin", 
                       "n_fac_diox_bin", "n_fac_chlor_solv_bin", 
                       "n_fac_cfc_bin", "src_epa_present_bin", 
                       "n_MFTA_airport_bin")

crude_nested_df <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L"))) %>% # temporary
  # Pivot outcomes
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
               names_to = "name", values_to = "outcome") %>%
  # Pivot predictors
  pivot_longer(cols = all_of(predictor_columns), names_to = "pred", values_to = "predictor") %>%
  select(PWSID, name, pred, outcome, predictor) %>%
  group_by(name, pred) %>%
  nest() %>%
  mutate(n = map_dbl(data, ~sum(!is.na(.$outcome))))
crude_nested_df

crude_results_cat <- crude_nested_df %>%
  mutate(model = map(data, run_log1)) %>%
  unnest(model)
crude_results_cat

100*(1.043 - 1)
(c(515.6, 197, 1468)/100)+1
(c(73.6, 35.6, 122.2)/100)+1
(c(926.5, 450, 1731)/100)+1
(c(73.6, 35.6, 122.2)/100)+1
(c(3981, 1839, 10401)/100)+1


crcat <- clean2(crude_results_cat) 
crcont <- clean2(crude_results_cont)

crude_results <- bind_rows(crcat, crcont) %>%
  # add stars
  mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
                             p.value >= 0.001 & p.value < 0.05 ~ "*", 
                             p.value >= 0.05 & p.value < 0.10 ~ "+", 
                             p.value >= 0.10 ~ " ", 
                             TRUE ~ "n.s.")) 

# cleaning 

TableOrder_vec <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
                    "size", "pws_type", "n_samples", "adj_wwtp_flow", "n_fac_any_bin",
                    "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
                    "src_epa_present_bin", "n_MFTA_airport_bin", 
                    "perc_hmown", "perc_pov_ppl", "perc_uninsur")

crude_results

crude_results_clean <- crude_results %>%
  mutate(p.value = ifelse(p.value < 0.001, "<0.001", paste(round(p.value, 2)))) %>%
  mutate(fmt_perc_change = custom_trimws(fmt_perc_change), 
         fmt_perc_change = str_replace(fmt_perc_change, "NA|Inf|\\+", as.character(NA))) %>%
  mutate(pred = factor(pred, levels = TableOrder_vec)) %>%
  arrange(pred)

crude_results %>%
  arrange(pred, term)

## pivot wider to format as table
crude_results1 <- crude_results_clean %>%
  rename(estimate = fmt_perc_change, p.value = p.value, star = p_stars) %>%
  pivot_wider(id_cols = c(pred, term), 
              names_from = c(name),
              values_from = c(estimate, p.value, star),
              names_glue = "{name}_{.value}") %>%
  #colnames() %>%
  select(pred, term, 
         starts_with("det_any"), 
         starts_with("viol_any"), # in this order in table
         starts_with("det_diox"), 
         starts_with("det_dca"), 
         starts_with("det_hcfc"), 
         starts_with("det_pfas"), 
         starts_with("viol_diox"), 
         #starts_with("viol_dca"),  # only 1 PWS exceeded the DCA HRL. This is not a meaningful outcome.
         starts_with("viol_pfas"))
crude_results1
# write.csv(crude_results1, paste0("outputs/Table 3. Crude Results_", Sys.Date(), ".csv"))

crude_results1

## playing with flextable

# library(ftExtra)
# library(flextable)
# crude_results1 %>%
#   flextable() %>%
#   separate_header(split = "_") %>%
#   theme_vanilla()

# rabbit hole - beware:
# crude_results1 %>%
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

# colnames(dat_clean)
base_formula <- paste("~ perc_hisp_any + perc_black_nohisp + mdi_rate +", 
                      "perc_urban + size + pws_type + n_samples + adj_wwtp_flow +", 
                      collapse = " ")
base_formula

temp <- dat_clean %>%
  
  mutate(size = factor(size, levels = c("S", "L"))) %>% # temporary. delete this after factoring in r script source0.
  
  # Pivot outcomes
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
  group_by(name) %>%
  nest() %>%
  
  # Add base formula 
  mutate(my_formula = paste("value", base_formula)) %>%
  
  # Customize my_formulas according to analyte of interest ('name')
  mutate(my_formula = case_when(str_detect(name,"any") ~ paste(my_formula, "+ n_fac_any_bin"), 
                                str_detect(name, "diox") ~ paste(my_formula, "+ n_fac_diox_bin"), 
                                str_detect(name, "dca") ~ paste(my_formula, "+ `n_fac_chlor_solv_bin`"), 
                                str_detect(name, "hcfc") ~ paste(my_formula, "+ n_fac_cfc_bin"), 
                                str_detect(name, "pfas") ~ paste(my_formula, "+  n_MFTA_airport_bin + src_epa_present_bin"), 
                                 TRUE ~ "9999")) %>%
  {stopifnot(nrow(filter(., my_formula == "9999"))==0); .;} %>%
  
  # Add state intercept 
  mutate(my_formula = paste(my_formula, " + (1|state)"))
  

#+ first attempt led to an error because of the outcome viol_dca. 
#+   viol_dca is not a meaningful outcome in these tables since only PWSID exceeded the DCA 
#+   HRL.
dat_clean %>% filter(viol_dca == 1) %>% pull(PWSID) # only one: IL2010300
#+ Viol_DCA is not meaningful as an outcome. There was only one sample in 
#+ one PWSID that exceeded the DCA health reference level.

#+ This takes approximately 3 minutes to run entirely.
adjusted_results <- temp %>%
  filter(name != "viol_dca") %>% #viol_dca was producing an error - investigate separately.
  mutate(n = map_dbl(data, ~sum(!is.na(.$value)))) %>%
  mutate(model_results = map(data, 
                             ~run_log2(dat = ., 
                                       my_formula = my_formula))) %>%
  unnest(model_results)

## cleaning
TableOrder_vec2 <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
                     "size", "sizeL",
                     "pws_type",  "pws_typeGW", "pws_typeMX", 
                     "n_samples", "adj_wwtp_flow", "n_fac_any_bin",
                     "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
                     "src_epa_present_bin", "n_MFTA_airport_bin", 
                     
                     # for adjusted regressions:
                     paste0(c("n_fac_any_bin",
                              "n_fac_diox_bin", "n_fac_chlor_solv_bin", "n_fac_cfc_bin",
                              "src_epa_present_bin", "n_MFTA_airport_bin"), "1"),
                     
                     "perc_hmown", "perc_pov_ppl", "perc_uninsur")

adj_results_clean <- adjusted_results %>% 
  filter(str_detect(term, "Intercept", negate = TRUE)) %>%
  select(name, term, n, p.value, estimate, conf.low, conf.high) %>% 
  mutate(perc_change = 100*(estimate - 1), 
         perc_change_LOW = 100*(conf.low - 1),
         perc_change_UPP = 100*(conf.high - 1)) %>%
  mutate_at(vars(matches("perc")), round, 1) %>%
  mutate_at(vars(matches("perc")), formatC, format = "g") %>%
  mutate(fmt_perc_change = 
           paste0(perc_change, " (", perc_change_LOW, ", ", perc_change_UPP, ")")) %>%
  mutate(p_stars = stars.pval(p.value)) %>%
  # mutate(p_stars = case_when(p.value < 0.001 ~ "**", 
  #                            p.value >= 0.001 & p.value < 0.05 ~ "*", 
  #                            p.value >= 0.05 & p.value < 0.10 ~ "+", 
  #                            p.value >= 0.10 ~ " ", 
  #                            TRUE ~ "n.s.")) %>%
  mutate(p_clean = ifelse(p.value < 0.001, "<0.001", 
                          paste(round(p.value, 2)))) %>%
  select(name, term, n, 
         #starts_with("perc_change"), 
         fmt_perc_change, 
         p_stars, p_clean) %>%
  mutate(term = factor(term, levels = TableOrder_vec2)) %>%
  arrange(term)

# 
# adj_results <- adjusted_results %>% select(name, term, n, fmt_perc_change, p.value)
# adj_results
# 
# adj_results <- adj_results  %>%
#   # add stars
#   mutate(p_stars = ) 
# 
# 
# 
# adj_results_clean <- adj_results %>%
#   mutate(fmt_perc_change = custom_trimws(fmt_perc_change)) %>%
#   mutate(fmt_perc_change = str_replace(fmt_perc_change, c("NA|Inf"), as.character(NA))) %>%
#   mutate(p.value = ifelse(p.value < 0.001, "<0.001", paste(round(p.value, 3)))) %>%
#   mutate(term = factor(term, levels = TableOrder_vec2)) %>%
#   arrange(term)
# adj_results_clean


## pivot wider to format as table
adjusted_results1 <- adj_results_clean %>%
  rename(outcome = name, 
         pred = term, estimate = fmt_perc_change, p.value = p_clean, star = p_stars) %>%
  pivot_wider(id_cols =  pred, 
              names_from = outcome,
              values_from = c(estimate, p.value, star),
              names_glue = "{outcome}_{.value}") %>% 
  select(pred, 
         starts_with("det_any"), 
         starts_with("viol_any"), # in this order in table
         starts_with("det_diox"), 
         starts_with("det_dca"), # dca before hcfc
         starts_with("det_hcfc"), 
         starts_with("det_pfas"), 
         starts_with("viol_diox"), 
         starts_with("viol_dca"), 
         starts_with("viol_pfas"))

# write.csv(adjusted_results1, paste0("outputs/Table 4. Adjusted Logistic Results_",
#                                 Sys.Date(),
#                                 ".csv"))


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

