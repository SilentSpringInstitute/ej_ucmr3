
#+ 10/4/23
#+ AM
#+ 
#+ I moved the stratified results into a separate script to make the 
#+ other code easier to read and faster to source().
#+ 

source("3. analyze ucmr3.R") # this takes a few minutes to run

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 5. Stratified by Size, adjusted logistic models ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# note that size is not in this anymore
base_formula <- paste("~ perc_hisp_any + perc_black_nohisp + mdi_rate +", 
                      "perc_urban + pws_type + n_samples + adj_wwtp_flow +", 
                      collapse = " ")

temp <- dat_clean %>%
  
  # Pivot outcomes
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
  group_by(name, size) %>% # stratify by size!
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

## tests
# temp %>% pull(my_formula) %>% unique()
# temp1 <- temp %>% filter(name == "det_any")

## function
run_log2 <- function(dat, my_formula){
  lme4::glmer(my_formula, data = dat, family = binomial) %>% 
    broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(estimate_perc = 100*(estimate - 1), 
           estimate_perc_low = 100*(conf.low - 1), 
           estimate_perc_hi = 100*(conf.high - 1), 
           fmt_perc_change = paste0(format(round(estimate_perc, 1), nsmall = 1), 
                                    " (", format(round(estimate_perc_low, 1), nsmall = 1), 
                                    ", ", format(round(estimate_perc_hi, 1), nsmall = 1), 
                                    ")"), 
           p_stars = gtools::stars.pval(p.value))
}

#+ This takes approximately 3 minutes to run entirely.
#+ temp_results is what happens after running ALL the regression models (list of 
#+ data still in df). Do not view().
temp_results <- temp %>%
  filter(name != "viol_dca") %>% 
  filter(name != "det_dca") %>% # this outcome was throwing an error - investigate separately
  mutate(model_results = map(data, 
                             ~run_log2(dat = ., 
                                       my_formula = my_formula))) %>%
  unnest(model_results)

colnames(temp_results)

## formatting to wider and ordering columns
stratified_results <- temp_results %>%
  filter(!str_detect(term, "(Intercept)")) %>%
  # mutate(p.value = format(round(p.value, 3), nsmall = 1)) %>% # formatting p.value
  select(-estimate) %>%
  rename(estimate = fmt_perc_change,, p.stars = p_stars) %>%
  pivot_wider(id_cols = c(term), 
              names_from = c(name, size),
              values_from = c(estimate, p.value, p.stars), 
              names_sort = TRUE, 
              names_glue = "{name}_{size}_{.value}") %>%
  #colnames() %>%
  select(term, 
         starts_with("det_any_L"), 
         starts_with("det_any_S"), 
         starts_with("viol_any_L"), # in this order in table
         starts_with("viol_any_S"), # in this order in table
         starts_with("det_diox_L"), 
         starts_with("det_diox_S"), 
         starts_with("det_hcfc_L"), 
         starts_with("det_hcfc_S"), 
         starts_with("det_dca_L"), 
         starts_with("det_dca_S"), 
         starts_with("det_pfas_L"), 
         starts_with("det_pfas_S"), 
         # not needed:
         # starts_with("viol_diox"), 
         # starts_with("viol_dca"), 
         # starts_with("viol_pfas")
         )

## more formatting 
stratified_results2 <- stratified_results %>%
  mutate_at(vars(matches("_estimate")), ~custom_trimws(.)) %>%
  mutate_at(vars(matches("_estimate")), ~str_replace(., c("NA|Inf|\\+"), as.character(NA))) %>%
  mutate_at(vars(matches("_estimate")), ~str_replace(., c(".{30,}"), "--")) %>%
  mutate_at(vars(matches("_p.value")), ~ifelse(. < 0.001, "<0.001", round(., 5))) %>%
  mutate(term = factor(term, levels = TableOrder_vec)) %>%
  arrange(term)

flex_basic <- function(dat){
  # dat = data.frame object 
  dat %>% 
    flextable() %>%
    separate_header(split = "_") %>%
    set_table_properties(layout = "autofit") %>%
    theme_box()
  
}

tempfile(fileext = ".docx")
tempdir()


pr_sect_default <- prop_section(
  page_size = page_size(
    orient = 'landscape', 
    width = 10, height = 12), 
  type = "continuous", 
)


stratified_results2 %>%
  select(term, starts_with("det_any"), starts_with("viol_any")) %>%
  flex_basic() %>%
  save_as_docx(path = "outputs/stratified results.docx", 
               pr_section = pr_sect_default
               )

final_table3 %>%
  mutate(det_any_freq = round(det_any_freq, 1), 
         exceed_any_freq = round(det_any_freq, 1), 
         pop_served = round(pop_served, 0)) %>% 
  flextable() %>%
  theme_box() %>%
  colformat_num(big.mark = " ", decimal.mark = ".", na_str = "N/A") %>%
  set_table_properties(layout = "autofit") %>%
  save_as_docx(path = "outputs/study characteristics.docx", 
               pr_section = pr_sect_default
  )

bleh3 %>%
  mutate(det_freq_samp = round(det_freq_samp, 1), 
         exc_freq_samp = round(exc_freq_samp, 1), 
         det_freq_sys = round(det_freq_sys, 1), 
         exc_freq_sys = round(exc_freq_sys, 1)) %>%
  flextable() %>%
  theme_box() %>%
  autofit()

crude_results1 %>%
  mutate_at(vars(matches("p.value")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
  select(term, starts_with("det_"), starts_with("viol_any")) %>%
  flex_basic()
  #save_as_docx(path = "outputs/stratified results.docx", 
               pr_section = pr_sect_default
  )

adjusted_results2 %>%
  mutate_at(vars(matches("p.value")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
  select(pred, starts_with("det_"), starts_with("viol_any")) %>%
  flex_basic()

means3 %>%
  mutate_at(vars(matches("p_")), ~ifelse(. < 0.001, "< 0.001", paste(round(., 3)))) %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  flex_basic()

# https://davidgohel.github.io/flextable/reference/save_as_docx.html




# write.csv(stratified_results, paste0("outputs/Table 5. Size Stratified Adjusted Logistic Results_",
#                                 Sys.Date(),
#                                 ".csv"))