## Supplemental tables and figures

# Run script "2. create main datasets.R" if starting from here.
# source("2. create main datasets.R")

# Also run script "3. analyze ucmr3 - crude & adj.R" to get adj_results_clean. 
# source("3. analyze ucmr3 - crude & adj.R")
adj_results_clean

library(tidyverse)
library(lme4)
library(broom.mixed)
library(gtools) # for stars.pval() function

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# With and without source terms ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Point sources are a direct pathway to contamination. The primary hypothesis is 
#+ that community demographics is associated with contamination independent of 
#+ source terms. This section evaluates demographic estimates (w/ other adjustments)
#+ with and without source terms.
#+ 
#+ This produces a table directly comparing covariate's percent change estimates
#+ with and without source terms in the model.

# colnames(dat_clean)
base_formula <- paste("~ perc_hisp_any + perc_black_nohisp + mdi_rate +", 
                      "perc_urban + size + pws_type + n_samples +", 
                      collapse = " ")
base_formula

noSources <- dat_clean %>%
  
  mutate(size = factor(size, levels = c("S", "L"))) %>% # temporary. delete this after factoring in r script source0.
  
  # Pivot outcomes
  pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
  group_by(name) %>%
  nest() %>%
  
  # Add base formula 
  mutate(my_formula = paste("value", base_formula)) %>%
  
  # Add state intercept 
  mutate(my_formula = paste(my_formula, "(1|state)"))
noSources

#+ This takes approximately 3 minutes to run entirely.
run_log2 <- function(dat, my_formula){
lme4::glmer(my_formula, data = dat, family = binomial) %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
}

noSources_reg_results <- noSources %>%
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

noSources_reg_results_clean <- noSources_reg_results %>% 
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
noSources_reg_results_clean

# create a tidier df to merge with dataframe adj_results_clean
adj_results_clean
noSources_reg_results_clean

# quick cleaning function 
tidy_output <- function(dat){
  dat %>% 
    mutate(fmt_perc_change = paste0(fmt_perc_change, " ", p_stars)) %>%
    distinct(name, term, fmt_perc_change)
}

noSources_reg_results_export <- bind_rows(
  adj_results_clean %>% tidy_output() %>% mutate(src_term = "Y"),
  noSources_reg_results_clean %>% tidy_output() %>% mutate(src_term = "N")
) %>%
  arrange(name, term) %>%
  pivot_wider(id_cols = c(term, src_term), 
              names_from = name, 
              values_from = fmt_perc_change) %>%
  select(term, src_term, det_any, viol_any, 
         det_diox, det_dca, det_hcfc, det_pfas)

# write.csv(noSources_reg_results_export,
#           paste0("outputs/", Sys.Date(), " - adj results with & without source terms.csv"))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other socioeconomic indicators ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ MDI is a multidimensional socioeconomic indicator. Unidimensional SES indicators-- 
#+ i.e., % of people below the poverty level, % of people without insurance, 
#+ and % of people who are homeowners--are explored here. We are looking at:
#+ 1) Is the direction AND/OR magnitude between det/viol and SES different between
#+    unidimensional vs multidimensional SES indicators?
#+    
#+ This produces a facet_wrap figure of percent change estimates for four primary
#+ community demographics: the SES variable of interest, % Hispanic, % NH Black,
#+ % urbanicity. The top header columns are the outcomes any det, any viol, 1,4-d, 
#+ 1,1-DCE, HCFC-22, and PFAS. X-axis is percent change estimate; y-axis is the 
#+ SES indicators. 
#+ 
#+ We may also want to use (poverty + insurance rate + homeownership) as a combination
#+ var.

colnames(dat_clean)[grepl("perc", colnames(dat_clean))]

# colnames(dat_clean)
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

primaryCols

uniSES_reg_df <- dat_clean %>%
  mutate(size = factor(size, levels = c("S", "L"))) # temporary. delete this after factoring in r script source0.
uniSES_reg_df

stopifnot(nrow(uniSES_reg_df) == 4808)


d <- bind_rows(
  glmer(paste("det_any", povFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "combined uniSES"), 
)

d

###### Plot of demographic coefficients in adjusted models with different SES measures

ggplot(d %>%
         filter(model_run != "combined uniSES") %>%
         filter(term %in% c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban", 
                            "perc_pov_ppl", "perc_uninsur", "perc_hmown")) %>%
         mutate(term2 = ifelse(term %in% c("perc_pov_ppl", "perc_uninsur", "perc_hmown", "mdi_rate"), 
                               "perc_ses", term)) %>%
         mutate(term2 = factor(term2, 
                               levels = c("perc_hisp_any", "perc_black_nohisp", "perc_urban", "perc_ses"), 
                               labels = c("(a) Percent Hispanic", 
                                          "(b) Percent non-Hispanic Black", 
                                          "(c) Percent urban households", 
                                          "(d) Percent SES variable"
                                         ))) %>%
         mutate(model_run = factor(model_run, 
                                   levels = c("mdi", "poverty", "insurance status", "homeownership"), 
                                   labels = c("Percent deprived", "Percent poverty", "Percent uninsured", "Percent homeownership"))) %>%
         mutate(perc_change = 100*(estimate - 1), 
                perc_change_LOW = 100*(conf.low - 1),
                perc_change_UPP = 100*(conf.high - 1)) %>%
         mutate(stars = stars.pval(p.value)),
         # mutate(stars = case_when(p.value < 0.001 ~ "**", 
         #                          p.value >= 0.001 & p.value < 0.05 ~ "*", 
         #                          p.value >= 0.05 & p.value < 0.10 ~ "+", 
         #                          TRUE ~ "")), 
       aes(x = model_run, y = perc_change)) +
  geom_pointrange(aes(ymin = perc_change_LOW, ymax = perc_change_UPP)) + 
  geom_text(aes(label = stars), nudge_y = 2) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~term2) + 
  labs(x = "", y = "Percent change (95% CI)") +
  theme_bw() +
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# ggsave(filename = paste0("outputs/", Sys.Date(), "- Different SES variables.pdf"),
#        height = 6, width = 6)

###### Tables for Supplement

d1 <- bind_rows(
  glmer(paste("det_any", povFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
)

d2 <- bind_rows(
  glmer(paste("viol_any", povFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("viol_any", insuranceFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("viol_any", hmownFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
)

d3 <- bind_rows(
  glmer(paste("det_diox", povFormula, "n_fac_diox_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_diox", insuranceFormula, "n_fac_diox_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_diox", hmownFormula, "n_fac_diox_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

d4 <- bind_rows(
  glmer(paste("det_dca", povFormula, "n_fac_chlor_solv_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_dca", insuranceFormula, "n_fac_chlor_solv_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_dca", hmownFormula, "n_fac_chlor_solv_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

d5 <- bind_rows(
  glmer(paste("det_hcfc", povFormula, "n_fac_cfc_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_hcfc", insuranceFormula, "n_fac_cfc_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_hcfc", hmownFormula, "n_fac_cfc_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

d6 <- bind_rows(
  glmer(paste("det_pfas", povFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "poverty"), 
  glmer(paste("det_pfas", insuranceFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "insurance status"), 
  glmer(paste("det_pfas", hmownFormula, "src_epa_present_bin + n_MFTA_airport_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
    tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
    mutate(model_run = "homeownership"), 
  # glmer(paste("det_any", combinedFormula, "n_fac_any_bin + (1|state)"), data = uniSES_reg_df, family = 'binomial') %>%
  #   tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE) %>%
  #   mutate(model_run = "combined uniSES"), 
) 

d_list <- bind_rows(d1 %>% mutate(name = "det_any"),
                    d2 %>% mutate(name = "viol_any"),
                    d3 %>% mutate(name = "det_diox"),
                    d4 %>% mutate(name = "det_dca"),
                    d5 %>% mutate(name = "det_hcfc"),
                    d6 %>% mutate(name = "det_pfas")) %>%
  group_by(model_run) %>%
  nest() %>%
  pull(data)


d_list2 <- lapply(d_list, function(x) 
  x %>% 
    filter(str_detect(term, "Intercept", negate = TRUE)) %>%
    select(term, name, p.value, estimate, conf.low, conf.high) %>% 
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
    select(term, name,
           fmt_perc_change,
           p_stars, p_clean) %>%
    mutate(term = factor(term, levels = TableOrder_vec2)) %>%
    mutate(fmt_perc_change = paste0(fmt_perc_change, " ", p_stars)) %>% 
    arrange(term) %>%
    pivot_wider(id_cols = term, 
                names_from = name, 
                values_from = fmt_perc_change)
)

d_list2[[1]] # pov
d_list2[[2]] # uninsur
d_list2[[3]] # hmown

# write.csv(d_list2[[1]],
#           paste0("outputs/", Sys.Date(), " - reg results pov.csv"))
# write.csv(d_list2[[2]],
#           paste0("outputs/", Sys.Date(), " - reg results uninsur.csv"))
# write.csv(d_list2[[3]],
#           paste0("outputs/", Sys.Date(), " - reg results hmown.csv"))


###############################################################################
##  ARCHIVE
###############################################################################

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
# 
# 
# 
# 
# 
