### AUTHOR: AM
### STARTED: 2023-03-13
### WRITTEN IN: R version 4.2.2
### Purpose: 

library(tidyverse)
library(ggforce)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

#load script 4 so that we can remove the PWSs with missing MDIs
source("1 - UCMR loading and processing.R")
# source("4a - UCMR Summary.R")
# source("4a - UCMR Summary_det freqs.R")


################################################################################
#  6. CLEAN FOR FIGURES ####
################################################################################


ucmrdf.plot <- ucmrdf.clean %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS")),
         test_chem = factor(test_chem, levels = c("1,1,1-TRICHLOROETHANE", 
                                                  "ETHYLIDENE DICHLORIDE", 
                                                  "1,4-DIOXANE","CHLORODIFLUOROMETHANE",
                                                  "DICHLORODIFLUOROMETHANE",
                                                  "CHLORINATED SOLVENTS", "CFCs",
                                                  "bin_TRI"),
                            labels = c("TRI: 1,1,1-trichloroethane", 
                                       "TRI: 1,1-dichloroethane", 
                                       "TRI: 1,4-dioxane",
                                       "TRI: HCFC-22",  "TRI: CFC-12", 
                                       "TRI: Chlorinated Solvents", 
                                       "TRI: CFCs",">=1 TRI facility")))



# detection frequency all 
ucmrdf.plot %>% 
  filter(contam.pfas == ">=1 UCMR Detection") %>% 
  filter(test_chem == ">=1 TRI facility") %>% 
  group_by(.) %>% 
  summarize(detfreq = length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID)))



################################################################################
# 7. SUMMARIZE SOURCES ####
################################################################################

pfas_sources_summary <- ucmrdf.clean %>%
  filter(contam.pfas == "evrdet"& test_chem == "bin_TRI") %>% 
  group_by(Size) %>% 
  summarize(n = length(unique(PWSID)),
            freq_airportMFTA = round(length(unique(PWSID[which(airportMFTA_bin == 1)]))/n*100,1),
            freq_epastewardship = round(length(unique(PWSID[which(n_epastewardship_bin == 1)]))/n*100,1),
            has_wwtp = round(length(unique(PWSID[which(n_WWTP > 0)]))/n*100,1)) %>% 
  bind_rows(ucmrdf.clean %>%
              filter(contam.pfas == "evrdet"& test_chem == "bin_TRI") %>% 
              group_by(.) %>% 
              summarize(n = length(unique(PWSID)),
                        freq_airportMFTA = round(length(unique(PWSID[which(airportMFTA_bin == 1)]))/n*100,1),
                        freq_epastewardship = round(length(unique(PWSID[which(n_epastewardship_bin == 1)]))/n*100,1),
                        has_wwtp = round(length(unique(PWSID[which(n_WWTP > 0)]))/n*100,1)) %>% 
              mutate(Size = "all"))


#write_csv(pfas_sources_summary, "results/output/pfas sources summary.csv")

#### SUMMARIZE TRI FACILITIES

# summarize completeness of tri data
tri_completion <- ucmrdf.clean %>%
  group_by(PWSID, test_chem) %>%
  summarize(n = length(unique(test_chem[which(!is.na(reporting_year))]))) %>%
  mutate(hasTRI = case_when(n == 0 ~ "N",
                            n == 1 ~ "Y")) %>%
  select(-n)

# for paper
tri_summary <- tri_completion %>%
  group_by(test_chem) %>%
  summarize(n_tri = length(unique(PWSID[which(hasTRI == "Y")])),
            n_sys = length(unique(PWSID)),
            perc_tri = length(unique(PWSID[which(hasTRI == "Y")]))/length(unique(PWSID)))

tri_summary_size <-  ucmrdf.clean %>%
  group_by(PWSID, test_chem, Size) %>%
  summarize(n = length(unique(test_chem[which(!is.na(reporting_year))]))) %>%
  mutate(hasTRI = case_when(n == 0 ~ "N",
                            n == 1 ~ "Y")) %>%
  select(-n) %>%
  group_by(test_chem, Size) %>%
  summarize(n_tri = length(unique(PWSID[which(hasTRI == "Y")])),
            n_sys = length(unique(PWSID)),
            perc_tri = length(unique(PWSID[which(hasTRI == "Y")]))/length(unique(PWSID)))

test_chem_sum <- ucmrdf.plot %>%
  select(contam.pfas, PWSID, test_chem, n_fac, Size) %>% 
  unique() %>%
  filter(test_chem %in% c( "TRI: Chlorinated Solvents", "TRI: CFCs", "TRI: 1,4-dioxane", ">=1 TRI facility"))%>% 
  group_by(contam.pfas, test_chem, n_fac) %>% 
  count() %>% 
  mutate(n_fac_2 = case_when(n_fac > 0 ~ "Y", 
                             TRUE ~ "N")) %>% 
  group_by(contam.pfas, test_chem, n_fac_2) %>% 
  summarize(n_count = sum(n)) %>% 
  group_by(contam.pfas, test_chem) %>% 
  mutate(perc_fac = n_count/sum(n_count)*100,
         perc_fac_label =  paste0(round(perc_fac, 2), "%"),
         total_sys = case_when( n_fac_2 == "Y" ~ paste0("n = ", sum(n_count)),
                                TRUE ~ "")) %>% 
  mutate(code = case_when(test_chem == "TRI: 1,4-dioxane" & contam.pfas == "1,4-dioxane" ~ "Y",
                          test_chem == "TRI: CFCs" & contam.pfas == "HCFC-22" ~ "Y",
                          test_chem == "TRI: Chlorinated Solvents" & contam.pfas == "1,1-dichloroethane" ~ "Y",
                          test_chem == ">=1 TRI facility" & contam.pfas == ">=1 UCMR Detection" ~ "Y",
                          TRUE ~ "N"))

#_# get output for paper write up
test_chem_sum %>% 
  ungroup() %>% 
  filter(n_fac_2 == "Y") %>% 
  #filter(contam.pfas == ">=1 UCMR Detection") %>%
  ggplot(aes(x= as.factor(test_chem), y = perc_fac, fill = code)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values = c("grey", "#d77659"))+
  geom_text(aes(label=perc_fac_label), position = position_dodge(width = 0.9), vjust = -0.25) + 
  # geom_text(aes(label = total_sys_l), x = 1, y = 18.5) +
  # geom_text(aes(label = total_sys_s), x = 2, y = 18.5) +
  ylab("Percent of PWSs with TRI Facility") + 
  xlab("System Size") + 
  theme_minimal() + 
  theme(axis.text = element_text(size=14),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  facet_grid(~contam.pfas)

# ggsave("results/output/Percent of PWSs with TRI facilities.png", width = 40, height = 10, units = "in")

## JML check: quick check of graph numbers
# ucmrdf.clean %>%
#   group_by(contam.pfas, test_chem) %>%
#   summarise(perc.TRI = sum(hasTRI == "Y")/n()) %>% ungroup() %>% 
#   View()
## all good!

### summarize and plot by system size ###
test_chem_sum_size <- ucmrdf.plot %>%
  select(contam.pfas, PWSID, test_chem, n_fac, Size) %>% 
  filter(!is.na(n_fac)) %>% 
  unique() %>%
  filter(test_chem %in% c( "TRI: Chlorinated Solvents", "TRI: CFCs", "TRI: 1,4-dioxane", "Any TRI facility"))%>% 
  group_by(contam.pfas, test_chem, n_fac, Size) %>% 
  count() %>% 
  mutate(n_fac_2 = case_when(n_fac > 0 ~ "Y", 
                             TRUE ~ "N")) %>% 
  group_by(contam.pfas, test_chem, n_fac_2, Size) %>% 
  summarize(n_count = sum(n)) %>% 
  group_by(contam.pfas, test_chem, Size) %>% 
  mutate(perc_fac = n_count/sum(n_count)*100,
         perc_fac_label =  paste0(round(perc_fac, 2), "%"),
         total_sys_l = case_when(Size == "L" & n_fac_2 == "Y" ~ paste0("n = ", sum(n_count)),
                                 TRUE ~ ""),
         total_sys_s = case_when(Size == "S" & n_fac_2 == "Y" ~ paste0("n = ", sum(n_count)),
                                 TRUE ~ ""))


test_chem_sum_size %>% 
  ungroup() %>% 
  filter(n_fac_2 == "Y") %>% 
  filter(contam.pfas == ">=1 UCMR Detection") %>%
  ggplot(aes(x= as.factor(Size), y = perc_fac, fill = as.factor(Size))) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_manual(name = "System Size",
                    labels=c("Large (n = 4033)", "Small (n = 779)"),
                    values = c("#457d7c", "#d77659"))+
  geom_text(aes(label=perc_fac_label), position = position_dodge(width = 0.9), vjust = -0.25) + 
  # geom_text(aes(label = total_sys_l), x = 1, y = 18.5) +
  # geom_text(aes(label = total_sys_s), x = 2, y = 18.5) +
  ylab("Percent of PWSs with TRI Facility") + 
  xlab("System Size") + 
  theme_minimal() + 
  theme(axis.text = element_text(size=14),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = "none")+
  #ylim(0, 20) + 
  facet_grid(~test_chem)

## JML check: quick check of graph numbers
# ucmrdf.clean %>%
#   group_by(test_chem, Size) %>%
#   summarise(perc.TRI = sum(hasTRI == "Y")/n()) %>% ungroup() %>%
#   View()
# all good!

################################################################################
# 8. TABLE 2 -- Characteristics of PWSs and median demographics of counties ####
################################################################################


# Get number of cumulative detects
getcount <- function(dat) {
  dat2 <- dat %>% 
    #filter(test_method == "PFAS") %>% 
    filter(contam.pfas != "evrdet") %>% 
    group_by(PWSID) %>% 
    summarize(n_det = length(unique(contam.pfas[which(detchem == 1)]))) %>%
    mutate(n_det = as.character(n_det))
  
  dat3 <- dat %>% 
    #filter(test_method == "PFAS") %>% 
    filter(contam.pfas == "evrdet") %>% 
    filter(detchem == 1) %>%
    mutate(n_det = "Any detected UCMR") %>%
    select(PWSID, n_det) %>% 
    unique() %>% 
    bind_rows(dat2)
  
  dat %>% 
    filter(contam.pfas == "evrdet") %>% 
    select(-contam.pfas, detchem, -hlvlchem) %>% 
    unique() %>% 
    left_join(dat3)
  
}


demobreakdown <- function(dat) {
  dat %>%
    summarize(
      n.sys = length(unique(PWSID)),
      n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
      `Percent small PWSs` = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 1),
      `Percent GW Systems` = round(length(unique(PWSID[which(source_type == "GW")]))/length(source_type)*100, 1),
      `Percent SW Systems` = round(length(unique(PWSID[which(source_type == "SW")]))/length(source_type)*100, 1),
      # 2023-03-02 AM: added mean number of samples
      `Mean number of samples (SD)` = paste0(round(mean(n_samples, na.rm = T), 1), 
                                             "\n (", round(sd(n_samples, na.rm = T), 1), ")"),
      `Median Percent Hispanic (Q1, Q3)` = paste0(round(median(perc_hisp_any, na.rm = TRUE),1),
                                                  "\n (",  round(quantile(perc_hisp_any, .25),1), ", ",
                                                  round(quantile(perc_hisp_any, .75),1), ")"),
      `Median Percent Black (Q1, Q3)` = paste0(round(median(perc_black_nohisp, na.rm = TRUE),1),
                                               "\n (",  round(quantile(perc_black_nohisp, .25),1), ", ",
                                               round(quantile(perc_black_nohisp, .75),1), ")"), 
      `Median MDI Rate (Q1, Q3)` = paste0(round(median(mdi, na.rm = TRUE),1),
                                          "\n (",  round(quantile(mdi, na.rm = TRUE, .25),1), ", ",
                                          round(quantile(mdi, na.rm = TRUE, .75),1), ")"),
      `Median Percent Urban (Q1, Q3)` = paste0(round(median(propurban*100, na.rm = TRUE),1),
                                               "\n (",  round(quantile(propurban*100, .25),1), ", ",
                                               round(quantile(propurban*100, .75),1), ")"),
      `Median Percent Poverty (Q1, Q3)` = paste0(round(median(perc_pov_ppl, na.rm = TRUE),1),
                                                 "\n (",  round(quantile(perc_pov_ppl, .25),1), ", ",
                                                 round(quantile(perc_pov_ppl, .75),1), ")"),
      `Median Percent Uninsured (Q1, Q3)` = paste0(round(median(perc_uninsur, na.rm = TRUE),1),
                                                   "\n (",  round(quantile(perc_uninsur, .25),1), ", ",
                                                   round(quantile(perc_uninsur, .75),1), ")"),
      `Median Percent Homeownersip (Q1, Q3)` = paste0(round(median(perc_hmown, na.rm = TRUE),1),
                                                      "\n (",  round(quantile(perc_hmown, .25),1), ", ",
                                                      round(quantile(perc_hmown, .75),1), ")")
    )
  
}

# AM added
n_samples_dat <- ucmr3_targetsamples %>%
  group_by(PWSID, SamplePointName, CollectionDate) %>% 
  summarise(n = n()) %>% 
  group_by(PWSID) %>%
  summarise(n_samples = n())

ucmrdf.clean_summ <- ucmrdf.clean %>% 
  filter(test_chem == "bin_TRI") %>%
  left_join(n_samples_dat, by = "PWSID")

overall_brkdwn <- ucmrdf.clean_summ %>%
  filter(contam.pfas == "evrdet") %>%
  group_by(.) %>% 
  demobreakdown() %>% 
  mutate(brktype = "Overall")


detected_brkdwn <- ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>% 
  group_by(detchem) %>%
  demobreakdown() %>%
  mutate(brktype = case_when(detchem == 0 ~ "Detected: 0",
                             TRUE ~ "Detected: 1-4")) %>% 
  select(-detchem)

size_brkdwn <- ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>%
  group_by(Size) %>%
  demobreakdown() %>% 
  mutate(brktype = paste0("System Size (", Size, ")"))

source_brkdwn <- ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>%
  group_by(source_type) %>%
  demobreakdown() %>% 
  mutate(brktype = paste0("Source Water (", source_type, ")"))

n.det_brkdwn <- ucmrdf.clean_summ %>%
  getcount() %>%
  group_by(n_det) %>%
  demobreakdown() %>%
  mutate(brktype = case_when(n_det == "Any detected UCMR" ~ ">=1 UCMR Detection",
                             TRUE ~ paste0("Number of detected UCMR chemicals (", n_det, ")"))) %>%
  arrange(brktype)

ovrhlvl_brkdwn <- ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>% 
  group_by(hlvlchem) %>% 
  demobreakdown() %>% 
  mutate(brktype = case_when(hlvlchem == 1 ~ "Over federal health guideline: 1-2",
                             TRUE ~ "Over federal health guideline: 0"))

sdwa_brkdwn <- ucmrdf.clean_summ %>%
  filter(contam.pfas == "evrdet") %>% 
  group_by(hasviolation) %>% 
  demobreakdown() %>% 
  mutate(brktype = case_when(hasviolation == "Y" ~ "Has SDWA violation",
                             TRUE ~ "No SDWA violation")) %>% 
  select(-hasviolation)

all_brkdwn <- bind_rows(overall_brkdwn, detected_brkdwn, ovrhlvl_brkdwn, 
                        sdwa_brkdwn, size_brkdwn, source_brkdwn) %>% 
  select(-source_type, -Size, -hlvlchem) %>% 
  relocate(`brktype`, .before = "n.sys")

#write_csv(all_brkdwn, "results/output/system characteristics by category.csv")

# #### JML: quick spot-checks of select stats/breakdowns ####
# if (!require("qwraps2")) install.packages("qwraps2")
# library(qwraps2) # table package I often use, makes things easier in RMarkdown
# 
# # test data
# JML.ucmr.test <- ucmrdf.clean_summ %>% select(PWSID, WS.POPULATION_SERVED_COUNT, Size,
#                                               perc_hisp_any, hlvlchem, source_type, contam.pfas,
#                                               detchem, n_det) %>%
#   filter(contam.pfas == "evrdet")

# make table
summary_stats <-
  list (
    list("n.ppl" = ~sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
         "n.sys" = ~n_distinct(PWSID),
         "perc.small" = ~n_perc(Size == "S"),
         "perc.Hisp" = ~median_iqr(perc_hisp_any),
         "perc.detchem" = ~n_perc(detchem)
    )
  )

# # checks with breakdowns of a few diff "types" of summary stats
# # these also check the detection frequencies below
# qwraps2::summary_table(JML.ucmr.test, summary_stats, by = "hlvlchem") %>% View()
# 
# qwraps2::summary_table(JML.ucmr.test, summary_stats, by = "detchem") %>% View ()
# 
# qwraps2::summary_table(JML.ucmr.test, summary_stats, by = "Size") %>% View()
# qwraps2::summary_table(JML.ucmr.test, summary_stats, by = "source_type") %>% View()
#####

################################################################################
# 8. DETECTION FREQUENCIES REFERENCED IN TEXT ####
################################################################################

#for reference in text
size_detfreq <-  ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>% 
  group_by(Size) %>% 
  summarize(n.sys = length(unique(PWSID)),
            n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
            det_freq = length(unique(PWSID[which(detchem == "1")]))/
              length(unique(PWSID))*100, 
            hlvl_freq = length(unique(PWSID[which(hlvlchem == "1")]))/
              length(unique(PWSID))*100)

source_detfreq <-  ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>% 
  group_by(source_type) %>% 
  summarize(n.sys = length(unique(PWSID)),
            n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
            det_freq = length(unique(PWSID[which(detchem == "1")]))/
              length(unique(PWSID))*100, 
            hlvl_freq = length(unique(PWSID[which(hlvlchem == "1")]))/
              length(unique(PWSID))*100)

overall_detfreq <-  ucmrdf.clean_summ %>% 
  filter(contam.pfas == "evrdet") %>% 
  summarize(n.sys = length(unique(PWSID)),
            n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
            det_freq = length(unique(PWSID[which(detchem == "1")]))/
              length(unique(PWSID))*100, 
            hlvl_freq = length(unique(PWSID[which(hlvlchem == "1")]))/
              length(unique(PWSID))*100)

#not currently referenced? 
# ucmrdf.clean_summ %>% 
#   filter(contam.pfas == "evrdet") %>% 
#   group_by(.) %>% 
#   summarize(n_ovr25perc_hisp = length(unique(PWSID[which(perc_hisp_any > 25)]))/length(unique(PWSID))*100,
#             n_ovr25perc_hisp_det = length(unique(PWSID[which(perc_hisp_any > 25 & detchem == 1)]))/length(unique(PWSID))*100,
#             n_ovr25perc_black = length(unique(PWSID[which(perc_black_nohisp > 25)]))/length(unique(PWSID))*100,
#             n_ovr25perc_black_det = length(unique(PWSID[which(perc_black_nohisp > 25 & detchem == 1)]))/length(unique(PWSID))*100,
#             n_ovr25either = length(unique(PWSID[which(perc_black_nohisp >25 | perc_hisp_any > 25)]))/length(unique(PWSID))*100
#             ) %>% 
#   View()


#######################################################################################
# 5. TABLE 3 -- mean unequal variances t-test  ####
#######################################################################################


#calculate means for detected chemicals

mean_calc_detected <- function(dat){
  dat %>% 
    filter(test_chem == ">=1 TRI facility") %>% 
    mutate(propurban = propurban*100) %>% 
    select(detchem, one_of("Size"), contam.pfas, PWSID, perc_black_nohisp, perc_pov_ppl, 
           perc_hisp_any, perc_hmown, propurban, mdi, perc_uninsur) %>%
    pivot_longer(names_to = 'variable', values_to = 'value', perc_black_nohisp:perc_uninsur) %>%
    group_by(across(one_of("contam.pfas", "variable","Size"))) %>%
    summarize(
      n = n(),
      ovrall.avg = mean(value, na.rm = TRUE),
      nd.avg = mean(value[which(detchem == 0)], na.rm = TRUE),
      det.avg = mean(value[which(detchem == 1)], na.rm = TRUE),
      nd.var = var(value[which(detchem == 0)], na.rm = TRUE),
      det.var = var(value[which(detchem == 1)], na.rm = TRUE),
      nd.n = length(unique(PWSID[which(detchem == 0)])),
      det.n = length(unique(PWSID[which(detchem == 1)]))
    ) 
  
}

# calculate means for health level exceedences

mean_calc_hlvl <- function(dat){
  dat %>% 
    # Table5 now Table4 since previous Table 3 was turned into a figure
    # this comes from 4b - UCMR analysis.R script
    filter(test_chem == ">=1 TRI facility") %>% 
    mutate(propurban = propurban*100) %>% 
    filter(contam.pfas %in% c(">=1 UCMR Detection", "1,4-dioxane", "PFAS")) %>% 
    select(hlvlchem, one_of("Size"), contam.pfas, PWSID, perc_black_nohisp, perc_pov_ppl, 
           perc_hisp_any, perc_hmown, propurban, mdi, perc_uninsur) %>%
    pivot_longer(names_to = 'variable', values_to = 'value', perc_black_nohisp:perc_uninsur) %>%
    group_by(across(one_of("contam.pfas", "variable","Size"))) %>%
    summarize(
      n = n(),
      ovrall.avg = mean(value, na.rm = TRUE),
      nd.avg = mean(value[which(hlvlchem == 0)], na.rm = TRUE),
      det.avg = mean(value[which(hlvlchem == 1)], na.rm = TRUE),
      nd.var = var(value[which(hlvlchem == 0)], na.rm = TRUE),
      det.var = var(value[which(hlvlchem == 1)], na.rm = TRUE),
      nd.n = length(unique(PWSID[which(hlvlchem == 0)])),
      det.n = length(unique(PWSID[which(hlvlchem == 1)]))
    ) 
}

# run the t.test (same for both detected and health level)

unequal_var_calc <- function(dat){
  dat %>% 
    mutate(welch.t = round(((det.avg - nd.avg)/sqrt((det.var/det.n) + (nd.var/nd.n))), 3),
           df = round(((det.var/det.n + nd.var/nd.n)^2/((det.var^2)/(det.n^2*(det.n-1)) + (nd.var^2)/(nd.n^2*(nd.n-1)))), 3),
           pval = round(((2*pt(abs(welch.t), df, lower=FALSE))), 3),
           pval_edit = case_when(pval < 0.001 ~ paste0("<0.001"),
                                 TRUE ~ paste0(pval)),
           flag = case_when(pval <= 0.001 ~ "***",
                            pval <= 0.01 ~ "**",
                            pval <= 0.05 ~ "*", 
                            TRUE ~ " "),
           flag2 = case_when(pval <= 0.001 ~ "< 0.001",
                             pval <= 0.01 ~ "< 0.01",
                             pval <= 0.05 ~ "< 0.05", 
                             TRUE ~ " "),
           flag3 = case_when(nd.avg-det.avg >0 ~ paste0("(-) ", pval_edit),
                             nd.avg-det.avg < 0 ~ paste0("(+) ", pval_edit),
                             TRUE ~ paste0(pval_edit))
    ) %>% 
    mutate(variable = factor(variable, levels = c("perc_hisp_any", "perc_black_nohisp", "mdi", "propurban",
                                                  "perc_hmown",  "perc_pov_ppl", "perc_uninsur"),
                             labels = c("Percent Hispanic", 
                                        "Percent Black, non-Hispanic","Percent deprived", 
                                        "Percent urban households","Percent home ownership",
                                        "Percent poverty", "Percent uninsured")),
           ovrall.avg = round(ovrall.avg, 1),
           nd.avg = round(nd.avg, 1),
           det.avg = round(det.avg, 1)) %>% 
    arrange(variable) 
}

# format the detected table 

table3_detected_format <- function(dat){
  dat %>%
    select(contam.pfas, variable, ovrall.avg, nd.avg, det.avg, flag3, one_of("Size")) %>%
    rename(`All PWSs` = ovrall.avg,
           `PWSs with no target contaminants detected` = nd.avg,
           `PWSs with >=1 target contaminant detected` = det.avg,
           `Comparison between groups` = flag3) 
}


# format the health exceedance table

table3_hlvl_format <- function(dat){
  dat %>% 
    select(contam.pfas, variable, ovrall.avg, nd.avg, det.avg, flag3, one_of("Size")) %>% 
    rename(`All PWSs` = ovrall.avg,
           `PWSs with no target contaminants exceeding federal guideline` = nd.avg,
           `PWSs with >=1 target contaminant exceeding federal guideline` = det.avg,
           `Comparison between groups (hlvlchem)` = flag3) %>%
    ungroup() 
}


#get table 3 for detected chemicals

table3_detected <-
  ucmrdf.plot %>%
  select(-Size) %>% 
  mean_calc_detected() %>% 
  unequal_var_calc() %>% 
  table3_detected_format()

# get table 4 for health exceedance chemicals

table3_hlvlchem <-
  ucmrdf.plot %>%
  select(-Size) %>% 
  mean_calc_hlvl() %>% 
  unequal_var_calc() %>% 
  table3_hlvl_format()

#bind together for table 3

table3 <- table3_detected %>% 
  filter(contam.pfas == ">=1 UCMR Detection") %>%
  ungroup() %>%
  select(-contam.pfas) %>% 
  left_join(table3_hlvlchem %>% 
              filter(contam.pfas == ">=1 UCMR Detection") %>% 
              select(-contam.pfas)) %>% 
  filter(!variable %in% c("Percent uninsured", "Percent poverty", "Percent home ownership"))

# 2023-03-02 AM: convert table 3 into a plot
table3_long <- table3 %>% 
  pivot_longer(cols = contains("PWSs"), 
               names_to = "name", 
               values_to = "value") %>%
  #filter(str_detect(variable, "Black|Hispanic")) %>%
  #pull(unique(name))
  mutate(short_name = case_when(str_detect(name, "All") ~ "Overall", 
                                str_detect(name, ">=1 target contaminant detected") ~ "Detected", 
                                str_detect(name, "no target contaminants detected") ~ "Not detected", 
                                str_detect(name, ">=1 target contaminant exceeding") ~ "Exceeded",
                                str_detect(name, "no target contaminants exceeding") ~ "Not exceeded")) %>%
  mutate(short_name = factor(short_name, levels = c("Overall", 
                                                    "Not detected", 
                                                    "Detected", 
                                                    "Not exceeded", 
                                                    "Exceeded")))
library(ggsignif)
library(ggh4x)

table3_long2 <- filter(table3_long, short_name != "Overall")
table3_long2 <- table3_long2 %>% mutate(short_name = factor(short_name, 
                                                            levels = c("Not detected", 
                                                                       "Detected", 
                                                                       "Not exceeded", 
                                                                       "Exceeded")))
# table3_long2$`Comparison between groups (hlvlchem)` %>% unique()
# table3_long3 <- table3_long2 %>%
#   mutate(p1 = case_when(str_detect(`Comparison between groups`, "0.041") ~ "*",
#                         str_detect(`Comparison between groups`, "0.491") ~ "n.s.",
#                         str_detect(`Comparison between groups`, "<0.001") ~ "***"),
#          p2 = case_when(str_detect(`Comparison between groups (hlvlchem)`, "0.573") ~ "n.s.",
#                         str_detect(`Comparison between groups (hlvlchem)`, "0.895") ~ "n.s.",
#                         str_detect(`Comparison between groups (hlvlchem)`, "<0.001") ~ "***"))


test <- table3_long2 %>%
  mutate(name1 = ifelse(str_detect(short_name, "Not"), "No", "Yes"), 
         name1 = factor(name1, levels = c("No", "Yes")), 
         name2 = ifelse(str_detect(short_name, "etected"), 
                        "Target contaminant\ndetected", 
                        "Health-reference level\nexceeded"), 
         name2 = factor(name2, levels = c("Target contaminant\ndetected", 
                                          "Health-reference level\nexceeded"
                                          )))

# test_ps <- test %>% select(variable, name, starts_with("Comparison"))
# test_ps2 <- test_ps %>% 
#   pivot_longer(cols = starts_with("Comp"), 
#                names_to = "test", 
#                values_to = "p") %>% 
#   mutate(help = case_when(str_detect(test, "hlvlchem") & str_detect(name, "detect") ~ "rm", 
#                           str_detect(name, "exceeding") & !str_detect(test, "hlvlchem") ~ "rm", 
#                           TRUE ~ "keep")) %>%
#   filter(help=='keep') %>%
#   select(-help) %>% 
#   mutate(p_fig = case_when(str_detect(name, ">=1") & str_detect(p, "<") ~ "***", 
#                            str_detect(name, ">=1") & str_detect(p, "0.04") ~ "+",
#                            str_detect(name, ">=1") & str_detect(p, "0.") ~ "n.s.",
                           # TRUE ~ "")) 
test2 <- test %>%
  select(-starts_with("Compar")) %>%
  left_join(test_ps2, by = c("variable", "name"))

plot_1 <- ggplot(test2 %>% filter((variable == "Percent Hispanic" | str_detect(variable, "deprived"))), 
                 aes(x = name1, 
                     y = value, 
                     label = p_fig,
                     fill = interaction(name1, name2))) + 
  geom_col() + 
  geom_text(vjust = -0.25) + 
  facet_grid(variable~name2, 
             scales = 'free', 
             space = 'free_x', 
             switch = "both") + 
  scale_y_continuous(expand = expansion(add = c(0, 5))) + 
  scale_fill_manual(name = "", values = c("#9bccc6", "#50a6a6",'#c2a5cf', '#7b3294')) + 
  theme_classic() + 
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white", 
                                        color = 'white'),  # Make facet label background white.
        axis.title = element_blank(), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 13),
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14))

plot_2 <- ggplot(test2 %>% filter(!(variable == "Percent Hispanic" | str_detect(variable, "deprived"))), 
                 aes(x = name1, 
                     y = value, 
                     label = p_fig,
                     fill = interaction(name1, name2))) + 
  geom_col() + 
  geom_text(vjust = -0.25) + 
  facet_grid(variable~name2, 
             scales = 'free', 
             space = 'free_x', 
             switch = "both") + 
  scale_y_continuous(expand = expansion(add = c(0, 5))) + 
  scale_fill_manual(name = "", values = c("#9bccc6", "#50a6a6",'#c2a5cf', '#7b3294')) + 
  theme_classic() + 
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white", 
                                        color = 'white'),  # Make facet label background white.
        axis.title = element_blank(), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 13),
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 14))

library(patchwork)
p_final <- plot_1 + plot_2 + 
  plot_annotation(subtitle = "Average demographic profile of counties served by the systems", 
                  theme = theme(plot.subtitle = element_text(size = 16)))
p_final

# ggsave(paste0("results/output/meandemos_", Sys.Date(), ".png"),
#        p_final,
#        device = 'png',
#        width = 7, height = 7, units = 'in')

#write_csv(table3, "results/output/table 3 -- mean county-level demographic characteristics.csv")

table3_supplemental_pov <- 
  table3_detected %>% 
  filter(contam.pfas == ">=1 UCMR Detection") %>%
  ungroup() %>%
  select(-contam.pfas) %>% 
  left_join(table3_hlvlchem %>% 
              filter(contam.pfas == ">=1 UCMR Detection") %>% 
              select(-contam.pfas)) %>% 
  filter(variable %in% c("Percent deprived","Percent uninsured", "Percent poverty", "Percent home ownership"))


#write_csv(table3_supplemental_pov, "results/output/table 3 (supplemental pov)-- mean county-level demographic characteristics.csv")

table3_supplemental_chems <- 
  table3_detected %>% 
  filter(contam.pfas != ">=1 UCMR Detection") %>%
  ungroup() %>%
  left_join(table3_hlvlchem %>% 
              filter(contam.pfas != ">=1 UCMR Detection")) %>% 
  filter(!variable %in% c("Percent uninsured", "Percent poverty", "Percent home ownership"))

#write_csv(table3_supplemental_chems, "results/output/table 3 (supplemental chems)-- mean county-level demographic characteristics.csv")


table3_detected_size <-
  ucmrdf.plot %>%
  mean_calc_detected() %>% 
  unequal_var_calc() %>% 
  table3_detected_format()

table3_hlvlchem_size <-
  ucmrdf.plot %>%
  mean_calc_hlvl() %>% 
  unequal_var_calc() %>% 
  table3_hlvl_format()

table3_size <- table3_detected_size %>% 
  left_join(table3_hlvlchem_size %>% 
              filter(contam.pfas == ">=1 UCMR Detection") %>% 
              select(-contam.pfas)) %>% 
  filter(!variable %in% c("Percent uninsured", "Percent poverty", "Percent home ownership"))

