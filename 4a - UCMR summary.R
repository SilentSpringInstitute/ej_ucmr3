### AUTHOR: AHz, JL
### LAST EDIT: 2021-08-09
### LAST REVIEW: 2021-07-06
### REVIEWED: 2023-03-05 (AM)
### WRITTEN IN: R version 3.5.1
### Purpose: Merge UCMR data with demographic data, get prelim tables/figures
### LAST UPDATED: 2023-03-11 AM: Updated Fig 1

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

### !!! START HERE !!! #######

## IF YOU'RE STARTING FROM SCRIPT 4a -- UNCOMMENT AND START HERE
# 
source("1 - UCMR loading and processing.R")
source("Janet functions May 2015_AH.r")
detach_all()

## IF YOU'VE ALREADY RUN SCRIPTS 1-3, UNCOMMENT AND START HERE

# source("Janet functions May 2015_AH.r")
# detach_all()

library(tidyverse)
library(broom)
library(measurements)
library(lubridate)
library(ggforce)
library(ggsignif)
library(cowplot)

options(stringsAsFactors = FALSE)

################################################################################
#  0. README  ###############################################################
################################################################################

### GOAL 
#
#     Merge demographic information with PWS information. Calculate new 
#     demographic values for PWSs that serve >1 county.   
#     
### CODE DICTIONARY 
#     #_# Code outputs something that is referenced in paper
#     #~# Code outputs something that is currently in slide deck (often the same
#     as what is in the paper)
#     
### DATA DICTIONARY
# 
#     contam.pfas == column created to assign “PFAS” to any of the 6 PFAS 
#                   contaminants and leaves all other contaminant names the same 
#     detchem == binary column indicates whether chemical was ever detected (1 = detected) 
#     hlvlchem == binary column indicates whether chemical was ever measured above health 
#                 level guideline (1 = exceeded)
#     evrdet == variable in the contam.pfas column. this "chemical" was created to 
#                 represent "any target contaminant"
#                 
#                 
### NOTES
#
# SECTION 2a. 
#
# Create ucmrdf.demo, which brings in the PWSID to the demo county info and then 
# converts variables to the right units and binary categories (present/not present). 
# Create ucmrdf.demo_detcode, which does a full join on ucmrdf.demo (all the demo info for 
# each PWSID) and ucmr_detcode (all the ucmr 3 testing data). 
#
# SECTION 4a. 
# 
# I'm creating a mock data frame that is going to give us a row for 
# each UCMR chemical and each TRI Chem (n = 8, 1,4-dioxane, 1,1,1-trichloroethane, 
# 1,1-dichloroethane, HCFC-22, CFC-12, CFCs, chloronated solvents, and "bin_TRI" 
# (any TRI facility with a release of one of those 7 chems)) with each 
# PWSID so the # of rows = ~4815 (# of PWSIDs) * 8 (# of TRI rows) * 5 (# of UCMR rows)
# the difference is bc there are different #s of PWSIDs for each UCMR chem. 
#
# SECTION 6
#
# Create ucmrdf.plot, which is just for use locally in this script. It just cleans
# up all the names and turns things into factors so that the ordering stays the 
# same for all the plots. 


# ################################################################################
# # 3. SDWA violation data ####
# ################################################################################
# colnames(sdwa_violations)
# unique(sdwa_violations$PWS_TYPE_CODE)
# unique(sdwa_violations$PRIMARY_SOURCE_CODE) 
# # [1] "SW"  "GW"  "GWP" "SWP" "GU"  NA    "GUP" 
# # GWP = purchased groundwater
# # SWP = purchased SW 
# # GU = GWUDI
# 
# 
# tab <- sdwa_violations %>%
#   filter(IS_HEALTH_BASED_IND == "Y") %>%
#   filter(PWS_TYPE_CODE != "TNCWS") %>%
#   mutate(SIZE = ifelse(POPULATION_SERVED_COUNT >= 10000, "L", "S")) %>%
#   count(PWSID, PWS_TYPE_CODE, SIZE, PRIMARY_SOURCE_CODE)
# 
# table(tab$SIZE)
# table(tab$PWS_TYPE_CODE)
# table(tab$PRIMARY_SOURCE_CODE)
# table(tab$PRIMARY_SOURCE_CODE, tab$SIZE)
# 
# dat <- data.frame(cat = c("vsma", "sma", "med", "la", "vl"), 
#                   hbviol = c(3018, 1365, 530, 343, 16), 
#                   total = c(27607, 13358, 4832, 3714, 410))
# dat %>% mutate(freq = hbviol/total, cOR = freq/0.10932010)
# sdwa_healthviolations_ucmr <- sdwa_violations %>% 
#   filter(PWSID %in% ucmrsys) %>% 
#   filter(IS_HEALTH_BASED_IND == "Y") 
# 
# 
#  
# library(lubridate)
# sdwa_healthviolations_ucmr_summary <- sdwa_healthviolations_ucmr %>% 
#   mutate(start_year = year(dmy(COMPL_PER_BEGIN_DATE)),
#          end_year = year(dmy(COMPL_PER_END_DATE))) %>% 
#   filter(start_year >= 2013) %>% 
#   group_by(PWSID, start_year, end_year, CATEGORY_CODE) %>% 
#   summarize(n_violations = n(),
#             n_contaminants = length(unique(CONTAMINANT_CODE))) 
# 
# #MCL -- Maximum Contaminant Level
# #MRDL -- Maximum Residual Disinfectant Level
# #TT -- Treatment Technique Violations 
# #M/R -- Monitoring and Reporting Violations 
# table(sdwa_violations$CATEGORY_CODE, sdwa_violations$IS_HEALTH_BASED_IND)


################################################################################
#  4a. PROCESS TRI DATA ####
################################################################################



ucmr_tri_dummy_rows <-  unique(ucmr_detcode[,c("PWSID", "contam.pfas")]) %>%
  mutate(test_chem = paste("ETHYLIDENE DICHLORIDE", "1,4-DIOXANE", "CHLORINATED SOLVENTS", 
                           "1,1,1-TRICHLOROETHANE", "CHLORODIFLUOROMETHANE", 
                           "DICHLORODIFLUOROMETHANE", "CFCs", "bin_TRI",sep = "  ")) %>% 
  separate_rows(test_chem, sep = "  ")
#should be 192,464

# summarize completeness of tri data
tri_completion <- ucmrdf.demo_detcode %>%
  group_by(PWSID, test_chem) %>%
  summarize(n = length(unique(test_chem[which(!is.na(reporting_year))]))) %>%
  mutate(hasTRI = case_when(n == 0 ~ "N",
                            n == 1 ~ "Y")) %>%
  select(-n)

################################################################################
#  5a. MERGE IT ALL TOGETHER ####
################################################################################

#merge demo data and test code (one row per system-contam.pfas pairing)
ucmrdf.all <- ucmr_detcode %>% 
  full_join(ucmr_tri_dummy_rows) %>% 
  #filter(contam.pfas != "PFAS" ) %>% 
  left_join(ucmrdf.demo) %>% 
  left_join(tri_completion) #%>% 
  # left_join(sdwa_healthviolations_ucmr_summary %>% 
  #             ungroup() %>%
  #             mutate(hasviolation = "Y") %>% 
  #             select(PWSID, hasviolation) %>% 
  #             unique()) %>% 
  # mutate(hasviolation = case_when(is.na(hasviolation) ~ "N",
  #                                 TRUE ~ hasviolation)) 

################################################################################
#  5b. RUN SOME CHECKS ####
################################################################################


check <-  ucmrdf.all %>%
  group_by(PWSID, contam.pfas) %>%
  count(sort = TRUE)
# all have 8 test_chem rows
#table(check$n, check$contam.pfas, useNA = "ifany")
stopifnot(check$n == 8)


checkcols_list <- list()
checkcols_df <- data.frame()


for(i in keepcols){
  checkcols_list[i] <- ifelse(length(unique(ucmrdf.all$PWSID[which(is.na(ucmrdf.all[,i]))])) != 0, 
                              paste(unique(i)), 
                              NA) 
}


checkcols_df <- bind_rows(checkcols_list) %>% 
  pivot_longer(names_to = "columns", values_to = "flag", everything()) %>% 
  filter(!is.na(flag))
table(checkcols_df$flag)

#this check requires a line in 3a that sets multicn.pwsids
# table(ucmrdf.clean$PWSID %in% multicn.pwsids, is.na(ucmrdf.clean$GEO.id2))
#GEO.id2 and geography are empty for PWSIDs that serve multiple counties


length(unique(ucmrdf.all$GEO.id2[which(is.na(ucmrdf.all$mdi))]))
ucmrdf.all %>% filter(is.na(mdi)) %>% group_by(PWSID, GEO.id2) %>% count()

#get PWSIDs of missing MDI counties
missing_mdi <- ucmrdf.all %>% 
  filter(is.na(mdi)) %>% 
  pull(PWSID) %>%  
  unique()

ucmrdf.clean <- ucmrdf.all %>% 
  filter(!PWSID %in% missing_mdi)


# stop source code in 4b
# stop("data loaded from 4a - UCMR summary")


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
#   if (!require("qwraps2")) install.packages("qwraps2")
#   library(qwraps2) # table package I often use, makes things easier in RMarkdown
#   
#   # test data
#   JML.ucmr.test <- ucmrdf.clean_summ %>% select(PWSID, WS.POPULATION_SERVED_COUNT, Size,
#                                                 perc_hisp_any, hlvlchem, source_type, contam.pfas,
#                                                 detchem, n_det) %>%
#     filter(contam.pfas == "evrdet")
#   
#   # make table
#   summary_stats <-
#     list (
#       list("n.ppl" = ~sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
#          "n.sys" = ~n_distinct(PWSID),
#          "perc.small" = ~n_perc(Size == "S"),
#          "perc.Hisp" = ~median_iqr(perc_hisp_any),
#          "perc.detchem" = ~n_perc(detchem)
#     )
#   )
#   
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
                             labels = c("Percent Hispanic", "Percent Black","Percent deprived", 
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

table3_long2 <- filter(table3_long, short_name != "Overall")
table3_long2 <- table3_long2 %>% mutate(short_name = factor(short_name, 
                                           levels = c("Not detected", 
                                                      "Detected", 
                                                      "Not exceeded", 
                                                      "Exceeded")))
table3_long2$`Comparison between groups (hlvlchem)` %>% unique()
table3_long3 <- table3_long2 %>%
  mutate(p1 = case_when(str_detect(`Comparison between groups`, "0.041") ~ "*", 
                        str_detect(`Comparison between groups`, "0.491") ~ "n.s.", 
                        str_detect(`Comparison between groups`, "<0.001") ~ "***"), 
         p2 = case_when(str_detect(`Comparison between groups (hlvlchem)`, "0.573") ~ "n.s.", 
                        str_detect(`Comparison between groups (hlvlchem)`, "0.895") ~ "n.s.", 
                        str_detect(`Comparison between groups (hlvlchem)`, "<0.001") ~ "***"))


library(ggh4x)

test <- table3_long2 %>%
  mutate(name1 = ifelse(str_detect(short_name, "Not"), "0", "≥1"), 
         name1 = factor(name1, levels = c("0", "≥1")), 
         name2 = ifelse(str_detect(short_name, "etected"), 
                        "Detected a \ntarget contaminant", 
                        "Exceeded a \nhealth-reference level"))

test_ps <- test %>% select(variable, name, starts_with("Comparison"))
test_ps2 <- test_ps %>% 
  pivot_longer(cols = starts_with("Comp"), 
               names_to = "test", 
               values_to = "p") %>% 
  mutate(help = case_when(str_detect(test, "hlvlchem") & str_detect(name, "detect") ~ "rm", 
                          str_detect(name, "exceeding") & !str_detect(test, "hlvlchem") ~ "rm", 
                          TRUE ~ "keep")) %>%
  filter(help=='keep') %>%
  select(-help) %>% 
  mutate(p_fig = case_when(str_detect(name, ">=1") & str_detect(p, "<") ~ "***", 
                           str_detect(name, ">=1") & str_detect(p, "0.04") ~ "+",
                           str_detect(name, ">=1") & str_detect(p, "0.") ~ "n.s.",
                           TRUE ~ "")) 
test2 <- test %>%
  select(-starts_with("Compar")) %>%
  left_join(test_ps2, by = c("variable", "name"))

plot_1 <- ggplot(test2 %>% filter(str_detect(variable, "deprived|Hispanic")), 
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
        legend.position = 'none')

plot_2 <- ggplot(test2 %>% filter(!str_detect(variable, "deprived|Hispanic")), 
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
        legend.position = 'none')

library(patchwork)
p_final <- plot_1 + plot_2  
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

################ ARCHIVED BY AM ON 2023-03-11 ##################################

# # 
# # ggplot(test, 
# #        aes(x = interaction(name1, name2), 
# #            y = value, 
# #            fill = interaction(name1, name2), 
# #            group = interaction(name1, name2))) + 
# #   geom_bar(stat = 'identity', 
# #            #position = position_dodge(0.5), 
# #            width = 0.5) + 
# #   theme(#axis.title = element_blank(),
# #     #axis.text.x = element_blank(),
# #     axis.title.x = element_text(size = 13),
# #     axis.text.y = element_text(size = 8),
# #     legend.text =  element_text(size = 14),
# #     axis.ticks=element_blank(),
# #     # axis.line=element_blank(),
# #     strip.background = element_blank(),
# #     strip.text = element_text(size = 14),
# #     strip.text.y.left = element_text(angle = 0),
# #     legend.position = "none")
# # 
# # ggplot(test, 
# #        aes(x = name2, 
# #            y = value, 
# #            fill = interaction(name1, name2))) + 
# #   geom_bar(stat = 'identity', position = position_dodge(), width = 0.7) + 
# #   facet_wrap(~variable, scales = 'free_y', 
# #              strip.position = 'left', 
# #              labeller = as_labeller(c(`Percent Hispanic` = "Percent Hispanic",
# #                                       `Percent Black` = "Percent Black", 
# #                                       `Percent deprived` = "Percent Deprived", 
# #                                       `Percent urban households` = "Percent Urbanicity") ) )  +
# #   scale_fill_manual(name = "", 
# #                     values = c("#9bccc6", "#50a6a6",'#c2a5cf', '#7b3294')) + 
# #   scale_x_discrete(guide = "axis_nested") + 
# #   theme(strip.placement = "outside",
# #         strip.text.y.left =  element_text(angle = 90,vjust = 1))
# # 
# # facet_wrap(~Unit, scales = "free_y", nrow = 2, 
# #            strip.position = "left", 
# #            labeller = as_labeller(c(A = "Currents (A)", V = "Voltage (V)") ) )  +
# # 
# #   scale_fill_manual(name = "", 
# #                     #values = c('#7b3294','#c2a5cf','#a6dba0','#008837'), 
# #                     values = c("#9bccc6", "#50a6a6",'#c2a5cf', '#7b3294')) + 
# #   theme_classic() + 
# #   scale_y_continuous(expand = expansion(add = c(0, 5)),
# #                      position = "left") + 
# #   scale_x_discrete(guide = "axis_nested") + 
# #   facet_wrap(~variable, 
# #              ncol = 2,
# #              scale = 'free') + 
# #   labs(y = "Average value among service counties", x = "") +
# #   theme(#axis.title = element_blank(),
# #         #axis.text.x = element_blank(),
# #         axis.title.x = element_text(size = 13),
# #         axis.text.y = element_text(size = 8),
# #         legend.text =  element_text(size = 14),
# #         axis.ticks=element_blank(),
# #         # axis.line=element_blank(),
# #         strip.background = element_blank(),
# #         strip.text = element_text(size = 14),
# #         strip.text.y.left = element_text(angle = 0),
# #         legend.position = "none") + 
# #   guides(fill = guide_legend(nrow=2,byrow=TRUE))
# # # 
# # # + 
# # #   geom_signif(aes(xmin = "No", xmax = "Yes", 
# # #       y_position = c(18, 18, 12, 12, 15, 15, 75, 75), 
# # #       annotations = p1), 
# # #   data = filter(table3_long3, short_name %in% c("Not detected", "Detected")) %>%
# # #     distinct(variable, p1, short_name), 
# # #   manual = TRUE, tip_length = 0.01) + 
# # #   geom_signif(
# # #     aes(xmin = "Not exceeded", xmax = "Exceeded", 
# # #         y_position = c(19, 19, 12, 12, 15, 15, 75, 75), 
# # #         annotations = `p2`), 
# # #     data = filter(table3_long3, short_name %in% c("Not exceeded", "Exceeded")) %>%
# # #       distinct(variable, p2, short_name), 
# # #     manual = TRUE, tip_length = 0.01)
# # 
# # p_final
# 
# ################ ARCHIVED BY AM ON 2023-03-05 ##################################
# 
# #write_csv(table3_size, "results/output/table 3 (stratified by size)-- mean county-level demographic characteristics.csv")
# 
# #### JML checks: checking t.test results with stats::t.test() function
#   # note: stats::t.test() assumes unequal variances
#   # quick check for perc_hisp_any
# # ucmrdf.plot %>% group_by(contam.pfas) %>%
# #   filter(test_chem == ">=1 TRI facility") %>%
# #   summarise(p.value = t.test(perc_hisp_any~detchem)$p.value,
# #             estimate = t.test(perc_hisp_any~detchem)$estimate) %>%
# #   ungroup() %>% View()
# # 
# # ucmrdf.plot %>% group_by(contam.pfas) %>%
# #   filter(test_chem == ">=1 TRI facility" &
# #         contam.pfas != "1,1-dichloroethane" &
# #         contam.pfas != "HCFC-22") %>%
# #   summarise(p.value = t.test(perc_uninsur~hlvlchem)$p.value,
# #             estimate = t.test(perc_uninsur~hlvlchem)$estimate) %>%
# #   ungroup() %>% View()
# ##### all good!
# 
# # ################################################################################
# # #  For paper  ####
# # ################################################################################
# # 
# # all_brkdwn2 <- all_brkdwn %>%
# #   left_join(size_detfreq %>% 
# #               bind_rows(source_detfreq) %>%
# #               bind_rows(overall_detfreq) %>%
# #               select(n.sys, n.ppl, det_freq, hlvl_freq),
# #             by = c('n.sys', 'n.ppl'))
# # 
# # Table1_key <- data.frame(colname = colnames(all_brkdwn2))
# # 
# # Table1_formatted <- all_brkdwn2 %>% 
# #   filter(!str_detect(brktype, "SDWA")) %>% 
# #   mutate(brktype2 = case_when(str_detect(brktype, "Detected") ~ "Detected a target contaminant", 
# #                               str_detect(brktype, "Over federal") ~ "Exceeded a health-reference level", 
# #                               str_detect(brktype, "System Size") ~ "System Size", 
# #                               str_detect(brktype, "Source Water") ~ "Source Water", 
# #                               TRUE ~ "Overall")) %>%
# #   mutate(n.ppl = round(n.ppl/1e6, 1)) %>%
# #   na_if(0) %>%
# #   na_if(100) %>%
# #   mutate_at(c("det_freq", "hlvl_freq"), round, 1) %>% 
# #   mutate(brktype = case_when(str_detect(brktype, "0") ~ "No", 
# #                              str_detect(brktype, "1-") ~ "Yes", 
# #                              str_detect(brktype, "(L)") ~ "Large", 
# #                              brktype == "System Size (S)" ~ "Small", 
# #                              str_detect(brktype, "(GW)") ~ "GW", 
# #                              str_detect(brktype, "(MIX)") ~ "MIX", 
# #                              str_detect(brktype, "SW") ~ "SW", 
# #                              str_detect(brktype, "Overall") ~ "")) %>%
# #   select(brktype2, brktype, n.sys, n.ppl, 
# #          det_freq, hlvl_freq, everything(), 
# #          -contains("Poverty"), -contains("Uninsured"), 
# #          -contains("Homeownersip")) %>%
# #   flextable() %>%
# #   theme_vanilla() %>%
# #   autofit() %>%
# #   merge_v(j = 1) %>%
# #   colformat_num(na_str = "--") %>%
# #   set_header_labels(brktype2 = "", 
# #                     brktype = "", 
# #                     n.sys = "Number of PWS", 
# #                     `Percent small PWSs` = "Small system (%)", 
# #                     `Percent GW Systems` = "GW system (%)", 
# #                     `Percent SW Systems` = "SW system (%)", 
# #                     `Median MDI Rate (Q1, Q3)` = "Median Depravity (Q1, Q3)",
# #                     n.ppl = "Population served (in millions)", 
# #                     det_freq = "% of systems that detected a target contaminant", 
# #                     hlvl_freq = "% of systems that exceeded a HRL") %>%
# #   width(j = 1, width = 1)  %>%
# #   width(j = 2:13, width = 1)  %>%
# #   align(align = 'center', j = 2:13, part = 'all')
# # 
# # Table2_key <- data.frame(contam.pfas = c("1,4-dioxane", "1,1-dichloroethane", 
# #                            "HCFC-22", "PFAS", "PFOA", "PFOS", "PFHpA", 
# #                            "PFHxS", "PFNA", "PFBS")) %>%
# #   mutate(reporting.limit = case_when(contam.pfas == "1,4-dioxane" ~ 70, 
# #                                      contam.pfas == "1,1-dichloroethane" ~ 30, 
# #                                      contam.pfas == "HCFC-22" ~ 80, 
# #                                      contam.pfas == "PFAS" ~ as.numeric(NA), 
# #                                      contam.pfas == "PFOA" ~ 20, 
# #                                      contam.pfas == "PFOS" ~ 40, 
# #                                      contam.pfas == "PFHpA" ~ 10, 
# #                                      contam.pfas == "PFHxS" ~ 30, 
# #                                      contam.pfas == "PFNA" ~ 20, 
# #                                      contam.pfas == "PFBS" ~ 90), 
# #          health.reference.level = case_when(contam.pfas == "1,4-dioxane" ~ 350, 
# #                                             contam.pfas == "1,1-dichloroethane" ~ 6140, 
# #                                             str_detect(contam.pfas, "PFOA|PFOS") ~ 70, 
# #                                             TRUE ~ as.numeric(NA)),
# #          common.sources = case_when(contam.pfas == "1,4-dioxane" ~ "Solvent production, consumer products", 
# #                                     contam.pfas == "1,1-dichloroethane" ~ "Solvent production", 
# #                                     contam.pfas == "HCFC-22" ~ "Refrigerant, low-temperature solvent, and in fluorocarbon resins", 
# #                                     str_detect(contam.pfas, "PF") ~ "Firefighting foams, consumer products, fluoropolymer coatings"))
# # Table2_formatted <- ucmr_sum %>%
# #   filter(contam.pfas!="evrdet") %>%
# #   left_join(Table2_key, by = "contam.pfas") %>%
# #   mutate(hlvl_freq = case_when(str_detect(contam.pfas, "PFOA|PFOS") ~ hlvl_freq[contam.pfas=="PFAS"],
# #                                str_detect(contam.pfas, "PFAS") ~ as.numeric(NA),
# #                                TRUE ~ hlvl_freq)) %>%
# #   na_if(0) %>%
# #   select(contam.pfas, reporting.limit, 
# #          detfreq_samps, detfreq_sys,
# #          health.reference.level, 
# #          hlvl_freq, 
# #          common.sources) %>%
# #   flextable() %>%
# #   colformat_num(na_str = "--") %>%
# #   set_header_labels(contam.pfas = "Contaminant", 
# #                     detfreq_samps = "Sample detection frequency (%)", 
# #                     detfreq_sys = "PWS detection frequency (%)", 
# #                     hlvl_freq = "% of PWS exceeding a health-reference level") %>%
# #   merge_v(j = 7) %>%
# #   merge_at(i = 5:6, j = 5) %>%
# #   merge_at(i = 5:6, j = 6) %>%
# #   border_remove() %>%
# #   theme_vanilla() %>%
# #   padding(i = 5:10, j = 1, padding.left = 20) %>% 
# #   set_header_labels(reporting.limit = "Reporting limit (ng/L)", 
# #                     health.reference.level = "Health-reference level (ng/L)", 
# #                     common.sources = "Common sources") %>%
# #   footnote(i=1, j=3, value = as_paragraph(c(" Percent of samples tested that contained each contaminant")), 
# #            ref_symbols = c("a"), part = "header", inline = FALSE) %>% 
# #   footnote(i=1, j=4, value = as_paragraph(c(" Percent of PWSs tested that contained each contaminant")), 
# #            ref_symbols = c("b"), part = "header", inline = FALSE) %>% 
# #   footnote(i=1, j=5, value = as_paragraph(c(" US EPA health-reference level (HRL)")), 
# #          ref_symbols = c("c"), part = "header", inline = FALSE) %>%
# #   footnote(i=1, j=6, value = as_paragraph(c(" Percent of PWSs with at least one HRL exceedance")), 
# #            ref_symbols = c("d"), part = "header", inline = FALSE) %>%
# #   autofit() %>%
# #   width(width = 2) %>%
# #   width(j = 1, width = 1)  %>%
# #   align(j = 2:7, align = 'center', part = 'all')
# # 
# # Table3_formatted <- table3 %>%
# #   flextable() %>%
# #   autofit() %>%
# #   add_header_row(
# #     top = TRUE, 
# #     values = c("Demographic variable", 
# #                "All PWSs", 
# #                "Detected a target contaminant", 
# #                "",
# #                "", 
# #                "Exceeded a health reference level", 
# #                "",
# #                ""
# #                )) %>%
# #   set_header_labels(
# #     variable = "", 
# #     `All PWSs` = "", 
# #     `PWSs with no target contaminants detected` = "No", 
# #     `PWSs with >=1 target contaminant detected` = "Yes", 
# #     `Comparison between groups` = "P-value", 
# #     `PWSs with no target contaminants exceeding federal guideline` = "No", 
# #     `PWSs with >=1 target contaminant exceeding federal guideline` = "Yes", 
# #     `Comparison between groups (hlvlchem)` = "P-value" 
# #   ) %>%
# #   merge_at(i = 1, j = 3:5, part = 'header') %>%
# #   merge_at(i = 1, j = 6:8, part = 'header') %>%
# #   border_remove() %>%
# #   theme_vanilla() %>%
# #   vline(part = 'all', j = 2) %>%
# #   vline(part = 'all', j = 5) %>%
# #   merge_at(i = 1:2, j = 1, part = 'header') %>%
# #   merge_at(i = 1:2, j = 2, part = 'header') %>%
# #   align(., align = 'center', j = c(2:8), part = 'all') %>%
# #   width(j = 1, width = 1)
# # 
# # TableS1_key <- data.frame(colnames = colnames(table3_supplemental_pov)) %>%
# #   mutate(group = case_when(colnames == "variable" ~ "", 
# #                            str_detect(colnames, "All PWSs") ~ "All PWSs", 
# #                            str_detect(colnames, "contaminants detected") ~ "Detected a target contaminant", 
# #                            str_detect(colnames, "exceeding federal") ~ "Exceeded a health reference level" ), 
# #          subgroup = case_when(str_detect(colnames, "no") ~ "No", 
# #                               str_detect(colnames, "with >=1 target") ~ "Yes", 
# #                               str_detect(colnames, "Comparison") ~ "P-value"))
# # 
# # TableS1_format <- table3_supplemental_pov %>% 
# #   flextable() %>%
# #   set_header_df(mapping = TableS1_key, key = "colnames") %>%
# #   theme_vanilla() %>%
# #   autofit() %>%
# #   merge_at(i = 1, j = 3:5, part = 'header') %>%
# #   merge_at(i = 1, j = 6:8, part = 'header') %>%
# #   vline(part = 'all', j = c(1, 2, 5)) %>%
# #   width(j = ~ `Comparison between groups` + `Comparison between groups (hlvlchem)`, 
# #         width = 1) %>%
# #   width(j = c(3, 6), width = 1) %>%
# #   width(j = c(4, 7), width = 1)  %>%
# #   align(., align = 'center', j = c(2:8), part = 'all')
# # 
# # 
# # doc <- read_docx() %>%
# #   body_add_par("Table 1. Characteristics of PWSs in the study sample (n = 4,808) and median demographic values of the counties they serve.") %>%
# #   body_add_par("") %>%
# #   body_add_flextable(value = Table1_formatted, align = "left") %>%
# #   body_add_break() %>%
# #   body_add_par("Table 2. Detection frequencies of UCMR3 target contaminants.") %>%
# #   body_add_par("") %>%
# #   body_add_flextable(value = Table2_formatted, align = "left") %>%
# #   body_add_break() %>%
# #   body_add_par("Table 3. Comparison of mean-county level demographics between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
# #   body_add_par("") %>%
# #   body_add_flextable(value = Table3_formatted, align = "left") %>%
# #   body_add_break() %>%
# #   body_add_par("Table S1. Comparison of mean-county univariate SES indicators between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
# #   body_add_par("") %>%
# #   body_add_flextable(value = TableS1_format, align = "left") %>%
# #   body_add_break() %>%
# #   body_add_par("Table S2. Comparison of PWS detection frequencies between systems in tribes, territories, or in a U.S. state or D.C.") %>%
# #   body_add_par("") %>%
# #   body_add_flextable(value = TableS2_format, align = "left") %>%
# #   body_add_break() %>%
# #   # body_add_par("Table 6. Multiple regression model evaluating associations between participant characteristics and report-type with environmental health knowledge index scores after receiving report-back.") %>%
# #   # body_add_par("") %>%
# #   # body_add_flextable(value = Table6_format, align = "left") %>%
# #   # body_add_break() %>%
# #   # body_add_par("Table 7. Proportion of participants who correctly answered each knowledge question at baseline and after report-back, for questions with a baseline percent correct less than 90 percent. McNemar's test was used to test for differences in proportion correct before and after report-back.") %>%
# #   # body_add_par("") %>%
# #   # body_add_flextable(value = Table7_format, align = "left") %>%
# #   # body_add_break() %>%
# #   # body_add_par("Table S1. Proportion of participants who correctly answered each knowledge question at baseline and after report-back, stratified by participant race. Frequencies for questions about the MyCHDSReport study are restricted to after report-back.") %>%
# #   # body_add_par("") %>%
# #   # body_add_flextable(value = TableS1_format, align = "left") %>%
# #   body_end_section_landscape(w = 8.5, h = 11)
# # 
# # #print(doc, target = paste0("results/output/UCMRsummary_tables_all_", Sys.Date(), ".docx"))  
# # 
# 
# 
# ################################################################################
# #  EXTREMELY BURDENED SYSTEMS  ####
# ################################################################################
# 
# library(ggrepel)
# 
# getcount2 <- function(dat) {
#   dat2 <- dat %>% 
#     filter(contam.pfas != "evrdet") %>% 
#     group_by(PWSID) %>% 
#     summarize(n_det = length(unique(contam.pfas[which(detchem == 1)]))) %>%
#     mutate(n_det = as.character(n_det))
#   
#   dat %>% 
#     filter(contam.pfas == "evrdet") %>% 
#     select(-contam.pfas, detchem, hlvlchem, hasviolation) %>% 
#     unique() %>% 
#     left_join(dat2)
#   
# }
# 
# burdened_system_code <- ucmrdf.clean %>%
#   filter(test_chem == "bin_TRI") %>% 
#   getcount2() %>% 
#   mutate(n_det = case_when(n_det > 1 ~ TRUE,
#                            TRUE ~ FALSE)) %>% 
#   mutate(hlvlchem = case_when(hlvlchem == "1" ~ TRUE,
#                               TRUE ~ FALSE),
#          hasviolation = case_when(hasviolation == "Y" ~ TRUE,
#                                   TRUE ~ FALSE))
# 
# upset_df <- burdened_system_code %>% 
#   mutate_at(vars("hasviolation", "n_det", "hlvlchem"), as.integer) %>% 
#   as.data.frame() %>% 
#   select(PWSID, hasviolation, n_det, hlvlchem, all_of(demo_cols), Size, source_type) %>% 
#   mutate(propurban = propurban*100,
#          Size = case_when(Size == "L" ~ "Large",
#                           TRUE ~ "Small")) %>% 
#   mutate(extr_burden = case_when(n_det == TRUE & hlvlchem == TRUE & hasviolation == TRUE ~ "n_det & hlvlchem & hasviolation",
#                                  n_det == TRUE & hlvlchem == TRUE ~ "n_det & hlvlchem",
#                                  n_det == TRUE & hasviolation == TRUE ~ "n_det & hasviolation",
#                                  n_det == TRUE ~ "n_det", 
#                                  hlvlchem == TRUE & hasviolation ~ "hlvlchem & hasviolation",
#                                  hasviolation == TRUE ~ "hasviolation",
#                                  hlvlchem == TRUE ~ "hlvlchem",
#                                  n_det == FALSE & hlvlchem == FALSE & hasviolation == FALSE ~ "none"))
# # get n in sets
# burd_sets_n <- upset_df %>%
#   group_by(extr_burden) %>% 
#   count %>% 
#   mutate(label_n = paste0("n = ", n)) 
# 
# 
# burd_sets_n_size <- upset_df %>%
#   group_by(extr_burden, Size) %>% 
#   count %>% 
#   mutate(label_n_size = paste0("n = ", n)) 
# 
# burd_sets_n_source <- upset_df %>%
#   group_by(extr_burden, source_type) %>% 
#   count %>% 
#   mutate(label_n_source = paste0("n = ", n)) 
# 
# burd_sets <- upset_df %>% 
#   select(hasviolation, n_det, hlvlchem, extr_burden) %>% 
#   unique() %>%
#   mutate(extr_burden_order = factor(extr_burden, levels = c("none",
#                                                             "n_det",
#                                                             "hlvlchem",
#                                                             "hasviolation",
#                                                             "hlvlchem & hasviolation",
#                                                             "n_det & hlvlchem",
#                                                             "n_det & hasviolation",
#                                                             "n_det & hlvlchem & hasviolation"))) %>% 
#   select(-extr_burden) %>% 
#   arrange((extr_burden_order)) %>% 
#   mutate(position = seq_along(1:nrow(.))) %>% 
#   pivot_longer(names_to = "burd_group", values_to = "in_set", c(-position, -extr_burden_order)) %>% 
#   mutate(burd_group = factor(burd_group, levels = c("n_det", "hlvlchem", "hasviolation"),
#                              labels = c("2+ target \nchemicals detected", 
#                                         "1+ target chemical \nabove a federal guideline",
#                                         "1+ health-based \nSDWA violation"))) %>% 
#   left_join(burd_sets_n, by = c("extr_burden_order" = "extr_burden")) %>% 
#   mutate(position = factor(position, levels = c(unique(position)), labels = c(unique(label_n))))
# 
# 
# 
# ovrall_line <- ucmrdf.clean_summ %>%
#   filter(contam.pfas == "evrdet") %>%
#   group_by(.) %>% 
#   summarize(`Median Percent Hispanic` = median(perc_hisp_any),
#             `Median Percent Black` = median(perc_black_nohisp)) %>%
#   pivot_longer(names_to = "variable", values_to = "median_val", everything()) %>% 
#   mutate(variable = factor(variable, labels = c("Median Percent Hispanic", "Median Percent Black"),
#                            levels = c("Median Percent Hispanic", "Median Percent Black")),
#          variable = fct_rev(variable))
# 
# p1 <- ggplot(burd_sets) + 
#   geom_point(aes(x = position, y = burd_group, shape = as.factor(in_set), color = as.factor(in_set)), size = 30)+
#   scale_color_manual(values = c("#3a3838","#b7b7b7")) +
#   scale_shape_manual(values=c(0, 15)) + 
#   scale_x_discrete(expand = c(0, 1, 0, 2.75)) + 
#   theme_classic() + 
#   theme(axis.title=element_blank(),
#         axis.text.x = element_text(size = 20, face = "italic"),
#         axis.text.y=element_text(size = 25),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         axis.text.x.top = element_text(size = 25),
#         legend.position = "none")
# 
# 
# p2 <- upset_df %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   group_by(extr_burden, variable, Size) %>%
#   summarize(median_val = median(value,na.rm = TRUE)) %>%
#   filter(variable %in% c("perc_hisp_any", "perc_black_nohisp")) %>% 
#   #filter(variable %in% c( "perc_hisp_any", "perc_black_nohisp", "propurban", "mdi")) %>% 
#   mutate(extr_burden = factor(extr_burden, levels = c("none",
#                                                       "n_det",
#                                                       "hlvlchem",
#                                                       "hasviolation",
#                                                       "hlvlchem & hasviolation",
#                                                       "n_det & hlvlchem",
#                                                       "n_det & hasviolation",
#                                                       "n_det & hlvlchem & hasviolation")),
#          variable = factor(variable, levels = c("perc_hisp_any", "perc_black_nohisp"),
#                            labels = c("Median Percent Hispanic", "Median Percent Black")),
#          variable = fct_rev(variable)) %>%
#   ggplot(aes(y = median_val, x = extr_burden, fill = Size)) + 
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.75) +
#   geom_hline(ovrall_line, mapping = aes(yintercept = median_val),
#              linetype = 2, size = 2, color = "black") +
#   ggtext::geom_richtext(ovrall_line, mapping = aes(y = median_val, x = 8.5,
#                                                    label = paste0("Overall median (", median_val, "%)")),
#                         size = 8,
#                         nudge_y = 0,
#                         nudge_x = 0,
#                         hjust = 0,
#                         na.rm = TRUE, 
#                         fill = "white", label.color = NA) +
#   ggtext::geom_richtext(aes(label=paste0(round(median_val,1), "%"), group = Size), fill = "white",
#                         position = position_dodge(width = 0.9), vjust = -0.25, size = 5,
#                         label.color = NA)+
#   #scale_fill_manual(values = c("#9bccc6", "#50a6a6","#457d7c","#004552")) + 
#   scale_fill_manual(values = c("#d77659", "#9bccc6")) + 
#   scale_y_continuous(limits = c(0, 18), position = "right")+
#   scale_x_discrete(expand = c(0, 1, 0, 2.75)) + 
#   facet_wrap(~variable, nrow = 2, strip.position = c("left")) +
#   theme_classic()+
#   theme(axis.title=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         strip.background = element_blank(),
#         strip.text.y = element_text(size = 25),
#         strip.text.y.left = element_text(angle = 0),
#         legend.position = "top",
#         legend.text = element_text(size = 25),
#         legend.title = element_blank()) 
# 
# 
# plot_grid(p2, p1, rel_heights = c(1, .5), ncol = 1)
# ggsave("results/output/upset-barplot of perc hisp and black for burdened systems by Size.png", width = 20, height = 15, units = "in")
# 
# p2 <- upset_df %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   group_by(extr_burden, variable, source_type) %>%
#   summarize(median_val = median(value,na.rm = TRUE)) %>%
#   filter(variable %in% c("perc_hisp_any", "perc_black_nohisp")) %>% 
#   #filter(variable %in% c( "perc_hisp_any", "perc_black_nohisp", "propurban", "mdi")) %>% 
#   mutate(extr_burden = factor(extr_burden, levels = c("none",
#                                                       "n_det",
#                                                       "hlvlchem",
#                                                       "hasviolation",
#                                                       "hlvlchem & hasviolation",
#                                                       "n_det & hlvlchem",
#                                                       "n_det & hasviolation",
#                                                       "n_det & hlvlchem & hasviolation")),
#          variable = factor(variable, levels = c("perc_hisp_any", "perc_black_nohisp"),
#                            labels = c("Median Percent Hispanic", "Median Percent Black")),
#          variable = fct_rev(variable),
#          source_type = factor(source_type, levels = c("GW", "SW", "MIX"))) %>%
#   ggplot(aes(y = median_val, x = extr_burden, fill = source_type)) + 
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.75) +
#   geom_hline(ovrall_line, mapping = aes(yintercept = median_val),
#              linetype = 2, size = 2, color = "black") +
#   ggtext::geom_richtext(ovrall_line, mapping = aes(y = median_val, x = 8.5,
#                                                    label = paste0("Overall median (", median_val, "%)")),
#                         size = 8,
#                         nudge_y = 0,
#                         nudge_x = 0,
#                         hjust = 0,
#                         na.rm = TRUE, 
#                         fill = "white", label.color = NA) +
#   ggtext::geom_richtext(aes(label=paste0(round(median_val,1), "%"), group = source_type), fill = "white",
#                         position = position_dodge(width = 0.9), vjust = -0.25, size = 5,
#                         label.color = NA)+
#   #scale_fill_manual(values = c("#9bccc6", "#50a6a6","#457d7c","#004552")) + 
#   scale_fill_manual(values = c("#d77659","#457d7c", "#9bccc6")) + 
#   scale_y_continuous(limits = c(0, 30), position = "right")+
#   scale_x_discrete(expand = c(0, 1, 0, 2.75)) + 
#   facet_wrap(~variable, nrow = 2, strip.position = c("left")) +
#   theme_classic()+
#   theme(axis.title=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         strip.background = element_blank(),
#         strip.text.y = element_text(size = 25),
#         strip.text.y.left = element_text(angle = 0),
#         legend.position = "top",
#         legend.text = element_text(size = 25),
#         legend.title = element_blank()) 
# 
# plot_grid(p2, p1, rel_heights = c(1, .5), ncol = 1)
# ggsave("results/output/upset-barplot of perc hisp and black for burdened systems by source type.png", width = 25, height = 15, units = "in")
# 
# 
# 
# stopifnot("reviewed code ends here")
# 
# 
# 
# burdened_system_summary_code <- 
#   burdened_system_code %>% 
#   mutate(extr_burden = case_when(n_det == TRUE & hlvlchem == TRUE & hasviolation == TRUE ~ "n_det & hlvlchem & hasviolation",
#                                  n_det == TRUE & hlvlchem == TRUE ~ "n_det & hlvlchem",
#                                  n_det == TRUE & hasviolation == TRUE ~ "n_det & hasviolation",
#                                  n_det == TRUE ~ "n_det", 
#                                  hlvlchem == TRUE & hasviolation ~ "hlvlchem & hasviolation",
#                                  hasviolation == TRUE ~ "hasviolation",
#                                  hlvlchem == TRUE ~ "hlvlchem",
#                                  n_det == FALSE & hlvlchem == FALSE & hasviolation == FALSE ~ "none"))
# 
# burdened_system_summary <- burdened_system_summary_code %>% 
#   group_by(extr_burden) %>% 
#   demobreakdown() %>% 
#   bind_rows(overall_brkdwn)
# #write_csv(burdened_system_summary, "results/output/burdened system summary.csv")
# 
# 
# burdened_system_plot <- burdened_system_summary_code %>% 
#   bind_rows(ucmrdf.clean %>%
#               select(-hasviolation, -hlvlchem) %>% 
#               filter(test_chem == "bin_TRI") %>% 
#               filter(contam.pfas == "evrdet") %>%
#               mutate(extr_burden = "Systems in UCMR")) %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   mutate(variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
#                               variable == "perc_black_nohisp" ~ "Percent Black",
#                               variable == "perc_pov_ppl" ~ "Percent Poverty",
#                               variable == "perc_hmown" ~  "Percent Homeownership",
#                               variable == "propurban" ~ "Percent Urban Households",
#                               variable == "mdi" ~ "MDI Rate",
#                               variable == "perc_uninsur" ~ "Percent Uninsured",
#                               TRUE ~ variable),
#          value = case_when(variable == "Percent Urban Households" ~ value*100,
#                            TRUE ~ value)) %>% 
#   select(PWSID, variable, value, extr_burden)
# 
# 
# burdened_system_plot_summary <- burdened_system_plot %>% 
#   group_by(extr_burden, variable) %>% 
#   summarize(mean_var = mean(value),
#             median_var = median(value))
# 
# 
# ################################################################################
# # WORK ZONE ######
# ################################################################################
# 
# cr <- ucmrdf.clean_summ %>% 
#   filter(contam.pfas == "evrdet") %>% 
#   group_by(GEO.id2, geography) %>% 
#   summarize(n_small = length(unique(PWSID[which(Size == "S")])),
#             n_small_det = length(unique(PWSID[which(Size == "S" & detchem == "1")])),
#             n_large = length(unique(PWSID[which(Size == "L")])),
#             n_large_det = length(unique(PWSID[which(Size == "L" & detchem == "1")]))
#   ) %>% 
#   mutate(has_both = case_when(n_small > 0 & n_large > 0 ~ "Y",
#                              TRUE ~ "N"),
#          det_both = case_when(has_both == "N" ~ "N",
#                               n_small_det > 0 & n_large_det > 0 ~ "Y",
#                               n_small_det > 0 & n_large_det == 0 ~ "Small Sys Det",
#                               n_small_det == 0 & n_large_det > 0 ~ "Large Sys Det",
#                               ))
# 
# table(cr$has_both, cr$det_both)
# 
# federal_owned <- ucmrdf.clean_summ %>% 
#   filter(WS.OWNER_TYPE_CODE == "F")
# 
# ################################################################################
# # MDI vs POV ######
# ################################################################################
# 
# # ggplot(ucmrdf.clean_summ, aes(x = perc_pov_ppl, y = mdi)) + geom_point() + facet_wrap(~contam.pfas)
# # ggsave("results/output/perc poverty and mdi scatterplot.png", width = 15, height = 10, units = "in")
# 
# ################################################################################
# # 6. NUMBER OF DETECTS SINA PLOT (FOR PAPER) ######
# ################################################################################
# 
# # 
# # n_det_per_system <- ucmrdf.clean %>% 
# #   filter(test_chem == "bin_TRI") %>% 
# #   select(PWSID, contam.pfas, detchem, all_of(demo_cols), hlvlchem, evrovrhlvl) %>% 
# #   getcount() %>% 
# #   select(-detchem) %>%
# #   unique() %>%
# #   bind_rows(ucmrdf.clean %>% filter(test_chem == "bin_TRI") %>% filter(contam.pfas == "evrdet") %>% mutate(n_det = "Systems in UCMR")) %>% 
# #   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
# #   mutate(variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
# #                               variable == "perc_black_nohisp" ~ "Percent Black",
# #                               variable == "perc_pov_ppl" ~ "Percent Poverty",
# #                               variable == "perc_hmown" ~  "Percent Homeownership",
# #                               variable == "propurban" ~ "Percent Urban Households",
# #                               variable == "mdi" ~ "MDI Rate",
# #                               variable == "perc_uninsur" ~ "Percent Uninsured",
# #                               TRUE ~ variable),
# #          value = case_when(variable == "Percent Urban Households" ~ value*100,
# #                            TRUE ~ value)) %>% 
# #   mutate(n_det_num = as.numeric(n_det),
# #          n_det = factor(n_det, levels = c("Systems in UCMR", "Any detected UCMR", "0", "1", "2", "3", "4"),
# #                         labels = c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical", "0 Detects","1 Detect",
# #                                    "2 Detects", "3 Detects", "4 Detects"))) %>% 
# #   select(PWSID, variable, value, n_det, n_det_num)
# # 
# # 
# # n_det_summary <- n_det_per_system %>%
# #   #filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>%
# #   group_by(n_det, variable) %>%
# #   summarize(median = median(value, na.rm = TRUE),
# #             mean = mean(value, na.rm = TRUE)) %>%
# #   mutate(fill_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ "0",
# #                                 TRUE ~ "1"))
# # 
# # 
# # n_det_per_system %>% 
# #   filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>% 
# #   mutate(variable = factor(variable, levels = c("Percent Hispanic", "Percent Black", "MDI Rate", "Percent Urban Households"), ordered = TRUE)) %>% 
# #   mutate(color_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ as.character(n_det))) %>% 
# #   ggplot() + 
# #   geom_sina(aes(x = as.factor(n_det), y = value, color = color_group)) + 
# #   geom_boxplot(aes(x = as.factor(n_det), y = value),width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") +
# #   geom_line(n_det_summary %>% 
# #               filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")) %>% 
# #               filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")), 
# #             mapping = aes(x = as.factor(n_det), y = mean), group = 1, color = "#666666", size = 1) + 
# #   geom_point(n_det_summary%>% 
# #                filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")),
# #              mapping = aes(x = as.factor(n_det), y = mean), fill = "#ab4028", shape = 23, size = 3) + 
# #   xlab("") + 
# #   ylab("") + 
# #   scale_color_manual(na.value = "#d77659", values = c("#9bccc6", "#50a6a6", "#457d7c", "#004552", "#1a3438")) + 
# #   theme_minimal() + 
# #   ylim(0, 100) + 
# #   theme(axis.text = element_text(size=14),
# #         strip.text = element_text(size = 16, face = "bold"),
# #         legend.position = "none")+
# #   facet_wrap(~as.factor(variable))
# # 
# # # ggsave("results/output/sina-boxplot of systems by number of detected UCMR chemicals.png", width = 20, height = 12.55, units = "in")
# # 
# # n_det_per_system %>% 
# #   filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "MDI Rate"))  %>%
# #   #mutate(variable = factor(variable, levels = c("Percent Hispanic", "Percent Black", "MDI Rate", "Percent Urban Households"), ordered = TRUE)) %>% 
# #   mutate(color_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ as.character(n_det))) %>% 
# #   ggplot() + 
# #   geom_sina(aes(x = as.factor(n_det), y = value, color = color_group)) + 
# #   geom_boxplot(aes(x = as.factor(n_det), y = value),width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") +
# #   geom_line(n_det_summary %>% 
# #               filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")) %>% 
# #               filter(variable %in% c("MDI Rate", "Percent Uninsured", "Percent Poverty", "Percent Homeownership")), 
# #             mapping = aes(x = as.factor(n_det), y = mean), group = 1, color = "#666666", size = 1) + 
# #   geom_point(n_det_summary%>% 
# #                filter(variable %in% c("MDI Rate", "Percent Uninsured", "Percent Poverty", "Percent Homeownership")), 
# #              mapping = aes(x = as.factor(n_det), y = mean), fill = "#ab4028", shape = 23, size = 3) + 
# #   xlab("") + 
# #   ylab("") + 
# #   scale_color_manual(na.value = "#d77659", values = c("#9bccc6", "#50a6a6", "#457d7c", "#004552", "#1a3438")) + 
# #   theme_minimal() + 
# #   ylim(0, 100) + 
# #   theme(axis.text = element_text(size=14),
# #         strip.text = element_text(size = 16, face = "bold"),
# #         legend.position = "none")+
# #   facet_wrap(~as.factor(variable))
# 
# 
# #ggsave("results/output/sina-boxplot of systems by number of detected UCMR chemicals (pov variables only).png", width = 20, height = 12.55, units = "in")
# 
# 
# ################################################################################
# # INVESTIGATE SOURCE TYPE DIFFERENCES ####
# ################################################################################
# 
# ucmrdf.demo_detcode %>%
#   filter(contam.pfas == "evrdet" & test_chem == "bin_TRI") %>%
#   mutate(state = str_extract(pattern = "[A-Z]{2}", string = PWSID)) %>%
#   # group_by(source_type) %>%
#   # count() 
#   group_by(state, source_type) %>%
#   summarize(n = n(),
#             med_hisp = median(perc_hisp_any)) %>%
#   view()
# 
# 
# 
# ################################################################################
# # 8. SUMMARIZE SYSTEMS NOT IN UCMR (NOT READY TO RUN) ####
# ################################################################################
# 
# library(lubridate)
# 
# #requires sdwis county linker from script 2
# sdwis_notucmr <- pwsid_fips %>% 
#   filter(!PWSID %in% ucmrsys) %>% 
#   filter(PWS_ACTIVITY_CODE == "A" | year(dmy(WS.PWS_DEACTIVATION_DATE)) %in% c(2012:2016)) %>% 
#   filter(PWS_TYPE_CODE != "TNCWS") %>% 
#   mutate(sys.size = case_when(WS.POPULATION_SERVED_COUNT < 501 ~ "V. Small",
#                               WS.POPULATION_SERVED_COUNT < 3301 ~ "Small",
#                               WS.POPULATION_SERVED_COUNT < 10001 ~ "Medium", 
#                               WS.POPULATION_SERVED_COUNT < 100001 ~ "Large",
#                               WS.POPULATION_SERVED_COUNT > 100000 ~ "V. Large")) 
# table(sdwis_notucmr$sys.size)
# #some of the large systems are in UCMR4, which means they are now over threshold 
# 
# 
# ucmr4sys <- read_csv("../Data/UCMR4 Data/UCMR4_All.csv") %>% 
#   #select(PWSID, Size, State) %>% 
#   unique() %>% 
#   #mutate(state = substr(PWSID,1,2)) %>% 
#   filter(!(State %in% c("NN", "PR", "01", "02", "05", "06", "08", "09", "10", "GU", "MP", "DC", "VI", "AS")))
# 
# ucmr_sizechange <- ucmr4sys %>% 
#   filter(Size == "L") %>% 
#   filter(!PWSID %in% ucmrsys) 
# 
# 
# sdwis_sizes <- ucmr_sizechange %>% 
#   left_join(pwsid_fips) %>%
#   mutate(sys.size = case_when(WS.POPULATION_SERVED_COUNT < 501 ~ "V. Small",
#                               WS.POPULATION_SERVED_COUNT < 3301 ~ "Small",
#                               WS.POPULATION_SERVED_COUNT < 10001 ~ "Medium", 
#                               WS.POPULATION_SERVED_COUNT < 100001 ~ "Large",
#                               WS.POPULATION_SERVED_COUNT > 100000 ~ "V. Large")) 
# 
# #need to set up script 2 and 3 for non-ucmr systems to get demo data
# 
# 
# pwsdemo_notucmr <- sdwis_notucmr[,c("PWSID", "sys.size", "COUNTY_SERVED")] %>%
#   filter(!is.na(COUNTY_SERVED)) %>% 
#   select(-COUNTY_SERVED) %>%
#   left_join(pws_demo)  %>% 
#   unique() 
# 
# 
# check <- pwsdemo_notucmr %>%
#   group_by(PWSID) %>% 
#   count()
# table(check$n, useNA = "ifany")
# #all 8 -- should be 1 ?
# 
# pws_notucmr_summary <- pwsdemo_notucmr %>% 
#   #filter(test_chem == "PFAS") %>% 
#   #group_by(test_chem) %>% 
#   summarize(
#     n.sys = length(unique(PWSID)),
#     n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
#     #`Percent Small System` = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 1),
#     `Percent GW Systems` = round(length(unique(PWSID[which(WS.GW_SW_CODE == "GW")]))/length(WS.GW_SW_CODE)*100, 1),
#     `Median Percent Hispanic (Q1, Q3)` = paste0(round(median(perc_hisp_any, na.rm = TRUE),1),
#                                                 " (",  round(quantile(perc_hisp_any, .25),1), ", ",
#                                                 round(quantile(perc_hisp_any, .75),1), ")"),
#     `Median Percent Black (Q1, Q3)` = paste0(round(median(perc_black_nohisp, na.rm = TRUE),1),
#                                              " (",  round(quantile(perc_black_nohisp, .25),1), ", ",
#                                              round(quantile(perc_black_nohisp, .75),1), ")"), 
#     `Median Percent Urban (Q1, Q3)` = paste0(round(median(propurban*100, na.rm = TRUE),1),
#                                              " (",  round(quantile(propurban*100, .25),1), ", ",
#                                              round(quantile(propurban*100, .75),1), ")"),
#     `Median MDI Rate (Q1, Q3)` = paste0(round(median(mdi, na.rm = TRUE),1),
#                                         " (",  round(quantile(mdi, na.rm = TRUE, .25),1), ", ",
#                                         round(quantile(mdi, na.rm = TRUE, .75),1), ")"),
#     `Median Percent Poverty (Q1, Q3)` = paste0(round(median(perc_pov_ppl, na.rm = TRUE),1),
#                                                " (",  round(quantile(perc_pov_ppl, .25),1), ", ",
#                                                round(quantile(perc_pov_ppl, .75),1), ")"),
#     `Median Percent Uninsured (Q1, Q3)` = paste0(round(median(perc_uninsur, na.rm = TRUE),1),
#                                                  " (",  round(quantile(perc_uninsur, .25),1), ", ",
#                                                  round(quantile(perc_uninsur, .75),1), ")"),
#     `Median Percent Homeownersip (Q1, Q3)` = paste0(round(median(perc_hmown, na.rm = TRUE),1),
#                                                     " (",  round(quantile(perc_hmown, .25),1), ", ",
#                                                     round(quantile(perc_hmown, .75),1), ")")
#   ) %>% 
#   mutate(brktype = "not in ucmr") %>% 
#   bind_rows(overall_brkdwn) %>% 
#   #bind_rows(sys.size_brkdwn) %>% 
#   mutate(#med.urb = med.urb*100,
#     # Size = case_when(Size == "S" ~ "Small Systems in UCMR",
#     #                  Size == "L" ~ "Large Systems in UCMR"),
#     brktype = case_when(brktype == "not in ucmr" ~ "Systems not in UCMR",
#                         brktype == "overall" ~ "All Systems in UCMR",
#                         TRUE ~ brktype),
#     brktype = fct_recode(brktype)) 
# 
# 
# cr <- pwsdemo_notucmr %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols))
# # 
# #   ggplot(cr, aes(x = variable, y = value)) + 
# #   geom_sina()
# 
# #all_brkdwn
# 
# pws_notucmr_plot <- pws_notucmr_summary %>% 
#   pivot_longer(names_to = "variable", values_to = "value", med.black:med.uninsur) %>% 
#   mutate(variable = case_when(variable == "med.hisp" ~ "Median Percent Hispanic",
#                               variable == "med.black" ~ "Median Percent Black",
#                               variable == "med.pov" ~ "Median Percent Poverty",
#                               variable == "med.hmown" ~  "Median Percent Homeownership",
#                               variable == "med.urb" ~ "Median Percent Urban Households",
#                               #variable == "prop.SW" ~ "Median Percent Surface Water",
#                               variable == "med.mdi" ~ "Median MDI Rate",
#                               variable == "med.uninsur" ~ "Median Percent Uninsured"),
#          brktype = str_wrap(brktype, width = 10))
# 
# ggplot(pws_notucmr_plot, aes(x = brktype, y = value, fill = brktype)) + 
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   geom_text(aes(label=paste(round(value,2), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   theme(axis.text = element_text(size=12),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text.x = element_text(size=12),
#         strip.text.y = element_text(size=12),
#         legend.position = "none")+
#   scale_fill_viridis_d() +
#   xlab("") + 
#   ylab("Median Percent") + 
#   facet_wrap(~variable)
# 
# 
# #ggsave("results/output/Summary of systems not in UCMR.png", width = 15, height = 10, units = "in")
# 
# 
# 
# 
# ################################################################################
# #  MISC  ####
# ################################################################################
# 
# 
# # which combos of detections were most common? 
# 
# cr <- ucmrdf.clean %>% 
#   filter(test_chem == "bin_TRI") %>% 
#   group_by(PWSID, contam.pfas, detchem) %>% 
#   count() %>% 
#   select(-n) %>% 
#   filter(detchem == "1" & contam.pfas != "evrdet") 
# 
# cr2 <- cr %>% 
#   arrange(PWSID, contam.pfas) %>% 
#   group_by(PWSID) %>% 
#   mutate(n_det = length(unique(contam.pfas)),
#          detchems = paste0(unique(contam.pfas), collapse = ",")) %>% 
#   select(-contam.pfas) %>% 
#   unique() %>% 
#   group_by(detchems) %>% 
#   count()
# 
# library(UpSetR)
# 
# cr3 <- cr %>%
#   mutate(detchem = as.integer(detchem)) %>%
#   ungroup() %>% 
#   pivot_wider(names_from = contam.pfas, values_from = detchem, values_fill = 0) %>% 
#   data.frame()
# 
# upset(cr3)
# 
# upset(cr3,order.by = "freq", empty.intersections = "on")
# 
# # ggsave("results/output/upset plot with combinations of chemicals detect.png", width = 20, height = 15, units = "in")
# # 
# 
# 
# # create_quartiles <- ucmrdf.plot %>% 
# #   select(PWSID, detchem, contam.pfas, all_of(demo_cols)) %>% 
# #   mutate(propurban = propurban*100) %>% 
# #   pivot_longer(names_to = "demo", values_to = "value", all_of(demo_cols)) %>% 
# #   filter(!is.na(value)) %>% 
# #   group_by(demo) %>% 
# #   mutate(quantile = ntile(value, 4)) %>% 
# #   group_by(contam.pfas, demo, quantile) %>% 
# #   summarize(nsys = length(unique(PWSID)),
# #             propdet = length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100,
# #             lower = round(min(value), 2),
# #             upper = round(max(value), 2)) %>% 
# #   mutate(quar = paste0(lower, "% - ", upper, "%")) %>% 
# #   ungroup() 
# 
# wwtp_summary <- ucmrdf.plot %>% 
#   filter(test_chem == "Any TRI facility" & contam.pfas == "Any UCMR Detection") %>% 
#   group_by(.) %>% 
#   summarize(n_sys = n(), 
#             n_land_area_inc = length(unique(PWSID[which(is.na(land.area))])), 
#             land_area_completeness = length(unique(PWSID[which(!is.na(land.area))]))/length(unique(PWSID)),
#             n_WWTP_missing = length(unique(PWSID[which((n_WWTP) == 0)])),
#             n_sys_missingwwtp.landarea = length(unique(PWSID[which(n_WWTP == 0 & is.na(land.area))])),
#             n_sys_missing_landarea)
# 
# investigate_missing_wwtpdata <- ucmrdf.plot %>% 
#   select(PWSID, detchem, test_chem, contam.pfas, WWTP_ML_km2, n_WWTP, WWTP_totalflow_mgd, land.area) %>% 
#   filter(test_chem == "Any TRI facility") %>% 
#   select(-test_chem) %>%
#   group_by(.) %>% 
#   mutate(quantile = ntile(WWTP_ML_km2, 4)) %>% 
#   filter(is.na(quantile))
# 
# wwtp_quartiles <- ucmrdf.plot %>% 
#   select(PWSID, detchem, test_chem, contam.pfas, WWTP_ML_km2, n_WWTP, WWTP_totalflow_mgd, land.area) %>% 
#   filter(test_chem == "Any TRI facility") %>% 
#   filter(!is.na(WWTP_ML_km2)) %>% 
#   select(-test_chem) %>%
#   # mutate(WWTP_ML_km2 = case_when(is.na(WWTP_ML_km2) ~ 0,
#   #                                TRUE ~ WWTP_ML_km2)) %>%
#   group_by(.) %>% 
#   mutate(quantile = ntile(WWTP_ML_km2, 4)) %>% 
#   group_by(quantile, contam.pfas) %>% 
#   summarize(detfreq = length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100, 
#                         lower = round(min(WWTP_ML_km2), 2),
#                         upper = round(max(WWTP_ML_km2), 2)) %>% 
#   mutate(quar = paste0(lower, " - ", upper, ""))
# 
# ggplot(wwtp_quartiles, aes(x = fct_reorder(quar, quantile), y = detfreq, fill = as.factor(quar))) + 
#   geom_bar(stat = "identity") + 
#   geom_text(aes(label=paste0(round(detfreq, 2), "%")), position = position_dodge(width = 0.9), vjust = -0.25, size = 8) +
#   scale_fill_viridis_d() + 
#   ylab("Percent of UCMR detections") + 
#   xlab("WWTP flow") + 
#   theme(axis.text = element_text(size=20),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text.x = element_text(size=20),
#         strip.text.y = element_text(size=20),
#         legend.position = "none") + 
#   facet_grid(~contam.pfas, scales = "free")
# 
# ggsave("results/output/wwtp flow quartiles with detection frequencies.png", width = 35, height = 12, units = "in")
# 
# ########################## GARBAGE <3 ###########################################
# 
# 
# #old burdened upset plot
# 
# # UPSET BARPLOT 1 -- red dots with lines ----------------------------------
# 
# 
# burd_sets <- upset_df %>% 
#   select(hasviolation, n_det, hlvlchem, extr_burden) %>% 
#   unique() %>%
#   mutate(extr_burden_order = factor(extr_burden, levels = c("none",
#                                                             "n_det",
#                                                             "hlvlchem",
#                                                             "hasviolation",
#                                                             "hlvlchem & hasviolation",
#                                                             "n_det & hlvlchem",
#                                                             "n_det & hasviolation",
#                                                             "n_det & hlvlchem & hasviolation"))) %>% 
#   select(-extr_burden) %>% 
#   arrange(desc(extr_burden_order)) %>% 
#   mutate(position = seq_along(1:nrow(.))) %>% 
#   pivot_longer(names_to = "burd_group", values_to = "in_set", c(-position, -extr_burden_order)) %>% 
#   mutate(burd_group = factor(burd_group, levels = c("n_det", "hlvlchem", "hasviolation"),
#                              labels = c("2+ target \nchemicals \ndetected", 
#                                         "1+ target \nchemical above \na federal guideline",
#                                         "1+ health-\nbased SDWA \nviolation"))) %>% 
#   left_join(burd_sets_n, by = c("extr_burden_order" = "extr_burden")) %>% 
#   mutate(position = factor(position, levels = c(unique(position)), labels = c(unique(label_n))))
# 
# # get data for drawing the connected lines 
# burd_sets2 <- upset_df %>% 
#   select(hasviolation, n_det, hlvlchem, extr_burden) %>% 
#   unique() %>%
#   mutate(extr_burden_order = factor(extr_burden, levels = c("none",
#                                                             "n_det",
#                                                             "hlvlchem",
#                                                             "hasviolation",
#                                                             "hlvlchem & hasviolation",
#                                                             "n_det & hlvlchem",
#                                                             "n_det & hasviolation",
#                                                             "n_det & hlvlchem & hasviolation"))) %>% 
#   select(-extr_burden) %>% 
#   arrange(desc(extr_burden_order)) %>% 
#   mutate(position = seq_along(1:nrow(.))) %>% 
#   pivot_longer(names_to = "burd_group", values_to = "in_set", c(-position, -extr_burden_order)) %>% 
#   mutate(burd_group = factor(burd_group, levels = c("n_det", "hlvlchem", "hasviolation"),
#                              labels = c("2+ target \nchemicals \ndetected", 
#                                         "1+ target \nchemical above \na federal guideline",
#                                         "1+ health-\nbased SDWA \nviolation"))) %>% 
#   left_join(burd_sets_n, by = c("extr_burden_order" = "extr_burden")) %>%
#   filter(in_set == 1)%>% 
#   mutate(position = factor(position, levels = c(unique(position)), labels = c(unique(label_n))))
# 
# #get data for the overall median line 
# ovrall_line <- ucmrdf.clean_summ %>%
#   filter(contam.pfas == "evrdet") %>%
#   group_by(.) %>% 
#   summarize(`Median Percent Hispanic` = median(perc_hisp_any),
#             `Median Percent Black` = median(perc_black_nohisp)) %>%
#   pivot_longer(names_to = "variable", values_to = "median_val", everything()) %>% 
#   mutate(variable = factor(variable, labels = c("\nMedian Percent Hispanic\n", "\nMedian Percent Black\n"),
#                            levels = c("Median Percent Hispanic", "Median Percent Black")))
# 
# 
# p1 <- ggplot(burd_sets) + 
#   geom_point(aes(x = burd_group, y = position, shape = as.factor(in_set), color = as.factor(in_set)), size = 10)+
#   scale_color_manual(values = c("dark grey","#ab4028")) +
#   scale_shape_manual(values=c(1,19)) + 
#   geom_line(burd_sets2, mapping = aes(x = burd_group, y = position, group = interaction(in_set, extr_burden_order)), 
#             size = 3, color = "#ab4028") + 
#   scale_x_discrete(position = "top") +
#   scale_y_discrete(expand = expansion(mult = c(0.15, 0.1)))+
#   theme_classic() + 
#   theme(axis.title=element_blank(),
#         axis.text.y=element_text(size = 25, face = "italic"),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         axis.text.x.top = element_text(size = 25),
#         legend.position = "none")
# 
# p2 <- upset_df %>%
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   group_by(extr_burden, variable) %>%
#   summarize(median_val = median(value,na.rm = TRUE)) %>%
#   filter(variable %in% c("perc_hisp_any", "perc_black_nohisp")) %>%
#   #filter(variable %in% c( "perc_hisp_any", "perc_black_nohisp", "propurban", "mdi")) %>%
#   mutate(extr_burden = factor(extr_burden, levels = c("none",
#                                                       "n_det",
#                                                       "hlvlchem",
#                                                       "hasviolation",
#                                                       "hlvlchem & hasviolation",
#                                                       "n_det & hlvlchem",
#                                                       "n_det & hasviolation",
#                                                       "n_det & hlvlchem & hasviolation")),
#          extr_burden = fct_rev(extr_burden),
#          variable = factor(variable, levels = c("perc_hisp_any", "perc_black_nohisp"),
#                            labels = c("\nMedian Percent Hispanic\n", "\nMedian Percent Black\n"))) %>%
#   ggplot(aes(x = median_val, y = extr_burden, fill = variable)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_vline(ovrall_line, mapping = aes(xintercept = median_val),
#              linetype = 2, size = 1, color = "dark grey") +
#   geom_text(ovrall_line, mapping = aes(x = median_val, y = 0.25, label = paste0("Overall Median (", median_val, "%)")),
#             hjust = -0.05, size = 8) +
#   geom_text_repel(ovrall_line, mapping = aes(x = median_val, y = 0.25,
#                                              label = paste0("Overall median (", median_val, "%)")),
#                   size = 10,
#                   nudge_x = 8,
#                   na.rm = TRUE,
#   ) +
#   geom_text(aes(label=paste0(round(median_val,2), "%")), position = position_dodge(width = 0.9), hjust = -0.25, size = 10)+
#   scale_fill_manual(values = c("#9bccc6", "#50a6a6","#457d7c","#004552")) +
#   theme_classic() +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
#                      limits = c(0, 18)) +
#   scale_y_discrete(expand = expansion(mult = c(0.15, 0.1)))+
#   theme(axis.title=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 25),
#         legend.position = "none") +
#   facet_grid(~variable, scales = "free_x")
# 
# 
# plot_grid(p1, p2, rel_width = c(1, 1.5))
# #ggsave("results/output/upset-barplot of perc hisp and black for burdened systems edits from R group.png", width = 20, height = 15, units = "in")
# 
# 
# p1 <- ggplot(burd_sets, aes(x = position, y = burd_group)) + 
#   geom_tile(aes(fill = as.factor(in_set)), color = "black", size = 2)+
#   #geom_point(aes(x = position, y = burd_group, shape = as.factor(in_set), color = as.factor(in_set)), size = 30)+
#   scale_fill_manual(values = c("white","light grey")) +
#   # scale_shape_manual(values=c(0, 15)) + 
#   # geom_line(burd_sets2, mapping = aes(x = position, y = burd_group, group = interaction(in_set, extr_burden_order)), 
#   #           size = 3, color = "#ab4028") + 
#   #scale_y_discrete(position = "top") +
#   # scale_x_discrete(expand = expansion(mult = c(0.15, 0.1)))+
#   theme_classic() + 
#   theme(axis.title=element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y=element_text(size = 25),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         axis.text.x.top = element_text(size = 25),
#         legend.position = "none")
# 
# plot_grid(p2, p1, rel_heights = c(1, .5), ncol = 1)
# #ggsave("results/output/upset-barplot of perc hisp and black for burdened systems edits from R group3.png", width = 20, height = 15, units = "in")
# 
# 
# p2 <- upset_df %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   # group_by(extr_burden, variable) %>%
#   # summarize(median_val = median(value,na.rm = TRUE)) %>% 
#   filter(variable %in% c("perc_hisp_any", "perc_black_nohisp")) %>% 
#   #filter(variable %in% c( "perc_hisp_any", "perc_black_nohisp", "propurban", "mdi")) %>% 
#   mutate(extr_burden = factor(extr_burden, levels = c("none",
#                                                       "n_det",
#                                                       "hlvlchem",
#                                                       "hasviolation",
#                                                       "hlvlchem & hasviolation",
#                                                       "n_det & hlvlchem",
#                                                       "n_det & hasviolation",
#                                                       "n_det & hlvlchem & hasviolation")),
#          # extr_burden = fct_rev(extr_burden),
#          variable = factor(variable, levels = c("perc_hisp_any", "perc_black_nohisp"),
#                            labels = c("Median Percent Hispanic", "Median Percent Black")),
#          variable = fct_rev(variable)) %>%
#   ggplot(aes(y = value, x = extr_burden, color = variable)) + 
#   geom_sina() +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") +
#   # geom_bar(stat = "identity", position = position_dodge()) +
#   geom_hline(ovrall_line, mapping = aes(yintercept = median_val),
#              linetype = 2, size = 1, color = "#d77659") +
#   # geom_text(ovrall_line, mapping = aes(x = median_val, y = 0.25, label = paste0("Overall Median (", median_val, "%)")), 
#   #            hjust = -0.05, size = 8) + 
#   # geom_text_repel(ovrall_line, mapping = aes(x = median_val, y = 0.25, 
#   #                                            label = paste0("Overall median (", median_val, "%)")),
#   #                 size = 10,
#   #                 nudge_x = 8,
#   #                 na.rm = TRUE,
#   #                 ) +
#   #geom_text(aes(label=paste0(round(median_val,2), "%")), position = position_dodge(width = 0.9), hjust = -0.25, size = 10)+
#   scale_color_manual(values = c("#9bccc6", "#50a6a6","#457d7c","#004552")) + 
#   theme_minimal() + 
#   # scale_x_discrete(expand = expansion(mult = c(0, 0.1))) +
#   #scale_y_discrete(expand = expansion(mult = c(0.15, 0.1)))+
#   scale_y_continuous(limits = c(0, 100),
#                      position = "right")+
#   theme(axis.title=element_blank(),
#         axis.text.x=element_blank(),
#         # axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.line=element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 25),
#         strip.text.y.left = element_text(angle = 0),
#         legend.position = "none") + 
#   facet_grid(rows = vars(variable), switch = "both")
# 
# # plot_grid(p1, p2, rel_widths = c(1, 1.5))
# plot_grid(p2, p1, rel_heights  = c(1, .5), ncol = 1)
# 
# #ggsave("results/output/upset-barplot of perc hisp and black for burdened systems.png", width = 27, height = 15, units = "in")
# 
# #ggsave("results/output/upset-barplot of perc hisp and black for burdened systems edits from R group sina.png", width = 20, height = 15, units = "in")
# 
# 
# # ucmrdf.clean_summ %>% 
# #  filter(contam.pfas %in% c("PFAS", "1,4-dioxane")) %>% 
# #  group_by(PWSID) %>% 
# #  summarize(n_sys = length(unique(contam.pfas[which(hlvlchem == 1)]))) %>%
# #   group_by(n_sys) %>% 
# #   count() %>% View()
# 
# 
# 
# 
# 
# # old unequal var test -- this was used to make the DET-ND bar plot 
# 
# 
# detsumm_graph <- detsumm %>% select(contam.pfas, variable, flag) %>% 
#   mutate(#variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
#     # variable == "perc_black_nohisp" ~ "Percent Black",
#     # variable == "perc_pov_ppl" ~ "Percent Poverty",
#     # variable == "perc_hmown" ~  "Percent Homeownership",
#     # variable == "propurban" ~ "Percent Urban Households",
#     # variable == "mdi" ~ "MDI Rate",
#     # variable == "perc_uninsur" ~ "Percent Uninsured"),
#     detchem = 1)
# 
# 
# 
# demosum_bydet <- ucmrdf.plot %>% 
#   filter(test_chem == ">=1 TRI facility") %>% 
#   group_by(contam.pfas, detchem) %>% 
#   summarize(n_sys = length(unique(PWSID)), 
#             `Percent Hispanic` = mean(perc_hisp_any),
#             `Percent Black` = mean(perc_black_nohisp),
#             `Percent Poverty` = mean(perc_pov_ppl),
#             `Percent Homeownership` = mean(perc_hmown),
#             `Percent Urban Households` = mean(propurban)*100,
#             `MDI Rate` = mean(mdi, na.rm = TRUE),
#             `Percent Uninsured` = mean(perc_uninsur, na.rm = TRUE)) %>% 
#   pivot_longer(names_to = "variable", values_to = "percent", `Percent Hispanic`:`Percent Uninsured`) %>% 
#   mutate(n_sys_det = case_when(detchem == 0 ~ paste0("n = ", n_sys)),
#          n_sys_nd = case_when(detchem == 1  ~ paste0("n = ",n_sys))) %>% 
#   left_join(detsumm_graph) %>% 
#   filter(!variable %in% c("Percent uninsured", "Percent poverty", "Percent homeownership", "perc_white_nohisp"))  %>%
#   mutate(variable = str_wrap(variable, width = 5), 
#          flag = case_when(is.na(flag) ~ "",
#                           TRUE ~ flag))
# 
# 
# signif_dat <- demosum_bydet %>% filter(detchem == "1")
# 
# 
# demosum_bydet %>% 
#   ungroup() %>% 
#   ggplot(aes(x = variable, y = percent, fill = as.factor(detchem))) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   scale_fill_manual(name = "UCMR Detection",
#                     labels=c("ND", "Detect"),
#                     values = c("#9bccc6", "#d77659"))+
#   geom_text(aes(label=paste0(round(percent,0), "%")), position = position_dodge(width = 0.9), size = 5, vjust = 0) +
#   geom_text(aes(label=flag), vjust = -2, size = 8) +
#   geom_text(aes(label = n_sys_det),x=1, y=95, color = "#457d7c") +
#   geom_text(aes(label = n_sys_nd),x=1, y=90, color = "#d77659") +
#   theme(legend.title = element_blank()) +
#   ylab("Mean percent") + 
#   xlab("") +
#   ylim(0, 100) + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold"), 
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=12))+
#   facet_wrap(~contam.pfas)
# 
# #ggsave("results/output/demographic summary of det-nd for UCMR chems.png", width = 15, height = 10, units = "in")
# 
# 
# 
# 
# 
# 
# #### VENN DIAGRAM #####
# # cr <- ucmrdf.clean %>%
# #   subset(contam.pfas %in% c("evrdet")) %>%
# #   filter(test_chem == "bin_TRI") %>% 
# #   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
# #                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
# #   select(PWSID, detchem, hlvlchem, hasviolation) %>% 
# #   mutate(detchem = case_when(detchem == "1" ~ TRUE, 
# #                              TRUE ~ FALSE),
# #          hlvlchem = case_when(hlvlchem == "1" ~ TRUE,
# #                               TRUE ~ FALSE),
# #          hasviolation = case_when(hasviolation == "Y" ~ TRUE,
# #                                   TRUE ~ FALSE))
# 
# #install.packages("VennDiagram")
# library(VennDiagram)
# 
# 
# d_in <- burdened_system_code[,c("hasviolation", "n_det", "hlvlchem")]
# 
# myCol <- c("#e8bf69", "#9ad6cc", "#ec7f78")
# label <- c("#cf9c2e", "#7ab4b1", "#ce615e")
# 
# # format data for venn diagragram
# d_plot = NULL
# for(i in 1:dim(d_in)[2]){
#   d_plot[[i]] = which(d_in[,i] == TRUE) 
#   
# }
# names(d_plot) = c(">=1 SDWA \nviolation", ">1 UCMR \nchemical detected",
#                   ">=1 UCMR chemical \nabove a federal guideline" )
# 
# venn.diagram(d_plot, 
#              filename = "results/output/vennplot.png",
#              output = TRUE,
#              height = 2500, 
#              width = 2500, 
#              col = "black",
#              lty = "blank",
#              lwd = 2,
#              fill = myCol,
#              #size of the numbers
#              cex = 1.25,
#              alpha = 0.50,
#              fontfamily = "serif",
#              fontface = "bold",
#              #labels
#              cat.col = label,
#              cat.cex = 1,
#              cat.fontfamily = "serif",
#              cat.default.pos = "outer",
#              cat.pos = c(-27, 27, 180),
#              cat.dist = c(0.15, 0.15, 0.1),
#              margin = 0.10)
# 
# 
# 
# # burdened_system_plot %>%
# #   #filter(variable %in% c("Percent Hispanic", "Percent Black", "Percent Urban Households", "MDI Rate")) %>%
# #   filter(variable %in% c("Percent Hispanic")) %>%
# #   mutate(extr_burden = factor(extr_burden, levels = c("Systems in UCMR" ,"none", "n_det", "hasviolation", "hlvlchem",
# #                                                       "hlvlchem & hasviolation", "n_det & hasviolation",
# #                                                       "n_det & hlvlchem", "n_det & hlvlchem & hasviolation"),
# #                               labels = c("Systems in UCMR" ,"none", "n_det", "hasviolation", "hlvlchem",
# #                                          "hlvlchem & \nhasviolation", "n_det & \nhasviolation",
# #                                          "n_det & \nhlvlchem", "n_det & \nhlvlchem & \nhasviolation"))) %>%
# #   #mutate(color_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ as.character(n_det))) %>%
# #   ggplot(aes(x = extr_burden, y = value, color = extr_burden)) +
# #   geom_sina()  +
# #   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color ="#3a3838" ) +
# #   xlab("") +
# #   ylab("") +
# #   #scale_color_manual(na.value = "#d77659", values = c("#9bccc6", "#50a6a6", "#457d7c", "#004552", "#1a3438")) +
# #   theme_minimal() +
# #   ylim(0, 100) +
# #   theme(axis.text = element_text(size=14),
# #         strip.text = element_text(size = 16, face = "bold"),
# #         legend.position = "none")+
# #   facet_grid(~as.factor(variable))
# 
# # geom_line(n_det_summary %>% 
# #             filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")) %>% 
# #             filter(variable %in% c("MDI Rate", "Percent Uninsured", "Percent Poverty", "Percent Homeownership")), 
# #           mapping = aes(x = as.factor(n_det), y = mean), group = 1, color = "#666666", size = 1) + 
# # geom_point(n_det_summary%>% 
# #              filter(variable %in% c("MDI Rate", "Percent Uninsured", "Percent Poverty", "Percent Homeownership")), 
# #            mapping = aes(x = as.factor(n_det), y = mean), fill = "#ab4028", shape = 23, size = 3) + 
# 
# 
# 
# 
# 
# 
# #install.packages("UpSetR")
# # library(UpSetR)
# 
# 
# 
# # cr3 <- ggplot(burdened_system_plot_summary, aes(x = extr_burden, y = mean_var, fill = extr_burden)) + 
# #   geom_bar(stat = "identity", position = "dodge") + 
# #   facet_grid(~variable)
# 
# 
# # myboxplot <- function(mydat,x, y){
# #   ggplot(mydat) + 
# #     geom_boxplot(aes(x, y, color = x))
# # }
# 
# # upset(upset_df, 
# #      #boxplot.summary = c("perc_hisp_any", "perc_black_nohisp"),
# #      #boxplot.summary = c("mdi", "propurban"),
# #      # attribute.plots = list(gridrows = 100, ncols = 1,
# #      #                        plots = list(list(plot = myboxplot, x = "extr_burden", y = "perc_hisp_any", queries = F))),
# #       # attribute.plots = list(gridrows = 100, ncols = 1,
# #       #                        plots = list(list(plot = histogram, y = "source_type"))),
# #       sets = c("hasviolation", "n_det", "hlvlchem"),
# #       sets.bar.color = "#56B4E9",
# #       order.by = "freq", empty.intersections = "on"
# #       
# # )
# 
# 
# ################################################################################
# #  5. MAP MAKING ####
# ################################################################################
# 
# 
# library(mapproj)
# 
# map_county <- read_csv("../Data/county mapping data.csv")
# 
# 
# #counties served by a pws in UCMR 
# ucmrdf.clean %>%
#   subset(contam.pfas %in% c("evrdet")) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
#   group_by(GEO.id2) %>%
#   count() %>% 
#   mutate(inUCMR = case_when(n > 0 ~ "Y",
#                             TRUE ~ "N")) %>% 
#   filter(n > 0) %>% 
#   mutate(fips = as.numeric(GEO.id2)) %>%
#   right_join(map_county) %>%
#   ggplot(aes(long, lat, group = group, fill = inUCMR)) +
#   geom_polygon(color = "black") +
#   coord_map() +
#   ggtitle(paste("Counties served by a PWS reporting List 1 chemicals in UCMR 3")) +
#   scale_fill_manual(na.value = "#e6e6e6",
#                     name = "",
#                     labels=c("County served by PWS in UCMR"),
#                     values = c("#9bccc6")) +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         legend.position = "none") 
# #ggsave("results/output/Counties served by a PWS in UCMR.png", width = 15, height = 10, units = "in")
# 
# 
# 
# #counties served by a pws in UCMR 
# ucmrdf.clean %>%
#   subset(contam.pfas %in% c("evrdet")) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
#   group_by(GEO.id2, PWSID) %>%
#   summarize(extr_burden_sys = case_when(hlvlchem == "1" & hasviolation == "Y" ~ "hlvl/sdwa",
#                                     hlvlchem == "1" & hasviolation == "N" ~ "hlvl",
#                                     hlvlchem == "0" & hasviolation == "Y" ~ "sdwa",
#                                     hlvlchem == "0" & hasviolation == "N" & detchem == "1" ~ "det",
#                                     hlvlchem == "0" & hasviolation == "N" & detchem == "0" ~ "nd")) %>%
#   group_by(GEO.id2, extr_burden_sys) %>%
#   count() %>% 
#   pivot_wider(names_from = extr_burden_sys, values_from = n, values_fill = 0) %>% 
#   mutate(extr_burden_co = case_when(`hlvl/sdwa` > 0 ~"Exceeds federal guideline for UCMR chem & has SDWA health-based violation",
#                                     hlvl > 0 & sdwa == 0 ~ "Exceeds federal guideline for UCMR chem",
#                                     hlvl == 0 & sdwa > 0 ~ "Has SDWA health-based violation",
#                                     hlvl == 0 & sdwa == 0 & det > 0 ~ ">= 1 UCMR chemical detected",
#                                     hlvl == 0 & sdwa == 0 & det == 0 ~ "No UCMR detections")) %>% 
# 
#   # summarize(extr_burden_co = case_when(extr_burden_sys == "hlvl/sdwa" ~"Exceeds federal guideline for UCMR chem & has SDWA health-based violation",
#   #                                      extr_burden_sys == "hlvl" ~ "Exceeds federal guideline for UCMR chem",
#   #                                      extr_burden_sys == "sdwa" ~ "Has SDWA health-based violation",
#   #                                      extr_burden_sys == "det" ~ ">= 1 UCMR chemical detected",
#   #                                      extr_burden_sys == "nd" ~ "No UCMR detections")) %>% 
#   # summarize(isdet = length(unique(PWSID[which(detchem == "1")])),
#   #           isovrhlvl = length(unique(PWSID[which(hlvlchem == "1")])),
#   #           isviol = length(unique(PWSID[which(hasviolation == "Y")]))) %>% 
#   # mutate(ovrhlvl = case_when(isovrhlvl> 0 ~ "Y",
#   #                           TRUE ~ "N"),
#   #        hasviol = case_when(isviol > 0 ~ "Y",
#   #                            TRUE ~ "N"),
#   #        hasdet = case_when(isdet > 0 ~ "Y",
#   #                           TRUE ~ "N"),
#   #        extr_burden = case_when(ovrhlvl == "Y" & hasviol == "Y" ~ "Exceeds federal guideline for UCMR chem & has SDWA health-based violation",
#   #                                ovrhlvl == "Y" & hasviol == "N" ~ "Exceeds federal guideline for UCMR chem",
#   #                                ovrhlvl == "N" & hasviol == "Y" ~ "Has SDWA health-based violation",
#   #                                ovrhlvl == "N" & hasviol == "N" & hasdet == "Y" ~ ">= 1 UCMR chemical detected",
#   #                                hasdet == "N" ~ "No UCMR detections")) %>% 
#   mutate(extr_burden = factor(extr_burden_co, levels = c("Exceeds federal guideline for UCMR chem & has SDWA health-based violation",
#                                                       "Exceeds federal guideline for UCMR chem",
#                                                       "Has SDWA health-based violation",
#                                                       ">= 1 UCMR chemical detected",
#                                                       "No UCMR detections"))) %>% 
#   #filter(n > 0) %>% 
#   mutate(fips = as.numeric(GEO.id2)) %>%
#   right_join(map_county) %>%
#   ggplot(aes(long, lat, group = group, fill = extr_burden, color = extr_burden)) +
#   geom_polygon(color = "grey") +
#   coord_map() +
#   #ggtitle(paste("Counties served by a PWS reporting List 1 chemicals in UCMR 3")) +
#   scale_fill_manual(na.value = "white",#"#e6e6e6",
#                     name = "Most burdened system in county",
#                     #labels=c("County served by PWS in UCMR"),
#                     values =  c("#9b83c7",  "#9bc5c6", "#e39e89", "#b5edd2", "#e4f8ee")) +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank()) 
# #ggsave("results/output/map of burdened UCMR 3 systems.png", width = 15, height = 10, units = "in")
# 
# 
# #percent of systems with detectable ucmr_chems
# for(i in unique(ucmrdf.demo_detcode$contam.pfas[which(!(is.na(ucmrdf.demo_detcode$contam.pfas)))])){
#   
#   label <- i
#   label[label == "evrdet"] <- "Any UCMR"
#   
#   cr <-  ucmrdf.demo_detcode %>%
#     subset(contam.pfas %in% i) %>%
#     subset(test_chem == "bin_TRI") %>% 
#     mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                                 labels = c("Any UCMR", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
#     group_by(GEO.id2) %>%
#     summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2) %>%
#     mutate(fips = as.numeric(GEO.id2)) %>% 
#     right_join(map_county) %>%
#     ggplot(aes(long, lat, group = group, fill = perc_sys_w_detect)) +
#     geom_polygon(color = "black") +
#     coord_map() +
#     ggtitle(paste0("Percent of Systems With ", label, " Detection")) +
#     scale_fill_viridis(na.value = "light grey",
#                        name = "Percent of systems \nwith a detect") +
#     theme(panel.background = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           legend.position = "right")
#   
#   print(cr)
#   # ggsave(paste0("results/output/Map of percent of systems with detectable ",i ,".png"), width = 15, height = 10, units = "in")
#   
# }
# 
# 
# # #percent of systems with any detectable UCMR chems
# # perc_any_det <- ucmrdf.demo_detcode %>%
# #   subset(test_chem == "PFAS") %>% 
# #   group_by(GEO.id2) %>%
# #   summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2,
# #             pop_served = sum(WS.POPULATION_SERVED_COUNT)) %>%
# #   mutate(fips = as.numeric(GEO.id2)) %>% 
# #   right_join(map_county) 
# # 
# # #percent of systems with any detectable UCMR chems
# # # ucmrdf.demo_detcode %>%
# # #   subset(test_chem == "PFAS") %>% 
# # #   group_by(GEO.id2) %>%
# # #   summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2) %>%
# # #   mutate(fips = as.numeric(GEO.id2)) %>% 
# # #   right_join(map_county) %>%
# # ggplot(perc_any_det) +
# #   geom_polygon(aes(long, lat, group = group, fill = perc_sys_w_detect), col = "grey", data = perc_any_det) +
# #   #geom_point(aes(long, lat, size = pop_served, color = perc_sys_w_detect), data = ca) +
# #   coord_map() +
# #   ggtitle(paste("percent of systems with detectable UCMR chem")) +
# #   #scale_fill_viridis(na.value = "light grey") +
# #   scale_color_viridis() +
# #   theme(panel.background = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         axis.title = element_blank(),
# #         legend.position = "right")
# 
# 
# 
# #percent of systems with any detectable UCMR chems
# perc_sys_det_ucmr <- ucmrdf.demo_detcode %>%
#   subset(test_chem == "bin_TRI") %>% 
#   filter(!is.na(contam.pfas)) %>% 
#   filter(contam.pfas != "evrdet") %>% 
#   group_by(PWSID, GEO.id2, Size) %>%
#   summarize(n_total_detects = sum(as.numeric(detchem), na.rm = FALSE)) 
# 
# 
# 
# for(i in unique(perc_sys_det_ucmr$n_total_detects)) {
#   cr <-  perc_sys_det_ucmr %>% 
#     group_by(GEO.id2) %>%
#     summarize(perc_sys = round(length(unique(PWSID[which(n_total_detects == i)]))/length(unique(PWSID))*100, 2)) %>% 
#     mutate(fips = as.numeric(GEO.id2)) %>% 
#     right_join(map_county) %>%
#     ggplot(aes(long, lat, group = group, fill = perc_sys)) +
#     geom_polygon(color = "black") +
#     coord_map() +
#     ggtitle(paste("percent of systems with",i ,"detectable UCMR chem")) +
#     scale_fill_viridis(na.value = "light grey") +
#     theme(panel.background = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           legend.position = "right")
#   
#   print(cr)
#   #ggsave(paste0("results/output/Map of percent of systems with ",i ," detectable UCMR chems.png"), width = 15, height = 10, units = "in")
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ################################################################################
# # STOP HERE######
# ################################################################################
# 
# 
# 
# 
# ################################################################################
# # SDWA violation data ####
# ################################################################################
# 
# violdataset_subset <- read_csv("C:/Users/AmandaHernandez/Desktop/UCMR3 data/UCMR3/Data/SDWA/all violations 2010-2015.csv")
# 
# violations_ucmr <- violdataset_subset %>% 
#   filter(PWSID %in% ucmrsys) %>% 
#   filter(IS_HEALTH_BASED_IND == "Y") 
# 
# vio_ucmr_summary <- violdataset_subset %>% 
#   filter(PWSID %in% ucmrsys) %>% 
#   mutate(start_year = year(dmy(COMPL_PER_BEGIN_DATE)),
#          end_year = year(dmy(COMPL_PER_END_DATE))) %>% 
#   filter(start_year >= 2013) %>% 
#   group_by(PWSID, start_year, end_year, CATEGORY_CODE, IS_HEALTH_BASED_IND) %>% 
#   summarize(n_violations = n(),
#             n_contaminants = length(unique(CONTAMINANT_CODE))) %>% 
#   filter(IS_HEALTH_BASED_IND == "Y") 
# 
# #MCL 
# #MRDL -- Maximum Residual Disinfectant Level
# #TT -- Treatment Technique Violations 
# #M/R -- Monitoring and Reporting Violations 
# table(violdataset_subset$CATEGORY_CODE, violdataset_subset$IS_HEALTH_BASED_IND)
# 
# cr <- ucmrdf.clean %>% 
#   #filter(contam.pfas == "evrdet") %>%
#   filter(test_chem == "bin_TRI") %>% 
#   unique() %>% 
#   left_join(vio_ucmr_summary %>% 
#               ungroup() %>%
#               mutate(hasviolation = "Y") %>% 
#               select(PWSID, hasviolation) %>% 
#               unique()) %>% 
#   mutate(hasviolation = case_when(is.na(hasviolation) ~ "N",
#                                   TRUE ~ hasviolation)) 
# 
# table(cr$hasviolation, cr$hlvlchem, useNA = "ifany")
# 
# # ucmrdet_viol_summary <- cr %>% group_by(hasviolation) %>% 
# #   summarize(detfreq = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100, 1))
# 
# 
# ucmrdet_viol_summary <- cr %>% group_by(contam.pfas, detchem, hlvlchem) %>% 
#   summarize(n_sys = length(unique(PWSID)), 
#             n_sys_w_viol = length(unique(PWSID[which(hasviolation == "Y")])),
#             violfreq = round(length(unique(PWSID[which(hasviolation == "Y")]))/length(unique(PWSID))*100, 1))
# 
# #write_csv(ucmrdet_viol_summary, "results/output/summary of UCMR detections and SDWA violations.csv")
# 
# ucmrdet_viol <- cr %>%
#   #filter(detchem == 1 & hasviolation == "Y") %>% 
#   group_by(hasviolation) %>% 
#   demobreakdown()
# 
# 
# 
# 
# ################################################################################
# # 4c. DEMO SUMMARY FIGURES ######
# ################################################################################
# 
# demo_cols <- c("perc_hisp_any", "mdi", "perc_pov_ppl", "perc_black_nohisp",
#                "perc_white_nohisp", "perc_hmown", "propurban", "perc_uninsur")
# 
# for(i in demo_cols){
#   
#  cr <-  ggplot(pws_demo%>% mutate(propurban = propurban*100), aes_string(x = i)) + 
#   geom_density(color = "#457d7c") +
#   geom_density(data = ucmrdf.demo %>% mutate(propurban = propurban*100), aes_string(x = i), color = "#d77659") + 
#   annotate("text", label = "ALL PWS", color = "#457d7c", x = 95, y = .1)+
#   annotate("text", label = "UCMR SYS", color = "#d77659", x = 95, y = .15)+
#   xlim(0, 100)
#  
#  print(cr)
#  
#  #ggsave(paste0("results/output/distribution plots/distribution plot ", i ,".png"), width = 15, height = 10, units = "in")
# }
# 
# 
# create_quartiles <- ucmrdf.plot %>% 
#   select(PWSID, detchem, contam.pfas, all_of(demo_cols)) %>% 
#   mutate(propurban = propurban*100) %>% 
#   pivot_longer(names_to = "demo", values_to = "value", all_of(demo_cols)) %>% 
#   filter(!is.na(value)) %>% 
#   group_by(demo) %>% 
#   mutate(quantile = ntile(value, 4)) %>% 
#   group_by(contam.pfas, demo, quantile) %>% 
#   summarize(nsys = length(unique(PWSID)),
#             propdet = length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100,
#             lower = round(min(value), 2),
#             upper = round(max(value), 2)) %>% 
#   mutate(quar = paste0(lower, "% - ", upper, "%")) %>% 
#   ungroup() 
# 
# 
# 
# for(i in demo_cols){
#   create_quartiles %>% 
#     filter(demo == i) %>%
#     ggplot(aes(x = fct_reorder(quar, quantile), y = propdet, fill = as.factor(quantile))) +
#     geom_bar(stat = "identity") + 
#     geom_text(aes(label=paste0(round(propdet, 2), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#     ylab("Percent of UCMR detections") + 
#     xlab(paste0(i)) +
#     scale_fill_viridis_d() +
#     theme(axis.text = element_text(size=12),
#           axis.title = element_text(size=14),
#           legend.text = element_text(size=12),
#           strip.text.x = element_text(size=12),
#           strip.text.y = element_text(size=12),
#           legend.position = "none") + 
#     facet_grid(~contam.pfas, scales = "free")
#   
#   #ggsave(paste0("results/output/", i, " quartile plots.png"), width = 30, height = 10, units = "in")
# }
# 
# 
# 
# ################################################################################
# #  5. MAP MAKING ####
# ################################################################################
# 
# 
# library(mapproj)
# 
# map_county <- read_csv("results/county mapping data.csv")
# 
# 
# #counties served by a pws in UCMR 
# ucmrdf.clean %>%
#   subset(contam.pfas %in% c("evrdet")) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
#   group_by(GEO.id2) %>%
#   count() %>% 
#   mutate(inUCMR = case_when(n > 0 ~ "Y",
#                             TRUE ~ "N")) %>% 
#   filter(n > 0) %>% 
#   mutate(fips = as.numeric(GEO.id2)) %>%
#   right_join(map_county) %>%
#   ggplot(aes(long, lat, group = group, fill = inUCMR)) +
#   geom_polygon(color = "black") +
#   coord_map() +
#   ggtitle(paste("Counties served by a PWS in UCMR")) +
#   scale_fill_manual(na.value = "#e6e6e6",
#                     name = "",
#                     labels=c("County served by PWS in UCMR"),
#                     values = c("#9bccc6")) +
#   theme(panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         legend.position = "none") 
# #ggsave("results/output/Counties served by a PWS in UCMR.png", width = 15, height = 10, units = "in")
# 
# #percent of systems with detectable ucmr_chems
# for(i in unique(ucmrdf.demo_detcode$contam.pfas[which(!(is.na(ucmrdf.demo_detcode$contam.pfas)))])){
#   
#   label <- i
#   label[label == "evrdet"] <- "Any UCMR"
# 
#   cr <-  ucmrdf.demo_detcode %>%
#     subset(contam.pfas %in% i) %>%
#     subset(test_chem == "bin_TRI") %>% 
#     mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                                 labels = c("Any UCMR", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>%
#     group_by(GEO.id2) %>%
#     summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2) %>%
#     mutate(fips = as.numeric(GEO.id2)) %>% 
#     right_join(map_county) %>%
#     ggplot(aes(long, lat, group = group, fill = perc_sys_w_detect)) +
#     geom_polygon(color = "black") +
#     coord_map() +
#     ggtitle(paste0("Percent of Systems With ", label, " Detection")) +
#     scale_fill_viridis(na.value = "light grey",
#                        name = "Percent of systems \nwith a detect") +
#     theme(panel.background = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           legend.position = "right")
#   
#   print(cr)
#  # ggsave(paste0("results/output/Map of percent of systems with detectable ",i ,".png"), width = 15, height = 10, units = "in")
#   
# }
# 
#  
# # #percent of systems with any detectable UCMR chems
# # perc_any_det <- ucmrdf.demo_detcode %>%
# #   subset(test_chem == "PFAS") %>% 
# #   group_by(GEO.id2) %>%
# #   summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2,
# #             pop_served = sum(WS.POPULATION_SERVED_COUNT)) %>%
# #   mutate(fips = as.numeric(GEO.id2)) %>% 
# #   right_join(map_county) 
# # 
# # #percent of systems with any detectable UCMR chems
# # # ucmrdf.demo_detcode %>%
# # #   subset(test_chem == "PFAS") %>% 
# # #   group_by(GEO.id2) %>%
# # #   summarize(perc_sys_w_detect = round(length(unique(PWSID[which(detchem == 1)]))/length(unique(PWSID))*100),2) %>%
# # #   mutate(fips = as.numeric(GEO.id2)) %>% 
# # #   right_join(map_county) %>%
# # ggplot(perc_any_det) +
# #   geom_polygon(aes(long, lat, group = group, fill = perc_sys_w_detect), col = "grey", data = perc_any_det) +
# #   #geom_point(aes(long, lat, size = pop_served, color = perc_sys_w_detect), data = ca) +
# #   coord_map() +
# #   ggtitle(paste("percent of systems with detectable UCMR chem")) +
# #   #scale_fill_viridis(na.value = "light grey") +
# #   scale_color_viridis() +
# #   theme(panel.background = element_blank(),
# #         axis.ticks = element_blank(),
# #         axis.text = element_blank(),
# #         axis.title = element_blank(),
# #         legend.position = "right")
# 
# 
# 
# #percent of systems with any detectable UCMR chems
# perc_sys_det_ucmr <- ucmrdf.demo_detcode %>%
#   subset(test_chem == "bin_TRI") %>% 
#   filter(!is.na(contam.pfas)) %>% 
#   filter(contam.pfas != "evrdet") %>% 
#   group_by(PWSID, GEO.id2, Size) %>%
#   summarize(n_total_detects = sum(as.numeric(detchem), na.rm = FALSE)) 
# 
#   
#   
# for(i in unique(perc_sys_det_ucmr$n_total_detects)) {
#   cr <-  perc_sys_det_ucmr %>% 
#     group_by(GEO.id2) %>%
#     summarize(perc_sys = round(length(unique(PWSID[which(n_total_detects == i)]))/length(unique(PWSID))*100, 2)) %>% 
#     mutate(fips = as.numeric(GEO.id2)) %>% 
#     right_join(map_county) %>%
#     ggplot(aes(long, lat, group = group, fill = perc_sys)) +
#     geom_polygon(color = "black") +
#     coord_map() +
#     ggtitle(paste("percent of systems with",i ,"detectable UCMR chem")) +
#     scale_fill_viridis(na.value = "light grey") +
#     theme(panel.background = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           legend.position = "right")
#   
#   print(cr)
#   #ggsave(paste0("results/output/Map of percent of systems with ",i ," detectable UCMR chems.png"), width = 15, height = 10, units = "in")
# }
# 
# 
# 
# ################################################################################
# # 6. SUMMARIZE CHARACTERISTICS OF UCMR SYSTEMS ####
# ################################################################################
# 
# 
# 
# # SUMMARIZE MOST CONTAMINATED SYSTEMS 
# getcount <- function(dat) {
#   dat2 <- dat %>% 
#     #filter(test_method == "PFAS") %>% 
#     filter(contam.pfas != "evrdet") %>% 
#     group_by(PWSID) %>% 
#     summarize(n_det = length(unique(contam.pfas[which(detchem == 1)]))) %>%
#     mutate(n_det = as.character(n_det))
#   
#   dat3 <- dat %>% 
#     #filter(test_method == "PFAS") %>% 
#     filter(contam.pfas == "evrdet") %>% 
#     filter(detchem == 1) %>%
#     mutate(n_det = "Any detected UCMR") %>%
#     select(PWSID, n_det) %>% 
#     bind_rows(dat2)
#   
#   dat %>% 
#     select(-contam.pfas, detchem, -hlvlchem, -evrovrhlvl) %>% 
#     unique() %>% 
#     left_join(dat3)
#   
# }
# 
# #drop systems with NA
# ucmrdf.clean_summ <- ucmrdf.clean %>% 
#   filter(contam.pfas == "evrdet") %>% 
#   filter(test_chem == "bin_TRI") 
# 
# demobreakdown <- function(dat) {
#   dat %>%
#     summarize(
#       n.sys = length(unique(PWSID)),
#       n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
#       `Median Percent Black (Q1, Q3)` = paste0(round(median(perc_black_nohisp, na.rm = TRUE),1),
#                          " (",  round(quantile(perc_black_nohisp, .25),1), ", ",
#                          round(quantile(perc_black_nohisp, .75),1), ")"), 
#       `Median Percent Hispanic (Q1, Q3)` = paste0(round(median(perc_hisp_any, na.rm = TRUE),1),
#                         " (",  round(quantile(perc_hisp_any, .25),1), ", ",
#                         round(quantile(perc_hisp_any, .75),1), ")"),
#       `Median Percent Poverty (Q1, Q3)` = paste0(round(median(perc_pov_ppl, na.rm = TRUE),1),
#                        " (",  round(quantile(perc_pov_ppl, .25),1), ", ",
#                        round(quantile(perc_pov_ppl, .75),1), ")"),
#       `Median Percent Uninsured (Q1, Q3)` = paste0(round(median(perc_uninsur, na.rm = TRUE),1),
#                            " (",  round(quantile(perc_uninsur, .25),1), ", ",
#                            round(quantile(perc_uninsur, .75),1), ")"),
#       `Median MDI Rate (Q1, Q3)` = paste0(round(median(mdi, na.rm = TRUE),1),
#                        " (",  round(quantile(mdi, na.rm = TRUE, .25),1), ", ",
#                        round(quantile(mdi, na.rm = TRUE, .75),1), ")"),
#       `Median Percent Homeownersip (Q1, Q3)` = paste0(round(median(perc_hmown, na.rm = TRUE),1),
#                          " (",  round(quantile(perc_hmown, .25),1), ", ",
#                          round(quantile(perc_hmown, .75),1), ")"),
#       `Median Percent Urban (Q1, Q3)` = paste0(round(median(propurban*100, na.rm = TRUE),1),
#                        " (",  round(quantile(propurban*100, .25),1), ", ",
#                        round(quantile(propurban*100, .75),1), ")"),
#       `Percent Small System` = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 2),
#       `Percent GW Systems` = round(length(unique(PWSID[which(source_type == "GW")]))/length(source_type)*100, 2)
#     )
#     
# }
# 
# region_census_brkdwn <- ucmrdf.clean_summ %>% group_by(region_census) %>%
#   demobreakdown() %>% mutate(brktype = "region_census")
# 
# region_usgs_brkdwn <- ucmrdf.clean_summ %>% group_by(region_usgs) %>%
#   demobreakdown() %>% mutate(brktype = "region_usgs")
# 
# overall_brkdwn <- ucmrdf.clean_summ %>%
#   group_by(.) %>% 
#   demobreakdown() %>% 
#   mutate(brktype = "All UCMR Systems")
# 
# # detected_brkdwn <- ucmrdf.clean_summ %>% group_by(detchem) %>% 
# #   demobreakdown() %>% 
# #   filter(detchem == "1") %>%
# #   mutate(brktype = "Any UCMR Detection")
# 
# size_brkdwn <- ucmrdf.clean_summ %>% group_by(Size) %>%
#   demobreakdown() %>% mutate(brktype = paste0("System Size (", Size, ")"))
# 
# sys.size_brkdwn <- ucmrdf.clean_summ %>% group_by(sys.size) %>%
#   demobreakdown() %>% mutate(brktype = "System Size")
# 
# source_brkdwn <- ucmrdf.clean_summ %>% group_by(source_type) %>%
#   demobreakdown() %>% mutate(brktype = paste0("Source Water (", source_type, ")"))
# 
# n.det_brkdwn <- ucmrdf.clean %>% 
#   filter(test_chem == "bin_TRI")  %>% 
#   getcount() %>% 
#   group_by(n_det) %>% 
#   demobreakdown() %>% 
#   mutate(brktype = case_when(n_det == "Any detected UCMR" ~ "Any UCMR Detection",
#                              TRUE ~ paste0("Number of detected UCMR chemicals (", n_det, ")"))) %>% 
#   arrange(brktype)
# 
# all_brkdwn <- bind_rows(overall_brkdwn, region_census_brkdwn, size_brkdwn, source_brkdwn)
# 
# subset_brkdwn <- bind_rows(overall_brkdwn, n.det_brkdwn, size_brkdwn, source_brkdwn) %>% 
#   select(-source_type, -Size, -n_det) %>% 
#   relocate(`brktype`, .before = "n.sys")
# 
# 
# #write_csv(subset_brkdwn, "results/output/system characteristics by grouping.csv")
# 
# #write_csv(all_brkdwn, "results/output/system characteristics by category.csv")
# 
# 
# # n_det_per_system <- ucmrdf.clean %>% 
# #   #filter(contam.pfas == "evrdet") %>% 
# #   filter(test_chem == "bin_TRI") %>% 
# #   #filter(test_method == "PFAS") %>% 
# #   #filter(!is.na(contam.pfas)) %>% 
# #   getcount() %>%
# #   group_by(n_det) %>% 
# #   summarize(
# #     n.sys = length(unique(PWSID)),
# #     n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
# #     `Median Percent Black` = round(median(perc_black_nohisp, na.rm = TRUE),1), 
# #     `Median Percent Hispanic` = round(median(perc_hisp_any, na.rm = TRUE),1),
# #     `Median Percent Poverty` = round(median(perc_pov_ppl, na.rm = TRUE),1),
# #     `Median Percent Uninsured` = round(median(perc_uninsur, na.rm = TRUE),1),
# #     `Median MDI Rate` = round(median(mdi, na.rm = TRUE),1),
# #     `Median Percent Homeownersip` = round(median(perc_hmown, na.rm = TRUE),1),
# #     `Median Percent Urban` = round(median(propurban*100, na.rm = TRUE),1),
# #     `Percent Small System` = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 2),
# #     `Percent GW Systems` = round(length(unique(PWSID[which(source_type == "GW")]))/length(source_type)*100, 2)
# #   ) %>% 
# #   #bind_rows(overall_brkdwn) %>%
# #   mutate(brktype = as.character(n_det)) %>% 
# #   pivot_longer(names_to = "variable", values_to = "value", `Median Percent Black`:`Percent GW Systems`) %>%
# #   # mutate(variable = case_when(variable == "med.hisp" ~ "Percent Hispanic",
# #   #                             variable == "med.black" ~ "Percent Black",
# #   #                             variable == "med.pov" ~ "Percent Poverty",
# #   #                             variable == "med.hmown" ~  "Percent Homeownership",
# #   #                             variable == "med.urb" ~ "Percent Urban Households",
# #   #                             variable == "prop.small" ~ "Percent Small System",
# #   #                             variable == "prop.GW" ~ "Percent GW",
# #   #                             variable == "med.mdi" ~ " MDI Rate",
# #   #                             variable == "med.uninsur" ~ "Percent Uninsured")) %>% 
# #   filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership"))
# # 
# # # bar plot with medians 
# # ggplot(n_det_per_system, aes(x = brktype, y = value, fill = brktype))+ 
# #   geom_bar(stat = "identity", position = position_dodge()) + 
# #   scale_fill_viridis_d(name = "Number of Detected UCMR Chemicals") +
# #   geom_text(aes(label = paste0(value, "%")),position = position_dodge(0.9), vjust = 0) + 
# #   theme(axis.text = element_text(size=12),
# #         axis.title = element_text(size=14),
# #         legend.text = element_text(size=12),
# #         strip.text.x = element_text(size=12),
# #         strip.text.y = element_text(size=12),
# #         legend.position = "none")+
# #   xlab("Number of Detected UCMR Chemicals") +
# #   ylab("")+
# #   facet_wrap(~variable, scales = "free_y")
# 
# n_det_per_system <- ucmrdf.clean %>% 
#   #filter(contam.pfas == "evrdet") %>% 
#   filter(test_chem == "bin_TRI") %>% 
#   select(PWSID, contam.pfas, detchem, all_of(demo_cols), hlvlchem, evrovrhlvl) %>% 
#   getcount() %>% 
#   select(-detchem) %>%
#   unique() %>%
#   bind_rows(ucmrdf.clean %>% filter(test_chem == "bin_TRI") %>% filter(contam.pfas == "evrdet") %>% mutate(n_det = "Systems in UCMR")) %>% 
#   pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#   mutate(variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
#                               variable == "perc_black_nohisp" ~ "Percent Black",
#                               variable == "perc_pov_ppl" ~ "Percent Poverty",
#                               variable == "perc_hmown" ~  "Percent Homeownership",
#                               variable == "propurban" ~ "Percent Urban Households",
#                               # variable == "prop.small" ~ "Percent Small System",
#                               # variable == "prop.GW" ~ "Percent GW",
#                               variable == "mdi" ~ "MDI Rate",
#                               variable == "perc_uninsur" ~ "Percent Uninsured",
#                               TRUE ~ variable),
#          value = case_when(variable == "Percent Urban Households" ~ value*100,
#                            TRUE ~ value)) %>% 
#   mutate(n_det_num = as.numeric(n_det),
#          n_det = factor(n_det, levels = c("Systems in UCMR", "Any detected UCMR", "0", "1", "2", "3", "4"),
#                         labels = c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical", "0 Detects","1 Detect",
#                                   "2 Detects", "3 Detects", "4 Detects"))) %>% 
#   select(PWSID, variable, value, n_det, n_det_num)
# 
# # 
# # # run all variables through the model one at a time by contaminant
# # cr_comp_list <- list()
# # cr_comp <- data.frame()
# # 
# # for(i in c(" MDI Rate", "Percent Hispanic", "Percent Black", "Percent Urban Households")){
# # cr4 <- cr %>% 
# #   filter(variable == i) %>% 
# #   filter(!is.na(value))
# # 
# # cr5 <- aov(value~n_det, data = cr4)
# # 
# # cr6 <- tidy(TukeyHSD(cr5))
# # cr6.2 <- cr6 %>% 
# #   mutate(adj.p.value = case_when((adj.p.value < .05 & adj.p.value > .01) ~ paste0("*"),
# #                              (adj.p.value < .01 & adj.p.value > .001) ~ paste0("**"),
# #                              (adj.p.value < .001) ~ paste0("***"),
# #                              TRUE ~ "")) %>% 
# #   select(contrast, adj.p.value) %>% 
# #   separate(contrast, into = c("group1", "group2"), sep = "-") %>% 
# #   pivot_wider(names_from = group2, values_from = adj.p.value) %>% 
# #   mutate(variable = i,
# #          type = "tukey")
# # 
# # cr7 <- tidy(pairwise.t.test(cr4$value, cr4$n_det))
# # cr8 <- cr7 %>% 
# #   mutate(p.value = case_when((p.value < .05 & p.value > .01) ~ paste0("*"),
# #                             (p.value < .01 & p.value > .001) ~ paste0("**"),
# #                             (p.value < .001) ~ paste0("***"),
# #                             TRUE ~ "")) %>% 
# #   pivot_wider(names_from = group2, values_from = p.value) %>% 
# #   mutate(variable = i,
# #          type = "pairwise")
# # 
# # cr_comp_list[[i]] <- bind_rows(cr6.2, cr8)
# # }
# # 
# # cr_comp <- bind_rows(cr_comp_list) 
# # 
# # 
# cr_summary <- n_det_per_system %>%
#   filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>%
#   group_by(n_det, variable) %>%
#   summarize(median = median(value, na.rm = TRUE),
#             mean = mean(value, na.rm = TRUE)) %>%
#   mutate(fill_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ "0",
#                          TRUE ~ "1"))
# cr_summary2 <- cr %>%
#   filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "MDI Rate")) %>%
#   group_by(n_det, variable) %>%
#   summarize(median = median(value, na.rm = TRUE),
#             mean = mean(value, na.rm = TRUE)) %>%
#   mutate(fill_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ "0",
#                                 TRUE ~ "1"))
# # 
# # just_detcount <- cr %>% 
# #   filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")) %>% 
# #   filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) 
# # 
# # just_detcount_pov <- cr %>% 
# #   filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")) %>% 
# #   filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", " MDI Rate")) 
# # 
# # modlm <- function(dat){
# #   lm(value ~ n_det_num, dat)
# # }
# # 
# # fit <-  just_detcount %>%
# #   group_by(variable) %>% 
# #   nest() %>% 
# #   mutate(mod = purrr::map(data, modlm),
# #          #mod_summ = purrr::map(mod, summary),
# #          mod_df = purrr::map(mod, tidy)) %>% 
# #   unnest(c(mod_df)) %>% 
# #   select(-data, -mod) %>% 
# #   filter(term!= "(Intercept)")
# # 
# # fit2 <-  just_detcount_pov %>%
# #   group_by(variable) %>% 
# #   nest() %>% 
# #   mutate(mod = purrr::map(data, modlm),
# #          #mod_summ = purrr::map(mod, summary),
# #          mod_df = purrr::map(mod, tidy)) %>% 
# #   unnest(c(mod_df)) %>% 
# #   select(-data, -mod) %>% 
# #   filter(term!= "(Intercept)")
# # 
# # cr01 <- just_detcount %>%
# #   filter(variable == "Percent Hispanic")
# # 
# # cr02 <- lm(value ~ n_det_num, cr01)
# 
# # dat2 <- cr %>%
# #   filter(!n_det %in% c("Systems in UCMR", "Any detected UCMR")) %>%
# #   mutate(n_det = as.numeric(n_det)) 
# # lm(as.numeric(n_det) ~ value, data = dat2)
# 
#   # ggplot(dat2,aes(x = (n_det), y = value), group = 1) +
#   # stat_smooth(method = "lm") + 
#   #   geom_boxplot(aes(group = as.factor(n_det))) + 
#   # xlab("") + 
#   # ylab("") + 
#   # theme_minimal() + 
#   # theme(axis.text = element_text(size=14),
#   #       strip.text = element_text(size = 16, face = "bold"),
#   #       legend.position = "none")+
#   # facet_wrap(~variable, scales = "free_y")
# 
# 
# n_det_per_system %>% 
#   filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>% 
#   mutate(variable = factor(variable, levels = c("Percent Hispanic", "Percent Black", "MDI Rate", "Percent Urban Households"), ordered = TRUE)) %>% 
#   mutate(color_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ as.character(n_det_num))) %>% 
#   #filter(!n_det %in% c("Systems in UCMR", "Any detected UCMR")) %>%
#   #mutate(n_det = as.numeric(n_det)) %>%
#   ggplot() + 
#   #geom_sina(aes(x = as.factor(n_det), y = value), color = "#9bccc6") + 
#   geom_sina(aes(x = as.factor(n_det), y = value, color = color_group)) + 
#   geom_boxplot(aes(x = as.factor(n_det), y = value),width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") +
#  # stat_smooth(just_detcount, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
#   # geom_signif(aes(x = as.factor(n_det), y = value), 
#   #             comparisons = list(c("Systems with \nAny Detected \nUCMR Chemical", "0 Detects")),
#   #                                # c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical"), 
#   #                                # c("All Systems \nin UCMR", "0 Detects")),
#   #             map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1, textsize=5, tip_length =  .01,
#   #             margin_top = .01) + 
#   geom_line(cr_summary %>%   filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")), 
#             mapping = aes(x = as.factor(n_det), y = mean), group = 1, color = "#666666", size = 1) + 
#   #stat_smooth(cr, aes(x =as.numeric()) geom="line", size = 3, group = 1) + 
#   geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = mean), fill = "#ab4028", shape = 23, size = 3) + 
#   xlab("") + 
#   ylab("") + 
#   #scale_color_viridis_d(na.value = "#55c667", begin = 0, end = 0.5) + 
#   scale_color_manual(na.value = "#d77659", values = c("#9bccc6", "#50a6a6", "#457d7c", "#004552", "#1a3438")) + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold"),
#         legend.position = "none")+
#   facet_wrap(~as.factor(variable))
#   
# # ggsave("results/output/sina-boxplot of systems by number of detected UCMR chemicals.png", width = 20, height = 12.55, units = "in")
# #for when using facet_grid ggsave("results/output/sina-boxplot of systems by number of detected UCMR chemicals_wide.png", width = 25, height = 12.55, units = "in")
# 
#   
#   # cr %>% 
#   #   filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", " MDI Rate"))  %>%
#   #   mutate(tag = case_when(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership") ~ "1",
#   #                          TRUE ~ "2")) %>% 
#   #   ggplot(aes(x = as.factor(n_det), y = value)) + 
#   #   geom_sina(aes(color = tag)) + 
#   #   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1) + 
#   #   #stat_smooth(just_detcount_pov, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
#   #   geom_signif(aes(x = as.factor(n_det), y = value), 
#   #               comparisons = list(c("Systems with \nAny Detected \nUCMR Chemical", "0 Detects")),
#   #               # c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical"), 
#   #               # c("All Systems \nin UCMR", "0 Detects")),
#   #               map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1, textsize=5, tip_length =  .01,
#   #               margin_top = .01) + 
#   #    geom_line(cr_summary2, mapping = aes(x = as.factor(n_det), y = median), group = 1) + 
#   #   #geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = median), color = "#d77659", shape = "*", size = 5) + 
#   #   xlab("") + 
#   #   ylab("") + 
#   #   scale_color_manual(values = c( "#d77659", "#9bccc6")) + 
#   #   theme_minimal() + 
#   #   theme(axis.text = element_text(size=14),
#   #         strip.text = element_text(size = 16, face = "bold"),
#   #         legend.position = "none")+
#   #   facet_wrap(~variable, scales = "free_y")
#   
# 
# 
# cr %>% 
#   filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "MDI Rate"))  %>%
#   mutate(color_group = case_when(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical") ~ as.character(n_det_num))) %>% 
#   #filter(!n_det %in% c("Systems in UCMR", "Any detected UCMR")) %>%
#   #mutate(n_det = as.numeric(n_det)) %>%
#   ggplot() + 
#   #geom_sina(aes(x = as.factor(n_det), y = value), color = "#9bccc6") + 
#   geom_sina(aes(x = as.factor(n_det), y = value, color = color_group)) + 
#   geom_boxplot(aes(x = as.factor(n_det), y = value),width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1, color = "#3a3838") +
#   # #stat_smooth(just_detcount_pov, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
#   # geom_signif(aes(x = as.factor(n_det), y = value), 
#   #             comparisons = list(c("Systems with \nAny Detected \nUCMR Chemical", "0 Detects")),
#   #             # c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical"), 
#   #             # c("All Systems \nin UCMR", "0 Detects")),
#   #             map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1, textsize=5, tip_length =  .01,
#   #             margin_top = .01) + 
#   geom_line(cr_summary2 %>% filter(!n_det %in% c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical")), 
#             mapping = aes(x = as.factor(n_det), y = mean), group = 1, color = "#666666", size = 1) + 
#   #stat_smooth(cr, aes(x =as.numeric()) geom="line", size = 3, group = 1) + 
#   geom_point(cr_summary2, mapping = aes(x = as.factor(n_det), y = mean), fill = "#ab4028", shape = 23, size = 3) + 
#   #geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = median), color = "#d77659", shape = "*", size = 5) + 
#   xlab("") + 
#   ylab("") + 
#   scale_color_manual(na.value = "#d77659", values = c("#9bccc6", "#50a6a6", "#457d7c", "#004552", "#1a3438")) + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold"),
#         legend.position = "none")+
#   facet_wrap(~variable, scales = "free_y")
# 
# 
# 
#   ggsave("results/output/sina-boxplot of systems by number of detected UCMR chemicals (pov variables only).png", width = 20, height = 12.55, units = "in")
#   
#   
#   
#   
# sys_size_sub <- ucmrdf.clean %>%
#     filter(test_chem == "bin_TRI") %>%
#     select(PWSID, contam.pfas, detchem, all_of(demo_cols), Size) %>%
#     #filter(detchem == "1") %>%
#     pivot_longer(names_to = "variable", values_to = "value", c(all_of(demo_cols))) %>%
#     mutate(variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
#                                 variable == "perc_black_nohisp" ~ "Percent Black",
#                                 variable == "perc_pov_ppl" ~ "Percent Poverty",
#                                 variable == "perc_hmown" ~  "Percent Homeownership",
#                                 variable == "propurban" ~ "Percent Urban Households",
#                                 # variable == "prop.small" ~ "Percent Small System",
#                                 # variable == "prop.GW" ~ "Percent GW",
#                                 variable == "mdi" ~ " MDI Rate",
#                                 variable == "perc_uninsur" ~ "Percent Uninsured",
#                                 TRUE ~ variable),
#            value = case_when(variable == "Percent Urban Households" ~ value*100,
#                              TRUE ~ value)) %>%
#     mutate(contam.pfas = case_when(contam.pfas == "evrdet" ~ "Any UCMR \ndetection",
#                                    contam.pfas == "HCFC-22" ~ " HCFC-22",
#                                    contam.pfas == "PFAS" ~ " PFAS",
#                                    TRUE ~ contam.pfas)) %>%
#     mutate(tag = case_when(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership") ~ "1",
#                            TRUE ~ "2")) %>% 
#     filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", " MDI Rate")) %>% 
#   filter(contam.pfas == "Any UCMR \ndetection")
#   
#   
#     
# ggplot(sys_size_sub, aes(x = as.factor(detchem), y = value)) + 
#     geom_sina(aes(color = as.factor(Size))) + 
#     geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1) + 
#     #stat_smooth(just_detcount_pov, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
#     geom_signif(aes(x = as.factor(detchem), y = value), 
#                 comparisons = list(c("1", "0")),
#                 # c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical"), 
#                 # c("All Systems \nin UCMR", "0 Detects")),
#                 map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1, textsize=5, tip_length =  .01,
#                 margin_top = .01) + 
#     #geom_line(cr_summary2, mapping = aes(x = as.factor(n_det), y = median), group = 1) + 
#     #geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = median), color = "#d77659", shape = "*", size = 5) + 
#     xlab("") + 
#     ylab("") + 
#     scale_color_manual(values = c( "#d77659", "#9bccc6")) + 
#     theme_minimal() + 
#     theme(axis.text = element_text(size=14),
#           strip.text = element_text(size = 16, face = "bold"),
#           legend.position = "none")+
#     facet_grid(variable~Size, scales = "free_y")
# ggsave("results/output/sina-boxplot of systems by Size (pov variables only).png", width = 10, height = 15, units = "in")
# 
# 
# ggplot(sys_size_sub, aes(x = as.factor(Size), y = value)) + 
#   geom_sina(aes(color = as.factor(Size))) + 
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1) + 
#   #stat_smooth(just_detcount_pov, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
#   geom_signif(aes(x = as.factor(Size), y = value), 
#               comparisons = list(c("L", "S")),
#               # c("All Systems \nin UCMR", "Systems with \nAny Detected \nUCMR Chemical"), 
#               # c("All Systems \nin UCMR", "0 Detects")),
#               map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1, textsize=5, tip_length =  .01,
#               margin_top = .01) + 
#   #geom_line(cr_summary2, mapping = aes(x = as.factor(n_det), y = median), group = 1) + 
#   #geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = median), color = "#d77659", shape = "*", size = 5) + 
#   xlab("") + 
#   ylab("") + 
#   scale_color_manual(values = c( "#d77659", "#9bccc6")) + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold"),
#         legend.position = "none")+
#   facet_grid(~variable, scales = "free_y")
# 
# 
#   ucmrdf.clean %>%
#     filter(test_chem == "bin_TRI") %>%
#     select(PWSID, contam.pfas, detchem, all_of(demo_cols)) %>%
#     #filter(detchem == "1") %>%
#     pivot_longer(names_to = "variable", values_to = "value", all_of(demo_cols)) %>%
#     mutate(variable = case_when(variable == "perc_hisp_any" ~ "Percent Hispanic",
#                                 variable == "perc_black_nohisp" ~ "Percent Black",
#                                 variable == "perc_pov_ppl" ~ "Percent Poverty",
#                                 variable == "perc_hmown" ~  "Percent Homeownership",
#                                 variable == "propurban" ~ "Percent Urban Households",
#                                 # variable == "prop.small" ~ "Percent Small System",
#                                 # variable == "prop.GW" ~ "Percent GW",
#                                 variable == "mdi" ~ " MDI Rate",
#                                 variable == "perc_uninsur" ~ "Percent Uninsured",
#                                 TRUE ~ variable),
#            value = case_when(variable == "Percent Urban Households" ~ value*100,
#                              TRUE ~ value)) %>%
#     mutate(contam.pfas = case_when(contam.pfas == "evrdet" ~ "Any UCMR \ndetection",
#                                    contam.pfas == "HCFC-22" ~ " HCFC-22",
#                                    contam.pfas == "PFAS" ~ " PFAS",
#                                    TRUE ~ contam.pfas)) %>%
#     filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp"))  %>%
#     #filter(variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", " MDI Rate"))  %>%
#     ggplot(aes(x = as.factor(detchem), y = value)) + 
#     geom_sina(mapping = aes(color = as.factor(detchem))) + 
#     geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1) +
#     geom_signif(aes(x = as.factor(detchem), y = value), 
#                 comparisons = list(c("0", "1")),
#                 map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .01) +
#     scale_color_manual(values = c("#457d7c", "#d77659"))+
#     xlab("") +
#     ylab("") +
#     theme(axis.text = element_text(size=12),
#           axis.title = element_text(size=14),
#           legend.text = element_text(size=12),
#           strip.text.x = element_text(size=12),
#           strip.text.y = element_text(size=12),
#           legend.position = "none")+
#     facet_grid(variable~contam.pfas, scales = "free_y")
#   
# ggsave("results/output/sina-boxplot of systems by UCMR chems.png", width = 25, height = 20, units = "in")
#   
#   
#   
# #ggsave("results/output/Summary of systems by number of detected UCMR chemicals.png", width = 15, height = 10, units = "in")
# 
# # 
# #   cr_summ3 <- cr %>% 
# #     filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>% 
# #     group_by(n_det, variable) %>% 
# #     summarize(mean = mean(value))
# #   
# #   cr %>% 
# #     filter(!variable %in% c("Percent Uninsured", "Percent Poverty", "Percent Homeownership", "perc_white_nohisp")) %>% 
# #     #filter(!n_det %in% c("Systems in UCMR", "Any detected UCMR")) %>%
# #     #mutate(n_det = as.numeric(n_det)) %>%
# #     ggplot() + 
# #     geom_sina(aes(x = as.factor(n_det), y = value), color = "#9bccc6") + 
# #     geom_boxplot(aes(x = as.factor(n_det), y = value),width = 0.1, guides = FALSE, outlier.shape = NA, alpha = 0.5, size = 1) +
# #     stat_smooth(just_detcount, mapping = aes(x = as.numeric(n_det), y = value), method = "lm", na.rm = TRUE, color = "#ab4028") + 
# #     geom_point(cr_summ3, mapping = aes(x = as.factor(n_det), y = mean)) +  
# #     #geom_text(fit, mapping = aes(label = paste0("p = ", round(p.value,4))), x = 5, y = 50) + 
# #     # labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
# #     #                    "Intercept =",signif(fit$coef[[1]],5 ),
# #     #                    " Slope =",signif(fit$coef[[2]], 5),
# #     #                    " P =",signif(summary(fit)$coef[2,4], 5))) + 
# #     # geom_signif(aes(x = as.factor(n_det), y = value), 
# #     #             comparisons = list(c("Systems with \nAny Detected \nUCMR Chemical", "0 Detects"), c("All Systems \nin UCMR", "0 Detects")),
# #     #             map_signif_level = TRUE, step_increase = .1) + 
# #     geom_signif(aes(x = as.factor(n_det), y = value), 
# #                 comparisons = list(c("Systems with \nAny Detected \nUCMR Chemical", "0 Detects"), c("All Systems \nin UCMR", "0 Detects")),
# #                 map_signif_level=c("***"=0.001,"**"=0.01, "*"=0.05, " "=2), step_increase = .1) + 
# #     #geom_line(cr_summary, mapping = aes(x = as.factor(n_det), y = median), group = 1) + 
# #     #stat_smooth(cr, aes(x =as.numeric()) geom="line", size = 3, group = 1) + 
# #     #geom_point(cr_summary, mapping = aes(x = as.factor(n_det), y = median), color = "#d77659", shape = "*", size = 5) + 
# #     xlab("") + 
# #     ylab("") + 
# #     theme_minimal() + 
# #     theme(axis.text = element_text(size=14),
# #           strip.text = element_text(size = 16, face = "bold"),
# #           legend.position = "none")+
# #     facet_wrap(~variable, scales = "free_y")
#   
# 
# 
# 
# 
# 
# ################### WORK SPACE ############################
# 
# raincloud_theme = theme(
#   text = element_text(size = 10),
#   axis.title.x = element_text(size = 16),
#   axis.title.y = element_text(size = 16),
#   axis.text = element_text(size = 14),
#   axis.text.x = element_text(angle = 45, vjust = 0.5),
#   legend.title=element_text(size=16),
#   legend.text=element_text(size=16),
#   legend.position = "right",
#   plot.title = element_text(lineheight=.8, face="bold", size = 16),
#   panel.border = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.grid.major = element_blank(),
#   axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#   axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
# 
# geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
#                              position = "dodge", trim = TRUE, scale = "area",
#                              show.legend = NA, inherit.aes = TRUE, ...) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomFlatViolin,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       trim = trim,
#       scale = scale,
#       ...
#     )
#   )
# }
# 
# GeomFlatViolin <-
#   ggproto("GeomFlatViolin", Geom,
#           setup_data = function(data, params) {
#             data$width <- data$width %||%
#               params$width %||% (resolution(data$x, FALSE) * 0.9)
#             
#             # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
#             data %>%
#               group_by(group) %>%
#               mutate(ymin = min(y),
#                      ymax = max(y),
#                      xmin = x,
#                      xmax = x + width / 2)
#             
#           },
#           
#           draw_group = function(data, panel_scales, coord) {
#             # Find the points for the line to go all the way around
#             data <- transform(data, xminv = x,
#                               xmaxv = x + violinwidth * (xmax - x))
#             
#             # Make sure it's sorted properly to draw the outline
#             newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
#                              plyr::arrange(transform(data, x = xmaxv), -y))
#             
#             # Close the polygon: set first and last point the same
#             # Needed for coord_polar and such
#             newdata <- rbind(newdata, newdata[1,])
#             
#             ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
#           },
#           
#           draw_key = draw_key_polygon,
#           
#           default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
#                             alpha = NA, linetype = "solid"),
#           
#           required_aes = c("x", "y")
#   )
# 
# ggplot(cr, aes(x = as.factor(n_det), y = value)) + 
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
#   geom_point(aes(y = value, color = n_det), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
#   geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) + 
#   guides(fill = FALSE) +
#   guides(color = FALSE) +
#   scale_color_brewer(palette = "Spectral") +
#   scale_fill_brewer(palette = "Spectral") +
#   coord_flip() + 
#   theme_bw() + 
#   facet_wrap(~variable, scales = "free_y") + 
#   raincloud_theme
# 
# 
# 

################################################################################
# 9. CHARACTERIZE MOST CONTAM SYSTEMS ######
################################################################################


# pws_bydetchem <- pws_demo %>% 
#   filter(test_chem == "PFAS") %>%
#   right_join(perc_sys_det_ucmr) %>% 
#   group_by(n_total_detects) %>% 
#   summarize(    n.sys = length(unique(PWSID)),
#                 n.ppl = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
#                 med.black = round(median(perc_black_nohisp, na.rm = TRUE),1), 
#                 med.hisp = round(median(perc_hisp_any, na.rm = TRUE),1),
#                 med.pov = round(median(perc_pov_ppl, na.rm = TRUE),1),
#                 med.hmown = round(median(perc_hmown, na.rm = TRUE),1),
#                 med.urb = round(median(propurban, na.rm = TRUE),1),
#                 med.mdi = round(median(mdi, na.rm = TRUE), 1),
#                 prop.small = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 2),
#                 prop.SW = round(length(unique(PWSID[which(WS.GW_SW_CODE == "SW")]))/length(WS.GW_SW_CODE)*100, 2)
#   )%>% 
#   mutate(brktype = as.character(n_total_detects)) %>% 
#   bind_rows(overall_brkdwn) %>% 
#   mutate(med.urb = med.urb*100,
#          brktype = case_when(brktype == "overall" ~ "All Systems in UCMR",
#                              TRUE ~ brktype)) 
# 
# pws_bydetchem_plot <- pws_bydetchem %>% 
#   pivot_longer(names_to = "variable", values_to = "value", med.black:prop.SW) %>% 
#   mutate(variable = case_when(variable == "med.hisp" ~ " Median Percent Hispanic",
#                               variable == "med.black" ~ " Median Percent Black",
#                               variable == "med.pov" ~ " Median Percent Poverty",
#                               variable == "med.hmown" ~  " Median Percent Homeownership",
#                               variable == "med.urb" ~ " Median Percent Urban Households",
#                               variable == "prop.SW" ~ " Median Percent Surface Water",
#                               variable == "prop.small" ~ "Median Percent Small System",
#                               variable == "med.mdi" ~ " Median MDI Rate"),
#          brktype = str_wrap(brktype, width = 15),
#          n.sys = case_when(variable == "Percent Black" ~ paste0("n = ", n.sys)))
# 
# # n_sys <- pws_bydetchem_plot %>% 
# #   select(n.sys, variable, brktype) %>% 
# #   filter(!(is.na(n.sys)))
# 
# ggplot(pws_bydetchem_plot, aes(x = brktype, y = value, fill = brktype)) + 
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   geom_text(aes(label=paste(round(value,2), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   theme(axis.text = element_text(size=12),
#         axis.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text.x = element_text(size=12),
#         strip.text.y = element_text(size=12),
#         legend.position = "none")+
#   scale_fill_viridis_d() +
#   xlab("") + 
#   ylab("Median Percent") + 
#   facet_wrap(~variable, scales = "free")
# ggsave("results/output/Summary of systems by number of detected UCMR chems.png", width = 15, height = 10, units = "in")

# 
# ggplot(pws_bydetchem_plot, aes(x = variable, y = value, fill = brktype)) + 
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   geom_text(aes(label=paste0(round(value,1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   theme(axis.text = element_text(size=18),
#         axis.title = element_text(size=18),
#         legend.text = element_text(size=18),
#         strip.text.x = element_text(size=18),
#         strip.text.y = element_text(size=18))+
#   scale_fill_viridis_d(name = "Number of Detected \nUCMR chems") +
#   xlab("") + 
#   ylab("Median Percent")

#ggsave("results/output/Summary of systems by number of detected UCMR chems.png", width = 15, height = 10, units = "in")


