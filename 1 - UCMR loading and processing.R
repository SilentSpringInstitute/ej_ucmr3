### AUTHOR: AHz
### LAST EDIT: 2021-06-20 (AHz)
### LAST REVIEW: 2021-06-20 (AHz)
### EDITTED: 2022-12-23 (AM)
### EDITTED: 2023-03-05 (AM)
### WRITTEN IN: R version 3.5.1 and 4.2.2
### Purpose: Load and clean all UCMR data -- create contam.pfas column where all pfas chems are changed to "pfas", 
### add regions, filter out only states and chemicals we want, add detchem/hlvlchem code, add pfoa and pfos to determine 
### if they are over 70ppb health level

library(tidyverse)
library(ggforce)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

################################################################################
#  0. README  ###############################################################
################################################################################

### GOAL 
#
#       UCMR systems sampled for >30 contaminants for different frequencies based 
#       on their source water type. In order to keep PWSs from being overrepresented 
#       in this study, we want to get down to one row per contaminant per PWS.
#       For each PWS we only need to know if that contaminant was ever detected or 
#       ever over an existing health guideline. We also want to know whether that 
#       system ever had a detect of any of the four target contaminants. 
#      
### CODE DICTIONARY 
#     #_# Code outputs something that is referenced in paper
#     #~# Code outputs something that is currently in slide deck (often the same
#     as what is in the paper)
#       
### DATA DICTIONARY
# 
#     Contaminant == name of column in UCMR 3 data download that identifies which
#                   contaminant each measurement is for. In this column, all 6 
#                   PFAS are listed independently
#     contam.pfas == column created to assign “PFAS” to any of the 6 PFAS 
#                   contaminants and leaves all other contaminant names the same 
#     sys.sampid == column holds a unique PWSID + sample ID code that is used to 
#                    group all the PFAS samples together (we want to check if 
#                    any of the 6 PFAS were detected for the same sample)
#     detchem == binary column indicates whether chemical was ever detected (1 = detected) 
#     hlvlchem == binary column indicates whether chemical was ever measured above health 
#                 level guideline (1 = exceeded)
#     evrdet == variable in the contam.pfas column. this "chemical" was created to 
#                 represent "any target contaminant"
#             
#             
### NOTES
# 
#   SECTION 2. UCMR DATA PROCESSING
#       We want to treat all 6 PFAS as one class, so we create a new column 
#       called “contam.pfas” that assigns “PFAS” to any of the 6 PFAS 
#       contaminants and leaves all other contaminant names the same. PWSs 
#       sampled for the 6 PFASs each time they were required to sample. 
#       We want to keep the 6 that were collected at the same time together, 
#       so we need to create a unique code for each sample (“sys.sampid”) by 
#       pasting together the PWSID and the Sample ID. This will allow us to use 
#       group_by(sys.sampid) to summarize down to one row per PFAS sample. 
#       
#       We only want to look at 1,4-dioxane, 1,1-dichloroethane, PFAS, and 
#       HCFC-22, so we filter down to that. We also only want PWSs in the 50 US
#       states and DC. I added a filter for the monitoring requirement to draw 
#       attention to the fact that these are list 1 contaminants, but there are
#       no other monitoring requirement codes once you filter down to the four
#       contaminants we want. 
# 
# SECTION 3. PROCESS PFAS AND BIND
# 
#       We want one row for PFAS for each PWS. In the “detchem” column, we want 
#       to know if any of the 6 PFAS were ever detected. In the “hlvlchem” 
#       column, we want to know if the sum of PFOA and PFOS for a single sample
#       were ever over 70 ppt. We need to group by “sys.sampid” so that we sum 
#       the right PFOA/PFOS samples together. After we get one “detchem” and 
#       “hlvlchem” for each individual sample taken (“sys.sampid”), we then can 
#       group by PWSID to determine whether any sample ever had a PFAS detection
#       or a PFOA/PFOS sum over 70 ppt. 
# 
# SECTION 4. BIND
# 
#       We want to filter out any PFAS samples from our cleaned UCMR dataset 
#       and then bind on the newly cleaned PFAS dataframe. The “ucmr3” dataframe 
#       shows detections of individual samples for our target contaminants – we 
#       still need to group by PWS to get down to one row per PWS. 
#       
#       Since some PWSs report more than 1 water source type (FacilityWaterType), 
#       we want to create a new column (“source_type”) and reassign “MX” “GU” 
#       and any PWSs that report more than one type of facility to a new 
#       category called “MIX” 
#       
#       We group by PWSID and contam.pfas to get one “detchem” and “hlvlchem” 
#       row per PWSID-contam.pfas pair. We also want to know whether a PWS ever 
#       had a detection of a target contaminant. Using a local function, we 
#       group by PWSID (and not contam.pfas) to test whether a system ever had a 
#       detect or health level exceedance and bind that row onto our dataframe.
#       
#       The final dataframe is ucmr_detcode, which is used in script 4a. 
#       
# SECTION 5 
# 
#       This section is meant to be run after script 4a has been run. We decided
#       to remove 7 PWSs that served counties with incomplete MDI data, but that 
#       check does not happen until script 4a. So after the top of 4a is run 
#       (stopifnot statement will keep everything else from running) then we 
#       come back to this script to get the detection frequencies without those 
#       7 PWSs. Detection frequencies are done here so that we can see the 
#       sample detection frequency and the PWS detection frequency (after this 
#       script we no longer reference individual sample data)
#       
# SECTION 6
# 
#       This section reruns the above code on the systems that are excluded from
#       our analysis based on geography. This helps us verify that the PWSs that
#       are excluded (in PR, VI, etc.) are not vastily different than the PWSs
#       that are included. 


################################################################################
#  1. LOAD DATA  ###############################################################
################################################################################

#load in UCMR data 
ucmr3_raw <- read_csv("raw/UCMR3_All.csv") %>% 
  rename(AnalyticalResultsValue = AnalyticalResultValue)

#how many PWSIDs? 
length(unique(ucmr3_raw$PWSID))
#5401

################################################################################
#### 2. UCMR DATA PROCESSING ###################################################
################################################################################

#clean UCMR data: add column that groups PFAS into one contaminant, add geo info, 
# filter to 1,4-dioxane,  1-1,dichloroethane, HCFC-22, and PFAS 
ucmr3_targetsamples <- ucmr3_raw %>% 
  mutate(contam.pfas = case_when(Contaminant %in% c("PFBS", "PFHpA", "PFOA", "PFOS", "PFNA", "PFHxS") ~ "PFAS",
                                 TRUE ~ Contaminant),
         state = substr(PWSID,1,2),
         #get unique PWS-sample ID pair
         sys.sampid = paste0(PWSID, "-", SampleID)) %>%
  mutate(detchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue > 0) ~ 1,
                             TRUE ~ 0),
         # AM: added health-reference limit for 1,1-dichloroethane 
         hlvlchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue >= .35 & contam.pfas == "1,4-dioxane") ~ 1,
                              (!is.na(AnalyticalResultsValue) & AnalyticalResultsValue >= 6.14 & contam.pfas == "1,1-dichloroethane") ~ 1,
                              TRUE ~ 0)) %>%
  filter(!(state %in% c("NN", "PR", "01", "05", "06", "08", "09", "10", "GU", "MP", "VI", "AS"))) %>% 
  filter(contam.pfas %in% c("1,4-dioxane", "1,1-dichloroethane", "PFAS", "HCFC-22")) %>% 
  filter(MonitoringRequirement == "AM")

#how many PWSIDs in analysis?
length(unique(ucmr3_targetsamples$PWSID))
#4815

#get IDs of systems in UCMR (for other scripts)
ucmrsys <- unique(ucmr3_targetsamples$PWSID)

#write_csv(ucmr3_targetsamples, paste0("results/preliminary/ucmr3 processed ", Sys.Date(), ".csv"))


################################################################################
#### 3. PROCESS PFAS AND BIND ##################################################
################################################################################


# investigate PFAS data ########################################################

# pfas_completion <- ucmr3_targetsamples %>% 
#   subset(contam.pfas == "PFAS") %>% 
#   group_by(sys.sampid, PWSID, Contaminant) %>% 
#   count() %>% 
#   group_by(sys.sampid, PWSID) %>% 
#   mutate(n_pfas = n()) %>%
#   pivot_wider(names_from = "Contaminant", values_from = "n", values_fill = 0)

# detcheck <- ucmr3_targetsamples %>% 
#   subset(contam.pfas == "PFAS") %>% 
#   group_by(sys.sampid, PWSID, Contaminant) %>% 
#   summarize(n_det = length(unique(Contaminant[which(detchem == "1")]))) %>% 
#   group_by(sys.sampid, PWSID) %>% 
#   mutate(n_pfas = n()) %>%
#   pivot_wider(names_from = "Contaminant", values_from = "n_det", values_fill = 0)

# process PFAS data ########################################################

pfospfoasum <- ucmr3_targetsamples %>% 
  subset(contam.pfas == "PFAS") %>% 
  group_by(sys.sampid, PWSID, contam.pfas) %>% 
  summarize(sum.pfospfoa = sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))], na.rm = TRUE),
            sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
                                    TRUE ~ 0),
            sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
                                     TRUE ~ 0)) %>% 
  left_join(ucmr3_targetsamples %>%
              select(PWSID:SampleID, SampleEventCode:sys.sampid)) %>%
  unique() 

length(unique(pfospfoasum$sys.sampid))
length(unique(pfospfoasum$PWSID))

# check by JML
length(unique(pfospfoasum$sys.sampid)) == dplyr::n_distinct(ucmr3_targetsamples$sys.sampid[ucmr3_targetsamples$contam.pfas=="PFAS"])
length(unique(pfospfoasum$PWSID)) == n_distinct(ucmr3_targetsamples$PWSID[ucmr3_targetsamples$contam.pfas=="PFAS"])
sum(pfospfoasum$sum.pfospfoa[pfospfoasum$sum.pfospfoa != 0]) == sum(ucmr3_targetsamples$AnalyticalResultsValue[ucmr3_targetsamples$Contaminant == "PFOA" | 
                                                                                                               ucmr3_targetsamples$Contaminant == "PFOS"], na.rm = T) # sum of all obs
#

pfospfoa_sample <- ucmr3_targetsamples %>% 
  subset(contam.pfas == "PFAS") %>% 
  group_by(sys.sampid, PWSID, contam.pfas) %>% 
  summarize(sum.pfospfoa = sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))], na.rm = TRUE),
            sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
                                    TRUE ~ 0),
            sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
                                     TRUE ~ 0)) %>% 
  left_join(ucmr3_targetsamples %>%
              select(PWSID:SampleID, SampleEventCode:sys.sampid)) %>%
  unique() 


################################################################################
#### 4. BIND ###################################################################
################################################################################

 
#drop all PFAS and then merge in pfoapfossum
ucmr3_targetsamples_wpfas <- ucmr3_targetsamples %>% 
  filter(!contam.pfas %in% c("PFAS")) %>% 
  bind_rows(pfospfoasum %>%
              select(-sum.pfospfoa) %>% 
              rename(detchem = sum.detchem,
                     hlvlchem = sum.hlvlchem)) %>% 
  group_by(PWSID, contam.pfas) %>%
  mutate(n.samp = length(unique(sys.sampid)),
         n.date = length(unique(CollectionDate)),
         n.sampfac = length(unique(FacilityID))) %>% 
  filter(contam.pfas %in% c("PFAS", "1,1-dichloroethane", "1,4-dioxane", "HCFC-22"))


#get detection by PWS instead of by facility
evrdet <- function(dat){
  evrdet_chem <- dat %>%
    group_by(PWSID, Size, source_type) %>%
    summarize(detchem = case_when(sum(detchem) >= 1 ~ 1, TRUE ~ 0),
              hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1, TRUE ~ 0)) %>%
    mutate(contam.pfas = "evrdet")
  
  bind_rows(dat, evrdet_chem)
}

#get 1 or 0 for various tests with one row per system and contaminant
ucmr_detcode <- ucmr3_targetsamples_wpfas %>% 
  group_by(PWSID) %>% 
  mutate(n_watertype = length(unique(FacilityWaterType)),
         source_type = case_when(FacilityWaterType == "MX" ~ "MIX",
                                 FacilityWaterType == "GU" ~ "MIX",
                                 TRUE ~ FacilityWaterType),
         source_type = case_when(n_watertype > 1 ~ "MIX",
                                 TRUE ~ source_type)) %>%
  group_by(PWSID, contam.pfas, Size, source_type) %>%
  summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
                                TRUE ~ 0),
            hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
                                 TRUE ~ 0)) %>% 
  ungroup() %>% 
  evrdet()

## check by JML ##
n_distinct(ucmr3_targetsamples_wpfas$PWSID) == n_distinct(ucmr_detcode$PWSID) # are # of PWSIDs equal?

ucmr_detcode %>% group_by(contam.pfas) %>%
  summarise(det.freq = mean(detchem)) %>% ungroup() # detection frequencies

# check for one chem: this should be equal to frequency above for PFAS, and it is!
n_distinct(ucmr3_targetsamples_wpfas$PWSID[ucmr3_targetsamples_wpfas$detchem == 1 &  
                                             ucmr3_targetsamples_wpfas$contam.pfas=="PFAS"]) / 
  n_distinct(ucmr3_targetsamples_wpfas$PWSID[ucmr3_targetsamples_wpfas$contam.pfas=="PFAS"])

## end of check ##

#write_csv(ucmr_detcode, paste0("results/preliminary/ucmr3 processed with detchem code ",Sys.Date(),".csv"))


# #stops script when it's called in other scripts
# 
# stop("end of script 1 - UCMR loading and processing")

# ################################################################################
# #### 5a . GET STATS FOR WRITE UP -- DETECTION FREQUENCIES ######################
# ################################################################################
# 
# #load script 4 so that we can remove the PWSs with missing MDIs
# source("4a - UCMR Summary.R")
# 
# #drop PWSIDs that are missing MDI 
# 
# ucmr3_targetsamples_included <- ucmr3_targetsamples %>% 
#   filter(!PWSID %in% missing_mdi)
# 
# ucmr_detcode_included <- ucmr_detcode %>% 
#   filter(!PWSID %in% missing_mdi)
# 
# #_# PWS detection frequencies --------------------------------------------
# 
# det_freq_contaminant <- ucmr3_targetsamples_included %>% 
#   group_by(PWSID, Contaminant) %>%
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   group_by(Contaminant) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2)) %>% 
#   rename(contam.pfas = Contaminant)
# 
# det_freq_grouped <- ucmr_detcode_included %>% 
#   group_by(contam.pfas) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)
#   )
# 
# #_# sample detection frequencies --------------------------------------------
# 
# 
# indivsample_grouped_detfreq <- ucmr3_targetsamples_included %>%
#   group_by(contam.pfas) %>%
#   summarize(n_sys = length(unique(PWSID)),
#             n_samps = length(unique(sys.sampid)),
#             detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100))
# 
# indivsample_contaminant_detfreq <- ucmr3_targetsamples_included %>%
#   filter(contam.pfas == "PFAS") %>% 
#   group_by(Contaminant) %>%
#   summarize(detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100)) %>% 
#   rename(contam.pfas = Contaminant)
# 
#  
# 
# ucmr_sum <- full_join(det_freq_contaminant, det_freq_grouped) %>% 
#   left_join(bind_rows(indivsample_grouped_detfreq, indivsample_contaminant_detfreq)) %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   select(contam.pfas, detfreq_samps, detfreq_sys, hlvl_freq ) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22",
#                                                       "PFAS", "PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "evrdet"))) %>%
#   arrange(contam.pfas)
#            
# 
# # write_csv(ucmr_sum, "results/preliminary/UCMR sys and sample detection frequency tables.csv")
# 
# #stops script when it's called in other scripts
# 
# # stop("end of script 1 - UCMR loading and processing")
# 
# ################################################################################
# #### 5b . GET STATS FOR WRITE UP -- DETECTION FREQUENCIES BY SYS SIZE ##########
# ################################################################################
# 
# #### STRATIFY BY SYSTEM SIZE ####
# 
# 
# det_freq_contaminant_size <- ucmr3_targetsamples_included %>% 
#   group_by(PWSID, Contaminant, Size) %>%
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   group_by(Contaminant, Size) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2)) %>% 
#   rename(contam.pfas = Contaminant)
# 
# det_freq_grouped_size <- ucmr_detcode_included %>% 
#   group_by(contam.pfas, Size) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)
#   )
# 
# indivsample_grouped_detfreq_size <- ucmr3_targetsamples_included %>%
#   group_by(contam.pfas, Size) %>%
#   summarize(detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100))
# 
# indivsample_contaminant_detfreq_size <- ucmr3_targetsamples_included %>%
#   filter(contam.pfas == "PFAS") %>% 
#   group_by(Contaminant, Size) %>%
#   summarize(detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100)) %>% 
#   rename(contam.pfas = Contaminant)
# 
# 
# ucmr_sum_size <- full_join(det_freq_contaminant_size, det_freq_grouped_size) %>% 
#   left_join(bind_rows(indivsample_grouped_detfreq_size, indivsample_contaminant_detfreq_size)) %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   select(contam.pfas, Size, detfreq_samps, detfreq_sys, hlvl_freq ) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22",
#                                                       "PFAS", "PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "evrdet"))) %>%
#   arrange(contam.pfas)
# 
# # write_csv(ucmr_sum_size, "results/preliminary/UCMR sys and sample detection frequency tables.csv")
# 
# #plot PWS detection frequency by size (visualization of ucmr_sum_size)
# ucmr_detcode_included %>% 
#   group_by(contam.pfas) %>% 
#   summarize(Small = length(unique(PWSID[which(detchem == "1" & Size == "S")]))/length(unique(PWSID[which(Size == "S")]))*100,
#             Large = length(unique(PWSID[which(detchem == "1" & Size == "L")]))/length(unique(PWSID[which(Size == "L")]))*100) %>% 
#   pivot_longer(names_to = "x_val", values_to = "detfreq", Small:Large) %>% 
#   mutate(x_val = factor(x_val, levels = c("Large", "Small"))) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>% 
#   ggplot(aes(x = x_val, y = detfreq, fill = x_val)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(name = "Size",
#                     values = c("#d77659","#004552", "#457d7c"), 
#                     na.value = "#e6e6e6")+
#   geom_text(aes(label=paste0(round(detfreq, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   #xlab("UCMR designated source water type") + 
#   ylab("Percent of samples with a detect") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=20),
#         axis.title.x = element_blank(),
#         strip.text = element_text(size = 20, face = "bold"),
#         legend.position = "none") +
#   facet_grid(~contam.pfas)
# #ggsave("results/output/System Size detection summary.png", width = 17, height = 7, units = "in")
# 
# 
# ################################################################################
# #### 5c . GET STATS FOR WRITE UP -- SOURCE WATER TYPE SUMMARY ##################
# ################################################################################
# 
# #_# summarize water system types
# watersourcetypes <- ucmr3_targetsamples_included %>% 
#   arrange(PWSID, FacilityWaterType) %>% 
#   group_by(PWSID, contam.pfas) %>% 
#   summarize(n_types = length(unique(FacilityWaterType)),
#             n_GW = length(unique(FacilityID[which(FacilityWaterType == "GW")])),
#             n_SW = length(unique(FacilityID[which(FacilityWaterType == "SW")])),
#             types = paste0(unique(FacilityWaterType), collapse = ",")) 
# 
# watersourcetypes_compare <- watersourcetypes %>% 
#   left_join(ucmrdf.demo %>% select(PWSID, Size, WS.GW_SW_CODE) %>% unique())
# 
# watersourcetypes_check <- watersourcetypes_compare %>% 
#   filter(contam.pfas == "PFAS") %>% 
#   mutate(sdwis_check_cat = case_when(n_types == 1 & types == "GW" & WS.GW_SW_CODE == "GW" ~ "GW match",
#                                  n_types == 1 & types == "SW" & WS.GW_SW_CODE == "SW" ~ "SW match",
#                                  n_types == 1 & types == "GW" & WS.GW_SW_CODE == "SW" ~ "GW UCMR mismatch",
#                                  n_types == 1 & types == "SW" & WS.GW_SW_CODE == "GW" ~ "SW UCMR mismatch",
#                                  n_types == 1 & types == "MX" ~ "MX",
#                                  n_types == 1 & types == "GU" ~ "GU",
#                                  is.na(WS.GW_SW_CODE) ~ paste0("NA ", types),
#                                  types == "GW,SW" ~ "GW/SW", 
#                                  n_types > 1 & !str_detect(types, "GW") ~ ">1 type (SW)",
#                                  n_types > 1 & str_detect(types, "GW") ~ ">1 type (GW)"),
#          sdwis_check_size = case_when(sdwis_check_cat %in% c("GW UCMR mismatch", "SW UCMR mismatch") ~ "SDWIS error"))
# 
# watersourcetypes_summary <- watersourcetypes %>% 
#   group_by(contam.pfas, types) %>% 
#   count() %>% 
#   pivot_wider(names_from = contam.pfas, values_from = n)
# 
# #write_csv(watersourcetypes_summary, "results/output/count of water source types by system.csv")
# 
# gwsw_mix <- ucmr3_targetsamples_included %>% 
#   arrange(PWSID, FacilityWaterType) %>% 
#   group_by(PWSID, contam.pfas) %>% 
#   mutate(n_types = length(unique(FacilityWaterType)),
#          types = paste0(unique(FacilityWaterType), collapse = ",")) %>% 
#   filter(types == "GW,SW") %>% 
#   group_by(PWSID, contam.pfas, FacilityName) %>% 
#   summarize(n_gw = length(unique(SampleID[which(FacilityWaterType == "GW")])),
#             n_sw = length(unique(SampleID[which(FacilityWaterType == "SW")])))
# 
# 
# ucmr_detcode_clean_source <- ucmr3_targetsamples_included %>% 
#   group_by(PWSID) %>% 
#   mutate(n_sw = length(unique(FacilityWaterType)),
#          FacilityWaterType = case_when(n_sw > 1 ~ ">1 type",
#                                        TRUE ~ FacilityWaterType),
#          source_type = case_when(FacilityWaterType == "MX" ~ "MIX",
#                                  FacilityWaterType == "GU" ~ "MIX",
#                                  FacilityWaterType == ">1 type" ~ "MIX",
#                                  TRUE ~ FacilityWaterType)) %>%
#   unique() %>% 
#   group_by(PWSID, contam.pfas, Size, FacilityWaterType, source_type) %>%
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0),
#             hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                  TRUE ~ 0)) %>% 
#   ungroup() %>% 
#   group_by(PWSID) %>% 
#   mutate(evrovrhlvl = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   evrdet() %>% 
#   mutate(FacilityWaterType = factor(FacilityWaterType, levels = c("GW",">1 type", "GU", "MX", "SW")))
# 
# 
# # plot recoded water source type PWS summary
# ggplot(ucmr_detcode_clean_source %>% filter(contam.pfas == "PFAS"), aes(x = source_type, fill = FacilityWaterType)) + 
#   geom_bar(aes(y = (..count..)/sum(..count..)*100)) + 
#   scale_fill_manual(name = "UCMR designated \nsource water type",
#                     values = c("#d77659", "#9bccc6","#50a6a6", "#457d7c", "#1a3438"))+
#   #geom_text(aes(label=perc_fac_label), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   xlab("Recoded water source type") + 
#   ylab("Percent of PWSs") + 
#   ggtitle("PWSs by recoded water source type") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold")) 
# #ggsave("results/output/recoded water source type PWS summary.png", width = 6, height = 4, units = "in")
# 
# # plot recoded water source type sample summary
# ucmr3_targetsamples_included %>% 
#   mutate(source_type = case_when(FacilityWaterType %in% c("MX", "GU") ~ "MIX",
#                                  TRUE ~ FacilityWaterType)) %>%
#   mutate(FacilityWaterType = factor(FacilityWaterType, levels = c("GW", "GU", "MX", "SW"))) %>% 
#   filter(Contaminant == "1,4-dioxane") %>% 
#   ggplot(aes(x = source_type, fill = FacilityWaterType)) + 
#   geom_bar(aes(y = (..count..)/sum(..count..)*100)) + 
#   scale_fill_manual(name = "UCMR designated \nsource water type",
#                     values = c("#d77659", "#50a6a6", "#457d7c", "#1a3438"))+
#   #geom_text(aes(label=perc_fac_label), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   xlab("Recoded water source type") + 
#   ylab("Percent of samples") + 
#   ggtitle("Samples by recoded water source type") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold")) 
# #ggsave("results/output/recoded water source type sample summary.png", width = 6, height = 4, units = "in")
# 
# 
# #plot detection frequencies of PWS by recoded source water type (ex: 7.6% of PWSs 
# #had >=1 target contamiannt detected and were MIX)
# ucmr_detcode_clean_source %>% 
#   group_by(contam.pfas) %>% 
#   summarize(SW = length(unique(PWSID[which(detchem == "1" & source_type == "SW")]))/length(unique(PWSID))*100,
#             GW = length(unique(PWSID[which(detchem == "1" & source_type == "GW")]))/length(unique(PWSID))*100,
#             MIX = length(unique(PWSID[which(detchem == "1" & source_type == "MIX")]))/length(unique(PWSID))*100) %>% 
#   pivot_longer(names_to = "x_val", values_to = "detfreq", SW:MIX) %>% 
#   mutate(x_val = factor(x_val, levels = c("GW", "SW", "MIX"))) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>% 
#   ggplot(aes(x = x_val, y = detfreq, fill = x_val)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(name = "UCMR designated \nsource water type",
#                     values = c("#d77659","#004552", "#457d7c"), 
#                     na.value = "#e6e6e6")+
#   geom_text(aes(label=paste0(round(detfreq, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   xlab("UCMR designated source water type") + 
#   ylab("Percent of samples with a detect") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=12),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.position = "none") +
#   facet_grid(~contam.pfas)
# #ggsave("results/output/detection frequency of PWSs by source water type.png", width = 17, height = 7, units = "in")
# 
# #percent of source water type PWS that are detections (ex: 44% of MIX systems had >=1 target contaminant detected)
# ucmr_detcode_clean_source %>% 
#   group_by(contam.pfas) %>% 
#   summarize(SW = length(unique(PWSID[which(detchem == "1" & source_type == "SW")]))/length(unique(PWSID[which(source_type == "SW")]))*100,
#             GW = length(unique(PWSID[which(detchem == "1" & source_type == "GW")]))/length(unique(PWSID[which(source_type == "GW")]))*100,
#             MIX = length(unique(PWSID[which(detchem == "1" & source_type == "MIX")]))/length(unique(PWSID[which(source_type == "MIX")]))*100) %>% 
#   pivot_longer(names_to = "x_val", values_to = "detfreq", SW:MIX) %>% 
#   mutate(x_val = factor(x_val, levels = c("GW", "SW", "MIX"))) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>% 
#   ggplot(aes(x = x_val, y = detfreq, fill = x_val)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(name = "UCMR designated \nsource water type",
#                     values = c("#d77659","#004552", "#457d7c"), 
#                     na.value = "#e6e6e6")+
#   geom_text(aes(label=paste0(round(detfreq, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   xlab("UCMR designated source water type") + 
#   ylab("Percent of samples with a detect") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=12),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.position = "none") +
#   facet_grid(~contam.pfas)
# 
# 
# #ggsave("results/output/detection frequency of PWSs by source water type (within group).png", width = 17, height = 7, units = "in")
# 
# 
# 
# ucmr3_targetsamples_included %>% 
#   group_by(Contaminant) %>% 
#   summarize(SW = length(unique(SampleID[which(detchem == "1" & FacilityWaterType == "SW")]))/length(unique(SampleID))*100,
#             GW = length(unique(SampleID[which(detchem == "1" & FacilityWaterType == "GW")]))/length(unique(SampleID))*100,
#             MX = length(unique(SampleID[which(detchem == "1" & FacilityWaterType == "MX")]))/length(unique(SampleID))*100,
#             GU = length(unique(SampleID[which(detchem == "1" & FacilityWaterType == "GU")]))/length(unique(SampleID))*100) %>% 
#   pivot_longer(names_to = "x_val", values_to = "detfreq", SW:GU) %>% 
#   mutate(x_val = factor(x_val, levels = c("GW", "SW", "GU", "MX"))) %>% 
#   # mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFAS"),
#   #                             labels = c("Any UCMR Detection", "1,4-Dioxane", "1,1-Dichloroethane", "HCFC-22", "PFAS"))) %>% 
#   ggplot(aes(x = x_val, y = detfreq, fill = x_val)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_manual(name = "UCMR designated \nsource water type",
#                     #labels=c("Large (n = 4033)", "Small (n = 779)"),
#                     values = c("#d77659","#004552", "#50a6a6", "#457d7c"), 
#                     na.value = "#e6e6e6")+
#   geom_text(aes(label=paste0(round(detfreq, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) + 
#   xlab("UCMR designated source water type") + 
#   ylab("Percent of samples with a detect") + 
#   theme_minimal() + 
#   theme(axis.text = element_text(size=12),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.position = "none") +
#   facet_grid(~Contaminant)
# 
# #ggsave("results/output/detection frequency of samples by source water type.png", width = 17, height = 7, units = "in")
# 
# 
# ucmr_detcode_clean_source %>% 
#   group_by(contam.pfas, source_type) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             hlvlfreq_sys = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100, 2)) 
# 
# 
# 

# ################################################################################
# #### 6. rerun above but on excluded systems ####################################
# ################################################################################
# 
# 
# #clean UCMR data: add column that groups PFAS into one contaminant, add geo info, 
# # filter to chems in ucmrchemtest (from 2019 plans) and then to only 1,4-dioxane, 1-1,dichloroethane, and PFAS (for 2020 plans)
# ucmr3_targetsamples_exclude <- ucmr3_raw %>% 
#   mutate(contam.pfas = case_when(Contaminant %in% c("PFBS", "PFHpA", "PFOA", "PFOS", "PFNA", "PFHxS") ~ "PFAS",
#                                  TRUE ~ Contaminant),
#          state = substr(PWSID,1,2),
#          #get unique PWS-sample ID pair
#          sys.sampid = paste0(PWSID, "-", SampleID)) %>% 
#   mutate(detchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue > 0) ~ 1,
#                              TRUE ~ 0),
#          hlvlchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue >= .35 & contam.pfas == "1,4-dioxane") ~ 1,
#                               TRUE ~ 0)) %>%
#   filter((state %in% c("NN", "PR", "01", "05", "06", "08", "09", "10", "GU", "MP", "VI", "AS"))) %>% 
#   filter(contam.pfas %in% c("1,4-dioxane", "1,1-dichloroethane", "PFAS", "HCFC-22")) %>% 
#   filter(MonitoringRequirement == "AM")
# 
# # how many PWSIDs are from tribes or territories?
# ucmr3_targetsamples_exclude %>%
#   mutate(tribe_or_terr = case_when(state %in% c("01", "05", "06", "08", "09", "10", "NN") ~ "tribe", 
#                                    TRUE ~ "territory")) %>%
#   group_by(tribe_or_terr) %>%
#   summarise(length(unique(PWSID)))
# 
# #get IDs of systems in UCMR (for other scripts)
# ucmrsys_exclude <- unique(ucmr3_targetsamples_exclude$PWSID)
# # write_csv(as.data.frame(ucmrsys_exclude), "results/output/excluded_ucmr_systems_IDs.csv")
# 
# #get detection frequencies
# pfospfoasum_exclude <- ucmr3_targetsamples_exclude %>% 
#   subset(contam.pfas == "PFAS") %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>% 
#   summarize(sum.pfospfoa = sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))], na.rm = TRUE),
#             sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
#                                     TRUE ~ 0),
#             sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
#                                      TRUE ~ 0)) %>% 
#   left_join(ucmr3_targetsamples_exclude %>%
#               select(PWSID:SampleID, SampleEventCode:sys.sampid)) %>%
#   unique() 
# 
# 
# length(unique(pfospfoasum_exclude$sys.sampid))
# length(unique(pfospfoasum_exclude$PWSID))
# 
# 
# #drop all PFAS and then merge in pfoapfossum
# ucmr3_exclude <- ucmr3_targetsamples_exclude %>% 
#   filter(!contam.pfas %in% c("PFAS")) %>% 
#   bind_rows(pfospfoasum_exclude %>%
#               select(-sum.pfospfoa) %>% 
#               rename(detchem = sum.detchem,
#                      hlvlchem = sum.hlvlchem)) %>% 
#   group_by(PWSID, contam.pfas) %>%
#   mutate(n.samp = length(unique(sys.sampid)),
#          n.date = length(unique(CollectionDate)),
#          n.sampfac = length(unique(FacilityID))) %>% 
#   filter(contam.pfas %in% c("PFAS", "1,1-dichloroethane", "1,4-dioxane", "HCFC-22"))
# 
# 
# #get detection by PWS instead of by facility
# 
# #get 1 or 0 for various tests with one row per system and contaminant
# ucmr_detcode_exclude <- ucmr3_exclude %>% 
#   group_by(PWSID) %>% 
#   mutate(n_watertype = length(unique(FacilityWaterType)),
#          source_type = case_when(FacilityWaterType == "MX" ~ "MIX",
#                                  FacilityWaterType == "GU" ~ "MIX",
#                                  TRUE ~ FacilityWaterType),
#          source_type = case_when(n_watertype > 1 ~ "MIX",
#                                  TRUE ~ source_type)) %>%
#   group_by(PWSID, contam.pfas, Size, source_type) %>%
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0),
#             hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                  TRUE ~ 0)) %>% 
#   ungroup() %>% 
#   evrdet()
# 
# 
# # PWS detection frequencies --------------------------------------------
# 
# det_freq_contaminant_exclude <- ucmr_detcode_exclude %>% 
#   mutate(state = substr(PWSID, 1, 2)) %>%
#   mutate(tribe_or_terr = case_when(state %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                                    TRUE ~ "territory")) %>%
#   bind_rows(ucmr_detcode_exclude %>% mutate(tribe_or_terr = "both")) %>%
#   # group_by(PWSID, contam.pfas, tribe_or_terr) %>%
#   # summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#   #                               TRUE ~ 0), 
#   #           hlvl_freq = case_when(sum(hlvlchem) >= 1 ~ 1, 
#   #                                 TRUE ~ 0)) %>% 
#   group_by(contam.pfas, tribe_or_terr) %>%
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2), 
#             hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)) %>% 
#   bind_rows(det_freq_grouped %>% mutate(tribe_or_terr = "stateside")) %>%
#   rename(Contaminant = contam.pfas) %>%
#   pivot_wider(id_cols = Contaminant, 
#               names_from = tribe_or_terr, 
#               values_from = c("detfreq_sys", 
#                               "hlvl_freq"))
# 
# # write_csv(det_freq_contaminant_exclude,
# #           paste0("results/output/excluded_ucmr_systems_freq_summary_",
# #                  Sys.Date(), ".csv"))
# 
# # calculate proportions among the cases (PWS detecting each contaminant)
# 
# terrtribes_amongdetsys <- ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                                    TRUE ~ "territory")) %>%
#   bind_rows(ucmr_detcode_exclude %>% mutate(cohort = 'both tribes and terr')) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   # filter(detchem == 1) %>%
#   group_by(cohort, contam.pfas) %>%
#   summarise(n_count = length(unique(PWSID)), 
#             n_detected = length(unique(PWSID[which(detchem == "1")]))) %>%
#   group_by(contam.pfas) %>% 
#   mutate(n_total_detected = sum(n_detected), 
#          perc_represented = n_detected/n_total_detected) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
#                                                       "1,1-dichloroethane", 
#                                                       "HCFC-22", "PFAS")))
# 
# terrtribes_amongdetsys_table <- terrtribes_amongdetsys %>%
#   mutate(var_name = paste0(contam.pfas, " (N=", n_total_detected, ")")) %>%
#   select(-contam.pfas, -n_total_detected, -n_count) %>%
#   mutate(perc_represented = round(100*perc_represented, 2)) %>%
#   pivot_wider(id_cols = cohort, 
#               names_from = var_name,
#               values_from = perc_represented) %>%
#   select(cohort, starts_with("evr"), 
#          starts_with("1,4-dioxane"), 
#          starts_with("1,1-dichlo"), 
#          starts_with("HCFC-22"), 
#          starts_with("PFAS"))
# 
# # write_csv(terrtribes_amongdetsys_table,
# #           paste0("results/output/exclvsincl_sys_caserepresentation_",
# #                  Sys.Date(), ".csv"))
# 
# ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                             TRUE ~ "territory")) %>%
#   # bind_rows(ucmr_detcode_exclude %>% mutate(cohort = 'both tribes and terr')) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   # filter(detchem == 1) %>%
#   group_by(cohort) %>%
#   summarise(n_count = length(unique(PWSID))) %>%
#   ungroup() %>%
#   mutate(n_total_detected = sum(n_count), 
#          perc_represented = round(100*n_count/n_total_detected,2))
# 
# mod_dat <- ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                             TRUE ~ "territory")) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   mutate(system_included = case_when(cohort == "stateside" ~ "included", 
#                                      TRUE ~ "excluded"), 
#          system_included = factor(system_included, levels = c("included", 
#                                                               "excluded")))
# 
# my_mod_function <- function(data){
#   glm(detchem ~ cohort, data = data, family = 'binomial')
# }
# 
# modresult1 <- mod_dat %>%
#   group_by(contam.pfas) %>%
#   nest() %>%
#   mutate(mod = map(data, my_mod_function), 
#          result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod) %>% 
#   pivot_wider(id_cols = term, 
#               names_from = contam.pfas, 
#               values_from = c(p.value, result))
# 
# my_mod_function2 <- function(data){
#   glm(detchem ~ system_included, data = data, family = 'binomial')
# }
# 
# modresult2 <- mod_dat %>%
#   group_by(contam.pfas) %>%
#   nest() %>%
#   mutate(mod = map(data, my_mod_function2), 
#          result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod) %>% 
#   pivot_wider(id_cols = term, 
#               names_from = contam.pfas, 
#               values_from = c(p.value, result))
# 
# excludedsys_model_table <- modresult1 %>% bind_rows(modresult2) %>%
#   select(term, 
#          ends_with("evrdet"), 
#          ends_with("1,4-dioxane"), 
#          ends_with("1,1-dichloroethane"), 
#          ends_with("HCFC-22"), 
#          ends_with("PFAS"))
# 
# # write_csv(excludedsys_model_table,
# #           paste0("results/output/exclvsincl_sys_model_",
# #                  Sys.Date(), ".csv"))
# 
# 
# modhlvl1 <- glm(hlvlchem ~ system_included, data = mod_dat %>%
#       filter(contam.pfas == "evrdet"), family = 'binomial')
# summary(modhlvl1)
# 
# modhlvl1 <- glm(hlvlchem ~ cohort*Size + source_type, data = mod_dat %>%
#                   filter(contam.pfas == "evrdet"), family = 'binomial')
# summary(modhlvl1)
# 
# my_mod_function3 <- function(data){
#   glm(hlvlchem ~ cohort, data = data, family = 'binomial')
# }
# 
# mod_dat %>%
#   filter(contam.pfas == "evrdet") %>%
#   nest(data = everything()) %>%
#   mutate(mod = map(data, my_mod_function3), 
#        result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod)
# 
# # ucmrdf.mod2
# 
# 
#             n_detected = length(unique(PWSID[which(detchem == "1")]))) %>%
#   group_by(contam.pfas) %>% 
#   mutate(n_total_detected = sum(n_detected), 
#          perc_represented = n_detected/n_total_detected) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
#                                                       "1,1-dichloroethane", 
#                                                       "HCFC-22", "PFAS")))

# 
# det_freq_grouped_exclude <- ucmr_detcode_exclude %>% 
#   group_by(contam.pfas) %>% 
#   summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)
#   )
# 
# # sample detection frequencies --------------------------------------------
# 
# indivsample_grouped_detfreq_exclude <- ucmr3_targetsamples_exclude %>%
#   group_by(contam.pfas) %>%
#   summarize(n_sys = length(unique(PWSID)),
#             n_samps = length(unique(sys.sampid)),
#             detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100))
# 
# indivsample_contaminant_detfreq_exclude <- ucmr3_targetsamples_exclude %>%
#   filter(contam.pfas == "PFAS") %>% 
#   group_by(Contaminant) %>%
#   summarize(detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100)) %>% 
#   rename(contam.pfas = Contaminant)
# 
# ucmr_sum_exclude <- full_join(det_freq_contaminant_exclude, det_freq_grouped_exclude) %>% 
#   left_join(bind_rows(indivsample_grouped_detfreq_exclude, indivsample_contaminant_detfreq_exclude)) %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   select(contam.pfas, detfreq_samps, detfreq_sys, hlvl_freq ) %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22",
#                                                       "PFAS", "PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "evrdet"))) %>%
#   arrange(contam.pfas)



################################################################################
#### WORK ZONE ####
################################################################################




# 
# pfospfoasum <- ucmr3_clean %>%
#   subset(contam.pfas == "PFAS") %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>%
#   summarize(
#     n.PFOS = length(which(Contaminant == "PFOS")),
#     n.PFOA = length(which(Contaminant == "PFOA"))
#   ) %>% 
#   #_#_# AHz: there is one PA system with different sample id that is creating 
#   # sys.sampid issue when calc PFOS-PFOA theres a whole set of PFAS chemicals 
#   # tested and PFOS is the only sys.sampid different -- seems believable
#   # that this one is also in that set and is currently getting dropped 
#   # !!! RESOLVE !!! for now drop with filter below
#   filter(n.PFOS == 1 & n.PFOA == 1) %>% 
#   left_join(ucmr3_clean[ucmr3_clean$contam.pfas %in% c("PFAS"), 
#                         c("PWSID", "Contaminant","AnalyticalResultsValue", "sys.sampid", "detchem")]) %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>% 
#   summarize(
#     sum.pfospfoa = sum(AnalyticalResultsValue, na.rm = TRUE),
#     # if at least one of PFOA/PFOS is detected, detchem == 1
#     sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
#                             TRUE ~ 0),
#     sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
#                              TRUE ~ 0)
#   ) %>% 
#   left_join(ucmr3_clean %>%
#               select(PWSID:SampleID, SampleEventCode:region_usgs)) %>%
#   unique() 
# mutate(hlvlchem = case_when(sum.pfospfoa > 0.7 ~ 1, 
#                             TRUE ~ 0)) %>% 
# select(-sum.pfospfoa) %>% 
# rename(detchem = sum.detchem,
#        hlvlchem = sum.hlvlchem)

# pfospfoa_sample <- ucmr3_clean %>%
#   subset(contam.pfas == "PFAS") %>% 
#   #subset(Contaminant %in% c("PFOS", "PFOA")) %>%
#   group_by(sys.sampid, PWSID, contam.pfas) %>%
#   summarize(
#     n.PFOS = length(which(Contaminant == "PFOS")),
#     n.PFOA = length(which(Contaminant == "PFOA"))
#   ) %>% 
#   #_#_# AHz: there is one PA system with different sample id that is creating 
#   # sys.sampid issue when calc PFOS-PFOA theres a whole set of PFAS chemicals 
#   # tested and PFOS is the only sys.sampid different -- seems believable
#   # that this one is also in that set and is currently getting dropped 
#   # !!! RESOLVE !!! for now drop with filter below
#   filter(n.PFOS == 1 & n.PFOA == 1) %>% 
#   left_join(ucmr3_clean[ucmr3_clean$contam.pfas %in% c("PFAS"), c("PWSID", "Contaminant","AnalyticalResultsValue", "sys.sampid", "detchem")]) %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>% 
#   summarize(
#     sum.pfospfoa = sum(AnalyticalResultsValue, na.rm = TRUE),
#     # if at least one of PFOA/PFOS is detected, detchem == 1
#     sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
#                             TRUE ~ 0),
#     sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
#                              TRUE ~ 0)
#   ) %>% 
#   left_join(ucmr3_clean %>%
#               select(PWSID:SampleID, SampleEventCode:region_usgs)) %>%
#   unique() 

# ggplot(pfospfoasum %>% filter(sum.pfospfoa != 0), aes(x = state, y = sum.pfospfoa)) +
#   geom_sina() + 
#   geom_hline(yintercept = .7) +
#   scale_y_continuous(n.breaks = 10)
# 


# 2020-04-08 AHz: this doesn't actually calculate the sum of PFOA/PFOS -- fine for
# now because we're doing a binary analysis -- need to make sure to filter down to just one PFAS 
# for hlvl analysis, BUT keep PFOA and PFOS for detchem 
#get pwsid of systems with a pfoa/pfos over 70ppt

# 2020-07-14 AHz: what we actually want is to create a PFAS in contam.pfas that 
# tells whether PFOA or PFOS was detected (detchem) and then whether the sum of 
# a PFOA/PFOS sample is > 70 ppt (hlvlchem) and then whether a system ever had a
#  sample over hlvl (evrovrhlvl)

# 
# #get 1 or 0 with one row per system and contaminant
# ucmr_detcode_clean <- ucmr3_clean %>% 
#   group_by(PWSID) %>% 
#   mutate(
#     #count number of water types per PWSID
#     n_watertype = length(unique(FacilityWaterType)),
#     #change MX and GU to surface water (SW)
#     source_type = case_when(FacilityWaterType == "MX" ~ "SW",
#                             FacilityWaterType == "GU" ~ "SW",
#                             TRUE ~ FacilityWaterType),
#     #change PWS with >1 source type to SW
#     source_type = case_when(n_watertype > 1 ~ "SW",
#                             TRUE ~ source_type)) %>%
#   group_by(PWSID, contam.pfas, Size, source_type, region_census, region_usgs) %>%
#   #alt approach: summarize case when 
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0),
#             hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                  #sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))]) >= 70 ~ 1,
#                                  TRUE ~ 0)) %>% 
#   # mutate(hlvlchem = case_when(PWSID %in% pfospfoasum & contam.pfas == "PFAS" ~ 1,
#   #                             TRUE ~ hlvlchem)) %>% 
#   ungroup() %>% 
#   group_by(PWSID) %>% 
#   mutate(evrovrhlvl = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   evrdet()


 # det_freq_grouped <- ucmr_detcode %>% 
 #   group_by(contam.pfas) %>% 
 #   summarize(det_freq = length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100,
 #             n_sys = length(unique(PWSID)))
 
 
 # nsys_ucmr <- ucmr3_raw %>%
 #   mutate(contam.pfas = "All systems in UCMR3") %>% 
 #   group_by(contam.pfas) %>%
 #   summarize(n.sys = length(unique(PWSID)))
 
 # 
 # indivchem_detfreq <- ucmr_detcode %>% 
 #   group_by(contam.pfas) %>% 
 #   summarize(n.sys = length(unique(PWSID)),
 #             detfreq_sys = length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100) %>% 
 #   mutate(contam.pfas = case_when(contam.pfas == "evrdet" ~ "All systems included in analysis",
 #                                  TRUE ~ contam.pfas))
 
 
 # ucmr_sum <- 
 #bind_rows(nsys_ucmr, indivchem_detfreq)
 
 #write_csv(ucmr_sum, "results/output/ucmr systems and det freq summary.csv")
 
# pfospfoasum <- ucmr3_clean %>%
#   subset(Contaminant %in% c("PFOS", "PFOA")) %>%
#   group_by(sys.sampid, PWSID, contam.pfas) %>%
#   summarize(
#     n.PFOS = length(which(Contaminant == "PFOS")),
#     n.PFOA = length(which(Contaminant == "PFOA"))
#   ) %>% 
#   #_#_# AHz: there is one PA system with different sample id that is creating 
#   # sys.sampid issue when calc PFOS-PFOA theres a whole set of PFAS chemicals 
#   # tested and PFOS is the only sys.sampid different -- seems believable
#   # that this one is also in that set and is currently getting dropped 
#   # !!! RESOLVE !!! for now drop with filter below
#   filter(n.PFOS == 1 & n.PFOA == 1) %>% 
#   left_join(ucmr3_clean[ucmr3_clean$Contaminant %in% c("PFOS", "PFOA"), c("PWSID", "Contaminant","AnalyticalResultsValue", "sys.sampid", "detchem")]) %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>% 
#   summarize(
#     sum.pfospfoa = sum(AnalyticalResultsValue, na.rm = TRUE),
#     # if at least one of PFOA/PFOS is detected, detchem == 1
#     sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
#                             TRUE ~ 0)
#   ) %>% 
#   left_join(ucmr3_clean %>%
#               select(PWSID:SampleID, SampleEventCode:region_usgs)) %>%
#   mutate(hlvlchem = case_when(sum.pfospfoa > 0.7 ~ 1, 
#                               TRUE ~ 0)) %>% 
#   select(-sum.pfospfoa) %>% 
#   rename(detchem = sum.detchem)


# 
# #get 1 or 0 for various tests with one row per system and contaminant
# ucmr_detcode_clean_exclude <- ucmr3_clean_exclude %>% 
#   group_by(PWSID) %>% 
#   mutate(n_sw = length(unique(FacilityWaterType)),
#          source_type = case_when(FacilityWaterType == "MX" ~ "SW",
#                                  FacilityWaterType == "GU" ~ "SW",
#                                  TRUE ~ FacilityWaterType),
#          source_type = case_when(n_sw > 1 ~ "SW",
#                                  TRUE ~ source_type)) %>%
#   group_by(PWSID, contam.pfas, Size, source_type, region_census, region_usgs) %>%
#   #alt approach: summarize case when 
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0),
#             hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                  #sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))]) >= 70 ~ 1,
#                                  TRUE ~ 0)) %>% 
#   # mutate(hlvlchem = case_when(PWSID %in% pfospfoasum & contam.pfas == "PFAS" ~ 1,
#   #                             TRUE ~ hlvlchem)) %>% 
#   ungroup() %>% 
#   group_by(PWSID) %>% 
#   mutate(evrovrhlvl = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   evrdet()
# 
# 
# pfospfoasum_exclude <- ucmr3_clean_exclude %>%
#   subset(contam.pfas == "PFAS") %>% 
#   #subset(Contaminant %in% c("PFOS", "PFOA")) %>%
#   group_by(sys.sampid, PWSID, contam.pfas) %>%
#   summarize(
#     n.PFOS = length(which(Contaminant == "PFOS")),
#     n.PFOA = length(which(Contaminant == "PFOA"))
#   ) %>% 
#   #_#_# AHz: there is one PA system with different sample id that is creating 
#   # sys.sampid issue when calc PFOS-PFOA theres a whole set of PFAS chemicals 
#   # tested and PFOS is the only sys.sampid different -- seems believable
#   # that this one is also in that set and is currently getting dropped 
#   # !!! RESOLVE !!! for now drop with filter below
#   filter(n.PFOS == 1 & n.PFOA == 1) %>% 
#   left_join(ucmr3_clean_exclude[ucmr3_clean_exclude$contam.pfas %in% c("PFAS"), c("PWSID", "Contaminant","AnalyticalResultsValue", "sys.sampid", "detchem")]) %>% 
#   group_by(sys.sampid, PWSID, contam.pfas) %>% 
#   summarize(
#     sum.pfospfoa = sum(AnalyticalResultsValue, na.rm = TRUE),
#     # if at least one of PFOA/PFOS is detected, detchem == 1
#     sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
#                             TRUE ~ 0),
#     sum.hlvlchem = case_when(sum.pfospfoa >= 0.7 ~ 1, 
#                              TRUE ~ 0)
#   ) %>% 
#   left_join(ucmr3_clean_exclude %>%
#               select(PWSID:SampleID, SampleEventCode:region_usgs)) %>%
#   unique() 
# 
# 
# ggplot(pfospfoasum_exclude %>% filter(sum.pfospfoa != 0), aes(x = state, y = sum.pfospfoa)) +
#   geom_sina() + 
#   geom_hline(yintercept = .7) +
#   scale_y_continuous(n.breaks = 10)
# 
# 
# 
# #drop all PFAS and then merge in pfoapfossum
# ucmr3_exclude <- ucmr3_clean_exclude %>% 
#   filter(!contam.pfas %in% c("PFAS")) %>% 
#   bind_rows(pfospfoasum_exclude %>%
#               select(-sum.pfospfoa) %>% 
#               rename(detchem = sum.detchem,
#                      hlvlchem = sum.hlvlchem)) %>% 
#   group_by(PWSID, contam.pfas) %>%
#   mutate(n.samp = length(unique(SampleID)),
#          n.date = length(unique(CollectionDate)),
#          n.sampfac = length(unique(FacilityID))) %>% 
#   filter(contam.pfas %in% c("PFAS", "1,1-dichloroethane", "1,4-dioxane", "HCFC-22"))
# 
# 
# #get 1 or 0 for various tests with one row per system and contaminant
# ucmr_detcode_exclude <- ucmr3_exclude %>% 
#   group_by(PWSID) %>% 
#   mutate(n_sw = length(unique(FacilityWaterType)),
#          source_type = case_when(FacilityWaterType == "MX" ~ "SW",
#                                  FacilityWaterType == "GU" ~ "SW",
#                                  TRUE ~ FacilityWaterType),
#          source_type = case_when(n_sw > 1 ~ "SW",
#                                  TRUE ~ source_type)) %>%
#   group_by(PWSID, contam.pfas, Size, source_type, region_census, region_usgs) %>%
#   #alt approach: summarize case when 
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0),
#             hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                  TRUE ~ 0)) %>% 
#   # mutate(hlvlchem = case_when(PWSID %in% pfospfoasum & contam.pfas == "PFAS" ~ 1,
#   #                             TRUE ~ hlvlchem)) %>% 
#   ungroup() %>% 
#   group_by(PWSID) %>% 
#   mutate(evrovrhlvl = case_when(sum(hlvlchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   evrdet()
# 
# 
# det_freq_indiv_exclude <- ucmr3_clean_exclude %>% 
#   group_by(PWSID, Contaminant) %>%
#   #alt approach: summarize case when 
#   summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
#                                 TRUE ~ 0)) %>% 
#   group_by(Contaminant) %>% 
#   summarize(det_freq = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             n_sys = length(unique(PWSID)))
# 
# det_freq_grouped_exclude <- ucmr_detcode_exclude %>% 
#   group_by(contam.pfas) %>% 
#   summarize(det_freq = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
#             hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2),
#             n_sys = length(unique(PWSID)))
# 
# # det_freq_grouped <- ucmr_detcode %>% 
# #   group_by(contam.pfas) %>% 
# #   summarize(det_freq = length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100,
# #             n_sys = length(unique(PWSID)))
# 
# 
# 
# indivchem_detfreq_exclude<- ucmr_detcode_exclude %>% 
#   group_by(contam.pfas) %>% 
#   summarize(n.sys = length(unique(PWSID)),
#             det_freq = length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100) %>% 
#   mutate(contam.pfas = case_when(contam.pfas == "evrdet" ~ "All systems included in analysis",
#                                  TRUE ~ contam.pfas))
# 
# 
# ucmr_sum <- bind_rows(nsys_ucmr, indivchem_detfreq)
# 
# 



# get detection frequencies for raw data, grouped PFOA/PFOS data, and binary detchem data
# det_freq_indiv <- ucmr3_clean %>% 
#   group_by(Contaminant) %>% 
#   summarize(det_freq = length(unique(SampleID[which(detchem == "1")]))/length(unique(SampleID))*100)
# det_freq_cleaned <- ucmr3 %>% 
#   group_by(contam.pfas) %>% 
#   summarize(det_freq = length(unique(SampleID[which(detchem == "1")]))/length(unique(SampleID))*100)


# #get pwsid of systems with a pfoa/pfos over 70ppt
# pfospfoasum70 <- ucmr3 %>%
#   subset(Contaminant %in% c("PFOS", "PFOA")) %>%
#   group_by(sys.sampid, PWSID) %>%
#   summarize(
#     n.PFOS = length(which(Contaminant == "PFOS")),
#     n.PFOA = length(which(Contaminant == "PFOA"))
#   ) %>% 
#   #AHz: there is one PA system with different sample id that is creating sys.sampid issue when calc PFOS-PFOA 
#   #theres a whole set of PFAS chemicals tested and PFOS is the only sys.sampid different -- seems believable
#   #that this one is also in that set and is currently getting dropped !!! RESOLVE !!! 
#   filter(n.PFOS == 1 & n.PFOA == 1) %>% 
#   left_join(ucmr3[ucmr3$Contaminant %in% c("PFOS", "PFOA"), c("PWSID", "Contaminant","AnalyticalResultsValue", "sys.sampid")]) %>% 
#   group_by(sys.sampid, PWSID) %>% 
#   summarize(
#     sum.pfospfoa = sum(AnalyticalResultsValue, na.rm = TRUE)
#   ) %>% 
#   filter(sum.pfospfoa >= 0.07) %>% 
#   pull(PWSID)

# #needed to include n.samp in model 
# nsampcontam_pfospfoa <-
#   ucmr3 %>%
#   subset(Contaminant %in% c("PFOS", "PFOA")) %>% 
#   group_by(PWSID) %>%
#   summarize(
#     n.samp = length(Contaminant)
#   )



# #pull number of samples and detection frequency for each compound
# ucmr.ndetfreq <-
#   group_by(ucmr3, Contaminant) %>%
#   summarize(
#     n.samp = length(PWSID),
#     prop.det = length(which(AnalyticalResultsSign == "="))/length(PWSID)*100
#   )
# 
# #~#~#~# n.samp is elevated for PFAS because there are 6 chemicals in that group
# #number of samples per contaminant per system
# nsampcontam <-
#   group_by(ucmr3, PWSID, contam.pfas) %>%
#   summarize(
#     n.samp = length(contam.pfas)
#   )
# 
# ndatescontam <-  group_by(ucmr3, contam.pfas, PWSID, FacilityWaterType) %>%
#   summarize(n.date = length(unique(CollectionDate)),
#             n.sampfac = length(unique(FacilityID)),
#             n.samp = length(contam.pfas)) %>% 
#   mutate(test = case_when(n.date*n.sampfac == n.samp ~ "Y",
#                           n.date*n.sampfac*6 == n.samp ~ "Y",
#                           TRUE ~ "N"))


#write_csv(ndatescontam, "results/preliminary/ndates ", Sys.Date(), ".csv")

