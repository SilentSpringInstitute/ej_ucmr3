### AUTHOR: AM
### STARTED: 2023-03-13
### WRITTEN IN: R version 4.2.2
### Purpose: 

library(tidyverse)
library(ggforce)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

# load script 4 so that we can remove the PWSs with missing MDIs
# source("1 - UCMR loading and processing.R")
# source("4a - UCMR Summary.R")

################################################################################
#  0. README  ###############################################################
################################################################################

### GOAL 
#

#### 5a . GET STATS FOR WRITE UP -- DETECTION FREQUENCIES ######################

#drop PWSIDs that are missing MDI 

ucmr3_targetsamples_included <- ucmr3_targetsamples %>% 
  filter(!PWSID %in% missing_mdi)

ucmr_detcode_included <- ucmr_detcode %>% 
  filter(!PWSID %in% missing_mdi)

#_# PWS detection frequencies --------------------------------------------

det_freq_contaminant <- ucmr3_targetsamples_included %>% 
  group_by(PWSID, Contaminant) %>%
  summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(Contaminant) %>% 
  summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2)) %>% 
  rename(contam.pfas = Contaminant)

det_freq_grouped <- ucmr_detcode_included %>% 
  group_by(contam.pfas) %>% 
  summarize(detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2),
            hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)
  )

#_# sample detection frequencies --------------------------------------------


indivsample_grouped_detfreq <- ucmr3_targetsamples_included %>%
  group_by(contam.pfas) %>%
  summarize(n_sys = length(unique(PWSID)),
            n_samps = length(unique(sys.sampid)),
            detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100))

indivsample_contaminant_detfreq <- ucmr3_targetsamples_included %>%
  filter(contam.pfas == "PFAS") %>% 
  group_by(Contaminant) %>%
  summarize(detfreq_samps = (length(unique(sys.sampid[which(detchem == 1)]))/length(unique(sys.sampid))*100)) %>% 
  rename(contam.pfas = Contaminant)



ucmr_sum <- full_join(det_freq_contaminant, det_freq_grouped) %>% 
  left_join(bind_rows(indivsample_grouped_detfreq, indivsample_contaminant_detfreq)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(contam.pfas, detfreq_samps, detfreq_sys, hlvl_freq ) %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22",
                                                      "PFAS", "PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "evrdet"))) %>%
  arrange(contam.pfas)


# write_csv(ucmr_sum, "results/preliminary/UCMR sys and sample detection frequency tables.csv")

#stops script when it's called in other scripts

# stop("end of script 1 - UCMR loading and processing")
