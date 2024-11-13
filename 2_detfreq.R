# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Calculate sample- and system-level detection frequencies (Table 1 in paper)
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 1. Reporting limits, detection freq, and common sources ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Start here:
source("1_combine_process.R")

# The script above downloads the UCMR3 dataset and defines outcome 
# variables (detection and exceedance) for each water system and target 
# contaminant separately. It sources the script that loads the original 
# UCMR3 dataset into the working environment (1__ucmr3_process.R), among 
# other datasets.

allContams <- c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFOA",
                "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "Any PFAS detected")

PFASonly <- c("PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS")

PWSID_included <- dat_clean$PWSID

# Use the following datasets (stored in working environment) from the sourced
# script to calculate detection frequencies: ucmr3.0 and ucmr3.2.

stopifnot(length(unique(ucmr3.0$PWSID))==4923) # total # of sys in List 1 monitoring
stopifnot(length(unique(ucmr3.2$PWSID))==4920) # total # of sys w/ PFAS samples
stopifnot(length(unique(ucmr3.2$exceed)) == 2) # ucmr3.2 has one additional column "exceed" (binary).

# The following chunk is needed to create "new" rows in the UCMR data whereby 
# the "Contaminant" is "Any PFAS detected."
# "Any PFAS detected" is defined as one or more detection of a chem in PFASonly 
# (ie, PFOA, PFOS, PFHpA, PFHxS, PFNA, PFBS). The chunk below creates a 
# dataset of all samples from the UCMR3 and creates columns of: (1) the total 
# amount of PFAS (simple sum), (2) a binary var whether the sample 
# exceeded 70 ppt for the combined amount of PFOA and PFOS, and (3) a 
# column name of "Any PFAS detected."

new_rows_any_pfas <- ucmr3.0 %>%
  filter(Contaminant %in% PFASonly) %>%
  group_by(PWSID, PWSName, Size, FacilityID, FacilityName, FacilityWaterType,
           SamplePointID, SamplePointName, SamplePointType, CollectionDate,
           SampleID) %>%
  summarise(sum_pfoa_pfos = sum(AnalyticalResultValue[Contaminant %in% c("PFOA", "PFOS")],
                                na.rm = T),
            AnalyticalResultValue = sum(AnalyticalResultValue, na.rm = T),
            exceed = ifelse(sum_pfoa_pfos >= 0.07, 1, 0),
            Contaminant = "Any PFAS detected") 

# Detection frequency. Denominator: total number of samples collected for each 
# contaminant. 

samp_det_freq <- ucmr3.0 %>%
  left_join(ucmr3.2) %>%                   # add "exceed" column
  bind_rows(new_rows_any_pfas) %>%         # add "any PFAS" rows
  filter(PWSID %in% PWSID_included) %>%    # filter for PWSs of interest
  filter(Contaminant %in% allContams) %>%  # filter for contaminants of interest
  group_by(Contaminant) %>% 
  summarise(n = n(), 
            num_samp_detected = sum((!is.na(AnalyticalResultValue) & 
                                       AnalyticalResultValue > 0)), 
            num_samp_exceeded = sum(exceed == 1, na.rm = T), 
            det_freq_samp = 100*num_samp_detected/n, 
            exc_freq_samp = 100*num_samp_exceeded/n) %>% 
  arrange(-det_freq_samp) 

samp_det_freq

# Detection frequency. Denominator: total number of water systems that 
# sampled for the contaminant. 

pws_det_freq <- ucmr3.0 %>%
  left_join(ucmr3.2) %>%
  bind_rows(new_rows_any_pfas)%>%
  filter(PWSID %in% PWSID_included) %>% 
  filter(Contaminant %in% allContams) %>% 
  group_by(Contaminant, PWSID) %>% 
  summarise(n = n(), 
            num_samp_detected = sum((!is.na(AnalyticalResultValue) & 
                                       AnalyticalResultValue > 0)), 
            num_samp_exceeded = sum(exceed == 1, na.rm = T), 
            det_freq_samp = 100*num_samp_detected/n) %>% 
  mutate(detected = ifelse(num_samp_detected > 0, 1, 0), 
         exceeded = ifelse(num_samp_exceeded > 0, 1, 0)) %>%
  group_by(Contaminant) %>% 
  summarise(n = n(), 
            num_sys_detected = sum(detected == 1), 
            num_sys_exceeded = sum(exceeded == 1),
            det_freq_sys = 100*num_sys_detected/n,
            exc_freq_sys = 100*num_sys_exceeded/n)

pws_det_freq

# Rename columns, then join together by contaminant.

col1 <- samp_det_freq %>% rename(n_samp = n)
col2 <- pws_det_freq %>% rename(n_pws = n)
tab <- col1 %>% left_join(col2, by = "Contaminant")
tab

# Save table (with additional formatting in MS word/excel).

# write.csv(tab, paste0("outputs/Table 2. DetFreq by Sys and Samps_", Sys.Date(), ".csv"))

# Archive -----------------------------------------------------------------

# # Headings are: Contaminant (1,4-dioxane, 1,1-DCE, HCFC-22, PFAS (overall), and 6 PFAS
# # individually - PFOA, PFOS, PFHpA, PFHxS, PFNA, PFBS.), Reporting Limit, Sample
# # detection frequency, PWS detection frequency, and % of systems with any HRL
# 
# stopifnot(length(unique(PWSID_included)) == nrow(dat_clean))
# 
# colnames(ucmr3.0)
# 

# ucmr3.0 %>%
#   filter(Contaminant %in% PFASonly) %>%
#   group_by(PWSID) %>%
#   summarise(Contaminant = "Any PFAS detected", 
#             sum_pfoa_pfos = sum(AnalyticalResultValue[Contaminant %in% c("PFOA", "PFOS")], 
#                                 na.rm = T)) %>%
#   arrange(-sum_pfoa_pfos)
# distinct(PWSID, Contaminant, AnalyticalResultValue) %>%
#   group_by(PWSID) %>%
#   summarise(any_pfas_det = ifelse(all(is.na(AnalyticalResultValue)), 0, 1)) %>%
#   arrange(-any_pfas_det)
# 
# dat_clean
# setdiff(ucmr3.0, ucmr3.2) 
# ucmr3.0 %>%
#   left_join(ucmr3.2) %>%
#   colnames()