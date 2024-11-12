# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Calculate sample- and system-level detection frequencies
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

## get main datasets from script 2. run below if needed:
## source("2. create main datasets.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 2. Reporting limits, detection freq, and common sources ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ Headings are: Contaminant (1,4-dioxane, 1,1-DCE, HCFC-22, PFAS (overall), and 6 PFAS
#+ individually - PFOA, PFOS, PFHpA, PFHxS, PFNA, PFBS.), Reporting Limit, Sample
#+ detection frequency, PWS detection frequency, and % of systems with any HRL
#+ 
#+ This will also involve one of the ucmr3.X datasets...
#+ 
#+ Just report the numbers here. Will do further editing in Word and Excel!

allContams <- c("1,4-dioxane", "1,1-dichloroethane", "HCFC-22", "PFOA", 
                "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS", "Any PFAS detected")

PFASonly <- c("PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA", "PFBS")

PWSID_included <- dat_clean$PWSID

stopifnot(length(unique(PWSID_included)) == nrow(dat_clean))


colnames(ucmr3.0)

new_rows_any_pfas <- ucmr3.0 %>%
  filter(Contaminant %in% PFASonly) %>%
  group_by(PWSID, PWSName, Size, FacilityID, FacilityName, FacilityWaterType, 
           SamplePointID, SamplePointName, SamplePointType, CollectionDate, 
           SampleID) %>%
  
  #+ To be more efficient, I'm making a df to attach to ucrm3.0 (the UCMR dataset
  #+ filtered for just our analytes of interest). The most important bits of the 
  #+ new dataset needs to change the AnalyticalResultValue [sum of all PFAS], 
  #+ Contaminant ["Any PFAS detected"], and the exceed column (whether 70 ppt of
  #+ PFOA and PFOS only). 
  
  summarise(sum_pfoa_pfos = sum(AnalyticalResultValue[Contaminant %in% c("PFOA", "PFOS")], 
                                na.rm = T),
            AnalyticalResultValue = sum(AnalyticalResultValue, na.rm = T),
            exceed = ifelse(sum_pfoa_pfos >= 0.07, 1, 0), 
            Contaminant = "Any PFAS detected") %>%
  arrange(-AnalyticalResultValue) %>%
  select(-sum_pfoa_pfos)

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

# detection frequencies by /number of samples
samp_det_freq <- ucmr3.0 %>%
  left_join(ucmr3.2) %>%
  bind_rows(new_rows_any_pfas) %>%
  #filter(str_detect(Contaminant, "PF")) %>% # this was a test
  filter(PWSID %in% PWSID_included) %>% # this was the restriction added to create Table 2 
  filter(Contaminant %in% allContams) %>% # this focuses on analytes of interest in study
  group_by(Contaminant) %>% 
  summarise(n = n(), 
            num_samp_detected = sum((!is.na(AnalyticalResultValue) & 
                                       AnalyticalResultValue > 0)), 
            num_samp_exceeded = sum(exceed == 1, na.rm = T), 
            det_freq_samp = 100*num_samp_detected/n, 
            exc_freq_samp = 100*num_samp_exceeded/n) %>% 
  arrange(-det_freq_samp) 
samp_det_freq

# detection frequencies by /total PWSs
pws_det_freq <- ucmr3.0 %>%
  left_join(ucmr3.2) %>%
  bind_rows(new_rows_any_pfas)%>%
  #filter(str_detect(Contaminant, "PF")) %>% # this was a test
  filter(PWSID %in% PWSID_included) %>% # this was the restriction added to create Table 2 
  filter(Contaminant %in% allContams) %>% # this focuses on analytes of interest in study
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
            exc_freq_sys = 100*num_sys_exceeded/n) %>%
  arrange(-det_freq_sys)
pws_det_freq

# Some dataframe manipulations! 
bleh <- samp_det_freq %>% rename(n_samp = n)
bleh2 <- pws_det_freq %>% rename(n_pws = n)
bleh3 <- bleh %>% left_join(bleh2, by = "Contaminant")
bleh3

# Save it!
# This will go through more editing in Word and Excel.
# write.csv(bleh3, paste0("outputs/Table 2. DetFreq by Sys and Samps_", Sys.Date(), ".csv"))
