# DATE STARTED: 2021-06-20
# AUTHOR: Amanda Hernandez
# PURPOSE: Check detection frequencies and create outcome variables from the UCMR3 dataset
# LATEST REVISION: 2024-10-02 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

library(tidyverse)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)
getwd()

options(stringsAsFactors = FALSE)

# Load data ---------------------------------------------------------------

ucmr3 <- read_csv("raw/UCMR3_All.csv") 
# str(ucmr3)
# colnames(ucmr3)

# Initial checks 
# How many PWSIDs? ## 5401
length(unique(ucmr3$PWSID)) 
# How many contaminants? ## 32 
length(unique(ucmr3$Contaminant)) 
# How many states, including tribes and territories? # 63
length(unique(ucmr3$State)) #63 

# Restriction ---------------------------------------------------------------

# List 1 Contaminant Monitoring (Assessment Monitoring)
ucmr3.0 <- ucmr3 %>%
  filter(MonitoringRequirement == "AM")

# Exceeding a health-reference concentration --------------------------------

# "exceed" = binary (1=exceeded a health reference concentration for 1,4-d, 
#   1,1-DCA, PFOA, or PFOS, 0=did not exceed). 
# Health reference concentration source file:
#   https://www.epa.gov/system/files/documents/2024-04/ucmr3-data-summary.pdf

ucmr3.1 <- ucmr3.0 %>%
  filter(Contaminant %in% c("1,4-dioxane", "1,1-dichloroethane", "PFOA", "PFOS"))

ucmr3.2 <- ucmr3.1 %>%
  group_by(
    PWSID,
    PWSName,
    FacilityID,
    FacilityName,
    CollectionDate,
    SamplePointID,
    SamplePointName
  ) %>%
  mutate(
    exceed = case_when(
      Contaminant == "1,4-dioxane" & AnalyticalResultValue >= 0.35 ~ 1,
      Contaminant == "1,1-dichloroethane" &
        AnalyticalResultValue >= 6.14 ~ 1,
      Contaminant %in% c("PFOA", "PFOS") &
        sum(AnalyticalResultValue[Contaminant %in% c("PFOA", "PFOS")],
            na.rm = T) >= 0.07 ~ 1,
      TRUE ~ 0
    )
  )

# Detecting a target contaminant at any concentration -----------------------

# "NA" is introduced if the system never sampled for the contaminant.
# For example, 4 systems ("CT0450011", "NJ0702001", "NY1600008", "NY5903469") 
# never sampled for HCFC-22; coded as "NA". 

ucmr3.3 <- ucmr3.2 %>%
  group_by(PWSID) %>%
  summarise(
    viol_any = if (any(
      str_detect(Contaminant, "1,4-dioxane|1,1,-dichloroethane|PF")
    ))
      ifelse(sum(exceed[str_detect(Contaminant, "1,4-dioxane|1,1,-dichloroethane|PF")]) > 0,
             1, 0)
    else
      NA_real_,
    viol_diox = if (any(Contaminant == "1,4-dioxane"))
      ifelse(sum(exceed[Contaminant == "1,4-dioxane"]) > 0, 1, 0)
    else
      NA_real_,
    viol_dca = if (any(Contaminant == "1,1-dichloroethane"))
      ifelse(sum(exceed[Contaminant == "1,1-dichloroethane"]) > 0, 1, 0)
    else
      NA_real_,
    viol_pfas = if (any(str_detect(Contaminant, "PF")))
      ifelse(sum(exceed[str_detect(Contaminant, "PF")]) > 0, 1, 0)
    else
      NA_real_
  )

ucmr3.4 <- ucmr3.0 %>%
  mutate(detect = case_when(
    str_detect(Contaminant, "PF|1,4-dioxane|1,1-dichloroethane|HCFC-22") & 
      !is.na(AnalyticalResultValue) ~ 1, 
    TRUE ~ 0)) %>% 
  group_by(PWSID) %>%
  summarise(det_any = if(any(str_detect(Contaminant, "1,4-dioxane|1,1-dichloroethane|HCFC-22|PF"))) 
              ifelse(sum(
                detect[str_detect(Contaminant, "1,4-dioxane|1,1-dichloroethane|HCFC-22|PF")]) > 0,
                1, 0)
            else NA_real_,  
            det_diox = if(any(Contaminant == "1,4-dioxane")) 
              ifelse(sum(detect[Contaminant == "1,4-dioxane"]) > 0, 1, 0)
            else NA_real_, 
            det_dca = if(any(Contaminant == "1,1-dichloroethane")) 
              ifelse(sum(detect[Contaminant == "1,1-dichloroethane"]) > 0, 1, 0)
            else NA_real_, 
            det_hcfc = if(any(Contaminant == "HCFC-22")) 
              ifelse(sum(detect[Contaminant == "HCFC-22"]) > 0, 1, 0)
            else NA_real_,  
            det_pfas = if(any(str_detect(Contaminant, "PF"))) 
              ifelse(sum(detect[str_detect(Contaminant, "PF")]) > 0, 1, 0)
            else NA_real_)


# Combine  ----------------------------------------------------------------

ucmr3.5 <- ucmr3.4 %>% left_join(ucmr3.3) 

# Inspect detection frequencies -------------------------------------------

# Will restrict to chemicals that were detected >1% 

# Denominator: total number of samples

ucmr3.0 %>%
  left_join(ucmr3.2) %>%
  group_by(Contaminant) %>% 
  summarise(n = n(), 
            num_samp_detected = sum((!is.na(AnalyticalResultValue) & 
                                        AnalyticalResultValue > 0)), 
            num_samp_exceeded = sum(exceed == 1, na.rm = T), 
            det_freq_samp = 100*num_samp_detected/n, 
            exc_freq_samp = 100*num_samp_exceeded/n) %>% 
  arrange(-det_freq_samp) 

# Denominator: total number of water systems

ucmr3.0 %>%
  left_join(ucmr3.2) %>%
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
            det_freq_sys = 100*num_sys_exceeded/n) %>%
  arrange(-det_freq_sys)
