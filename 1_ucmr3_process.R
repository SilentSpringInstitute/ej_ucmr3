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

options(stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

#load in UCMR data 
ucmr3 <- read_csv("raw/UCMR3_All.csv") 
pfas <- ucmr3 %>% filter(str_detect(Contaminant, "PF"))

colnames(ucmr3)

#how many PWSIDs? 
length(unique(ucmr3$PWSID)) #5401
length(unique(ucmr3$Contaminant)) #32
length(unique(ucmr3$State)) #63 (50 states + D.C. + 6 tribes + 6 territories)

ucmr3 %>%
  mutate(StateClass = case_when(State %in% c(state.abb, "DC") ~ "state",
                                State %in% c("1", "10", "5", "6", 
                                              "8", "9", "NN") ~ "tribe", 
                                State %in% c("AS", "GU", "MP", 
                                             "PR", "VI") ~ "territory", 
                                TRUE ~ "oops")) %>%
  {stopifnot(nrow(filter(., StateClass == "oops"))==0); .}


state.abb
unique(ucmr3$State) %>% sort()

# restrict to systems doing List 1 Contaminant Monitoring (Assessment Monitoring)
ucmr3.0 <- ucmr3 %>%
  filter(MonitoringRequirement == "AM")
setdiff(ucmr3, ucmr3.0)

# how many systems in each state? 
ucmr3 %>% 
  group_by(State) %>% 
  distinct(PWSID) %>%
  summarise(num_systems = n()) %>% 
  arrange(-num_systems)

# create a "exceed an HRL" column 
ucmr3.1 <- ucmr3.0 %>%
  filter(Contaminant %in% c("1,4-dioxane", "1,1-dichloroethane", "PFOA", "PFOS"))

# All units are in ug/L. <-- @AM - double check if this statement is true. 
ucmr3.2 <- ucmr3.1 %>% 
  group_by(PWSID, PWSName, FacilityID, FacilityName, CollectionDate, SamplePointID, SamplePointName) %>% 
  mutate(exceed = case_when(Contaminant == "1,4-dioxane" & AnalyticalResultValue >= 0.35 ~ 1, 
                            Contaminant == "1,1-dichloroethane" & AnalyticalResultValue >= 6.14 ~ 1, 
                            Contaminant %in% c("PFOA", "PFOS") & 
                              sum(AnalyticalResultValue[Contaminant %in% c("PFOA", "PFOS")], 
                                  na.rm = T) >= 0.07 ~ 1, 
                            TRUE ~ 0))

#+ AM 9/15/2023: I think I have to get rid of the 'na.rm = T' args. Some PWSIDs 
#+ didn't measure certain contaminants at all. For example, 4 PWSs --
#+ "CT0450011", "NJ0702001", "NY1600008", "NY5903469" -- never sampled for HCFC-22. 
#+ Note than in the AHz's previous df, there were 8 PWSs that never sampled for HCFC-22.
#+ Solved with adding an if(any(...)) condition.
#+ This introdues "NA" into the outcome columns. NAs = system did not collect sample for specific analyte.

ucmr3.3 <- ucmr3.2 %>% 
  #filter(PWSID == "NJ0702001") %>% #this PWS never collected 1,4-dioxane samples
  group_by(PWSID) %>%
  summarise(viol_any = if(any(str_detect(Contaminant, "1,4-dioxane|1,1,-dichloroethane|PF")))
                ifelse(sum(exceed[str_detect(Contaminant, "1,4-dioxane|1,1,-dichloroethane|PF")]) > 0, 
                       1, 0)
                else NA_real_,
            viol_diox = if(any(Contaminant == "1,4-dioxane")) 
                ifelse(sum(exceed[Contaminant == "1,4-dioxane"]) > 0, 1, 0)
                else NA_real_,
            viol_dca = if(any(Contaminant == "1,1-dichloroethane"))
                ifelse(sum(exceed[Contaminant == "1,1-dichloroethane"]) > 0, 1, 0)
                else NA_real_, 
            viol_pfas = if(any(str_detect(Contaminant, "PF")))
                ifelse(sum(exceed[str_detect(Contaminant, "PF")]) > 0, 1, 0)
                else NA_real_
    )

# ucmr3.3 %>% filter(apply(is.na(.), 1, any))
  
# ucmr3.3 <- ucmr3.2 %>% 
#   group_by(PWSID)%>%
#   summarise(viol_any = ifelse(sum(exceed) > 0, 
#                              1, 0), 
#             viol_diox = ifelse(sum(exceed[Contaminant == "1,4-dioxane"]) > 0, 
#                               1, 0), 
#             viol_dca = ifelse(sum(exceed[Contaminant == "1,1-dichloroethane"]) > 0, 
#                              1, 0), 
#             viol_pfas = ifelse(sum(exceed[str_detect(Contaminant, "PF")]) > 0, 
#                               1, 0))

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

#ucmr3.4 %>% filter(is.na(det_pfas))

ucmr3.5 <- ucmr3.4 %>% left_join(ucmr3.3) 

# detection frequencies by /number of samples
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
#%>% View()

# detection frequencies by /total PWSs
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
