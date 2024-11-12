# DATE STARTED: 2021-06-20
# AUTHOR: Amanda Hernandez
# PURPOSE: Merge county-level data with water systems and aggregate to summarize variables per water system ID
# LATEST REVISION: 2024-10-02 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

library(tidyverse)

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)
getwd()

# ! Start here
# Sourcing previous scripts --------------------------------------------------

source("1__ucmr3_process.R")
source("1__demo_process.R")

# Function --------------------------------------------------

# Classifies whether a state belongs to the a state (or D.C.), U.S. territory, or a tribal area. 

classify_state <- function(dat, state_col) {
  dat %>%
    mutate(
      state_status = case_when(
        !!sym(state_col) %in% c(state.abb, "DC") ~ "state", 
        !!sym(state_col) %in% c("1", "10", "5", "6", "8", "9", "NN") ~ "tribe",
        !!sym(state_col) %in% c("AS", "GU", "MP", "PR", "VI") ~ "territory",
        TRUE ~ "oops"
      )
    ) %>%
  
  stopifnot(nrow(filter(., state_status == "oops")) == 0)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. INITIAL MERGE --------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data sources: 
# fips: dataset of PWSID linked to SDWIS data (popn served) and county ids (GEO.id2) 
# cn15: dataset of county-level demographics and pollution sources

# length(unique(fips$PWSID)) == nrow(fips)
# fips %>% count(PWSID) %>% filter(n > 1) # systems that serve >1 county

# length(unique(cn15$GEO.id2)) == nrow(cn15)
# cn15 %>% count(GEO.id2) %>% filter(n > 1) # county #34015 (gloucester county, new jersey) duplicated
# cn15 %>% filter(GEO.id2=="34015") %>% distinct()
# distinct(cn15) %>% nrow() == nrow(cn15) - 1
cn15 <- distinct(cn15)

# This merges FIPS codes with census info
fips_cn15 <- fips %>% left_join(cn15, relationship = "many-to-many")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIPS_CN15.2 == demographics (e.g., perc_hisp) ------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ This subsets all demographic vars that will need to be summarized 
#+ into averages (i.e., pop-weighted means for PWS serving >1 counties).

fips_cn15.1 <- fips_cn15 %>%
  mutate(perc_hmown = 100*owned.house/all.house14) %>% 
  # WWTP flow is in million liters per day over m^2
  mutate(adj_wwtp_flow_mgd_per_sqmeters = WWTP_totalflow_mgd/land.area, 
         adj_wwtp_flow = (3.78541*1e6)*adj_wwtp_flow_mgd_per_sqmeters) %>% 
  select(PWSID, GEO.id2, total_pop, mdi_rate, adj_wwtp_flow, starts_with("perc_"))

# fips_cn15.1 %>%
#   filter(PWSID %in% pwsids_diff_urban)

# Which PWSIDs have more than one counties (GEO.id2)?
fips_cn15 %>%
  distinct(PWSID, GEO.id2) %>%
  group_by(PWSID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Summarise water system demographics
# This may take a while to run! 
fips_cn15.2 <- fips_cn15.1 %>% 
  group_by(PWSID) %>%
  summarise(across(c(mdi_rate, adj_wwtp_flow, starts_with("perc_")), 
                   ~weighted.mean(., w = total_pop)))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIPS_CN15.3 == PWS chars from EPA SDWIS (e.g., pop served and ownership) -----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fips_cn15.3 <- fips_cn15 %>%
  select(PWSID, PWS_NAME, WS.POPULATION_SERVED_COUNT, 
         WS.OWNER_TYPE_CODE) %>%
  rename(PWSID = 1, PWSName = 2, pop_served = 3, owner_type = 4) %>%
  distinct()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIPS_CN15.4 == source terms (e.g., facility present or absent) ------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Summarise source terms vars for each PWS
fips_cn15.4 <- fips_cn15 %>%
  group_by(PWSID) %>% 
  summarise_at(vars(starts_with("n_"), src_epa_present),
               sum, na.rm = T) %>%
  mutate(across(
    .cols = c(starts_with("n_"), src_epa_present), 
    .fns = ~ifelse(. > 0, 1, 0), 
    .names = "{.col}_bin")) %>%
  mutate(n_MFTA_airport_bin = ifelse(n_MFTA_bin == 1 | n_airports_bin == 1, 
                                     1, 0))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UCMR3.6 == PWS chars from UCMR (e.g., size, pws_type, state, n_samples) ------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ucmr3.6 <- ucmr3.0 %>% 
  distinct(PWSID, Size, FacilityWaterType, FacilityID, FacilityName, SamplePointID, SamplePointName, CollectionDate, State) %>%
  rename(size = 2, pws_type = 3, state = State) %>%
  mutate(sample_id_new = paste(FacilityID, FacilityName, SamplePointID, SamplePointName, CollectionDate, sep = "_")) %>%
  group_by(PWSID, size, state) %>%
  summarise(
    pws_type = case_when(
      any(pws_type %in% c("MX", "GU")) | n_distinct(pws_type) > 1 ~ "MX",
      all(pws_type == "GW") ~ "GW", 
      all(pws_type == "SW") ~ "SW", 
      TRUE ~ "oops"), 
    n_samples = n_distinct(sample_id_new)
  )

# check
ucmr3.0 %>% 
  distinct(PWSID, FacilityWaterType) %>%
  group_by(PWSID) %>% mutate(n = n()) %>% filter(n > 1)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MERGE IT ALL TOGETHER -------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

main <- ucmr3.5 %>%          # processed UCMR3 data, one row per PWSID w/ outcomes
  left_join(ucmr3.6) %>%     # PWS chars (e.g., size, pws_type, state, n_samples)
  left_join(fips_cn15.2) %>% # demographics (e.g., perc_hisp)
  left_join(fips_cn15.3) %>% # PWS chars (e.g., pop served and ownership)
  left_join(fips_cn15.4)     # source terms (e.g., facility present or absent)

stopifnot(main %>% 
            group_by(PWSID) %>% 
            summarise(n = n()) %>% 
            filter(n > 1) %>%
            nrow() == 0)

# check
ucmr3.5 %>% 
  left_join(ucmr3.6) %>%
  left_join(fips_cn15.2) %>% 
  classify_state(state_col = 'state') %>%
  select(PWSID, starts_with("det_")) %>%
  pivot_longer(cols = starts_with("det_")) %>%
  group_by(name) %>%
  summarise(n = n(), 
            count = sum(!is.na(value)))

dat_ucmr3 <- main %>%
  mutate(across(starts_with("det_"), as.factor), 
         across(starts_with("viol_"), as.factor), 
         across(ends_with("_bin"), as.factor),
         size = factor(size, levels = c("S", "L")), 
         pws_type = factor(pws_type, levels = c("SW", "GW", "MX"))
         ) %>%
  # make WWTP flow 0 when there is NA
  mutate(adj_wwtp_flow = ifelse(is.na(adj_wwtp_flow), 0, adj_wwtp_flow)) %>%
  classify_state(state_col = 'state')

# filter(dat_ucmr3, is.na(mdi_rate)) %>% filter(state_status == 'state') %>%
#   View()
# colnames(dat_ucmr3)
# summary(dat_ucmr3)

# UNCOMMENT TO SAVE ------------
# write.csv(dat_ucmr3, paste0("processed/main-ucmr3-dataset-merged_", Sys.Date(), ".csv"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Making a dat_clean ----------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ This section also gives quick peeks into the data.
#+ 
#+        dat_ucmr3 = data without MDI restriction
#+        dat_clean = data with MDI restriction 
#+        
 
dat_ucmr3

filter(dat_ucmr3, is.na(mdi_rate)) %>% nrow() #115 PWSs w/o MDI data
filter(dat_ucmr3, is.na(mdi_rate)) %>% count(state_status) 
#+ 7 PWSs in states, 79 in territories, 29 in tribes

ucmr3$PWSID %>% unique() %>% length() # 5401 UCMR PWSs
dat_ucmr3$PWSID %>% length() # 4923 PWSs in sample, total
setdiff(ucmr3$PWSID, dat_ucmr3$PWSID) %>% unique() %>% length() # 478 UCMR PWSs not in this analysis

dat_clean <- dat_ucmr3 %>% filter(!is.na(mdi_rate))

setdiff(dat_ucmr3, dat_clean) %>% pull(state_status) == "territory"
setdiff(dat_ucmr3$PWSID, dat_clean$PWSID) %>% length() # 115 PWSs removed

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ARCHIVE -----------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write.csv(main, "main.csv")
# 
# # This creates a simple dataframe to compare with previous UCMR dataframes
# newdata <- main %>% select(PWSID, perc_black_nohisp, perc_hisp_any, mdi_rate)
# newdata %>% arrange(-perc_black_nohisp)
# main %>% filter(PWSID == "IA0400900") %>% View()
# ucmr3_fips %>% filter(PWSID == "IA0400900") %>% distinct()
# str(newdata)
# 
# fips_cn15.1 %>%
#   filter(PWSID %in% ucmr3_fips$PWSID) %>%
#   distinct(PWSID, GEO.id2) %>%
#   group_by(PWSID) %>% 
#   count() %>%
#   arrange(-n)
