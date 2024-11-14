# DATE STARTED: 2021-06-20
# AUTHOR: Amanda Hernandez
# PURPOSE: Merge county-level data with water systems and aggregate to summarize variables per water system ID
# LATEST REVISION: 2024-10-02 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

library(tidyverse)

# If needed:
# workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(workingdir)
# getwd()

# start here:
source("1__ucmr3_process.R")
source("1__demo_process.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview --------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# This script combines the outputs of 1__ucmr3_process.R and 1__demo_process.R
# and creates a data set to use for all subsequent scripts. The first script
# used the UCMR3 data to define outcome variables. The second script 
# used various public data on information about customers served by US water 
# systems. In particular, the demographic processing script loads and processes
# county-level information about demographics like ethnicity (e.g., the percent of Hispanic 
# residents in a county according to estimates from the 2010-2014 ACS survey).
#
# The end result of this script is "dat_clean," which is used throughout the repo. 
# It is a dataset of UCMR3 water systems with columns linking them to information 
# about county or counties served. Each row in dat_clean is one PWS. For PWS 
# that serve multiple counties, this script computes a population-weighted 
# average of demographic variables of interest (e.g., % Hispanic). 

# Function --------------------------------------------------

# Classifies whether a state belongs to the a state (or D.C.), U.S. territory, or a tribal area. 

classify_state <- function(dat, state_col) {
  dat <- dat %>%
    mutate(
      state_status = case_when(
        !!sym(state_col) %in% c(state.abb, "DC") ~ "state", 
        !!sym(state_col) %in% c("1", "10", "5", "6", "8", "9", "NN") ~ "tribe",
        !!sym(state_col) %in% c("AS", "GU", "MP", "PR", "VI") ~ "territory",
        TRUE ~ "oops"
      )
    )
  stopifnot(nrow(filter(dat, state_status == "oops")) == 0)
  return(dat)
}

# First merge --------------------------------------------------------

# This merges FIPS codes with census info
fips_cn15 <- fips %>% left_join(cn15, relationship = "many-to-many")

# check that each water system-county pair does not appear more than once. 
# Two systems, both in Bedford County, Virginia, are duplicated in the data but 
# are not the UCMR3. These duplicates come from some of the manual editing from 
# AHz in 1__demo_process.R.
# fips_cn15 %>% count(PWSID, GEO.id2) %>% filter(n > 1) # n=2 systems

# Finish processing demographics and wastewater flow ------------

# For each system-county pair, calculate % homeownership as the proportion of residents who own a home. 
# Also calculate the amount of wastewater flow in MGD/km2. Note that 
# the original land area values were reported in acres. 
# Then select (subset) demographic vars that will need to be summarized 
# into population weighted means for systems that serve multiple counties.

fips_cn15.1 <- fips_cn15 %>%
  mutate(perc_hmown = 100*owned.house/all.house14) %>% 
  # WWTP flow is in million liters per day over m^2
  mutate(adj_wwtp_flow_mgd_per_sqmeters = WWTP_totalflow_mgd/land.area, 
         adj_wwtp_flow = (3.78541*1e6)*adj_wwtp_flow_mgd_per_sqmeters) %>% 
  select(PWSID, GEO.id2, total_pop, mdi_rate, adj_wwtp_flow, starts_with("perc_"))

# How many water systems serve more than one county (GEO.id2)? # n=283
fips_cn15 %>%
  distinct(PWSID, GEO.id2) %>%
  group_by(PWSID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# For each water system, calculate the population-weighted mean. 
# This may take a few minutes to complete.
fips_cn15.2 <- fips_cn15.1 %>% 
  group_by(PWSID) %>%
  summarise(across(c(mdi_rate, adj_wwtp_flow, starts_with("perc_")), 
                   ~weighted.mean(., w = total_pop)))

# PWS chars from EPA SDWIS (e.g., population served) ---------------

# Collect some information from SDWIS and rename the columns for clarity. 
# Note that system ownership was not used in the analysis.

fips_cn15.3 <- fips_cn15 %>%
  select(PWSID, PWS_NAME, WS.POPULATION_SERVED_COUNT, 
         WS.OWNER_TYPE_CODE) %>%
  rename(PWSID = 1, PWSName = 2, pop_served = 3, owner_type = 4) %>%
  distinct()

# Presence of pollution sources of unregulated contaminants ------------

# For each water system, determine whether the system served a county that 
# was linked to pollution source. Do this for each pollution source term separately. 

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

# System characteristics from UCMR3 -------------

# The UCMR3 data contained some information about water systems that we will 
# use for future analyses, including system size, source water type, state location. 
# The total number of samples collected in the UCMR3 for each system was derived
# based on data from the UCMR3. Samples were defined as unique 
# combinations of facility ID, facility name, sample point ID, sample point name, 
# and collection date. 
# 
# For source water type, we categorized systems as one of the following: 
# * Groundwater system (GW) - reliant only on GW sources
# * Surface water system (SW) - reliant only on SW sources
# * MIX system (MIX) - reliant on a GW source that is under the influence of SW, 
#   or is known to use a mix of GW and SW sources. 
#

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
# ucmr3.0 %>% 
#   distinct(PWSID, FacilityWaterType) %>%
#   group_by(PWSID) %>% mutate(n = n()) %>% filter(n > 1)

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

# Last restriction ----------------------------------------------------------

# This line produces the final dataset used in later scripts. It restricts 
# to systems that do not have missing data for MDI. Note that in later analyses, 
# we found that most of systems excluded by this criteria were systems serving US tribal areas and 
# US territories. A separate analysis based on data from "dat_ucmr3" is available 
# elsewhere.
# 
#       dat_ucmr3 = data without MDI restriction
#       dat_clean = data with MDI restriction 

dat_clean <- dat_ucmr3 %>% filter(!is.na(mdi_rate))

# Save progress. 

# write.csv(dat_ucmr3, paste0("processed/main-ucmr3-dataset-merged_", Sys.Date(), ".csv"))
# write.csv(dat_clean, paste0("processed/main-ucmr3-dataset-processed_", Sys.Date(), ".csv"))

# ARCHIVE -----------------------------------------------------------------

# #+        
# 
# dat_ucmr3
# 
# filter(dat_ucmr3, is.na(mdi_rate)) %>% nrow() #115 PWSs w/o MDI data
# filter(dat_ucmr3, is.na(mdi_rate)) %>% count(state_status) 
# #+ 7 PWSs in states, 79 in territories, 29 in tribes
# 
# ucmr3$PWSID %>% unique() %>% length() # 5401 UCMR PWSs
# dat_ucmr3$PWSID %>% length() # 4923 PWSs in sample, total
# setdiff(ucmr3$PWSID, dat_ucmr3$PWSID) %>% unique() %>% length() # 478 UCMR PWSs not in this analysis

# setdiff(dat_ucmr3, dat_clean) %>% pull(state_status) == "territory"
# setdiff(dat_ucmr3$PWSID, dat_clean$PWSID) %>% length() # 115 PWSs removed

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
