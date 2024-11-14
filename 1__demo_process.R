# DATE STARTED: 2021-06-20
# AUTHOR: Amanda Hernandez
# PURPOSE: Compile county-level data from various public databases (5Y census, MDI, urbanicity)
# LATEST REVISION: 2024-10-02 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

library(tidyverse)
library(janitor) # for clean_names()
library(readxl)

options(stringsAsFactors = FALSE)

# If needed, uncomment to set working directory here.
# workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(workingdir)
# getwd()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script downloads various county-level data and processes them prior to linking
# them to water systems in a later script ("1_combine_process.R"). Data sets 
# were downloaded from online public databases in 2020. Copies of the original 
# input files are available upon request. County-level data included 
# race/ethnicity and socioeconomic variables from 2010-2014 American Community Survey (ACS),
# deprivation rates and urbanicity from the U.S. Census Bureau, wastewater treatment 
# plants and effluent flow from the US EPA 2012 EPA Clean Watersheds Needs Survey, 
# select industrial facilities from the Toxics Release Inventory, PFAS point 
# sources identified in Hu et al. (2016) (https://doi.org/10.1021/acs.estlett.6b00260), 
# and public water system characteristics from US EPA Safe Drinking Water
# Information System Federal Reporting System. Not all variables compiled in 
# the following script was used in subsequent analyses and was an artefact of 
# previous EJ analyses. 
# 

# Function ---------------------------------------------------------------

# This function cleans county names manually to create linkages between datasets. 
# It was mostly used to clean names in datasets of point sources. 

correct_counties <- function(dat){
  dat %>% 
    mutate(
      # Alaska 
      geography = gsub("aleutians west county, alaska", "aleutians west census area, alaska", geography),
      geography = gsub("aleutians east county, alaska", "aleutians east borough, alaska", geography),
      geography = gsub("anchorage county, alaska", "anchorage municipality, alaska", geography),
      geography = gsub("angoon county, alaska", "hoonah-angoon census area, alaska", geography),
      geography = gsub("bethel county, alaska", "bethel census area, alaska", geography),
      geography = gsub("dillingham county, alaska", "dillingham census area, alaska", geography),
      geography = gsub("nome county, alaska", "nome census area, alaska", geography),
      geography = gsub("valdez-cordova county, alaska", "valdez-cordova census area, alaska", geography),
      geography = gsub("yukon-koyukuk county, alaska", "yukon-koyukuk census area, alaska", geography),
      geography = gsub("southeast fairbanks county, alaska", "southeast fairbanks census area, alaska", geography),
      geography = gsub("bristol bay county, alaska", "bristol bay borough, alaska", geography),
      geography = gsub("fairbanks north star county, alaska", "fairbanks north star borough, alaska", geography),
      geography = gsub("juneau county, alaska", "juneau city and borough, alaska", geography),
      geography = gsub("kenai-cook inlet county, alaska", "kenai peninsula borough, alaska", geography),
      geography = gsub("kenai peninsula county, alaska", "kenai peninsula borough, alaska", geography),
      geography = gsub("ketchikan gateway county, alaska", "ketchikan gateway borough, alaska", geography),
      geography = gsub("kodiak island county, alaska", "kodiak island borough, alaska", geography),
      geography = gsub("northwest arctic county, alaska", "northwest arctic borough, alaska", geography),
      geography = gsub("sitka county, alaska", "sitka city and borough, alaska", geography),
      geography = gsub("north slope county, alaska", "north slope borough, alaska", geography),
      geography = gsub("wrangell-petersburg county, alaska", "wrangell city and borough, alaska", geography),
      geography = gsub("yakutat county, alaska", "yakutat city and borough, alaska", geography),
      geography = gsub("matanuska-susitna county, alaska", "matanuska-susitna borough, alaska", geography),
      geography = gsub("denali county, alaska", "denali borough, alaska", geography),
      
      # Louisiana
      geography = gsub("caddo county, louisiana", "caddo parish, louisiana", geography),
      geography = gsub("calcasieu county, louisiana", "calcasieu parish, louisiana", geography),
      geography = gsub("east baton rouge county, louisiana", "east baton rouge parish, louisiana", geography),
      geography = gsub("iberia county, louisiana", "iberia parish, louisiana", geography),
      geography = gsub("jefferson county, louisiana", "jackson parish, louisiana", geography),
      geography = gsub("lafayette county, louisiana", "lafayette parish, louisiana", geography),
      geography = gsub("la salle parish, louisiana", "lasalle parish, louisiana", geography),
      geography = gsub("ouachita county, louisiana", "ouachita parish, louisiana", geography),
      geography = gsub("rapides county, louisiana", "rapides parish, louisiana", geography),
      
      # Virginia
      geography = gsub(" city county, virginia", " city, virginia", geography),
      geography = gsub("arlington county, NA", "arlington county, virginia", geography), # assuming Arlington DC is Arlington, VA
      geography = gsub("loudoun county, NA", "loudoun county, virginia", geography),
      geography = gsub("newport news county, virginia", "newport news city, virginia", geography),
      geography = gsub("charlottesville county, virginia", "charlottesville city, virginia", geography),
      geography = gsub("covington county, virginia", "covington city, virginia", geography),
      geography = gsub("danville county, virginia", "danville city, virginia", geography),
      geography = gsub("alexandria county, virginia", "alexandria city, virginia", geography),
      geography = gsub("buena vista county, virginia", "buena vista city, virginia", geography),
      geography = gsub("emporia county, virginia", "emporia city, virginia", geography),
      geography = gsub("fredericksburg county, virginia", "fredericksburg city, virginia", geography),
      geography = gsub("emporia county, virginia", "emporia city, virginia", geography),
      geography = gsub("hopewell county, virginia", "hopewell city, virginia", geography),
      geography = gsub("lynchburg county, virginia", "lynchburg city, virginia", geography),
      geography = gsub("martinsville county, virginia", "martinsville city, virginia", geography),
      #geography = gsub("portsmouth city county, virginia", "portsmouth city, virginia", geography),
      geography = gsub("petersburg county, virginia", "petersburg city, virginia", geography),
      #geography = gsub("richmond city county, virginia", "richmond city, virginia", geography),
      #geography = gsub("roanoke city county, virginia", "roanoke city, virginia", geography),
      geography = gsub("suffolk county, virginia", "suffolk city, virginia", geography),
      #geography = gsub("suffolk city county, virginia", "suffolk city, virginia", geography),
      geography = gsub("waynesboro county, virginia", "waynesboro city, virginia", geography),
      # geography = gsub("norfolk city county, virginia", "norfolk city, virginia", geography),
      geography = gsub("norfolk county, virginia", "norfolk city, virginia", geography),
      geography = gsub("virginia beach county, virginia", "virginia beach city, virginia", geography),
      # geography = gsub("virginia beach city county, virginia", "virginia beach city, virginia", geography),
      geography = gsub("chesapeake county, virginia", "chesapeake city, virginia", geography),
      #geography = gsub("hampton city county, virginia", "hampton city, virginia", geography),
      geography = gsub(replacement = "james city county, virginia", pattern = "james city, virginia", geography),
      
      # Colorado 
      geography = gsub("portable source county, colorado", "pueblo county, colorado", geography),
      
      # Florida
      geography = gsub("hills county, florida", "hillsborough county, florida", geography),
      geography = gsub("^dade county, florida", "miami-dade county, florida", geography),
      
      # Illinois
      geography = gsub("la salle county, illinois", "lasalle county, illinois", geography),
      
      # Indiana
      geography = gsub("de kalb county, indiana", "dekalb county, indiana", geography),
      geography = gsub("la porte county, indiana", "laporte county, indiana", geography),
      geography = gsub("la porte county, indiana", "laporte county, indiana", geography),
      
      # Nevada
      geography = gsub("carson city county, nevada", "carson city, nevada", geography),
      
      # New Hampshire
      geography = gsub("hills county, new hampshire", "hillsborough county, new hampshire", geography),
      
      # New Mexico
      geography = gsub("debaca county, new mexico", "de baca county, new mexico", geography),
      #geography = gsub("otero county county, new mexico", "otero county, new mexico", geography),      
      geography = gsub(replacement = "doña ana county, new mexico", pattern = "dona ana county, new mexico", geography),
      #geography = gsub("do?a ana county, new mexico", "dona ana county, new mexico", geography)
      
      # North Carolina
      # geography = gsub("cumberland county county, north carolina", "cumberland county, north carolina", geography),
      geography = gsub("badin county, north carolina", "stanly county, north carolina", geography),
      
      # Maryland
      geography = gsub("prince georges county, maryland", "prince george's county, maryland", geography),
      geography = gsub("baltimore city county, maryland", "baltimore city, maryland", geography),
      geography = gsub("saint marys county, maryland", "st. mary's county, maryland", geography),
      geography = gsub("hartford county, maryland", "harford county, maryland", geography),
      
      # Minnesota
      geography = gsub("saint louis county, minnesota", "st. louis county, minnesota", geography),
      
      # South Carolina
      geography = gsub("greenvile county, south carolina", "greenville county, south carolina", geography),
      
      # West Virginia
      geography = gsub("kanahwa county, west virginia", "kanawha county, west virginia", geography),
      
      # D.C.
      geography = gsub("washington county, district of columbia", "district of columbia, district of columbia", geography),
      geography = gsub("district of columbia county, NA", "district of columbia, district of columbia", geography),
      geography = gsub("district of columbia county, district of columbia", "district of columbia, district of columbia", geography),
      
      # Other
      geography = gsub("mc ", "mc", geography),
      geography = gsub("^st ", "st. ", geography),
      geography = gsub(" county county, ", " county, ", geography),
      
    )
}

# Dataframe of states --------------------------------------------------------

# Used throughout the script for linkages.

key_states <- data.frame(
  state_abbr = state.abb,
  state_name = state.name,
  state_name_lower = tolower(state.name)) %>%
  bind_rows(data.frame(state_abbr = "DC", 
                       state_name = "District of Columbia",
                       state_name_lower = "district of columbia"))


# Load 2010-2014 ACS -----------------------------------------------------------

# 5-year American Community Survey
# https://factfinder.census.gov/bkmk/navigation/1.0/en/d_dataset:ACS_14_5YR/d_product_type:DATA_PROFILE/

county14socraw <- read.csv("raw/2014 5yr/ACS_14_5YR_DP02_with_ann.csv")
county14ecoraw <- read.csv("raw/2014 5yr/ACS_14_5YR_DP03_with_ann.csv")
county14demraw <- read.csv("raw/2014 5yr/ACS_14_5YR_DP05_with_ann.csv")
county14tenureraw <- read.csv("raw/2014 5yr/ACS_14_5YR_B25003_with_ann.csv")

# Scrape EJ vars and rename based on ACS documentation.

county14soc <- county14socraw[2:nrow(county14socraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC95", "HC03_VC142", "HC03_VC173")] %>%
  rename(geography = GEO.display.label, 
         # perc_hs_grad = ,
         perc_foreign_noncit = HC03_VC142, 
         perc_poor_eng = HC03_VC173)

county14eco <- county14ecoraw[2:nrow(county14ecoraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC12", "HC03_VC50", "HC01_VC85", "HC01_VC86", "HC03_VC161", "HC03_VC171", "HC03_VC134")] %>%
  rename(geography = GEO.display.label, 
         perc_unemp = HC03_VC12, 
         perc_ag_ind = HC03_VC50, 
         med_inc = HC01_VC85, 
         mean_inc = HC01_VC86, 
         perc_pov_fam = HC03_VC161, 
         perc_pov_ppl = HC03_VC171, 
         perc_uninsur = HC03_VC134)

county14dem <- county14demraw[2:nrow(county14demraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC03", "HC03_VC49", "HC03_VC50", "HC03_VC88", "HC03_VC80",
                                                        "HC03_VC94", "HC03_VC95")] %>%
  rename(geography = GEO.display.label, 
         total_pop = HC01_VC03, 
         perc_white_only = HC03_VC49, 
         perc_black_only = HC03_VC50, 
         perc_hisp_any = HC03_VC88,
         perc_white_nohisp = HC03_VC94, 
         perc_black_nohisp = HC03_VC95, 
         perc_am.ind_any = HC03_VC80)

county14tenure <- county14tenureraw[2:nrow(county14tenureraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01", "HD01_VD02")] %>%
  rename(geography = GEO.display.label,
         all.house14 = HD01_VD01, 
         owned.house = HD01_VD02)

# Bind county demographic data together.

county14all <- left_join(county14eco, county14soc) %>%
  left_join(county14dem) %>%
  left_join(county14tenure) %>% 
  mutate(geography = case_when(GEO.id2 == "35013" ~ "doña ana county, new mexico",
                               TRUE ~ geography), 
         geography = tolower(geography))

cn14 <- county14all 

# Load 2017 multiple deprivation index (MDI) ----------------------------------------

# https://www.census.gov/library/publications/2019/acs/acs-40.html

# Ignores the last few rows (footers).

mdi <- read_xls("raw/county-level-mdi-rates-2017.xls", n_max = 3143) 

# Three counties did not have MDI rates published in this dataset (GEO.id2 = "10003", "35039", "42101"). 
# For these counties, use NA instead of supplying a value. (Consider an alternative.)
# In addition, all county IDs should have five digit codes. For IDs with 4 digits, 
# add a leading zero. This chunk returns a warning message that can be ignored.

mdi2 <- mdi %>% 
  mutate(GEO.id2 = if_else(
    nchar(county) == 4, 
    paste0("0", county), 
    as.character(county)
    )) %>% 
  mutate(mdi_rate = if_else(
    GEO.id2 %in% c("10003", "35039", "42101"), 
    as.numeric(NA), 
    100*as.numeric(`MDI rate`)
    )) 

# Load 2010 urbanicity ---------------------------------------------------------

# Urbanicity, called "perc_urban", was calculated as the proportion of the 
# county area (defined by census tracts or blocks) with greater than 50,000 people,
# from 2010 estimates. 

cnurban <- read.csv("raw/2010 urban/DEC_10_SF1_H2_with_ann.csv", skip = 1)

# count housing units in urban areas
cnurban2 <- cnurban %>% 
  mutate(Total. = gsub("\\(.*\\)", "", Total.),
         Total. = as.numeric(Total.),
         Id2 = case_when(nchar(Id2) == 4 ~ paste0("0", Id2),TRUE ~ as.character(Id2)),
         Geography = case_when(Id2 == "35013" ~ "doña ana county, new mexico",
                               TRUE ~ Geography),
         Geography = tolower(Geography)) %>%
  rename(geography = Geography, 
         all.house = Total., 
         urb.house = Urban....Inside.urbanized.areas, 
         urb.house.c = Urban....Inside.urban.clusters, 
         rur.house = Rural,
         GEO.id2 = Id2) %>%
  select("geography", "GEO.id2", "all.house", "urb.house", "urb.house.c", "rur.house") %>%
  mutate(geography = gsub("petersburg census area, alaska", "petersburg borough, alaska", geography),
         geography = gsub("la salle parish, louisiana", "lasalle parish, louisiana", geography),
         geography = gsub("bedford city, virginia", "bedford county, virginia", geography)
  ) %>%
  #add bedford city and bedford county together
  group_by(geography, GEO.id2) %>% 
  summarize_all(sum)

# JML check: identical house #s #
sum(as.numeric(cnurban$Urban....Inside.urbanized.areas)) == sum(cnurban2$urb.house)
sum(as.numeric(cnurban$Urban....Inside.urban.clusters)) == sum(cnurban2$urb.house.c)
sum(as.numeric(cnurban$Rural)) == sum(cnurban2$rur.house)

cnurban3 <- cnurban2 %>%
  group_by(geography, GEO.id2) %>%
  summarise(perc_urban = 100*urb.house/all.house)
cnurban3

# Load wastewater data ------------------------------------------------------

# Wastewater treatment plant locations and reported effluent flows were reported in 
# EPA 2012 Clean Watershed Survey. Loading triggered a parsing warning since 
# two columns were expected to contain all numbers, but for two counties, 
# values were inputted as "-". These were read in as "NA"; replace with zeros. 

src_wwtp <- read_csv("raw/PFAS point source data/Data/WWTP facility_details.csv")

src_wwtp1 <- src_wwtp %>%
  mutate(`Existing Total Flow (Mgal/d)` = replace_na(`Existing Total Flow (Mgal/d)`, 0))

# mean(src_wwtp1$`Existing Total Flow (Mgal/d)`) # 2.25 
# max(src_wwtp1$`Existing Total Flow (Mgal/d)`) # 812

# Set up correct county names for linkages. 

src_wwtp2 <- src_wwtp1 %>% 
  left_join(key_states, by = c("State" = "state_abbr")) %>% 
  mutate(geography = ifelse(State != "LA",
                            paste0(tolower(`County Name`), " county, ", 
                                   tolower(state_name)),
                            paste0(tolower(`County Name`), " parish, ", 
                                   tolower(state_name))))

src_wwtp3 <- src_wwtp2 %>% correct_counties() 

# For each county, calculate the number of wastewater plants and the 
# total effluent flow. 

src_wwtp4 <- src_wwtp3 %>% 
  group_by(geography) %>% 
  summarise(n_WWTP = length(unique(`CWNS Number`)), 
            WWTP_totalflow_mgd = sum(`Existing Total Flow (Mgal/d)`))

# Load county land area size from Census Bureau. Use to normalize wastewater 
# flow by area. Normalizing occurs in a different script. 
# Note that the original file reported land area in acres.

landarea <- read.csv("raw/land area/DEC_10_SF1_G001_with_ann.csv")

# Pull out land area
landarea1 <- landarea[2:nrow(landarea),] %>%
  select("GEO.id2", "VD067") %>%
  rename(land.area = VD067) %>%
  mutate(land.area = as.numeric(land.area),
         #merge bedford city into bedford county
         GEO.id2 = case_when(GEO.id2 == "51515" ~ "51019",
                             TRUE ~ GEO.id2))

landarea2 <- landarea1 %>% 
  group_by(GEO.id2) %>% 
  summarize_all(sum)

length(unique(landarea2$GEO.id2)) #3142 counties

stopifnot(landarea2$GEO.id2 %in% cn14$GEO.id2)

# Load Toxics Release Inventory (TRI) data ------------------------------------

# https://www.epa.gov/toxics-release-inventory-tri-program/tri-toolbox

# Read in TRI Basic Data Files. 

tri_basic10 <- read_csv("raw/TRI/TRI_2010_US.csv")
tri_basic11 <- read_csv("raw/TRI/TRI_2011_US.csv")
tri_basic12 <- read_csv("raw/TRI/TRI_2012_US.csv")
tri_basic13 <- read_csv("raw/TRI/TRI_2013_US.csv")
tri_basic14 <- read_csv("raw/TRI/TRI_2014_US.csv") %>% 
  mutate(`10. BIA` = as.numeric(`10. BIA`))
tri_basic15 <- read_csv("raw/TRI/TRI_2015_US.csv") %>% 
  mutate(`9. ZIP` = as.character(`9. ZIP`), 
         `15. PARENT CO DB NUM` = as.character(`15. PARENT CO DB NUM`)) # added AM 11-25-22

tri_basic <- bind_rows(tri_basic10, tri_basic11, tri_basic12, tri_basic13, tri_basic14, tri_basic15) 
colnames(tri_basic) <- gsub("\\d+\\.\\d+ - ", "", colnames(tri_basic))
colnames(tri_basic) <- gsub("\\d+\\. ", "", colnames(tri_basic))
colnames(tri_basic) <- gsub("\\d+\\.\\d+ ", "", colnames(tri_basic))

# CAS numbers 
# 000123911 -- 1,4-dioxane
# 000075343 -- 1,1-dichloroethane (ethylidene dichloride)
# 000071556 -- 1,1,1-trichloroethane
# 000075456 -- HCFC-22
# 000075718 -- CFC-12

tri_basic_sub <- tri_basic %>% 
  filter(`CAS #/COMPOUND ID` %in% c(
    "000123911", 
    "000075343", 
    "000071556", 
    "000075456", 
    "000075718"))

# If needed, delete from global environment (save tri_basic_sub).
# rm(list = ls(pattern = "^tri_basic1"))

# Read in TRI R and A download data. Compiles FIPS codes for linkages.

tri_raw10 <- read_csv("raw/TRI originals/481818639_2010.CSV")
tri_raw11 <- read_csv("raw/TRI originals/481838923_2011.CSV")
tri_raw12 <- read_csv("raw/TRI originals/481847511_2012.CSV")
tri_raw13 <- read_csv("raw/TRI originals/481855955_2013.CSV")
tri_raw14 <- read_csv("raw/TRI originals/481864670_2014.CSV")
tri_raw15 <- read_csv("raw/TRI originals/481873288_2015.CSV")

tri_raw <- bind_rows(tri_raw10, tri_raw11, tri_raw12, tri_raw13, tri_raw14, tri_raw15)
colnames(tri_raw) <- gsub("V_TRI_FORM_R_EZ.","", colnames(tri_raw))
tri_raw_sub <- tri_raw %>%
  select(-CAS_CHEM_NAME, -CITY_NAME, -GENERIC_CHEM_NAME, -LATITUDE, -LONGITUDE, -FACILITY_NAME, -PRIMARY_NAICS_CODE,
         -PRIMARY_SIC_CODE, -STATE_ABBR, -STREET_ADDRESS, -ZIP_CODE) %>%
  filter(TRI_CHEM_ID %in% c("000123911", 
                            "000075343",
                            "000071556",
                            "000075456", 
                            "000075718"))

# If needed, delete from global environment (save tri_raw_sub).
# rm(list = ls(pattern = "^tri_raw1"))

# Combine TRI data files. 

tri0 <- tri_raw_sub %>%
  distinct(COUNTY_NAME, FRS_ID, REPORTING_YEAR, STATE_COUNTY_FIPS_CODE) 

tri1 <- tri_basic_sub %>% 
  left_join(tri0, by = c("FRS ID" = "FRS_ID",
                         "YEAR" = "REPORTING_YEAR"))

# Combine TRI data files and clean county names.

tri1.1 <- tri1 %>%
  select(`FRS ID`, COUNTY,  ST, STATE_COUNTY_FIPS_CODE, `CAS #/COMPOUND ID`, 
         `CAS #/COMPOUND ID`,
         `TOTAL TRANSFER`, `TOTAL RELEASES`) %>% 
  left_join(key_states, by = c("ST" = "state_abbr")) %>%
  mutate(geography = ifelse(ST != "LA",
                            paste0(tolower(COUNTY), " county, ", 
                                   state_name_lower),
                            paste0(tolower(COUNTY), " parish, ", 
                                   state_name_lower))) %>% 
  janitor::clean_names()

# create columns indicating specific chemical and chemical group.

tri1.2 <- tri1.1 %>%
  mutate(chem = case_when(cas_number_compound_id == "000123911" ~ "1,4-dioxane", 
                          cas_number_compound_id == "000071556" ~ "1,1,1-trichloroethane", 
                          cas_number_compound_id == "000075456" ~ "HCFC-22", 
                          cas_number_compound_id == "000075343" ~ "1,1-dichloroethane", 
                          cas_number_compound_id == "000075718" ~ "CFC-12", 
                          TRUE ~ "oops")) %>% 
  mutate(chem_cat = case_when(chem == "1,4-dioxane" ~ "diox", 
                              chem %in% c("1,1,1-trichloroethane", 
                                          "1,1-dichloroethane") ~ "chlor_solv", 
                              chem %in% c("HCFC-22", "CFC-12") ~ "cfc", 
                              TRUE ~ "oops"))

tri2 <- tri1.2 %>%
  group_by(geography, state_county_fips_code, chem_cat) %>% 
  summarise(n_fac = length(unique(frs_id)), 
            bin_fac = 1, 
            mean_releases = mean(total_releases))

tri2.1 <- tri1.2 %>%
  mutate(chem_cat = "any") %>%
  group_by(geography, state_county_fips_code, chem_cat) %>% 
  summarise(n_fac = length(unique(frs_id)), 
            bin_fac = 1, 
            mean_releases = mean(total_releases))

tri3 <- bind_rows(tri2, tri2.1)

tri4 <- tri3 %>%
  pivot_wider(id_cols = state_county_fips_code, 
               names_from = chem_cat, 
              values_from = c(n_fac, bin_fac), 
              names_glue = "{.value}_{chem_cat}") %>%
  mutate_all(., ~replace_na(.,0)) %>%
  rename(GEO.id2 = 1)


# Load PFAS point sources ------------------------------------------------------

src_epa <- epastewardship <- read_excel("raw/PFAS point source data/Data/EPA 2010.2015 PFOA Stewardship Program sites.xlsx")
src_mfta <- MFTA <- read_excel("raw/PFAS point source data/Data/all MFTA_county.xlsx")
src_airprt <- airports <- read_excel("raw/PFAS point source data/Data/Part 139_cert_airports.xlsx")

# * EPA Stewardship facilities -------------------------------------------------

src_epa1 <- src_epa %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(State))) %>%
  mutate(src_epa_present = 1) %>% 
  select(geography, src_epa_present)

# * AFFF-certified airports ----------------------------------------------------

src_airprt1 <- src_airprt %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(StateName))) %>% 
  correct_counties() %>% 
  mutate(src_airport_present = 1) %>% 
  select(geography, src_airport_present)

src_airprt2 <- src_airprt1 %>%
  group_by(geography) %>% 
  summarise(n_airports = n())


# * Military and fire training areas (MFTA) ------------------------------------

src_mfta1 <- src_mfta %>% 
  mutate(geography = ifelse(`State/Territory`!= "LOUISIANA",
                            paste0(tolower(COUNTY_NAME), " county, ", 
                                   tolower(`State/Territory`)),
                            paste0(tolower(COUNTY_NAME), 
                                   " parish, ",
                                   tolower(`State/Territory`))))

src_mfta1.1 <- src_mfta1 %>% correct_counties()

src_mfta2 <- src_mfta1.1 %>% 
  mutate(geography = ifelse(geography == "NA county, new hampshire", 
                            "rockingham county, new hampshire", 
                            geography))

src_mfta3 <- src_mfta2 %>% 
  group_by(geography) %>% 
  summarize(n_MFTA = length(unique(`Installation name`))) 

# Load SDWIS data --------------------------------------------------------------

ucmr3 <- read_csv("raw/UCMR3_All.csv") 
allsdwis <- read_csv("clean/processed_aggregated_sdwis_geoandsys.csv")

#add in DC systems
allsdwis2 <- read_csv("clean/DC_manual_download_2021-03-05.csv") %>%
  rename(PWSID = `PWS ID`,
         PWS_NAME = `PWS Name`) %>% 
  mutate(PRIMACY_AGENCY_CODE = "DC",
         PWS_TYPE_CODE = case_when(`PWS Type` == "Community water system" ~ "CWS",
                                   `PWS Type` == "Non-Transient non-community system" ~ "NTNCS"),
         WS.POPULATION_SERVED_COUNT = `Population Served Count`,
         COUNTY_SERVED = "District of Columbia") %>% 
  select(PWSID, PWS_NAME, PWS_TYPE_CODE, PRIMACY_AGENCY_CODE, WS.POPULATION_SERVED_COUNT, COUNTY_SERVED)

#bind sdwis with DC systems
allsdwis3 <- bind_rows(allsdwis, allsdwis2)

#get list of PWS in UCMR 
sdwis_in_ucmr <- allsdwis3 %>% filter(PWSID %in% ucmr3$PWSID)
unique(sdwis_in_ucmr$PWSID) %>% length() #5280 systems matched
unique(allsdwis3 %>% filter(!PWSID %in% ucmr3$PWSID)) %>% length() #21 sys did not match
5401 - (21+5280) # 100 sys in UCMR, but not in SDWIS?

#how many UCMR PWSs have city served information?
sdwis_in_ucmr %>% 
  #group_by(PWSID, PWS_NAME) %>%
  summarize(n_sys_wo_cityserved = length(unique(PWSID[which(is.na(CITY_SERVED))])),
            n_sys_w_cityserved = length(unique(PWSID[which(!is.na(CITY_SERVED))])),
            perc_sys_wo_cityserved = 100*n_sys_wo_cityserved/n()) # 43%

# Create water system linker  --------------------------------------------------

weirdco <- (c("BRISTOL","RADFORD","CHARLOTTESVILLE","HARRISONBURG","SALEM","STAUNTON","WAYNESBORO",     
              "WINCHESTER","CHESAPEAKE","COLONIAL HEIGHTS", "HOPEWELL","NEWPORT NEWS","NORFOLK","PETERSBURG",      
              "PORTSMOUTH","SUFFOLK","VIRGINIA BEACH","WILLIAMSBURG","RICHMOND CITY","DANVILLE","LYNCHBURG" ,    
              "MARTINSVILLE","ALEXANDRIA","FAIRFAX CITY","FALLS CHURCH","FREDERICKSBURG","MANASSAS","MANASSAS PARK"))

link1 <- allsdwis3 %>% 
  left_join(key_states, by = c("PRIMACY_AGENCY_CODE" = "state_abbr"))

link1.1 <- link1 %>% 
  mutate(countyname = str_split(COUNTY_SERVED, pattern = ",")) %>%
  unnest(countyname)

link1.2 <- link1.1 %>% 
  mutate(countyname = gsub("^ ", "", countyname),
         geography = str_c(tolower(countyname), ", ", state_name_lower)) 

link2 <- link1.2 %>% 
  mutate(geography = gsub("la salle parish, louisiana", "lasalle parish, louisiana", geography),
         geography = case_when(!(geography %in% cn14$geography) ~ gsub(",", " county,",  geography), 
                               TRUE ~ geography),
         geography = gsub(replacement = "doña ana county, new mexico", pattern = "dona ana county county, new mexico", geography),
         geography = case_when(geography == " county, nevada" ~ "carson city, nevada",
                               geography == " county, new york" ~ "nassau county, new york",
                               TRUE ~ geography),
         geography = case_when(CITY_SERVED %in% weirdco & state_name_lower == "virginia" ~ paste0(tolower(weirdco), " city, " ,state_name_lower),
                               #bedford city is a former independent city within bedford county virginia (as of 2013)
                               geography == "bedford city county, virginia" ~ "bedford county, virginia",
                               CITY_SERVED == "CARSON CITY" ~ "carson city, nevada",
                               PWSID == "NY2902841" ~ "nassau county, new york",
                               TRUE ~ geography),
         geography = case_when(geography == "fairfax city city, virginia" ~ "fairfax city, virginia",
                               geography == "richmond city city, virginia" ~ "richmond city, virginia",
                               TRUE ~ geography)) %>% 
  unique()

link3 <- link2 %>% 
  select(PWSID, PWS_NAME, WS.POPULATION_SERVED_COUNT, 
         WS.OWNER_TYPE_CODE, CITY_SERVED, COUNTY_SERVED, 
         geography)

fips <- link3 %>% 
  left_join(cn14 %>% select("geography", "GEO.id2"))

fips1 <- fips %>%
  distinct(PWSID, GEO.id2)

fips_check <- ucmr3 %>% left_join(fips1, by = "PWSID") 
fips_check1 <- fips_check %>% filter(is.na(GEO.id2))
fips_check1 %>% summarise(n = length(unique(PWSID)))
fips_check1.1 <- fips_check1 %>% distinct(PWSID, State) # 122 systems do not have FIPS codes

# Combine all datasets ---------------------------------------------------------

cn14.1 <- cn14 %>%
  left_join(mdi2) %>%
  left_join(cnurban3) %>%
  left_join(src_wwtp4) %>% 
  left_join(src_epa1) %>% 
  left_join(src_airprt2) %>% 
  left_join(src_mfta3) %>%
  left_join(tri4) %>%
  left_join(landarea2)

cn14.2  <- cn14.1 %>%
  select(-`Standard Error`, -`MDI rate`, -GEO.id, 
         -county) %>%
  mutate_at(vars(matches("src_|bin_|n_")), 
            ~ifelse(is.na(.), 0, .)) 

cn15 <- cn14.2 %>%
  mutate_at(vars(matches("perc_|_inc|_pop|house")), as.numeric)
