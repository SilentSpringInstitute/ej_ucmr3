### AUTHOR: AHz, LRS, JL 
### LAST EDIT: 2021-06-20 (AHz)
### LAST REVIEW: 2021-06-20(AHz)
### WRITTEN IN: R version 3.5.1
### Purpose: process all demo info 
### Depends on: UCMR loading and processing.R (for FIPS only)

library(tidyverse)
library(dplyr)
library(janitor)
library(readxl)

options (stringsAsFactors = FALSE)

# Unclear if we still need this post-Github:
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

# Get UCMR3 data
source("1 - UCMR loading and processing.R")

# Keep two objects in the working directory:
# (1) a raw copy of the UCMR3 data, 
# (2) a intermediate dataset that processed whether a system: 
#     (-) detected a target contaminant 
#     (-) exceeded an health-based reference level 
# Target contaminants: a PFAS (any of 6), 1,4-dioxane, 1,1-DCA,
# HCFC-22, or any of these target contaminants. 

rm(
  list = 
    setdiff(ls(),
            c("ucmr3_raw", "ucmr_detcode")
            )
  )

################################################################################
#  0. README ####
################################################################################

### GOAL 
#
#       Clean and match all demographic and sources data to a county in the 
#       census. This script creates a demo file for all counties in the US 
#       and matches them to a PWSID, but does not filter down to only UCMR PWSs.
#       
#       The ACS will be our “source of truth” for direct county name matching. 
#       
#        
### DATA DICTIONARY
# 
# county14all 
#     USE: made up of county14soc, county14eco, county14dem, and county14tenure (county level data)
#     SOURCE: ACS -- matches easily with other ACS data (housing, etc.)
#     MATCH CODES: FIPS code (GEO.id2), county name (geography)

# MDI data    
#     USE: multidimensional deprivation index (2017) 
#     SOURCE: ACS
#     MATCH CODES: FIPS code (GEO.id2), county name (geography)

# TRI data     
#     USE: TBD 
#     SOURCE: EPA Toxic Release Inventory
#     MATCH CODES: FIPS code, county name 

# cnurban     
#     USE: urbanicity
#     SOURCE: 
#     MATCH CODES: county name (geography)

# censuslandarea     
#     USE: to calc values taking into account size
#     SOURCE: 
#     MATCH CODES: FIPS code (GEO.id2)

# cnmetros     
#     USE: 
#     SOURCE: 
#     MATCH CODES: county names (geography) -- name mismatches exist, must be corrected

# EPA Stewardship facility list
#     USE: 
#     SOURCE: EPA 
#     MATCH CODES: county (+ State)

# Part-139-certified airports
#     USE: 
#     SOURCE: https://www.faa.gov/airports/airport_safety/airportdata_5010/
#     MATCH CODES: County (+ State)

# Wastewater treatment plants
#     USE: 
#     SOURCE: EPA Clean Watersheds Needs Survey 2012; https://ofmpub.epa.gov/apex/cwns2012/f?p=cwns2012:1:
#     MATCH CODES: county

# Military fire-training installations
#     USE: 
#     SOURCE: AFFF report to Congress
#     MATCH CODES: COUNTY_NAME, FIPS_CODE
#             
#             
### NOTES
#
#     There is a consistent issue with bedford city virginia (FIPS code 51515), 
#     which was an independent city until 2013. It is now a part of bedford county, virginia
#     (FIPS code 51019). THe Census data we are using is a 5 yr estimate so bedford city
#     is not included as a separate county. Other census data include bedford city, so 
#     we have to combine the two together -- either bedford city is summed with bedford
#     county or bedford city gets dropped. 
#     
# SECTION 1. LOAD DATA
# 
#       
# SECTION 2. LOAD ACS COUNTY LEVEL DEMOGRAPHICS
# 
#       Census columns renamed based on documentation. All the percent and 
#       numeric columns were loaded in as characters, so we tell R to read them
#       as numeric values. Check that columns aren’t empty.  
#        
# SECTION 2b. SUPPLEMENTAL POVERTY MEASUREMENTS
# 
#       Load in MDI data. There are three counties that get an “X” which affects
#       7 PWSs in UCMR. Text at the bottom of this file gets dropped when we 
#       read in the xls file. “GEO.id2” is the county FIPS code, and we need 
#       that to read as a character so that leading zeroes aren’t deleted. MDI 
#       rate is interpreted as a percent, so we multiply by 100.   
#        
#       Other metrics considered: Social Vulnerability Index (see garbage)
# 
# # SECTION 3. READ IN WATER SYSTEM COUNTY LINKER
# 
#       SDWIS does not come with a FIPS code, so we have to string match on 
#       county name. We create a new column called “geography” which creates 
#       a lowercase “county name, state” string for each PWS. From there, 
#       we are looking for exact matches to our ACS census data.
#       
#       NOTE: Bedford City, Virginia was an independent city until 2013, at 
#       which point it became a part of Bedford County, Virginia. Our 5 yr ACS 
#       census estimate only recognizes Bedford County (after Bedford City 
#       merged), so we need to manually change all Bedford City references to 
#       Bedford County. 
# 
# SECTION 4. PROCESS TRI DATA 
# 
#       Load the TRI Basic Data Files (2010-2015) and bind them together. The 
#       basic data files don't include the FIPS code, which makes binding a lot 
#       easier/more accurate, so we also downloaded TRI data from the Form R & A 
#       that lets you pick specific columns to download. 
#       
#       For both files, we want to subset down to just the chemicals we're 
#       interested in (by CAS #). The raw files are pretty big, so we delete 
#       them from the global environment to make things run faster. 
#       
#       We bind the two TRI dataframes together by facility ID, year, and 
#       compound and then drop some less relevant columns. 
#       
#        We are interested in both the compound that is released that pairs with
#        a UCMR compound AND it's parent compound when available. This is true 
#        for 1,1-dichloroethane (and its parent compound 1,1,1-trichloroethane) 
#        and HCFC-22 (and its parent compound CFC-12). We want to keep the 
#        compounds by themselves but also add on a new compound that groups 
#        them together. We made a new "test_chem" that groups the relevant 
#        compounds together ("Chlorinated Solvents" and "CFCs") and sums the 
#        numeric columns for both compounds. We also want to know if there was 
#        ever a TRI facility (that reports one of our relevant compounds) in a 
#        county, so we do the same thing and call the chemical  "bin_TRI."
#       
#       
# SECTION 5. PROCESS URBANICITY DATA
# 
#       
#       
# SECTION 6. PROCESS CENSUS LAND AREA DATA
# 
#       
#       
# SECTION 7. PROCESS METRO DATA
# 
#       
#       
# SECTION 8. LOAD PFAS POINT SOURCE FILES
# 
#       Load the PFAS point source files and create a local function that cleans
#        county names ("correct_counties") to reduce repetitive code.
#       
# SECTION 8a. EPA STEWARDSHIP PROGRAM
# 
#       
# SECTION 8b. AIRPORTS
# 
#       
# SECTION 8c. WWTP Facilities
# 
#       
# SECTION 8d. MFTA
# 
#       
# SECTION 9. MERGE ALL
# 
#       Using "county14all" (census data), left join all the other dataframes 
#       except the TRI data ("cn14demo_merge"). 
#       
#       Create a mock data frame that is going to give us a row for each TRI Chem
#        within each county (regardless of whether a facility exists) (nrows = 
#        number of counties x8), if a county has no TRI facility that reports 
#        then the TRI fields will be empty. 
#        
#        Take the county level demo data (cn14demo_merge) and full join with 
#        the TRI data (cndemo_tri) -- we use a full join because we want to 
#        retain separate rows for all the TRI chemicals. 



################################################################################
#  1. LOAD DATA  ####
################################################################################

# Load clean SDWIS file
allsdwis <- read_csv("clean/processed_aggregated_sdwis_geoandsys.csv")

# Clean SDWIS file does not contain DC systems. Load in separately and adjust 
# variables for consistency with the primary SDWIS file
dcsys <- read_csv("clean/DC_manual_download_2021-03-05.csv") %>%
  rename(PWSID = `PWS ID`,
         PWS_NAME = `PWS Name`) %>% 
  mutate(PRIMACY_AGENCY_CODE = "DC",
         PWS_TYPE_CODE = case_when(`PWS Type` == "Community water system" ~ "CWS",
                                   `PWS Type` == "Non-Transient non-community system" ~ "NTNCS"),
         WS.POPULATION_SERVED_COUNT = `Population Served Count`,
         COUNTY_SERVED = "District of Columbia"
         ) %>% 
  select(PWSID, PWS_NAME, PWS_TYPE_CODE, PRIMACY_AGENCY_CODE, WS.POPULATION_SERVED_COUNT, COUNTY_SERVED)

# Combine U.S. and D.C. SDWIS files 
allsdwis_dc <- bind_rows(allsdwis, dcsys)

# get list of PWS in UCMR 
sdwis_in_ucmr <- allsdwis_dc %>% filter(PWSID %in% ucmr_detcode$PWSID)

# check by JML
n_distinct(sdwis_in_ucmr$PWSID) == n_distinct(ucmr_detcode$PWSID) # all in SDWIS


#how many UCMR PWSs have city served information?
sdwis_in_ucmr %>% 
  group_by(.) %>%
  summarize(n_sys_wo_cityserved = length(unique(PWSID[which(is.na(CITY_SERVED))])),
            n_sys_w_cityserved = length(unique(PWSID[which(!is.na(CITY_SERVED))])),
            perc_sys_wo_cityserved = n_sys_wo_cityserved/n()*100, 
            perc_sys_w_cityserved = n_sys_w_cityserved/n()*100)
#42%

### for matching FIPS/state abb only data with county names 
#add converter for state abbreviations to full state name

add_dc <- data.frame("stateabb" = "DC",
                     "fullstate" = "District of Columbia",
                     "fullstate_lower" = "district of columbia")

stateabbnamekey <- as.data.frame(matrix(ncol = 3, nrow = 50))
colnames(stateabbnamekey) <- c("stateabb", "fullstate", "fullstate_lower")
stateabbnamekey <- stateabbnamekey %>% 
  mutate(stateabb = state.abb,
         fullstate = state.name,
         fullstate_lower = tolower(state.name))  %>% 
  bind_rows(add_dc)




################################################################################
#  2. LOAD ACS COUNTY LEVEL DEMOGRAPHICS  ####
################################################################################


#get demographic information for all counties
county14socraw <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/2014 5yr/ACS_14_5YR_DP02_with_ann.csv")
county14ecoraw <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/2014 5yr/ACS_14_5YR_DP03_with_ann.csv")
county14demraw <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/2014 5yr/ACS_14_5YR_DP05_with_ann.csv")
county14tenureraw <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/2014 5yr/ACS_14_5YR_B25003_with_ann.csv")

#scrape just EJ vars and rename (based on ACS documentation)
county14soc <- county14socraw[2:nrow(county14socraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC95", "HC03_VC142", "HC03_VC173")] %>%
  rename(geography = GEO.display.label, perc_hs_grad = HC03_VC95, perc_foreign_noncit = HC03_VC142, perc_poor_eng = HC03_VC173)

county14eco <- county14ecoraw[2:nrow(county14ecoraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC03_VC12", "HC03_VC50", "HC01_VC85", "HC01_VC86", "HC03_VC161", "HC03_VC171", "HC03_VC134")] %>%
  rename(geography = GEO.display.label, perc_unemp = HC03_VC12, perc_ag_ind = HC03_VC50, med_inc = HC01_VC85, 
         mean_inc = HC01_VC86, perc_pov_fam = HC03_VC161, perc_pov_ppl = HC03_VC171, perc_uninsur = HC03_VC134)

county14dem <- county14demraw[2:nrow(county14demraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HC01_VC03", "HC03_VC49", "HC03_VC50", "HC03_VC88", "HC03_VC80",
                                                        "HC03_VC94", "HC03_VC95")] %>%
  rename(geography = GEO.display.label, total_pop = HC01_VC03, perc_white_only = HC03_VC49, perc_black_only = HC03_VC50, perc_hisp_any = HC03_VC88,
         perc_white_nohisp = HC03_VC94, perc_black_nohisp = HC03_VC95, perc_am.ind_any = HC03_VC80)

county14tenure <- county14tenureraw[2:nrow(county14tenureraw), c("GEO.id", "GEO.id2", "GEO.display.label", "HD01_VD01", "HD01_VD02")] %>%
  rename(geography = GEO.display.label, all.house14 = HD01_VD01, owned.house = HD01_VD02)


#bind all county demographic data together (not subset)
county14all <- left_join(county14eco, county14soc) %>%
  left_join(county14dem) %>%
  left_join(county14tenure) %>% 
  mutate(geography = case_when(GEO.id2 == "35013" ~ "doña ana county, new mexico",
                               TRUE ~ geography), 
         geography = tolower(geography))


#fix column types, make numeric
numcols <- c("total_pop", "perc_white_only","perc_black_only", "perc_hisp_any", "perc_white_nohisp", "perc_black_nohisp", "perc_am.ind_any", "perc_unemp", 
             "all.house14", "owned.house", "perc_ag_ind", "med_inc", "mean_inc", "perc_pov_fam", "perc_pov_ppl", "perc_hs_grad", 
             "perc_poor_eng", "perc_foreign_noncit", "perc_uninsur")

for (i in numcols){
  county14all[,i] <- as.numeric(county14all[,i])
}

checkcols_list <- list()
checkcols_df <- data.frame()

for(i in numcols){
  stopifnot(length(unique(county14all$PWSID[which(is.na(county14all$i))])) == 0)
  checkcols_list[i] <- length(unique(county14all$PWSID[which(is.na(county14all$i))]))
  
  #checkcols_list[i] <- ifelse(is.na(county14all[,i]), paste(unique(i)), NA) %>% unique()

}

checkcols_df <- bind_rows(checkcols_list) %>% 
  pivot_longer(names_to = "columns", values_to = "flag", everything()) %>% 
  filter(!is.na(flag))
# checkcols_df
# none 

#write_csv(county14all, "results/preliminary/all census data ", Sys.Date(), ".csv")


################################################################################
#  2b. SUPPLEMENTAL POVERTY MEASURE  ####
################################################################################

mdi_17 <- read_xls("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/Multidimensional Deprivation Index Rates/county-level-mdi-rates-2017.xls") %>% 
  filter(!is.na(county))
# 3 counties get "X" -- X = Estimate suppresed due to data collection issues 
# this affects 7 of our UCMR systems ("DE0000552" "DE0000564" "DE0000614" 
# "DE0000630" "DE0000663" "DE00A0683" (all in GEO.id2 == 10003) and "PA1510001"
# (in GEO.id2 == 42101))
# drop bottom rows with this text

mdi2merge <- mdi_17 %>% 
  mutate(GEO.id2 = case_when(nchar(county) == 4 ~ paste0("0", county),TRUE ~ as.character(county)),
         mdi = as.numeric(`MDI rate`)*100) %>% 
  select(-county, -`Standard Error`, -`MDI rate`)
#introduces NAs for "X"

table(is.na(mdi2merge$mdi))
# 3 missing due to data collection issues. 



  

################################################################################
#  3. READ IN WATER SYSTEM COUNTY LINKER ####
################################################################################

weirdco <- (c("BRISTOL","RADFORD","CHARLOTTESVILLE","HARRISONBURG","SALEM","STAUNTON","WAYNESBORO",     
             "WINCHESTER","CHESAPEAKE","COLONIAL HEIGHTS", "HOPEWELL","NEWPORT NEWS","NORFOLK","PETERSBURG",      
              "PORTSMOUTH","SUFFOLK","VIRGINIA BEACH","WILLIAMSBURG","RICHMOND CITY","DANVILLE","LYNCHBURG" ,    
              "MARTINSVILLE","ALEXANDRIA","FAIRFAX CITY","FALLS CHURCH","FREDERICKSBURG","MANASSAS","MANASSAS PARK"))

sdwis_county_linker <- allsdwis_dc %>% 
  left_join(stateabbnamekey, by = c("PRIMACY_AGENCY_CODE" = "stateabb")) %>%
  mutate(countyname = str_split(COUNTY_SERVED, pattern = ",")) %>%
  unnest(cols = c(countyname)) %>%
  mutate(countyname = gsub("^ ", "", countyname),
         geography = str_c(tolower(countyname), ", ", fullstate_lower)) %>% 
  mutate(geography = gsub("la salle parish, louisiana", "lasalle parish, louisiana", geography),
         geography = case_when(!(geography %in% county14all$geography) ~ gsub(",", " county,",  geography), 
                               TRUE ~ geography),
         geography = gsub(replacement = "doña ana county, new mexico", pattern = "dona ana county county, new mexico", geography),
         geography = case_when(geography == " county, nevada" ~ "carson city, nevada",
                               geography == " county, new york" ~ "nassau county, new york",
                               TRUE ~ geography),
         geography = case_when(CITY_SERVED %in% weirdco & fullstate_lower == "virginia" ~ paste0(tolower(weirdco), " city, " ,fullstate_lower),
                               #bedford city is a former independent city within bedford county virginia (as of 2013)
                               geography == "bedford city county, virginia" ~ "bedford county, virginia",
                               CITY_SERVED == "CARSON CITY" ~ "carson city, nevada",
                               PWSID == "NY2902841" ~ "nassau county, new york",
                               TRUE ~ geography),
         geography = case_when(geography == "fairfax city city, virginia" ~ "fairfax city, virginia",
                               geography == "richmond city city, virginia" ~ "richmond city, virginia",
                               TRUE ~ geography)) %>% 
  #dropping bc bedford city/bedford county are the same place, but this column keeps them from being grouped as one row (impt: for script 3)
  select(-countyname) %>% 
  unique()

# check by JML: checking that sum of commas + nrow(allsdwis_dc) == nrow(sdwis_county_linker)
n_distinct(sdwis_county_linker$PWSID) == n_distinct(allsdwis_dc$PWSID) # T
sum(str_count(string = allsdwis_dc$COUNTY_SERVED, pattern = ","), na.rm = T) + nrow(allsdwis_dc) # 413237
# note: a little unsure what unique() is removing above (n = 2 difference)


setdiff(sdwis_county_linker$geography, county14all$geography)
table(sdwis_county_linker$AREA_TYPE_CODE, is.na(sdwis_county_linker$geography))
#all with "NA county, state" don't have county info in sdwis (CT/CT & ZC/multiCT/No Geo Available)
#sdwis_county_linker %>% filter(AREA_TYPE_CODE == "CT") %>% filter(!is.na(geography)) %>%  view


### note from JML: may be able to salvage some of these systems, if they're relevant 
  # by using the city served to geocode counties using google maps API, although this may not alter results
  # depending on how many of these without county info are in UCMR (and whether this is differential by model covariates)
  # quick check:
  no_county <- unique(sdwis_county_linker$PWSID[is.na(sdwis_county_linker$geography)])
  sum(no_county %in% ucmrsys) # none appear to be in UCMR, so this isn't a problem
###
  
#sdwis_county_linker %>% filter(is.na(geography)) %>% filter(PWSID %in% ucmrsys) %>% View
##_#_# fine for now, only 34 virginia ones (+ one NY and one NV)in ucmr are affected. 
# 2020-09-25 AHz: fixed the virginia and nevada systems. Looked up the NY system and 
# it seems to be in nassau county http://www.waterauthorityofgreatnecknorth.com/who_we_are.html


pwsid_fips <- left_join(sdwis_county_linker, county14all[, c("geography", "GEO.id2")])

#write_csv(pwsid_fips, paste0("results/preliminary/all sdwis with demo PWSID to FIPS linker ", Sys.Date(),".csv"))


ucmrcounties_fips <- pwsid_fips %>% subset(PWSID %in% ucmrsys) %>% pull(GEO.id2)
table(is.na(ucmrcounties_fips))
stopifnot(!is.na(ucmrcounties_fips))
#all have match

# note from JL: i'm seeing missing geography from the system(s) serving doña ana county, new mexico,
  # but not sure if that matters

################################################################################
#  4. PROCESS TRI DATA  ####
################################################################################

#CAS numbers 
# 000075343 -- 1,1-dichloroethane (ethylidene dichloride)
# 000123911 -- 1,4-dioxane
# 000071556 -- 1,1,1-trichloroethane
# 000075456 -- HCFC-22
# 000075718 -- CFC-12


# read in TRI Basic Data File
tri_basic10 <- read_csv("../Data/TRI data/basic data file/TRI_2010_US.csv")
tri_basic11 <- read_csv("../Data/TRI data/basic data file/TRI_2011_US.csv")
tri_basic12 <- read_csv("../Data/TRI data/basic data file/TRI_2012_US.csv")
tri_basic13 <- read_csv("../Data/TRI data/basic data file/TRI_2013_US.csv")
tri_basic14 <- read_csv("../Data/TRI data/basic data file/TRI_2014_US.csv") %>% 
  mutate(`10. BIA` = as.numeric(`10. BIA`))
tri_basic15 <- read_csv("../Data/TRI data/basic data file/TRI_2015_US.csv") %>% 
  mutate(`9. ZIP` = as.character(`9. ZIP`), 
         `15. PARENT CO DB NUM` = as.character(`15. PARENT CO DB NUM`)) # added AM 11-25-22

tri_basic <- bind_rows(tri_basic10, tri_basic11, tri_basic12, tri_basic13, tri_basic14, tri_basic15) 
colnames(tri_basic) <- gsub("\\d+\\.\\d+ - ", "", colnames(tri_basic))
colnames(tri_basic) <- gsub("\\d+\\. ", "", colnames(tri_basic))
colnames(tri_basic) <- gsub("\\d+\\.\\d+ ", "", colnames(tri_basic))
#close enough


tri_basic_sub <- tri_basic %>% 
  filter(`CAS #/COMPOUND ID` %in% c(
    #1,4-dioxane
    "000123911", 
    #1,1-dichloroethane (ethylidene dichloride)
    "000075343", 
    #1,1,1-trichloroethane
    "000071556", 
    #HCFC-22
    "000075456", 
    #CFC-12
    "000075718"))


#delete from global environment to make things run faster
tri_basic10 <- NA
tri_basic11 <- NA
tri_basic12 <- NA
tri_basic13 <- NA
tri_basic14 <- NA
tri_basic15 <- NA

 
# read in TRI R and A download data
# has to read in in order to get fips code
tri_raw10 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481818639_2010.CSV")
tri_raw11 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481838923_2011.CSV")
tri_raw12 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481847511_2012.CSV")
tri_raw13 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481855955_2013.CSV")
tri_raw14 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481864670_2014.CSV")
tri_raw15 <- read_csv("../Data/TRI data/originals/Downloaded 2020-07-24/481873288_2015.CSV")

tri_raw <- bind_rows(tri_raw10, tri_raw11, tri_raw12, tri_raw13, tri_raw14, tri_raw15)
colnames(tri_raw) <- gsub("V_TRI_FORM_R_EZ.","", colnames(tri_raw))
tri_raw_sub <- tri_raw %>%
  select(-CAS_CHEM_NAME, -CITY_NAME, -GENERIC_CHEM_NAME, -LATITUDE, -LONGITUDE, -FACILITY_NAME, -PRIMARY_NAICS_CODE,
         -PRIMARY_SIC_CODE, -STATE_ABBR, -STREET_ADDRESS, -ZIP_CODE) %>%
  filter(TRI_CHEM_ID %in% c("000123911", "000075343", "000071556", "000075456", "000075718"))

#delete from global environment to make things run faster
tri_raw10 <- NA
tri_raw11 <- NA
tri_raw12 <- NA
tri_raw13 <- NA
tri_raw14 <- NA
tri_raw15 <- NA

#bind tri basic with tri form RA by ID, year, and compound to get FIPS codes
tri_all_subset <- tri_basic_sub %>%
  left_join(stateabbnamekey, by = c("ST" = "stateabb")) %>% 
  left_join(tri_raw_sub, by = c("FRS ID" = "FRS_ID", "YEAR" = "REPORTING_YEAR", 
                                "CAS #/COMPOUND ID" = "TRI_CHEM_ID")) %>% 
  mutate(geography = paste0(tolower(COUNTY), ", ", fullstate_lower)) %>% 
  rename(reporting_year = YEAR,
         test_chem = CHEMICAL)

#make dataset even smaller number of columns
tri_small_subset <-
  tri_all_subset %>%
  select( -`CAS #/COMPOUND ID`, -ST, -`STANDARD PARENT CO NAME`, -`STREET ADDRESS`, -`PARENT CO DB NUM`,
         -`ELEMENTAL METAL INCLUDED`, -`METAL`, -`METAL CATEGORY`, -fullstate, -(`SIC 2`:`SIC 6`), -(M41:`M61 METAL`),
         -`SRS ID`, -LATITUDE, -LONGITUDE) %>%
  select(reporting_year, STATE_COUNTY_FIPS_CODE, geography, COUNTY, TRIFD, test_chem, `FORM TYPE`,`UNIT OF MEASURE`, `FUGITIVE AIR`, `STACK AIR`, UNDERGROUND, `5.LANDFILLS`,
         `ON-SITE RELEASE TOTAL`, `POTW - TRNS RLSE`, `POTW - TRNS TRT`, `POTW - TOTAL TRANSFERS`, `OFF-SITE RELEASE TOTAL`,
         `OFF-SITE TREATED TOTAL`, `TOTAL TRANSFER`, `TOTAL RELEASES`, `TREATMENT ON SITE`, `TREATMENT OFF SITE`)

#write_csv(tri_clean, paste0("results/preliminary/all TRI data SMALL", Sys.Date(), ".csv"))

#clean names 
tri_clean_subset <- tri_small_subset %>% 
  select(-geography, -COUNTY) %>% 
  mutate(reporting_year = as.character(reporting_year)) %>% 
  clean_names() %>% 
  rename(GEO.id2 = state_county_fips_code)

#add contaminant that captures chlorinated solvents
tri_chlorinated_solv <- tri_clean_subset %>% 
  filter(test_chem %in% c("ETHYLIDENE DICHLORIDE", "1,1,1-TRICHLOROETHANE")) %>% 
  group_by(GEO.id2, reporting_year) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(test_chem = "CHLORINATED SOLVENTS")

#add contaminant that captures chlorinated solvents
tri_cfc <- tri_clean_subset %>% 
  filter(test_chem %in% c("DICHLORODIFLUOROMETHANE", "CHLORODIFLUOROMETHANE")) %>% 
  group_by(GEO.id2, reporting_year) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(test_chem = "CFCs")

#add contaminant that captures any of the TRI chems of interest
bin_tri <- tri_clean_subset %>% 
  filter(test_chem %in% c("DICHLORODIFLUOROMETHANE", "CHLORODIFLUOROMETHANE", "ETHYLIDENE DICHLORIDE", "1,1,1-TRICHLOROETHANE", "1,4-DIOXANE")) %>% 
  group_by(GEO.id2, reporting_year) %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(test_chem = "bin_TRI")


tri_clean_subset2 <- bind_rows(tri_clean_subset, tri_chlorinated_solv, tri_cfc, bin_tri)

#get tri sum release values by county (one row per county per year)
tri_county_sum <- tri_clean_subset2 %>%
  group_by(GEO.id2, test_chem, reporting_year) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)

#get yearly average total release for each county (one row per county)
tri_yearly_avg <- tri_county_sum %>%
  group_by(GEO.id2, test_chem) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate(reporting_year = "avg")

#get summary of facilities within a county (one row per county)
tri_fac_summary <- tri_clean_subset2 %>%
  group_by(GEO.id2, test_chem) %>%
  summarize(n_fac = length(unique(trifd)),
            #n_fac_w_release = length(unique(trifd[which(total_on_off_site_release > 0)])),
            n_reporting_year = length(unique(reporting_year)))

# check 
# tri_fac_summary %>% filter(test_chem == "bin_TRI") %>% View()

# a new df with yearly avg total releases in each county, number of facilities, 
# and number of reporting years
tri_merge <- left_join(tri_yearly_avg, tri_fac_summary) 


setdiff(tri_merge$GEO.id2, county14all$GEO.id2)
# 4 mismatches -- manual search shows these are in PR, guam, etc. 
# with HCFC-22 is added -- 2020-08-21

#how many counties have TRI data? 
length(unique(tri_merge$GEO.id2))
table(tri_merge$test_chem, tri_merge$GEO.id2 %in% ucmrcounties_fips)
# 18 not in ucmr county (check against false/bin_TRI)

#_#_# NOTE: there is more than one row per county (with some data repeated) bc 
#we grouped by chem



################################################################################
#  5. PROCESS URBANICITY DATA  ####
################################################################################

#get urbanicity data
cnurban <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/2010 urban/DEC_10_SF1_H2_with_ann.csv", skip = 1)

c

# JML check: identical house #s #
sum(as.numeric(cnurban$Urban....Inside.urbanized.areas)) == sum(cnurban2merge$urb.house)
sum(as.numeric(cnurban$Urban....Inside.urban.clusters)) == sum(cnurban2merge$urb.house.c)
sum(as.numeric(cnurban$Rural)) == sum(cnurban2merge$rur.house)
####

# setdiff(cnurban2merge$geography, county14all$geography)
# #perfect match (fine when you merge bedford city and bedford county)

stopifnot(cnurban2merge$geography %in% county14all$geography)


################################################################################
#  6. PROCESS CENSUS LAND AREA DATA  ####
################################################################################

#bring in land area amount, necessary to quantify % land use for systems with aggregated regions
censuslandarea <- read.csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/land area/DEC_10_SF1_G001_with_ann.csv")

#pull out land area
landarea <- censuslandarea[2:nrow(censuslandarea),] %>%
  select("GEO.id", "GEO.id2", "GEO.display.label", "VD067") %>%
  rename(land.area = VD067) %>%
  #convert sq meters to acres
  mutate(land.area = as.numeric(land.area),
         land.area = land.area*0.000247105,
         #merge bedford city into bedford county
         GEO.id2 = case_when(GEO.id2 == "51515" ~ "51019",
                             TRUE ~ GEO.id2)) %>%
  select("GEO.id2", "land.area") %>% 
  group_by(GEO.id2) %>% 
  summarize_all(sum)


length(unique(landarea$GEO.id2))
#3143 counties
# JML comment: I get 3142 here but guessing that's because the bedford counties were combined?

stopifnot(landarea$GEO.id2 %in% county14all$GEO.id2)

################################################################################
#  7. PROCESS METRO DATA  ####
################################################################################

#add metro area status to counties
cnmetros <- read.csv("../Data/Demographic, County, and Water System Data/raw/Geography/ruralurban_counties2013.csv")

cnmetros$metrostatus <- ifelse(grepl("^Metro", cnmetros$Description) == TRUE, "Metro",
                               ifelse(grepl("Completely rural", cnmetros$Description) == TRUE, "Rural", "Nonmetro, Nonrural"))

metroformatted <- merge(cnmetros, stateabbnamekey, by.x = "State", by.y = "stateabb")

#clean up mismatches

metroformatted <- inner_join(cnmetros, stateabbnamekey, by = c("State" = "stateabb")) %>% 
  mutate(countyname.form = paste0(tolower(County_Name), ", ", fullstate_lower)) %>% 
  mutate(GEO.id2 = case_when(nchar(FIPS) == 4 ~ paste0("0", as.character(FIPS)),TRUE ~ as.character(FIPS)),
         #add bedford city, va to bedford county, va, both metro so fine
         GEO.id2 = case_when(GEO.id2 == "51515" ~ "51019",
                             TRUE ~ GEO.id2)) %>%
  select("metrostatus", GEO.id2) %>% 
  unique()


setdiff(metroformatted$GEO.id2, county14all$GEO.id2)
#doesn't match with 51515 -- bedford city, virginia was an independent city until 2013 now a 
#part of bedford county, virginia (2020-08-21) (see note at top of script)



################################################################################
#  8.   Load PFAS point source files ####
################################################################################

# load point source files
epastewardship <- read_excel("../Data/PFAS point source data/Data/EPA 2010.2015 PFOA Stewardship Program sites.xlsx")

WWTPfacilities <- read_csv("../Data/PFAS point source data/Data/WWTP facility_details.csv")

MFTA <- read_excel("../Data/PFAS point source data/Data/all MFTA_county.xlsx")

airports <- read_excel("../Data/PFAS point source data/Data/Part 139_cert_airports.xlsx")


correct_counties <- function(dat){
  dat %>% 
    mutate(geography = gsub("aleutians west county, alaska", "aleutians west census area, alaska", geography),
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
      
           geography = gsub("caddo county, louisiana", "caddo parish, louisiana", geography),
           geography = gsub("calcasieu county, louisiana", "calcasieu parish, louisiana", geography),
           geography = gsub("east baton rouge county, louisiana", "east baton rouge parish, louisiana", geography),
           geography = gsub("iberia county, louisiana", "iberia parish, louisiana", geography),
           geography = gsub("jefferson county, louisiana", "jackson parish, louisiana", geography),
           geography = gsub("lafayette county, louisiana", "lafayette parish, louisiana", geography),
           geography = gsub("la salle parish, louisiana", "lasalle parish, louisiana", geography),
           geography = gsub("ouachita county, louisiana", "ouachita parish, louisiana", geography),
           geography = gsub("rapides county, louisiana", "rapides parish, louisiana", geography),
           
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
           
           geography = gsub("carson city county, nevada", "carson city, nevada", geography),
           
           geography = gsub("de kalb county, indiana", "dekalb county, indiana", geography),
           geography = gsub("la porte county, indiana", "laporte county, indiana", geography),
           geography = gsub("la porte county, indiana", "laporte county, indiana", geography),
           
           geography = gsub("debaca county, new mexico", "de baca county, new mexico", geography),
           #geography = gsub("otero county county, new mexico", "otero county, new mexico", geography),      
           geography = gsub(replacement = "doña ana county, new mexico", pattern = "dona ana county, new mexico", geography),
           
           geography = gsub("la salle county, illinois", "lasalle county, illinois", geography),
           
           geography = gsub("mc ", "mc", geography),
           geography = gsub("^st ", "st. ", geography),
           geography = gsub(" county county, ", " county, ", geography),
           
           geography = gsub("washington county, district of columbia", "district of columbia, district of columbia", geography),
           geography = gsub("district of columbia county, NA", "district of columbia, district of columbia", geography),
           geography = gsub("district of columbia county, district of columbia", "district of columbia, district of columbia", geography),
           
           # geography = gsub("cumberland county county, north carolina", "cumberland county, north carolina", geography),
           geography = gsub("badin county, north carolina", "stanly county, north carolina", geography),
           geography = gsub("greenvile county, south carolina", "greenville county, south carolina", geography),
           
           
           geography = gsub("hills county, florida", "hillsborough county, florida", geography),
           geography = gsub("^dade county, florida", "miami-dade county, florida", geography),
           
           geography = gsub("hills county, new hampshire", "hillsborough county, new hampshire", geography),
           geography = gsub("kanahwa county, west virginia", "kanawha county, west virginia", geography),
           geography = gsub("saint louis county, minnesota", "st. louis county, minnesota", geography),
           geography = gsub("portable source county, colorado", "pueblo county, colorado", geography),
           
           geography = gsub("prince georges county, maryland", "prince george's county, maryland", geography),
           geography = gsub("baltimore city county, maryland", "baltimore city, maryland", geography),
           geography = gsub("saint marys county, maryland", "st. mary's county, maryland", geography),
           geography = gsub("hartford county, maryland", "harford county, maryland", geography),
           
           #geography = gsub("do?a ana county, new mexico", "dona ana county, new mexico", geography)
    )

}

################################################################################
#  8a.   EPA Stewardship program ####
################################################################################
# Reshaping, processing, and renaming columns for point source data by counties
epastewardship_county <- epastewardship %>% 
  count(County, State) %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(State))) %>% 
  rename(n_epastewardship = n)


epastewardship_merge <- epastewardship_county %>% 
  select(geography, n_epastewardship)

################################################################################
#  8b.   airports ####
################################################################################

airports_county <- airports %>% 
  # add full state name and fix several geography names for ease of merging
  left_join(stateabbnamekey, by = c("CountyState" = "stateabb")) %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(fullstate))) %>% 
  correct_counties()

airports_merge <- airports_county %>% 
  group_by(geography) %>% 
  summarize(n_airports = length(unique(SiteNumber)))

################################################################################
#  8c.   WWTP facilities ####
################################################################################


WWTPfacilities_county <- WWTPfacilities %>% 
  mutate(`Existing Total Flow (Mgal/d)` = case_when(is.na(`Existing Total Flow (Mgal/d)`) ~ 0, 
                                                    TRUE ~ `Existing Total Flow (Mgal/d)`)) %>% 
  # add full state name and fix several geography names for ease of merging
  left_join(stateabbnamekey, by = c("State" = "stateabb")) %>% 
  mutate(geography = case_when(State!= "LA" ~ paste0(tolower(`County Name`), " county, ",
                                                     tolower(fullstate)),
                               State == "LA" ~ paste0(tolower(`County Name`), " parish, ",
                                                      tolower(fullstate)))) %>% 
  group_by(geography) %>% 
  summarize(n_WWTP = length(unique(`CWNS Number`)),
            WWTP_totalflow_mgd = sum(`Existing Total Flow (Mgal/d)`)) %>% 
  correct_counties() 

WWTPfacilities_merge <- WWTPfacilities_county %>% 
  select(geography, WWTP_totalflow_mgd, n_WWTP)


################################################################################
#  8d.   MFTA ####
################################################################################


MFTA_county <- MFTA %>% 
  mutate(geography = case_when(`State/Territory`!="LOUISIANA" ~ paste0(tolower(COUNTY_NAME), " county, ",
                                                                       tolower(`State/Territory`)),
                               `State/Territory`=="LOUISIANA" ~ paste0(tolower(COUNTY_NAME), " parish, ",
                                                                       tolower(`State/Territory`)))) %>% 
  correct_counties() %>% 
  mutate(geography = gsub("NA county, new hampshire", "rockingham county, new hampshire", geography))


  
MFTA_merge <- MFTA_county %>% 
  group_by(geography) %>% 
  #drop repeated Stanly/Badin national guard base
  summarize(n_MFTA = length(unique(`Installation name`))) 


# note: it looks like non-US-states (Guam, PR etc will be dropped)

# check matches
setdiff(epastewardship_county$geography, county14all$geography)
# none
setdiff(airports_merge$geography, county14all$geography)
# NA county, NA is in PR  (6)
setdiff(WWTPfacilities_county$geography, county14all$geography)
#all "NA" state (48)
setdiff(MFTA_merge$geography, county14all$geography)
#all PR/Guam/Marshall Islands (7)

################################################################################
#  9. MERGE ALL  ####
################################################################################



cn14demo_merge <- county14all %>% 
  left_join(mdi2merge) %>% 
  left_join(cnurban2merge) %>% 
  left_join(landarea) %>%
  left_join(metroformatted) %>%
  left_join(WWTPfacilities_merge) %>%
  left_join(epastewardship_merge) %>%
  left_join(airports_merge) %>%
  left_join(MFTA_merge)


# Create a mock data frame that is going to give us a row for each TRI Chem 
# within each county (regardless of whether a facility exists)
# (nrows = number of counties x8), if a county has no TRI facility that reports 
# then the TRI fields will be empty 
cndemo_tri <-  cn14demo_merge %>% 
  select(GEO.id2) %>% 
  mutate(test_chem = paste("ETHYLIDENE DICHLORIDE", "1,4-DIOXANE", "CHLORINATED SOLVENTS", 
                           "1,1,1-TRICHLOROETHANE", "CHLORODIFLUOROMETHANE", "DICHLORODIFLUOROMETHANE", "CFCs", "bin_TRI", sep = "  ")) %>% 
  separate_rows(test_chem, sep = "  ")%>%
  left_join(tri_merge) 

check <- cndemo_tri %>%
  group_by(GEO.id2) %>%
  count()
table(check$n, useNA = "ifany")
#all 8 okay


cn14all <- cn14demo_merge  %>%
  full_join(cndemo_tri) %>% 
  mutate(hasurban = ifelse(!is_empty(all.house), "Y", "N"),
         propurban = ifelse(hasurban == "N", NA, urb.house/all.house),
         propurban.c = ifelse(hasurban == "N", NA, urb.house.c/all.house),
         propurban.weight = ifelse(hasurban == "N", NA, (urb.house+(urb.house.c/2))/all.house),
         proprural = ifelse(hasurban == "N", NA, rur.house/all.house),
         n_fac = ifelse(is.na(n_fac), 0, as.numeric(n_fac)),
         perc_hmown = (owned.house/all.house14)*100,
         n_epastewardship_peracre = round(n_epastewardship/land.area, 3),
         n_airports_peracre = round(n_airports/land.area, 3),
         WWTP_totalflow_mgd_peracre = round(WWTP_totalflow_mgd/land.area, 3),
         n_MFTA_peracre = round(n_MFTA/land.area, 3),
         metronum = case_when(metrostatus == "Metro" ~ 1,
                              metrostatus == "Nonmetro, Nonrural" ~ 2,
                              TRUE ~ 3)
  )

check <- cn14all %>% 
  group_by(GEO.id2) %>% 
  count()
table(check$n, useNA = "ifany")
# all good! 2020-10-13

stopifnot(!is.na(cn14all$GEO.id2))

#write_csv(cn14all, paste0("results/preliminary/UCMR demo data pre-PWSID match ", Sys.Date(),".csv"))

# ################################################################
# library(sp)
# library(sf)
# 
# basemap <- read_sf("../Data/basemaps/USA_Counties.shp") %>% select(FIPS, geometry)
# tri_loc <- tri_all_subset %>%
#   select(reporting_year, TRIFD, LATITUDE, LONGITUDE)
# 
# TRI_locs <- st_as_sf(tri_loc, coords = c('LONGITUDE', 'LATITUDE'))
# counties <- st_as_sf(basemap)
# st_crs(TRI_locs) = st_crs(counties)
# TRI_locs_km = st_transform(TRI_locs, "+proj=utm +zone=42N +datum=WGS84 +units=km")
# counties_km = st_transform(counties, "+proj=utm +zone=42N +datum=WGS84 +units=km")
# TRI_buffer_50 <- st_buffer(TRI_locs_km, 50) # 50 km buffer
# TRI_buffer_5 <- st_buffer(TRI_locs_km, 5) # 5 km buffer
# TRI_counties_50 <- st_intersection(TRI_buffer_50, counties_km)
# TRI_counties_5 <- st_intersection(TRI_buffer_5, counties_km)
# 
# TRI_counties_bin_50 <- TRI_counties_50 %>%
#   as_tibble() %>%
#   select(FIPS, reporting_year, TRIFD) %>%
#   pivot_wider(names_from = reporting_year, names_glue = "year_{reporting_year}", values_from = TRIFD, 
#               values_fn = ~length(unique(.x))) %>% 
#   group_by(FIPS) %>%
#   mutate(n_fac = max(c_across(2:5), na.rm = T)) %>%
#   mutate(evr_fac = ifelse(!is.na(n_fac), "1", "0")) %>%
#   select(evr_fac)
# 
# TRI_counties_bin_5 <- TRI_counties_5 %>%
#   as_tibble() %>%
#   select(FIPS, reporting_year, TRIFD) %>%
#   pivot_wider(names_from = reporting_year, names_glue = "year_{reporting_year}", values_from = TRIFD, 
#               values_fn = ~length(unique(.x))) %>% 
#   group_by(FIPS) %>%
#   mutate(n_fac = max(c_across(2:5), na.rm = T)) %>%
#   mutate(evr_fac = ifelse(!is.na(n_fac), "1", "0")) %>%
#   select(evr_fac)
# 
# # 1012 counties within 50 km range of a TRI facility 
# # 224 counties within 5 km range of a TRI facility 
# 
# cn14all %>% filter(test_chem == 'bin_TRI') %>% select(GEO.id2, geography, n_fac) %>%
#   group_by(n_fac) %>%
#   count()
# 
# ################################################################################
# #  GARBAGE <3  ####
# ################################################################################
# 
# #  2c. SOCIAL VULNERABILITY INDEX  ####
# 
# svi18 <- read_csv("../Data/Demographic, County, and Water System Data/originals/ACS Census Data/by County/Social Vulnerability Index/SVI2018_US_COUNTY.csv")
# 
# table(svi18$RPL_THEMES)
# 
# svi_mdi <- svi18 %>% select(FIPS, RPL_THEME1, RPL_THEME2, 
#                             RPL_THEME3, RPL_THEME4, RPL_THEMES) %>% 
#   rename(GEO.id2 = FIPS) %>% 
#   full_join(mdi2merge)
# 
# ucmr_svi_mdi <- svi_mdi %>% filter(GEO.id2 %in% ucmrcounties_fips)
# 
# ggplot(svi_mdi, aes(x = RPL_THEMES, y = mdi)) + 
#   geom_point() + 
#   geom_smooth() + 
#   xlim(0,1) + 
#   ylim(0,100)