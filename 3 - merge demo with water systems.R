### AUTHOR: AHz
### LAST EDIT: 2021-03-05 (AHz)
### WRITTEN IN: R version 3.5.1
### Purpose: Merge all demographic information with water system information and aggregate demographic info for 
### systems that serve more than one county

library(tidyverse)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

################################################################################
#  0. README  ###############################################################
################################################################################

### GOAL 
#
#     Merge demographic information with PWS information. Calculate new 
#     demographic values for PWSs that serve >1 county.   

### NOTES
#
#   * Some sections of this code are commented out because they were not used in 
#     this analysis. For future work, these pieces of code could be uncommented
#     and used. 
# 
#   SECTION 2. MERGE WATER SYSTEM INFO WITH DEMO 
#       
#       Merge the demographic data with the PWSID-FIPS linker data frame. Pull 
#       PWSIDs of PWSs that serve more than one county. 
#       
#       
#   SECTION 3. COMBINE DEMO FOR >1 COUNTY SERVED 
#   
#       Filter the demo data to show just PWSs that serve more than one county
#       (multicndemo). Convert all percent columns into numbers based on the 
#       total population of a county. We need this to calculate new values 
#       for PWSs that serve >1 county.
#       
#       After aggregating the numbers, we convert them back into percents. 
#   
#   SECTION 4. 
#       
#       Merge the PWSs that serve more than one county (multicndemo) with the 
#       PWSs that only serve once county. 
#       
#       We need to "fix" some columns. If the column is empty, that means that 
#       a facility did not exist. In that case, we want to assign a 0 as the 
#       value (for example, a PWS that has an NA in the n_fac means they did 
#       not report a TRI facility -- since no TRI facility was reported, we 
#       believe that there are 0 TRI facilities of that kind. 
#       
#       
################################################################################
#  1. LOAD DATA  ####
################################################################################

# #read in UCMR data
ucmr3 <- read_csv("intermediate/ucmr3 processed with detchem code 2021-06-28.csv")
 
# #get a log of which systems were tested in UCMR
ucmrsys <- unique(ucmr3$PWSID)

#use read.csv because it doesn't try to correct col types

cn14all <- read.csv("intermediate/UCMR demo data pre-PWSID match 2021-05-10.csv")%>% 
  mutate(GEO.id2 = case_when(nchar(GEO.id2) == 4 ~ paste0("0", GEO.id2),TRUE ~ as.character(GEO.id2)))

pwsid_fips <- read_csv("intermediate/all sdwis with demo PWSID to FIPS linker 2021-06-28.csv") %>% 
  mutate(GEO.id2 = case_when(nchar(GEO.id2) == 4 ~ paste0("0", GEO.id2),TRUE ~ as.character(GEO.id2))) %>% 
  select(-geography)

(setdiff(pwsid_fips$GEO.id2, cn14all$GEO.id2))
#NA because there are ~34304 systems without FIPS codes 
#table(is.na(pws_counties$GEO.id2))

#check if any of the PWSID w/o FIPS code are in UCMR
stopifnot(!is.na(pwsid_fips$GEO.id2[which(pwsid_fips$PWSID %in% ucmrsys)]))
#okay to proceed

################################################################################
#  2. MERGE WATER SYSTEM INFO WITH DEMO ####
################################################################################


pws_demoall <- cn14all %>% 
  left_join(pwsid_fips)

check <- pws_demoall %>% 
  group_by(GEO.id2, PWSID, test_chem) %>% 
  count()

stopifnot(check$n == 1)
#all one -- hooray! 2021-03-05

# JML check:
n_distinct(pwsid_fips$PWSID) *n_distinct(cn14all$test_chem)

#pull PWSIDs that serve more than one county
multicn.pwsids <- pws_demoall %>% 
  group_by(PWSID) %>% 
  summarize(n = length(unique(GEO.id2))) %>% 
  filter(n > 1) %>% 
  pull(PWSID)

## JML: check
JL.test <- unique(pws_demoall$PWSID[grepl(pattern = ",", pws_demoall$COUNTY_SERVED)])

# View(subset(pws_demoall, PWSID %in% setdiff(JL.test, multicn.pwsids)))
# these look to all be the bedford county/city PWSIDs, so this checks out!
##

# RUN SOME CHECKS ---------------------------------------------------------

#did dona ana county merge okay? 
stopifnot(length(unique(pws_demoall$test_chem[which(pws_demoall$GEO.id2 == "35013")])) == 8)


#how many/which FIPS without PWSID? 
length(unique(pws_demoall$GEO.id2[which(!pws_demoall$GEO.id2 %in% pwsid_fips$GEO.id2)]))
# 10 -- 2021-04-27 (make sure DC isn't in here)

checkna <- pws_demoall %>% 
  select(PWSID, GEO.id2, geography) %>% 
  filter(is.na(PWSID)) %>% 
  unique()



################################################################################
#  3. COMBINE DEMO FOR >1 COUNTY SERVED ####
################################################################################


#subset multi county PWSID with demo data
multicndemo <- pws_demoall[pws_demoall$PWSID %in% multicn.pwsids,] %>% 
  filter(!PWSID %in% checkna$PWSID)

perccols <- c("perc_white_only", "perc_white_nohisp", "perc_black_only", "perc_black_nohisp", "perc_hisp_any", 
              "perc_am.ind_any", "perc_unemp", "perc_ag_ind", "perc_pov_fam",
              "perc_pov_ppl", "perc_hs_grad", "perc_poor_eng", "perc_foreign_noncit", "mdi", "perc_uninsur")
numbercols <- c("num_white_only", "num_white_nohisp", "num_black_only", "num_black_nohisp", "num_hisp_any", "num_am.ind_any", 
                "num_unemp", "num_ag_ind", "num_pov_fam","num_pov_ppl", "num_hs_grad", "num_poor_eng", "num_foreign_noncit", "num_mdi", "num_uninsur")


#set number cols blank prior to calculating them
multicndemo[, numbercols] <- ""

#calculate number cols
for(i in 1:length(numbercols)){
  perccol <- perccols[i]
  numcol <- numbercols[i]
  multicndemo[,numcol] <- round(multicndemo$total_pop*(multicndemo[,perccol]/100))
}

#first part of calculating weighted income, where the incomes are aggregated 
#based on their population proportion (uncomment for future analysis involving 
#median/mean income)

# multicndemo <- multicndemo %>%
#   mutate(weight_medinc = med_inc * total_pop,
#          weight_meaninc = mean_inc * total_pop)

#calculate aggregate numbers (commented out were not used for this analysis)
aggcndemo <-
  group_by(multicndemo, PWSID, test_chem, WS.GW_SW_CODE) %>%
  summarise(
    total_pop = sum(total_pop),
    WS.POPULATION_SERVED_COUNT = sum(WS.POPULATION_SERVED_COUNT, na.rm = TRUE),
    num_white_only = sum(num_white_only),
    num_white_nohisp = sum(num_white_nohisp),
    num_black_only = sum(num_black_only),
    num_black_nohisp = sum(num_black_nohisp),
    num_hisp_any = sum(num_hisp_any),
    num_am.ind_any = sum(num_am.ind_any),
    num_unemp = sum(num_unemp),
    num_ag_ind = sum(num_ag_ind),
    num_pov_fam = sum(num_pov_fam),
    num_pov_ppl = sum(num_pov_ppl),
    num_hs_grad = sum(num_hs_grad),
    num_poor_eng = sum(num_poor_eng),
    num_foreign_noncit = sum(num_foreign_noncit),
    num_uninsur = sum(num_uninsur),
    #avg_med_inc = mean(med_inc),
    #weighted_medinc = sum(weight_medinc),
    #weighted_meaninc = sum(mean_inc),
    sum_house14 = sum(all.house14),
    sum_owned = sum(owned.house),
    sum_house = sum(all.house),
    sum_urban = sum(urb.house),
    sum_urban.c = sum(urb.house.c),
    sum_rural = sum(rur.house),
    n.miss.urb = length(which(hasurban == "N")),
    land.area = sum(land.area),
    num_mdi = sum(num_mdi, na.rm = TRUE),
    #fugitive_air = sum(fugitive_air, na.rm = TRUE),
    #stack_air = sum(stack_air, na.rm = TRUE),
    #underground = sum(underground, na.rm = TRUE),
    #x5_landfills = sum(x5_landfills, na.rm = TRUE),
    #on_site_release_total = sum(on_site_release_total, na.rm = TRUE),
    #potw_trns_rlse = sum(potw_trns_rlse, na.rm = TRUE),
    #potw_trns_trt = sum(potw_trns_trt, na.rm = TRUE),
    #potw_total_transfers = sum(potw_total_transfers, na.rm = TRUE),
    #off_site_release_total = sum(off_site_release_total, na.rm = TRUE),
    #off_site_treated_total = sum(off_site_treated_total, na.rm = TRUE),
    #total_transfer = sum(total_transfer, na.rm = TRUE),
    #total_releases = sum(total_releases, na.rm = TRUE),
    #treatment_on_site = sum(treatment_on_site, na.rm = TRUE),
    #treatment_off_site = sum(treatment_off_site, na.rm = TRUE),
    #air_total_release= sum(air_total_release, na.rm = TRUE),
    #total_on_off_site_release = sum(total_on_off_site_release, na.rm = TRUE),
    #total_on_site_release = sum(total_on_site_release, na.rm = TRUE), 
    #total_production_related_waste = sum(total_production_related_waste, na.rm = TRUE),
    #water_total_release = sum(water_total_release, na.rm = TRUE), 
    n_fac = sum(n_fac, na.rm = TRUE), 
    reporting_year = paste(unique(reporting_year[which(!is.na(reporting_year))]), collapse = ""),
    #n_fac_w_release = sum(n_fac_w_release, na.rm = TRUE),
    #n_reporting_year = sum(n_reporting_year, na.rm = TRUE),
    n_WWTP = sum(n_WWTP, na.rm = TRUE),
    WWTP_totalflow_mgd = sum(WWTP_totalflow_mgd, na.rm = TRUE),
    #n.miss.TRI = length(which(hasTRI == "N")),
    n_epastewardship = sum(n_epastewardship, na.rm = TRUE),
    n_airports = sum(n_airports, na.rm = TRUE),
    n_MFTA = sum(n_MFTA, na.rm = TRUE),
    #metroclass = min(metronum, na.rm = TRUE)
  ) %>%
  mutate(
    #calculate proportion urban, whether all points have urban, and home ownership
    hasurban = ifelse(n.miss.urb > 0, "N", "Y"),
    propurban = ifelse(hasurban == "N", NA, sum_urban/sum_house),
    propurban.c = ifelse(hasurban == "N", NA, sum_urban.c/sum_house),
    propurban.weight = ifelse(hasurban == "N", NA, (sum_urban + (sum_urban.c/2))/sum_house),
    proprural = ifelse(hasurban == "N", NA, sum_rural/sum_house),
    perc_hmown = (sum_owned/sum_house14) * 100,
    #n_WWTP_peracre = (n_WWTP/land.area),
    WWTP_totalflow_mgd_peracre = (WWTP_totalflow_mgd/land.area),
    # n_epastewardship_peracre = (n_epastewardship/land.area),
    # n_airports_peracre = (n_airports/land.area),
    # n_MFTA_peracre = (n_MFTA/land.area),
    #assign metro status
    # metrostatus = case_when(metroclass == 1 ~ "Metro",
    #                         metroclass == 2 ~ "Nonmetro, Nonrural",
    #                         TRUE ~ "Rural")
  )


#set perc cols blank prior to calculating them
aggcndemo[, perccols] <- ""

#calculate percentages in the aggregate
for(i in 1:length(numbercols)){
  perccol <- perccols[i]
  numcol <- numbercols[i]
  aggcndemo[,perccol] <- round(((aggcndemo[,numcol]/aggcndemo$total_pop))*100,1)
}

#columns we want to keep in our analysis
keepcols <- c("PWSID","test_chem", perccols, "WS.POPULATION_SERVED_COUNT",
              "perc_hmown", "propurban", "land.area", 
              "total_pop","n_fac", "reporting_year","n_WWTP", 
              "WWTP_totalflow_mgd", "n_epastewardship","n_airports" , 
              "n_MFTA", "WWTP_totalflow_mgd_peracre", "WS.GW_SW_CODE")

#columns that we want to make sure are not empty
checkcols <- c("PWSID", "test_chem", perccols, "WS.POPULATION_SERVED_COUNT", 
               "perc_hmown", "propurban", "land.area", "n_fac", "n_WWTP", 
               "WWTP_totalflow_mgd", "n_epastewardship","n_airports" , "n_MFTA", 
               "WWTP_totalflow_mgd_peracre")

aggcn_merge <- aggcndemo %>%
  select(all_of(keepcols)) %>%
  mutate(reporting_year = case_when(reporting_year == "avg" ~ "avg")) %>%
  left_join(pwsid_fips %>% 
              select(1:14)) %>% 
  unique()

## JML check: spot checking some weighted averages with a different method
JL.test2 <- multicndemo %>% 
  group_by(PWSID, test_chem, WS.GW_SW_CODE) %>%
  summarise(mean.pwhitenohisp = stats::weighted.mean(x = perc_white_nohisp, w = total_pop),
            mean.pblacknohisp = stats::weighted.mean(x = perc_black_nohisp, w = total_pop),
            mean.ppov_fam = stats::weighted.mean(x = perc_pov_fam, w = total_pop)
            ) %>%
  ungroup() # looks good!
##


# RUN SOME CHECKS ---------------------------------------------------------

check <- aggcn_merge %>% 
  group_by(PWSID) %>% 
  count()
stopifnot(check$n == 8)

#check that length of PWSID in aggcn_merge is the same as the number of multicn.pwsids
stopifnot(length(unique(aggcn_merge$PWSID)) == length(unique(multicndemo$PWSID)))

#check that none of our columns are empty
for(i in checkcols){
  stopifnot(length(unique(aggcn_merge$PWSID[which(is.na(aggcn_merge[,i]))])) == 0)
}


################################################################################
#  4. MERGE MULTICN AND SINGLE CN   ####
################################################################################

#this filters out anything with NA in PWSID column
pws_demo <- bind_rows(pws_demoall[which(!(pws_demoall$PWSID %in% multicn.pwsids)),],aggcn_merge)

#some columns are empty because the data does not exist. In this case, we can 
#fill in the row with 0 instead of NA

fixcols <- c("n_fac", "n_WWTP","WWTP_totalflow_mgd", "n_epastewardship",
             "n_airports" , "n_MFTA", "WWTP_totalflow_mgd_peracre")

for(i in fixcols){
  pws_demo[,i] <- ifelse(is.na(pws_demo[,i]), 0, pws_demo[,i])
}



# RUN SOME CHECKS ---------------------------------------------------------

stopifnot(!is.na(pws_demo[,fixcols]))

check <- pws_demo %>% 
  group_by(PWSID, test_chem) %>% 
  count() 
stopifnot(check$n == 1)

for(i in keepcols){
  stopifnot(length(unique(pws_demo$GEO.id2[which(is.na(pws_demo$i))])) == 0)
}

################################################################################
#  1a. LOAD DATA  ####
################################################################################

ucmrsys <- unique(ucmr3_targetsamples$PWSID)

#### load demo data (write out from script 2)
pws_demo <- read.csv("intermediate/pws demo data 2021-06-29.csv") %>%
  mutate(GEO.id2 = case_when(nchar(GEO.id2) == 4 ~ paste0("0", GEO.id2),
                             TRUE ~ as.character(GEO.id2)))

# filter to just UCMR3 systems
ucmr_demo <- pws_demo %>% filter(PWSID %in% ucmrsys)

#### load PWSID and FIPS linker
pwsid_fips <- read_csv("intermediate/all sdwis with demo PWSID to FIPS linker 2021-06-28.csv")

pws_cols <- c("PWSID", "contam.pfas", "Size",  "source_type", "detchem", 
              "hlvlchem", "GEO.id2", "geography")

demo_cols <- c("perc_hisp_any", "mdi", "perc_pov_ppl", "perc_black_nohisp",
               "perc_white_nohisp", "perc_hmown", "propurban", "perc_uninsur")

sources_cols <- c("test_chem","WWTP_ML_km2", "n_epastewardship_bin", "airportMFTA_bin", "n_fac_bin")

keepcols <- c(pws_cols, demo_cols, sources_cols)

# if needed, load SDWA violation data
# sdwa_violations <- read_csv("../Data/SDWA/all violations 2010-2015.csv")


################################################################################
#  1b. RUN SOME INITIAL CHECKS ####
################################################################################

for(i in demo_cols){
  stopifnot(length(unique(pws_demo$PWSID[which(is.na(pws_demo$i))])) == 0)
}

for(i in colnames(ucmr_detcode)){
  stopifnot(length(unique(ucmr_detcode$PWSID[which(is.na(ucmr_detcode[,i]))])) == 0)
}

stopifnot(ucmr_detcode$PWSID %in% ucmr_demo$PWSID)


################################################################################
#  2a. MERGE UCMR DATA WITH DEMOGRAPHICS  ####
################################################################################

# data dictionary: 
# detchem == chemical was ever detected (1)
# hlvlchem == chemical was ever measured above health level guideline (1)

ucmrdf.demo <- unique(ucmr_detcode[,c("PWSID", "contam.pfas", "Size", "source_type")]) %>%
  full_join(ucmr_demo) %>% 
  select(-fugitive_air, -stack_air,-underground,-x5_landfills,-on_site_release_total,
         -potw_trns_rlse,-potw_trns_trt,-potw_total_transfers,-off_site_release_total ,
         -off_site_treated_total,-total_transfer,-total_releases,-treatment_on_site,
         -treatment_off_site) %>% 
  mutate(n_fac_bin = case_when(n_fac > 0 ~ 1,
                               TRUE ~ 0),
         # create WWTP total flow per 1000 acre variable
         WWTP_mgd_1000acre = WWTP_totalflow_mgd/(land.area/1000),
         # convert to million L per km2
         WWTP_ML_km2 = conv_unit(WWTP_mgd_1000acre, "us_gal", "l") / 
           conv_unit(1000, "acre", "km2"),
         n_airports_bin = case_when(n_airports > 0 ~ 1,
                                    TRUE ~ 0), 
         n_epastewardship_bin = case_when(n_epastewardship > 0 ~ 1,
                                          TRUE ~ 0),
         n_MFTA_bin = case_when(n_MFTA > 0 ~ 1, 
                                TRUE ~ 0),
         airportMFTA_bin = case_when(n_airports_bin == 1 | n_MFTA_bin == 1 ~ 1, 
                                     TRUE ~ 0),
         pov_status_co = case_when(perc_pov_ppl > 20 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(sdwis.sys.size = case_when(WS.POPULATION_SERVED_COUNT < 501 ~ "V. Small",
                                    WS.POPULATION_SERVED_COUNT < 3301 ~ "Small",
                                    WS.POPULATION_SERVED_COUNT < 10001 ~ "Medium", 
                                    WS.POPULATION_SERVED_COUNT < 100001 ~ "Large",
                                    WS.POPULATION_SERVED_COUNT > 100000 ~ "V. Large")) 

#merge demo data and test code (one row per system-contam.pfas pairing)
ucmrdf.demo_detcode <- full_join(ucmrdf.demo, ucmr_detcode) %>% 
  ungroup()

### JML check: some systems have NA for sdwis.sys.size; are these dropped in modelling later?
no.sdwis.size <- unique(ucmrdf.demo_detcode$PWSID[is.na(ucmrdf.demo_detcode$sdwis.sys.size)]) # 5 systems
# these do have values in the "Size" column though. substitute these in?
###

################################################################################
#  2b. RUN SOME CHECKS  ####
################################################################################

#how many PWSID  for each ucmr and tri chem
check <- ucmrdf.demo_detcode %>% 
  group_by(contam.pfas, test_chem) %>% 
  summarize(n = length(unique(PWSID)))

table(ucmrdf.demo_detcode$contam.pfas, ucmrdf.demo_detcode$test_chem, useNA = "ifany")
# 1,1-dichloroethane = 4807
# 1.4-dioxane = 4810
# evrdet = 4815
# HCFC-22 = 4811
# PFAS = 4815

stopifnot(!is.na(ucmrdf.demo_detcode$test_chem))

#investigate if needed
# empty_test_chem <- ucmrdf.demo_detcode %>% 
#   filter(is.na(test_chem)) %>% 
#   pull(PWSID) %>% 
#   unique()
# 
# stopifnot(is_empty(empty_test_chem))
#hooray!

# END OF SCRIPT 3 ---------------------------------------------------------
# 
# # WRITE OUT FINAL PRODUCT
# #write_csv(pws_demo, paste0("results/preliminary/pws demo data ", Sys.Date(),".csv"))
# main <- read.csv("main.csv")
# newdata <- main %>% select(PWSID, perc_black_nohisp, perc_hisp_any, mdi_rate)
# 
# colnames(pws_demo)
# olddata <- pws_demo %>% 
#   select(PWSID, perc_hmown, perc_hisp_any, mdi) %>% 
#   distinct() %>%
#   rename('mdi_rate' = 'mdi')
# olddata <- olddata %>% filter(PWSID %in% ucmr3_raw$PWSID) %>% mutate_if(is.numeric, round, 1)
# olddata %>% arrange(-perc_hmown)
# 
# setdiff(newdata$PWSID, olddata$PWSID)
# 
# olddata %>% filter(PWSID == "IA0400900")
