### AUTHOR: AHz, JL
### LAST EDIT: 2021-08-09
### LAST REVIEW: 2021-07-06
### WRITTEN IN: R version 3.5.1
### Purpose: run UCMR3 regression models (detection test (Y/N) and health level 
### exceedence (Y/N))
### UPDATED: 2023-01-27 AM: added number of samples as a regression variable, 
### UPDATED: 2023-03-05 AM: Used glmer() for adjusted models and other edits

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

### !!! START HERE !!! #######

## IF YOU'RE STARTING FROM SCRIPT 4b -- UNCOMMENT AND START HERE

source("1 - UCMR loading and processing.R")
source("Janet functions May 2015_AH.r")
detach_all()
source("4a - UCMR summary.R")
detach_all()
getwd()

## IF YOU'VE ALREADY RUN SCRIPTS 1-4a, UNCOMMENT AND START HERE

# source("Janet functions May 2015_AH.r")
# detach_all()

library(tidyverse)
library(broom)
library(measurements)
library(corrplot)
library(reshape2)
library(viridis)
# library(officer)
# library(flextable)

################################################################################
#  0. README  ###############################################################
################################################################################

### GOAL 
#
#     Run regressions 
#     
### CODE DICTIONARY 
#     #_# Code outputs something that is referenced in paper
#     #~# Code outputs something that is currently in slide deck (often the same
#     as what is in the paper)
#     
### DATA DICTIONARY
# 
#     contam.pfas == column created to assign “PFAS” to any of the 6 PFAS 
#                   contaminants and leaves all other contaminant names the same 
#     detchem == binary column indicates whether chemical was ever detected (1 = detected) 
#     hlvlchem == binary column indicates whether chemical was ever measured above health 
#                 level guideline (1 = exceeded)
#     evrdet == variable in the contam.pfas column. this "chemical" was created to 
#                 represent "any target contaminant"
#                 
#                 
### NOTES
#
# SECTION 1
#
#     Take ucmrdf.clean,  drop unused columns, and then make it wider (one row 
#     per PWSID-contam.pfas pair). The pivot wider will get each TRI chemical as
#      a column and then the value in that column will be a code to indicate 
#      whether a facility was ever reported in the county where the PWS resides 
#      (0 = no, 1 = yes). 
#      
#      We want to manually set the reference level to GW (so the model will 
#      compare GW to SW and GW to MIX). 
#      
# SECTION 2
#     
#     Create local functions: 
#     
#     get_residuals 
#     
#         allows us to pull the numbner of observations in each model
#     
#     crude2df 
#     
#         tidys the crude/univariate regression model and converts the output to a percent change with the p value. 
#     
#     mod2df 
#     
#         tidys the multivariate regression model output.
#     
#     clean_output 
#     
#         tidys up the results so that they are formatted as they are in the paper. 
# 
# 
# SECTION 3: CRUDE REGRESSIONS
#     
#     Create a local function that is our model and use a for loop to loop 
#     through the list of columns and contaminants we want to run in our crude 
#     regression. Ex: we test one column for each contam.pfas before moving 
#     onto the next column.
#     
#     The for loop runs through the list of columns (pasting each one into the 
#     model function for each iteration) and the nest %>% map separates out our 
#     different contaminants (contam.pfas) so that the model is run independently 
#     on each one. We also get the number of observations in each model (nobs via 
#     get_residuals function).
#     
#     Create a list and then turn it into a df with all the results. 
#     
#     "spec1" is a df that I created for formatting purposes. It allows us to 
#     manually set the column order when we pivot wider. 
#     
#     Repeat the same process for hlvlchem (exceedance of a health guideline).
#     
#     Join the detchem crude regression with the hlvlchem crude regression and 
#     write out. 
#     
# SECTION 4: MULTIVARIATE REGRESSIONS
#     
#     We want to run our multivariate model with all 4 poverty indicators, so 
#     we use a for loop to run each model subbing in the different poverty 
#     metrics for each contam.pfas model. 
#     
#     There is a for loop in a for loop (first is for contam.pfas, second is for
#      pov_var). Since each model is slightly different for each contaminant 
#      (i.e. we want to know about PFAS point sources only for PFAS), we use a 
#      bunch of if statements to determine which model to use. Results from each 
#      are stored in a list and tidied before binding it all together and making 
#      it long (by poverty variable).
#      
#      For the hlvl mod we don't necessarily need all the poverty variables 
#      tested, so the model just includes MDI. We also wanted to stratify by 
#      system size, but didn't run it for each poverty variable again. 
#
# SECTION 5: CORRELATION PLOTS
#     
#     Created a correlation plot of all variables used in the regressions for
#     paper
#      
# CODE NOT CHECKED BEYOND HERE
# 
#     Code hasn't been checked beyond this point or has been archived
# 

################################################################################
#  1. LOAD DATA  ####
################################################################################



# demo_tri <- read_csv("results/preliminary/all demo data pre-PWSID match (with all TRI) 2020-09-28.csv")%>% 
#   mutate(GEO.id2 = case_when(nchar(GEO.id2) == 4 ~ paste0("0", GEO.id2),TRUE ~ as.character(GEO.id2)))


keepcols <- c("PWSID", "GEO.id2","contam.pfas","test_chem", "detchem", "hlvlchem",
              "Size","fullstate", "source_type","perc_hisp_any", 
              "propurban", "perc_hmown","land.area","perc_black_nohisp",
              "n_WWTP", "n_epastewardship", "n_epastewardship_bin",
              "n_MFTA", "n_MFTA_bin", "n_airports", "n_airports_bin", "perc_pov_ppl",
              "WWTP_totalflow_mgd", 
              "WWTP_ML_km2", "n_fac_bin", "mdi", "perc_uninsur", "WS.GW_SW_CODE", 
               "airportMFTA_bin", "pov_status_co")

ucmrdf.mod <- ucmrdf.clean %>% 
  select(all_of(keepcols)) %>% 
  group_by(PWSID) %>% 
  mutate(bin_ucmrdet = case_when(sum(detchem) > 1 ~ 1, TRUE ~ 0)) %>% 
  pivot_wider(names_from = test_chem, values_from = n_fac_bin, names_prefix = "bin_") %>% 
  rename(bin_14DIOXANE = `bin_1,4-DIOXANE`,
         bin_CHLORINATED_SOLVENTS = `bin_CHLORINATED SOLVENTS`,
         bin_ETHYLIDENE_DICHLORIDE = `bin_ETHYLIDENE DICHLORIDE`,
         bin_111TRICHLOROETHANE = `bin_1,1,1-TRICHLOROETHANE`,
         bin_TRI = `bin_bin_TRI`) 

# set reference as GW
## 2023-03-16 AM: Changed to SW
## 2023-03-16 AM: Also set Small systems as referent
## ucmrdf.mod$source_type <-  relevel(as.factor(ucmrdf.mod$source_type), ref="GW")
ucmrdf.mod$source_type <-  relevel(as.factor(ucmrdf.mod$source_type), ref="SW")
ucmrdf.mod$Size <- relevel(as.factor(ucmrdf.mod$Size), ref = "S")

# AM: turn propurban into a proportion
ucmrdf.mod <- ucmrdf.mod %>% mutate(propurban = propurban*100)

# AM: add number of samples as a column in ucmrdf.mod
n_samples_dat <- ucmr3_targetsamples %>%
  group_by(PWSID, SamplePointName, CollectionDate) %>% 
  summarise(n = n()) %>% 
  group_by(PWSID) %>%
  summarise(n_samples = n())

ucmrdf.mod <- ucmrdf.mod %>%
  left_join(n_samples_dat, by = c("PWSID"))

# AM: make a new column to indicate state 
ucmrdf.mod <- ucmrdf.mod %>%
  mutate(stateabbr = substr(PWSID, 1, 2)) %>%
  {stopifnot(nrow(filter(., is.na(stateabbr))) == 0); .;}

#write_csv(ucmrdf.mod, paste0("results/output/ucmr data with demo-tri ", Sys.Date(),".csv"))

################################################################################
#  2. WRITE FUNCTIONS  ####
################################################################################


get_residuals <- function(mod){
  length(mod$residuals)
}


crude2df <- function(mod){
  
  #convert to percent change and add p.value
  mod %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    
    # calculate percent change estimates and p-value
    # note conv = 1 when model converges (CIs go to Inf)
    mutate(conv = ifelse(round((exp(estimate + 1.96*std.error)-1)*100, digits = 1)>9999, 1, 0), 
           perc.change = ifelse(conv == 1, NA, exp(estimate)-1)*100,
           lo.ci = ifelse(conv == 1, NA, exp(estimate - 1.96*std.error)-1)*100,
           hi.ci = ifelse(conv == 1, NA, exp(estimate + 1.96*std.error)-1)*100, 
           p.value = ifelse(conv == 1, NA, 
                            case_when(p.value < 0.001 ~ "<0.001", 
                                      TRUE ~ formatC(round(p.value, 2), digit = 2, format = "f")))) %>%
    
    # apply formatting rules
    mutate(perc.change = ifelse(perc.change > 100, 
                                round(perc.change, 0),
                                formatC(round(perc.change, 1), digits = 1, format = "f")), 
           lo.ci = ifelse(lo.ci > 100, 
                          round(lo.ci, 0),
                          formatC(round(lo.ci, 1), digits = 1, format = "f")), 
           hi.ci = ifelse(hi.ci > 100, 
                          round(hi.ci, 0),
                          formatC(round(hi.ci, 1), digits = 1, format = "f"))) %>%
    
    # format Percent change (95% CI) result
    mutate(result = ifelse(is.na(perc.change), as.character(NA), 
                           paste0(perc.change, " (", lo.ci, ", ", hi.ci, ")"))) %>%
    
    # select only result and p-value
    select(-c(estimate:statistic, perc.change:hi.ci, conv))
}


mod2df <- function(mod){
  #convert to percent change and add significance indicator
  mod %>% 
    # calculate percent change estimates and p-value
    # note conv = 1 when model converges (CIs go to Inf)
    mutate(conv = ifelse(round((exp(estimate + 1.96*std.error)-1)*100, digits = 1)>9999, 1, 0), 
           perc.change = ifelse(conv == 1, NA, exp(estimate)-1)*100,
           lo.ci = ifelse(conv == 1, NA, exp(estimate - 1.96*std.error)-1)*100,
           hi.ci = ifelse(conv == 1, NA, exp(estimate + 1.96*std.error)-1)*100, 
           p.value = ifelse(conv == 1, NA, 
                            case_when(p.value < 0.001 ~ "<0.001", 
                                      TRUE ~ formatC(round(p.value, 2), digit = 2, format = "f")))) %>%
    
    # apply formatting rules
    mutate(perc.change = ifelse(perc.change > 100, 
                                round(perc.change, 0),
                                formatC(round(perc.change, 1), digits = 1, format = "f")), 
           lo.ci = ifelse(lo.ci > 100, 
                          round(lo.ci, 0),
                          formatC(round(lo.ci, 1), digits = 1, format = "f")), 
           hi.ci = ifelse(hi.ci > 100, 
                          round(hi.ci, 0),
                          formatC(round(hi.ci, 1), digits = 1, format = "f"))) %>%
    
    # format Percent change (95% CI) result
    mutate(result = ifelse(is.na(perc.change), as.character(NA), 
                           paste0(perc.change, " (", lo.ci, ", ", hi.ci, ")"))) %>%
    
    # select only result and p-value
    select(-c(estimate:statistic, perc.change:hi.ci, conv))
}


clean_output <- function(mod){
  mod %>% 
    mutate(term = case_when(term == "perc_hisp_any" ~ "Percent Hispanic",
                            term == "perc_black_nohisp" ~ "Percent Black, non-Hispanic",
                            term == "perc_pov_ppl" ~ "Percent poverty",
                            term == "perc_hmown" ~  "Percent home ownership",
                            term == "propurban" ~ "Percent urban households",
                            term == "mdi" ~ "Percent deprived",
                            term %in% c("as.factor(Size)L", "SizeL")  ~ "System size: Large (ref: Small)",
                            # term %in% c("as.factor(Size)S", "SizeS")  ~ "System size: Small (ref: Large)",
                            # term %in% c("as.factor(source_type)SW", "source_typeSW")  ~ "Source water: SW (ref: GW)",
                            # term %in% c("as.factor(source_type)MIX", "source_typeMIX") ~ "Source water: MIX (ref: GW)",
                            term %in% c("as.factor(source_type)GW", "source_typeGW")  ~ "Source water: GW (ref: SW)",
                            term %in% c("as.factor(source_type)MIX", "source_typeMIX") ~ "Source water: MIX (ref: SW)",
                            term == "WS.GW_SW_CODEGW" ~ "SDWIS Water Source (GW)",
                            # AM: added term=='sesval' and sesvars to work with pivot
                            # later in code
                            term == "sesval" & ("sesvar" %in% colnames(mod) && sesvar == "mdi") ~ "Percent deprived",
                            term == "sesval" & ("sesvar" %in% colnames(mod) && sesvar == "perc_uninsur") ~ "Percent uninsured",
                            term == "sesval" & ("sesvar" %in% colnames(mod) && sesvar == "perc_hmown") ~ "Percent home ownership",
                            term == "sesval" & ("sesvar" %in% colnames(mod) && sesvar == "perc_pov_ppl") ~ "Percent poverty",
                            term == "perc_uninsur" ~ "Percent uninsured",
                            term == "bin_CHLORINATED_SOLVENTS" ~ "TRI (Chlorinated Solvents) facility present",
                            term == "bin_111TRICHLOROETHANE" ~ "TRI (1,1,1-Trichloroethane) facility present",
                            term == "bin_ETHYLIDENE_DICHLORIDE" ~ "TRI (1,1-Dichloroethane) facility present",
                            term == "bin_CHLORODIFLUOROMETHANE" ~ "TRI (HCFC-22) facility present",
                            term == "bin_14DIOXANE" ~ "TRI (1,4-Dioxane) facility present",
                            term == "bin_CFCs" ~ "TRI (CFCs) facility present",
                            term == "bin_DICHLORODIFLUOROMETHANE" ~ "TRI (CFC-12) facility present",
                            term == "pov_status_co" ~ ">20% of county is below pov line",
                            term == "n_MFTA_bin" ~ "MFTA present",
                            term == "n_MFTA" ~ "Number of MFTAs",
                            term == "n_WWTP" ~ "Number of WWTPs",
                            term == "WWTP_ML_km2" ~ "WWTP flow (million L per km^2)",
                            term == "n_airports_bin" ~ "Airport present",
                            term == "n_airports" ~ "Number of airports",
                            term == "n_epastewardship_bin" ~ "Major PFAS industrial facility present",
                            term == "n_epastewardship" ~ "Number of major industrial facilities present",
                            term == "airportMFTA_bin" ~ "Certified airports (≥1 in county) or Military fire-training areas (≥1 in county)",
                            term == "bin_TRI" ~ "Any TRI facility",
                            term == "bin_IndFac" ~ "Any industrial facility",
                            # AM added
                            term == "n_samples" ~ "Number of samples",
                            term == "nobs" ~ "Number of observations in model",
                            TRUE ~ term))  %>% 
    mutate(term = factor(term, levels = c("Percent Hispanic",
                                          "Percent Black, non-Hispanic",
                                          "Percent deprived", 
                                          "Percent home ownership",
                                          "Percent uninsured",
                                          "Percent poverty",
                                          "Percent urban households",
                                          "System size: Small (ref: Large)",
                                          "System size: Large (ref: Small)",
                                          "Source water: SW (ref: GW)",
                                          "Source water: MIX (ref: GW)",
                                          "Source water: GW (ref: SW)", 
                                          "Source water: MIX (ref: SW)",
                                          "≥1 Industrial facility present",
                                          "Number of samples",
                                          "WWTP flow (million L per km^2)", 
                                          "Any TRI facility",
                                          "TRI (1,4-Dioxane) facility present",
                                          "TRI (Chlorinated Solvents) facility present",
                                          "TRI (CFCs) facility present", 
                                          "Major PFAS industrial facility present",
                                          "Certified airports (≥1 in county) or Military fire-training areas (≥1 in county)")))
}


################################################################################
# 3. CRUDE REGRESSIONS ######
################################################################################

### variables to include in model 
modcols <- c("perc_hisp_any", "perc_black_nohisp", "propurban","mdi", 
             "Size","source_type", "WWTP_ML_km2", "bin_TRI","bin_14DIOXANE", 
             "bin_CFCs",  "bin_CHLORINATED_SOLVENTS", "n_epastewardship_bin",
             "airportMFTA_bin","perc_hmown", "perc_pov_ppl","perc_uninsur", 
             "n_samples")

#_# RUN DETCHEM CRUDE MODEL
detchem_crude_mod <- function(dat, key){
  glm(paste("detchem ~", modcols[[i]]), data = dat, family = binomial())
}

# run all variables through the model one at a time by contaminant
detchem_crude_list <- list()
detchem_crude_all <- data.frame()

for(i in 1:length(modcols)) {
  
  #run model
  detchem_crude_ind <- ucmrdf.mod %>% 
    group_by(contam.pfas) %>%
    nest() %>% 
    mutate(model = purrr::map(data, detchem_crude_mod)) %>%
    mutate(result_modify = purrr::map(model, crude2df)) %>% 
    mutate(nobs = purrr::map(model, get_residuals)) %>% 
    unnest(cols = c(result_modify, nobs)) %>% 
    select(-data, -model)
  
  detchem_crude_list[[i]] <- detchem_crude_ind
  
}

detchem_crude_all <- bind_rows(detchem_crude_list) 

#### JML check ####
# if (!require("jtools")) install.packages("jtools")
# library(jtools) # jtools is nice for quick model views

# chems <- unique(ucmrdf.mod$contam.pfas)
# 
# 
# 
# mods <- lapply(chems,
#                FUN = function(x) {glm(detchem ~ perc_hisp_any,
#                                       family=binomial,
#                                       data = subset(ucmrdf.mod, contam.pfas == x))})
# 
# lapply(mods, jtools::summ)
# 
# 
# mods <- lapply(chems,
#                FUN = function(x) {glm(detchem ~ propurban,
#                                       family=binomial,
#                                       data = subset(ucmrdf.mod, contam.pfas == x))})
# 
# lapply(mods, jtools::summ)

# AM: this has been addressed!
# # one note for 'propurban' is that it's in fraction, not percent, so associations will 
# # be one-hundred times smaller (on log scale) when interpreting as one-percent increase in urbanicity
# 
# # looks correct so far! one note is that "percent changes" in the outcome are percent changes 
# # for the odds of detecting the outcome, not "probability" of detecting the outcome
# # I will include a longer explanation in paper text as odds/probability are slightly different
# # one nitpicky thing I also wonder (depending on how it's defined) is whether the number of obs 
# # for evrdet should be the number of systems that sampled all 4 contaminants 
# # (model currently has 4808 obs when the 1,4-dioxane model includes 4804); this likely won't change any results 


#####

spec1 <- detchem_crude_all %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  build_wider_spec(names_from = contam.pfas, values_from = c(result, p.value, nobs)) %>% 
  arrange(contam.pfas)


detchem_crude_table <- detchem_crude_all %>%  
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  #select(contam.pfas, term, result) %>%
  #pivot_wider(names_from = contam.pfas, values_from = c(result, p.value, nobs)) %>% 
  pivot_wider_spec(spec1) %>% 
  clean_output() %>% 
  arrange(term) %>% 
  arrange(term %in% c("Percent Poverty", "Percent Homeownership", "Percent Uninsured"))


# RUN hlvlchem CRUDE MODEL

hlvlchem_crude_mod <- function(dat, key){
  glm(paste("hlvlchem ~", modcols[[i]]), data = dat, family = binomial())
  
}

# run all variables through the model one at a time by contaminant
hlvlchem_crude_list <- list()
hlvlchem_crude_all <- data.frame()

for(i in 1:length(modcols)) {
  
  #run model
  hlvlchem_crude_ind <- ucmrdf.mod %>% 
    filter(contam.pfas == "evrdet") %>% 
    group_by(contam.pfas) %>%
    nest() %>% 
    mutate(model = purrr::map(data, hlvlchem_crude_mod)) %>%
    mutate(result_modify = purrr::map(model, crude2df)) %>% 
    mutate(nobs = purrr::map(model, get_residuals)) %>% 
    unnest(cols = c(result_modify, nobs)) %>% 
    select(-data, -model)
  
  hlvlchem_crude_list[[i]] <- hlvlchem_crude_ind
  
}

hlvlchem_crude_all <- bind_rows(hlvlchem_crude_list) 

spec2 <- hlvlchem_crude_all %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 target contaminant over health guideline", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  build_wider_spec(names_from = contam.pfas, values_from = c(result, p.value, nobs)) %>% 
  arrange(contam.pfas)


hlvlchem_crude_table <- hlvlchem_crude_all %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 target contaminant over health guideline", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  #select(contam.pfas, term, result) %>%
  #pivot_wider(names_from = contam.pfas, values_from = c(result, p.value, nobs)) %>% 
  pivot_wider_spec(spec2) %>% 
  clean_output() %>% 
  arrange(term) %>% 
  arrange(term %in% c("Percent Poverty", "Percent Homeownership", "Percent Uninsured"))

#### JML check ####
# if (!require("jtools")) install.packages("jtools")
# library(jtools) # jtools is nice for quick model views
# for (i in 1:length(modcols)) {
#   print( jtools::summ(
#                       glm(paste("hlvlchem ~", modcols[i]), 
#                           data = subset(ucmrdf.mod, contam.pfas == "evrdet"), 
#                           family = binomial) 
#                      )
#   )
# }

# AM: this has been addressed!
# one note for 'propurban' is that it's in fraction, not percent, so associations will
# be one-hundred times smaller *on log-odds scale) when interpreting as one-percent increase in urbanicity
# they will NOT be equal to percent change/100 (due to logit transformation)
# looks correct so far! one note is that "percent changes" in the outcome are percent changes
# for the odds of detecting the outcome, not "probability" of detecting the outcome
# I will include a longer explanation in paper text as odds/probability are slightly different
# one nitpicky thing I also wonder (depending on how it's defined) is whether the number of obs
# for evrdet should be the number of systems that sampled all 4 contaminants
# (model currently has 4808 obs when the 1,4-dioxane model includes 4804); this likely won't change any results

crude_table <- left_join(detchem_crude_table, hlvlchem_crude_table)

# write_csv(crude_table,
#           paste0("results/output/all crude regressions_",
#                  Sys.Date(), ".csv"))

################################################################################
# 4. MULTIVARIATE REGRESSIONS ######
################################################################################

#_# run the multivariate models for each contam.pfas and each poverty variable

# 2023-01-27 AM: added n_samples
# 2023-03-02 AM: added random state intercepts; used lme4::glmer()
# 2023-03-02 AM: rescaled continuous vars to address some warning messages
# slightly more efficient code below here; archived previous code (see below)
# ---------------------------------------------------------------------------- #
# re-process ucmrdf.mod df so that each PWSID is each row, with five columns as 
# outcomes: detected any, detected each target contam, and exceeded a health
# reference level (hlvlchem)

# after using 'pivot_wider' PWSIDs that were hlvlchem == 1 were duplicated
# because previous processing code used 1,4-dioxane, PFAS, and 1,1-DCA columns.
# separated df, and re-processed
process0 <- ucmrdf.mod %>%
  pivot_wider(names_from = "contam.pfas", 
              values_from = "detchem", 
              names_prefix = "contam_") %>% 
  group_by(PWSID) %>%
  mutate(ncount = n())

# table(process0$hlvlchem, process0$ncount)

# keep PWSIDs that were not duplicated (all hlvlchem == 0)
process1 <- process0 %>%
  filter(ncount == 1)

# reprocess duplicated PWSIDs (all hlvlchem == 1)
# couldn't figure out how to work with coalesce() function, so used group_by
# and summarise() instead; note that hlvlchem = 1 was reinserted manually
process2 <- process0 %>%
  filter(ncount == 2) %>% 
  group_by_at(vars(-hlvlchem, -starts_with("contam_"))) %>%
  summarise(across(starts_with("contam_"), sum, na.rm = T)) %>%
  mutate(hlvlchem = 1) %>% 
  ungroup()

# bind processed columns
processed <- bind_rows(process1, process2) %>% select(-ncount)

# used new processed df to run glmer() models
## this takes a few minutes to run 
## there were 39 warnings(); about half of them were suggesting to rescale variables
## some of these warnings were expected; may need to discuss with LAS or JL

adjglmermods <- processed %>%
  pivot_longer(cols = c(hlvlchem, starts_with("contam_")),
               names_to = "contam.pfas", 
               values_to = "detchem") %>%
  mutate(contam.pfas = str_remove(contam.pfas, "contam_")) %>%
  pivot_longer(cols = c("mdi", "perc_hmown", "perc_pov_ppl", "perc_uninsur"), 
               names_to = "sesvar", 
               values_to = "sesval") %>%
  group_by(contam.pfas, sesvar) %>%
  nest() %>% # there are 24 combinations of ses metrics and outcomes (4*6)
  mutate(base = paste("detchem ~ perc_hisp_any + perc_black_nohisp + 
                                           propurban + as.factor(Size) + 
                                           as.factor(source_type) + 
                                           WWTP_ML_km2 + n_samples + sesval + 
                             (1|stateabbr)")) %>%
  mutate(specformula = case_when(str_detect(contam.pfas,"evrdet|hlvlchem") ~ paste(base, "+ bin_TRI"), 
                                 contam.pfas == "1,4-dioxane" ~ paste(base, "+ `bin_14DIOXANE`"), 
                                 contam.pfas == "1,1-dichloroethane" ~ paste(base, "+ `bin_CHLORINATED_SOLVENTS`"), 
                                 contam.pfas == "HCFC-22" ~ paste(base, "+ bin_CFCs"), 
                                 contam.pfas == "PFAS" ~ paste(base, "+  airportMFTA_bin + n_epastewardship_bin"), 
                                 TRUE ~ "999")) %>%
  {stopifnot(nrow(filter(., specformula == "999"))==0); .;} %>% # just a check
  mutate(glmer1 = map(data, ~lme4::glmer(specformula, 
                                         data = ., 
                                         family = binomial))) %>% 
  mutate(tidyglmer1 = map(glmer1, broom.mixed::tidy)) %>%
  mutate(tidyglmer2 = map(tidyglmer1, mod2df)) %>%
  select(-data, -glmer1, -tidyglmer1, -specformula) %>%
  unnest(tidyglmer2)

# there are warnings associated with clean_output() but all are ignorable
## AM: this could mean that the case_when in clean_output() could be further modified
cladjglmermods <- adjglmermods %>%
  select(-effect, -group, -base) %>%
  filter(!str_detect(term, "Intercept")) %>%
  clean_output() %>% 
  mutate(contam.pfas = factor(contam.pfas, 
                              levels = c("evrdet",  "hlvlchem", 
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"),
                              labels = c(">=1 UCMR Detection",  ">=1 over a health guideline",
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"), 
                              ordered = TRUE)) %>%
  pivot_wider(names_from = contam.pfas, values_from = c(result, p.value))

## 2023-03-26: adj regression without pollution source terms (save WWTP)

adjglmermods_nosources <- processed %>%
  pivot_longer(cols = c(hlvlchem, starts_with("contam_")),
               names_to = "contam.pfas", 
               values_to = "detchem") %>%
  mutate(contam.pfas = str_remove(contam.pfas, "contam_")) %>%
  pivot_longer(cols = c("mdi", "perc_hmown", "perc_pov_ppl", "perc_uninsur"), 
               names_to = "sesvar", 
               values_to = "sesval") %>%
  group_by(contam.pfas, sesvar) %>%
  nest() %>% # there are 24 combinations of ses metrics and outcomes (4*6)
  mutate(base = paste("detchem ~ perc_hisp_any + perc_black_nohisp + 
                                           propurban + as.factor(Size) + 
                                           as.factor(source_type) + 
                                           WWTP_ML_km2 + n_samples + sesval + 
                             (1|stateabbr)")) %>%
  mutate(specformula = base) %>%
  {stopifnot(nrow(filter(., specformula == "999"))==0); .;} %>% # just a check
  mutate(glmer1 = map(data, ~lme4::glmer(specformula, 
                                         data = ., 
                                         family = binomial))) %>% 
  mutate(tidyglmer1 = map(glmer1, broom.mixed::tidy)) %>%
  mutate(tidyglmer2 = map(tidyglmer1, mod2df)) %>%
  select(-data, -glmer1, -tidyglmer1, -specformula) %>%
  unnest(tidyglmer2)

cladjglmermods_nosources <- adjglmermods_nosources %>%
  select(-effect, -group, -base) %>%
  filter(!str_detect(term, "Intercept")) %>%
  clean_output() %>% 
  mutate(contam.pfas = factor(contam.pfas, 
                              levels = c("evrdet",  "hlvlchem", 
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"),
                              labels = c(">=1 UCMR Detection",  ">=1 over a health guideline",
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"), 
                              ordered = TRUE)) %>%
  pivot_wider(names_from = contam.pfas, values_from = c(result, p.value))

## Use new processed df to run glmer() models AFTER rescaled variables

# rescale continuous variables (AM: is this sufficient? Binary vars?)
contvars <- c("perc_hisp_any", "propurban", "perc_hmown", 
              "perc_black_nohisp", "perc_pov_ppl", "WWTP_ML_km2", 
              "mdi", "perc_uninsur", "n_samples")
processed_scaled <- processed %>% ungroup() %>% mutate_at(contvars, scale)

## this takes a few minutes to run 
## Fewer than 39 warnings now - only 16 warnings
adjglmermods_scaled <- processed_scaled %>%
  pivot_longer(cols = c(hlvlchem, starts_with("contam_")),
               names_to = "contam.pfas", 
               values_to = "detchem") %>%
  mutate(contam.pfas = str_remove(contam.pfas, "contam_")) %>%
  pivot_longer(cols = c("mdi", "perc_hmown", "perc_pov_ppl", "perc_uninsur"), 
               names_to = "sesvar", 
               values_to = "sesval") %>%
  group_by(contam.pfas, sesvar) %>%
  nest() %>%
  mutate(base = paste("detchem ~ perc_hisp_any + perc_black_nohisp + 
                                           propurban + as.factor(Size) + 
                                           as.factor(source_type) + 
                                           WWTP_ML_km2 + n_samples + sesval + 
                             (1|stateabbr)")) %>%
  mutate(specformula = case_when(str_detect(contam.pfas,"evrdet|hlvlchem") ~ paste(base, "+ bin_TRI"), 
                                 contam.pfas == "1,4-dioxane" ~ paste(base, "+ `bin_14DIOXANE`"), 
                                 contam.pfas == "1,1-dichloroethane" ~ paste(base, "+ `bin_CHLORINATED_SOLVENTS`"), 
                                 contam.pfas == "HCFC-22" ~ paste(base, "+ bin_CFCs"), 
                                 contam.pfas == "PFAS" ~ paste(base, "+  airportMFTA_bin + n_epastewardship_bin"), 
                                 TRUE ~ "999")) %>%
  {stopifnot(nrow(filter(., specformula == "999"))==0); .;} %>%
  mutate(glmer1 = map(data, ~lme4::glmer(specformula, 
                                         data = ., 
                                         family = binomial))) %>%
  mutate(tidyglmer1 = map(glmer1, broom.mixed::tidy)) %>%
  mutate(tidyglmer2 = map(tidyglmer1, mod2df)) %>%
  select(-data, -glmer1, -tidyglmer1, -specformula) %>%
  unnest(tidyglmer2)

cladjglmermods_scaled <- adjglmermods_scaled %>%
  select(-effect, -group) %>%
  filter(!str_detect(term, "Intercept")) %>%
  clean_output() %>% 
  # clean_output() missed SES values because of the pivoting I did. Doing it by hand:
  mutate(term = case_when(is.na(term) & sesvar == "mdi" ~ "Percent Deprived", 
                          is.na(term) & sesvar == "perc_pov_ppl" ~ "Percent Poverty", 
                          is.na(term) & sesvar == "perc_uninsur" ~ "Percent Uninsured", 
                          is.na(term) & sesvar == "perc_hmown" ~ "Percent Homeownership", 
                          TRUE ~ as.character(term)))  %>% 
  mutate(contam.pfas = factor(contam.pfas, 
                              levels = c("evrdet",  "hlvlchem", 
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"),
                              labels = c(">=1 UCMR Detection",  ">=1 over a health guideline",
                                         "1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS"), 
                              ordered = TRUE)) %>%
  pivot_wider(names_from = contam.pfas, values_from = c(result, p.value))

## Re-scaling continuous variables led to BIG estimate changes for many cont variables,
## but not all. %Hispanic, propurban, n_samples were most affected. %NHBlack is 
## sometimes affected. Need to review whether or not re-scaling is necessary, and
## the difference in interpretations.

### 
# Stratified by size
## 

strat.adjglmermods <- processed %>%
  pivot_longer(cols = c(hlvlchem, starts_with("contam_")),
               names_to = "contam.pfas", 
               values_to = "detchem") %>%
  mutate(contam.pfas = str_remove(contam.pfas, "contam_")) %>%
  pivot_longer(cols = c("mdi", "perc_hmown", "perc_pov_ppl", "perc_uninsur"), 
               names_to = "sesvar", 
               values_to = "sesval") %>%
  # group by outcome, ses variable, and system size
  group_by(contam.pfas, sesvar, Size) %>%
  # only interested in any detection as an outcome in the stratified model
  filter(contam.pfas == "evrdet") %>%
  nest() %>%
  mutate(base = paste("detchem ~ perc_hisp_any + perc_black_nohisp + 
                                           propurban + 
                                           as.factor(source_type) + 
                                           WWTP_ML_km2 + n_samples + sesval + 
                             (1|stateabbr)")) %>%
  mutate(specformula = case_when(str_detect(contam.pfas,"evrdet|hlvlchem") ~ paste(base, "+ bin_TRI"), 
                                 contam.pfas == "1,4-dioxane" ~ paste(base, "+ `bin_14DIOXANE`"), 
                                 contam.pfas == "1,1-dichloroethane" ~ paste(base, "+ `bin_CHLORINATED_SOLVENTS`"), 
                                 contam.pfas == "HCFC-22" ~ paste(base, "+ bin_CFCs"), 
                                 contam.pfas == "PFAS" ~ paste(base, "+  airportMFTA_bin + n_epastewardship_bin"), 
                                 TRUE ~ "999")) %>%
  {stopifnot(nrow(filter(., specformula == "999"))==0); .;} %>%
  mutate(glmer1 = map(data, ~lme4::glmer(specformula, 
                                         data = ., 
                                         family = binomial))) %>% 
  mutate(tidyglmer1 = map(glmer1, broom.mixed::tidy)) %>%
  mutate(tidyglmer2 = map(tidyglmer1, mod2df)) %>%
  select(-data, -glmer1, -tidyglmer1, -specformula) %>%
  unnest(tidyglmer2)

cladjglmermods_strat <- strat.adjglmermods %>%
  select(-effect, -group, -base) %>%
  filter(!str_detect(term, "Intercept")) %>%
  clean_output() %>% 
  filter(sesvar == "mdi") %>%
  pivot_wider(names_from = Size, values_from = c(result, p.value))

# ---------------------------------------------------------------------------- #

################################################################################
# 5. CORR. PLOTS ######
################################################################################

###### Correlations ######

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

colorders <- c(
  "Percent Hispanic",
  "Percent Black, non-Hispanic",
  "Percent deprived",
  "Percent home ownership", 
  "Percent uninsured",
  "Percent poverty",
  ">20% of county is below pov line",
  "Percent urban households",
  "System size",
  "Source water: GW", 
  "Source water: SW",
  "Source water: MIX",
  "Number of samples",
  "WWTP total flow per area",
  "Any TRI facility",
  "TRI 1,4-Dioxane",
  "TRI CFCs",
  "TRI CFC-12",
  "TRI HCFC-22",
  "TRI chlorinated solvents", 
  "TRI 1,1,1-trichloroethane",
  "TRI 1,1-dichloroethane",
  "MFTA present",
  "Airport present",
  "Major industrial facility present")

# AM: added n_samples, source water types, Size, etc.
ucmrdf.test_evrdet_corr <- ucmrdf.mod[ucmrdf.mod$contam.pfas=="evrdet",] %>%
  # filter(!PWSID %in% empty_test_chem) %>% 
  select("PWSID", "contam.pfas", "perc_hisp_any", "propurban", "mdi", "perc_uninsur", "perc_pov_ppl", 
         "pov_status_co",
         "perc_hmown","perc_black_nohisp", "WWTP_ML_km2","bin_14DIOXANE", 
         "bin_CHLORINATED_SOLVENTS", "bin_111TRICHLOROETHANE", "bin_ETHYLIDENE_DICHLORIDE",
         "bin_CHLORODIFLUOROMETHANE", "bin_DICHLORODIFLUOROMETHANE", "bin_CFCs",  "bin_TRI", "n_epastewardship_bin", 
         "n_MFTA_bin", "n_airports_bin","WWTP_ML_km2", "Size", 
         # AM added
         "n_samples", 
         "Size", 
         "source_type") %>% 
  mutate(WWTP_ML_km2 = case_when(is.na(WWTP_ML_km2) ~ 0,
                                 TRUE ~ WWTP_ML_km2), 
         Size = case_when(Size == "L" ~ 1, TRUE ~ 0), 
         GW_bi = ifelse(source_type=="GW", 1, 0), 
         SW_bi = ifelse(source_type=="SW", 1, 0), 
         MIX_bi = ifelse(source_type=="MIX", 1, 0)) %>% 
  unique() %>% 
  rename("Percent Hispanic" = "perc_hisp_any",
         "Percent Black, non-Hispanic" = "perc_black_nohisp",
         "Percent urban households" = "propurban",
         "Percent home ownership" = "perc_hmown",
         "Percent poverty" = "perc_pov_ppl",
         "Percent deprived" = "mdi", 
         "Percent uninsured" = "perc_uninsur", 
         "Any TRI facility" = bin_TRI,
         "TRI chlorinated solvents" = bin_CHLORINATED_SOLVENTS,
         "TRI 1,1,1-trichloroethane" = "bin_111TRICHLOROETHANE",
         "TRI 1,1-dichloroethane" = "bin_ETHYLIDENE_DICHLORIDE",
         "TRI HCFC-22" = bin_CHLORODIFLUOROMETHANE,
         "TRI 1,4-Dioxane" = bin_14DIOXANE,
         "TRI CFCs" = bin_CFCs,
         "TRI CFC-12" = bin_DICHLORODIFLUOROMETHANE,
         ">20% of county is below pov line" = pov_status_co,
         "MFTA present" = n_MFTA_bin,
         "Airport present" = n_airports_bin,
         "WWTP total flow per area" = WWTP_ML_km2,
         "Major industrial facility present" = n_epastewardship_bin, 
         # AM added
         "Number of samples" = n_samples, 
         "System size" = Size, 
         "Source water: GW" = GW_bi, 
         "Source water: SW" = SW_bi, 
         "Source water: MIX" = MIX_bi) %>%
  ungroup() %>%
  select(all_of(colorders))

cor.matrix_evrdet <- cor(ucmrdf.test_evrdet_corr, method = "spearman",
                         use = "complete.obs")

p.mat <- cor.mtest(ucmrdf.test_evrdet_corr)

# corrplot::corrplot(cor.matrix_evrdet,method = 'color',  type = 'upper', order = 'hclust', 
#                    taddCoef.col = 'black', tl.col = 'black', tl.srt = 45, 
#                    p.mat = p.mat, sig.level = 0.01, insig = 'blank', diag = FALSE)
# 
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# 
# corrplot::corrplot(cor.matrix_evrdet, method="color", col=col(200),  
#                    type="upper", order="hclust", 
#                    addCoef.col = "black", # Add coefficient of correlation
#                    tl.col="black", tl.srt=45, #Text label color and rotation
#                    # Combine with significance
#                    p.mat = p.mat, sig.level = 0.05, insig = "blank", 
#                    # hide correlation coefficient on the principal diagonal
#                    diag=FALSE 
# )

library(reshape2)

Figure1corr <- melt(get_upper_tri(cor.matrix_evrdet)) %>%
  left_join(., melt(p.mat) %>% rename(p.value = value), by = c("Var1", "Var2")) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=Var1, y=fct_rev(Var2), fill=value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 2))) + 
  # geom_text(aes(label = ifelse(p.value < 0.001, paste0(round(value,2), "***"), 
  #                              ifelse(p.value < 0.01, paste0(round(value, 2), "**"), 
  #                                     ifelse(p.value < 0.05, paste0(round(value, 2), "*"), 
  #                                            paste0(round(value, 2))))))) + 
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
                       low = "#29af7f", high =  "#b8de29", mid = "white", 
                       name = "Cor value") + 
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        #axis.text.y = element_text(size=11),
        axis.text.x = element_text(angle = 90), 
        axis.title.x = element_text(size=14),
        #axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("")

#ggsave("results/output/all vars spearman corr plot.png", width = 25, height = 12.5, units = "in")

# ## subset for write up
# ucmrdf.test_evrdet_corr_subset <- ucmrdf.mod[ucmrdf.mod$contam.pfas=="evrdet",] %>%
#   filter(!PWSID %in% empty_test_chem) %>% 
#   select("PWSID", "contam.pfas", "perc_hisp_any", "propurban", "mdi",
#          "perc_black_nohisp", "WWTP_ML_km2","bin_14DIOXANE", 
#          "bin_CHLORINATED_SOLVENTS", "bin_CFCs",  "bin_TRI", "n_epastewardship_bin", 
#          "airportMFTA_bin","WWTP_ML_km2", "Size") %>%
#   mutate(WWTP_ML_km2 = case_when(is.na(WWTP_ML_km2) ~ 0,
#                                  TRUE ~ WWTP_ML_km2),
#          Size = case_when(Size == "L" ~ 2,
#                           Size == "S" ~ 1)) %>% 
#   unique() %>% 
#   rename("Percent \nHispanic" = "perc_hisp_any",
#          "Percent \nBlack" = "perc_black_nohisp",
#          "Percent \nUrban" = "propurban",
#          "MDI Rate" = "mdi", 
#          "TRI facility \n(any)" = bin_TRI,
#          "TRI Chlorinated \nSolvents" = bin_CHLORINATED_SOLVENTS,
#          "TRI 1,4-Dioxane" = bin_14DIOXANE,
#          "TRI CFCs" = bin_CFCs,
#          "Airport or MFTA \npresent" =airportMFTA_bin,
#          "Major industrial \nfacility present" = n_epastewardship_bin)
# 
# cor.matrix_evrdet_subset <- cor(ucmrdf.test_evrdet_corr_subset[,3:14], method = "spearman",
#                                 use = "complete.obs")
# 
# melt(get_upper_tri(cor.matrix_evrdet_subset)) %>%
#   ggplot(aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile() + 
#   geom_text(aes(label = round(value,2))) + 
#   scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
#                        low = "#29af7f", high =  "#b8de29", mid = "white", 
#                        name = "Cor value") + 
#   scale_x_discrete(position = "top") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size=12),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         legend.text = element_text(size=12)) +
#   xlab("")+
#   ylab("")


#ggsave("results/output/small vars spearman corr plot.png", width = 15, height = 7.5, units = "in")


### corr with all systems


### corr with all tri

demo_tri_corr <- demo_tri %>%
  mutate(n_WWTP = case_when(is.na(n_WWTP) ~ 0,
                            TRUE ~ n_WWTP),
         # create WWTP total flow per 1000 acre variable
         WWTP_mgd_1000acre = WWTP_totalflow_mgd/(land.area/1000),
         # convert to million L per km2
         WWTP_ML_km2 = conv_unit(WWTP_mgd_1000acre, "us_gal", "l") / 
           conv_unit(1000, "acre", "km2")) %>% 
  select("perc_hisp_any", "propurban", 
         "perc_hmown","perc_black_nohisp", "perc_pov_ppl", "mdi","WWTP_ML_km2",
         "n_fac", "n_chems", ) %>%
  mutate(WWTP_ML_km2 = case_when(is.na(WWTP_ML_km2) ~ 0,
                                 TRUE ~ WWTP_ML_km2)) %>% 
  unique() %>% 
  rename("Percent Hispanic" = "perc_hisp_any",
         "Percent Black" = "perc_black_nohisp",
         "Percent Urban" = "propurban",
         "Percent \nHomeownership" = "perc_hmown",
         "Percent Poverty" = "perc_pov_ppl",
         "MDI Rate" = "mdi")

cor.matrix_demo_tri <- cor(demo_tri_corr, method = "spearman",
                           use = "complete.obs") 

melt(get_upper_tri(cor.matrix_demo_tri)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value,2))) + 
  #scale_fill_binned(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), type = "viridis") + 
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
                       low = "#29af7f", high =  "#b8de29", mid = "white",
                       name = "Cor value") + 
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("")

#ggsave("results/output/all county TRI spearman corr plot.png", width = 15, height = 10, units = "in")

# CODE NOT CHECKED BEYOND HERE --------------------------------------------

stop("code review ended here 2021-07-06")

############### ARCHIVED BY AM ON 2023-03-12 ###################################

crude2df <- function(mod){
  
  #convert to percent change and add p.value
  mod %>%
    tidy() %>% 
    filter(term != "(Intercept)") %>% 
    mutate(conv = ifelse(round((exp(estimate + 1.96*std.error)-1)*100, digits = 1)>9999, 1, 0), 
           perc.change = round((exp(estimate)-1)*100, digits = 1),
           lo.ci = ifelse(conv == 1, NA, round((exp(estimate - 1.96*std.error)-1)*100, digits = 1)),
           hi.ci = ifelse(conv == 1, NA, round((exp(estimate + 1.96*std.error)-1)*100, digits = 1)), 
           result = ifelse(conv == 1, NA, 
                           paste0(ifelse(perc.change > 100, round(perc.change, 0), format(round(perc.change, 1), nsmall = 1)), " (",
                                  ifelse(lo.ci > 100, round(lo.ci, 0), round(lo.ci, 2)), ", ", 
                                  ifelse(hi.ci > 100, round(hi.ci, 0), round(hi.ci, 2)), ")")),
           p.value = ifelse(conv == 1, NA, 
                            case_when(p.value < 0.001 ~ paste0("<0.001"), 
                                      TRUE ~ paste0(format(round(p.value, 2), nsmall = 2))))) %>%
    select(-c(estimate:statistic, perc.change:hi.ci, conv))
}

############### ARCHIVED BY AM ON 2023-03-05 ###################################

# pov_var <- c("mdi", "perc_pov_ppl", "perc_uninsur", "perc_hmown")
# 
# model_fun <- list()
# 
# result <- list()
# multi_var_result_list <- list()
# 
# residuals <- list()
# multi_var_residuals_list <- list()

# for(contam in unique(ucmrdf.mod2$contam.pfas)){
#   
#   det_mod_df <- ucmrdf.mod2 %>%
#     filter(contam.pfas == contam)
#   
#   for(i in pov_var){
#     
#     if(contam == "evrdet"){
#       
#       model_fun[[i]] <- paste0("detchem ~ perc_hisp_any + perc_black_nohisp+propurban+", 
#                                i, "+as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2 + 
#                                n_samples + (1|stateabbr)")
#       
#       #result[[i]] <- glm(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       result[[i]] <- lme4::glmer(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       
#       #residuals[[i]] <- length(result[[i]]$residuals)
#       residuals[[i]] <- length(residuals(result[[i]]))
#       
#     }
#     if(contam == "1,4-dioxane"){
#       model_fun[[i]] <- paste0("detchem ~ perc_hisp_any + perc_black_nohisp+propurban+",i,
#               "+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2 + `bin_14DIOXANE` + 
#               n_samples + (1|stateabbr)")
#       #result[[i]] <- glm(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       result[[i]] <- lme4::glmer(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#      #residuals[[i]] <- length(result[[i]]$residuals)
#       residuals[[i]] <- length(residuals(result[[i]]))
#       
#     }
#     if(contam == "1,1-dichloroethane"){
#       model_fun[[i]] <- paste0("detchem ~ perc_hisp_any + perc_black_nohisp+propurban+",i,
#              "+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ `bin_CHLORINATED_SOLVENTS` + 
#              n_samples + (1|stateabbr)")
#       # result[[i]] <- glm(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       result[[i]] <- lme4::glmer(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       #residuals[[i]] <- length(result[[i]]$residuals)
#       residuals[[i]] <- length(residuals(result[[i]]))
#       
#     }
#     if(contam == "HCFC-22"){
#       model_fun[[i]] <- paste0("detchem ~ perc_hisp_any + perc_black_nohisp+propurban+", i,
#              "+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ bin_CFCs + 
#              n_samples + (1|stateabbr)")
#       #result[[i]] <- glm(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       result[[i]] <- lme4::glmer(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       #residuals[[i]] <- length(result[[i]]$residuals)
#       residuals[[i]] <- length(residuals(result[[i]]))
#       
#     }
#     if(contam == "PFAS"){
#       model_fun[[i]] <- paste0("detchem ~ perc_hisp_any + perc_black_nohisp+propurban+",i,
#              "+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2  +  airportMFTA_bin + n_epastewardship_bin + 
#              n_samples + (1|stateabbr)")
#       #result[[i]] <- glm(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       result[[i]] <- lme4::glmer(as.formula(model_fun[[i]]), data = det_mod_df, family = binomial())
#       #residuals[[i]] <- length(result[[i]]$residuals)
#       residuals[[i]] <- length(residuals(result[[i]]))
#       
#     }
#     
#     
#   }
#   
#   multi_var_result_list[[contam]] <-  purrr::map(result, broom.mixed::tidy) %>% 
#     purrr::map(., mod2df) 
#   
#   multi_var_residuals_list[[contam]] <- residuals
# 
#   
# }
# 
# 
# multi_residuals_df <- bind_rows(multi_var_residuals_list, .id = "id") %>% 
#   pivot_longer(names_to = "pov_var_model", values_to = "nobs", mdi:perc_hmown)
# 
# 
# unnested_list <- list()
# for(i in names(multi_var_result_list)){
#   unnested_list[[i]] <- bind_rows(multi_var_result_list[[i]], .id = "pov_var_model")
# }
# 
# 
# 
# unnested_df <- bind_rows(unnested_list, .id = "contam.pfas") %>% 
#   filter(term != "(Intercept)") %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane",
#                                                       "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c(">=1 UCMR Detection", "1,4-dioxane",
#                                          "1,1-dichloroethane", "HCFC-22",
#                                          "PFAS"))) %>%
#   clean_output() %>% 
#   arrange(pov_var_model, contam.pfas, term)
# 
# 
# # write_csv(unnested_df,
# #           paste0("results/output/all contaminants demo and sources regression model_withpvalue_",
# #           Sys.Date(), ".csv"))
# 
# spec3 <- unnested_df %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c(">=1 UCMR Detection", "1,4-dioxane", 
#                                                       "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
#                               labels = c(">=1 UCMR Detection", "1,4-dioxane",
#                                          "1,1-dichloroethane", "HCFC-22", 
#                                          "PFAS", ">=1 over a health guideline"))) %>% 
#   build_wider_spec(names_from = contam.pfas, values_from = c(result, p.value)) %>% 
#   arrange(contam.pfas)
# 
# table5_w_allpov <- unnested_df %>% 
#   pivot_wider_spec(spec3)
# 
# # write_csv(table5_w_allpov,
# #           paste0("results/output/Table5_w_allpov_",
# #           Sys.Date(), ".csv"))
# 
# 
# ### JML check: these looks similar to above, so I'm doing a few coefficient spot checks
#   # mod.covs.test <- glm("detchem ~ perc_hisp_any + perc_black_nohisp + propurban + mdi + as.factor(Size) + as.factor(source_type) + WWTP_ML_km2  +  airportMFTA_bin + n_epastewardship_bin",
#   #                      data = subset(ucmrdf.mod, contam.pfas == "PFAS"), family = binomial)
#   # 
#   # summary(mod.covs.test)
#   # mod.covs.test <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+perc_pov_ppl+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2 + bin_14DIOXANE,
#   #                      data = subset(ucmrdf.mod, contam.pfas == "1,4-dioxane"), family = binomial)
#   # 
#   # summary(mod.covs.test)
# ### looks correct (along with comments from crude, bivariate regressions)
#   
# model_evrdet_size <- function(dat) {
#   (glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+
#              as.factor(source_type)+ bin_TRI + WWTP_ML_km2 + n_samples,
#            data = dat, family = binomial()))
# }
# 
# model_hlvl <- function(dat){
#   glm(hlvlchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+
#         as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2 + n_samples,
#       data = dat, family = binomial())
# }
# 
# 
# ## Health level exceedences 
# hlvl_mod_df <- ucmrdf.mod %>%
#   filter(contam.pfas == "evrdet") 
# 
# hlvl_mod <- model_hlvl(hlvl_mod_df)
# 
# hlvl_mod_result <- tidy(hlvl_mod) %>% 
#   filter(term != "(Intercept)") %>% 
#   mod2df() %>% 
#   clean_output() %>% 
#   bind_rows(data.frame(n = length(hlvl_mod$residuals), term = "nobs"))
# 
# colnames(hlvl_mod_df)
# 
# hlvl_mods_otherses <- hlvl_mod_df %>% 
#   pivot_longer(cols = c(perc_pov_ppl, perc_uninsur, perc_hmown), 
#                names_to = "ses_var", 
#                values_to = "ses_value") %>% 
#   group_by(ses_var) %>%
#   nest() %>%
#   mutate(mod = map(data, ~glm(hlvlchem ~ perc_hisp_any + perc_black_nohisp+propurban+ ses_value+
#                                 as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2 + n_samples, 
#                               data = ., 
#                               family = 'binomial')), 
#          vif = map(mod, car::vif)) %>%
#   mutate(tidymod = map(mod, tidy)) %>%
#   unnest(tidymod) %>%
#   select(-data, -mod) %>%
#   filter(term != "(Intercept)") %>%
#   mod2df() %>%
#   clean_output() %>%
#   mutate(term = case_when(is.na(term) & ses_var == "perc_pov_ppl" ~ "Percent Poverty", 
#                           is.na(term) & ses_var == "perc_uninsur" ~ "Percent Uninsured", 
#                           is.na(term) & ses_var == "perc_hmown" ~ "Percent Homeownership", 
#                           TRUE ~ as.character(term)))
# 
# # write_csv(hlvl_mod_result,
# #           paste0("results/output/health level exceedence multivar model_",
# #           Sys.Date(), ".csv"))
# 
# det_mod_evrdet_size <- ucmrdf.mod %>%
#   filter(contam.pfas == "evrdet") %>% 
#   group_by(Size) %>% 
#   nest() %>% 
#   mutate(mod = purrr::map(data, model_evrdet_size)) %>% 
#   mutate(result = purrr::map(mod, tidy)) %>% 
#   mutate(result2 = purrr::map(result, mod2df)) %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   select(-data, -result, -mod) %>% 
#   unnest(c(result2, nobs)) %>% 
#   clean_output()
# 
# write_csv(det_mod_evrdet_size,
#           paste0("results/output/Any UCMR detection model stratified by system size.",
#                  Sys.Date(), ".csv"))


# ### JML check: these looks similar to above, so I'm doing a few coefficient spot checks
# mod.covs.test <- glm(hlvlchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+
#                        as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                      data = subset(ucmrdf.mod, contam.pfas == "evrdet"), family = binomial)
# 
# summary(mod.covs.test)
# 
# mod.covs.test <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                      data = subset(ucmrdf.mod, contam.pfas == "evrdet" & Size == "L"), family = binomial)
# 
# summary(mod.covs.test)
# 
# mod.covs.test <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                      data = subset(ucmrdf.mod, contam.pfas == "evrdet" & Size == "S"), family = binomial)
# 
# summary(mod.covs.test)
# 
# ## I did the below as a small check on sample sizes (below is a model with interactions)
# # as a quick note, this is a model where i did not assume that the other variables can differentially affect the outcome by system size
#  # # this is what we can allow when we stratify by system size (as a counterintuitive modelling note: a more complex model = FEWER assumptions)
#   # i am only allowing that the associations between each demographic variable can vary (linearly) by system size (either L or S) below:
# 
#   mod.covs.test_int <- glm(detchem ~ as.factor(Size)*(perc_hisp_any + perc_black_nohisp+propurban+ mdi)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                      data = subset(ucmrdf.mod, contam.pfas == "evrdet"), family = binomial)
# 
#   mod.covs.test_no.int <- glm(detchem ~ as.factor(Size) + perc_hisp_any + perc_black_nohisp+propurban+ mdi+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                            data = subset(ucmrdf.mod, contam.pfas == "evrdet"), family = binomial)
# 
#   summary(mod.covs.test.int)
# 
#   # this is then a comparison of the model without interactions and the model with interactions to see if we get a significantly better fit to the data
#     # when we include interactions (which is essentially a more complicated model)
#   if (!require("lmtest")) install.packages("lmtest")
#   lrtest(mod.covs.test_no.int, mod.covs.test_int) # we don't, so the demographics may not meaningfully vary in their association with detchem by system size
#   # please let me know if this makes sense!
# 
# #### overall, this looks good (along with comments from crude, bivariate regressions on interpretation)

#####
### JML check #2: modelling with cluster-robust standard errors
  # This is a method to adjust standard errors because we may violate the assumption of 
  # that our water systems were independently sampled because we're using county-level data
  # essentially, we may just have clustering in demographics due to county-level data,
  # so this can mean our standard errors are too small (because we effectively have a smaller sample size than the actual total)
  # this could be something to mention in limitations (if we don't include this), or we could adapt the code above to include
  # these adjusted (more conservative) standard errors (reviewer may comment on it?)

## COMMENT OUT BELOW HERE:
#  if (!require("sandwich")) install.packages("sandwich")
#  if (!require("lmtest")) install.packages("lmtest")
# 
#  # how many counties do we have?
#  JML.test.mod <- ucmrdf.mod %>%
#    mutate(GEO.id2 = ifelse(is.na(GEO.id2), paste("multiple", PWSID, sep = "_"), GEO.id2)
# ) # this is making a "county" ID for systems that served multiple counties
#  
#  n_distinct(JML.test.mod$GEO.id2) # 1693
#   
#  logistic.f <- function(dataframe, formula, cluster) {
#     
#     model <- glm(formula, data = dataframe, family = "binomial")
#     
#     # adjustment for clustering
#     crse <- lmtest::coeftest(model, vcov. = vcovCL(model, cluster = cluster))
#     
#     return(list(model, crse))
#   }
#   
#   # run model on PFAS with covariates  
#   mod.covs.test <- logistic.f(formula = "detchem ~ perc_hisp_any + perc_black_nohisp + propurban + mdi + 
#                               as.factor(Size) + as.factor(source_type) + 
#                               WWTP_ML_km2  +  airportMFTA_bin + n_epastewardship_bin",
#                        dataframe = subset(JML.test.mod, contam.pfas == "PFAS"), 
#                        cluster = subset(JML.test.mod, contam.pfas == "PFAS")$GEO.id2)
#   
#   # this prints the associations with the adjusted standard errors
#   mod.covs.test[[2]] # no terms are no longer significant (no changes in conclusions for this model)
#   
#   # testing again with a different outcome and chemical of focus
#   mod.covs.test <- logistic.f(formula = "detchem ~ perc_hisp_any + perc_black_nohisp+propurban+
#                               perc_pov_ppl+as.factor(Size)+as.factor(source_type) + WWTP_ML_km2 + bin_14DIOXANE",
#                               dataframe = subset(JML.test.mod, contam.pfas == "1,4-dioxane"), 
#                               cluster = subset(JML.test.mod, contam.pfas == "1,4-dioxane")$GEO.id2)
#   
#   mod.covs.test[[2]] # slight changes here some covariates 'less significant' but no substantially different takeaways...
#   
#   # testing again with a different outcome
#   mod.covs.test <- logistic.f(formula = "hlvlchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+
#                                                     as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2",
#                               dataframe = subset(JML.test.mod, contam.pfas == "evrdet"), 
#                               cluster = subset(JML.test.mod, contam.pfas == "evrdet")$GEO.id2)
#   
#   mod.covs.test[[2]] # slight changes here with some covariates 'less significant' but no substantially different takeaways...
#####
# 
# ################################################################################
# # WRITE-UP TABLES FOR PAPER ######
# ################################################################################
# 
# Table4_key <- data.frame(term = colnames(crude_table)) %>%
#   mutate(outcome = case_when(str_detect(term, "UCMR Detection") ~ "Detected ≥1 target contaminant", 
#                              str_detect(term, "health guideline") ~ "Exceeded ≥1 HRL", 
#                              str_detect(term, "1,4-dioxane") ~ "1,4-dioxane", 
#                              str_detect(term, "1,1-dichloroethane") ~ "1,1-dichloroethane", 
#                              str_detect(term, "HCFC-22") ~ "HCFC-22", 
#                              str_detect(term, "PFAS") ~ "PFAS"), 
#          what = case_when(str_detect(term, "result_") ~ "Percent change (95% CI)", 
#                           str_detect(term, "p.value_") ~ "P-value"))
# 
# Table4_format <- crude_table %>% 
#   filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
#   select(term, 
#          contains(">=1 UCMR"), contains(">=1 target contaminant"), 
#          contains("1,4-dioxane"), contains("1,1-dichloroethane"), 
#          contains("HCFC-22"), contains("PFAS"), 
#          -contains("nobs")) %>% 
#   mutate(`result_1,1-dichloroethane` = 
#            ifelse(str_detect(`result_1,1-dichloroethane`,"Inf"), 
#                   as.character(NA), `result_1,1-dichloroethane`)) %>%
#   mutate(`p.value_1,1-dichloroethane` = 
#            ifelse(str_detect(`p.value_1,1-dichloroethane`,"0.96"),
#                   as.character(NA), 
#                   `p.value_1,1-dichloroethane`)) %>%
#   # mutate(term = case_when(term == "Any TRI facility" ~ "Any TRI facility present (ref = absent)", 
#   #                         term == "TRI (1,4-Dioxane) facility present" ~ "TRI (1,4-dioxane) facility present (ref = absent)", 
#   #                         term == "TRI (CFCs) facility present" ~ "TRI (CFCs) facility present (ref = absent)", 
#   #                         term == "TRI (Chlorinated Solvents) facility present" ~ "TRI (chlorinated solvents) facility present (ref = absent)", 
#   #                         term == "Major PFAS industry facility present" ~ "Major PFAS industry present (ref = absent)", 
#   #                         term == "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)" ~ 
#   #                           "Certified airports (≥1 in county) or Military fire-training areas (≥1 in county) (ref = absent)", 
#   #                         TRUE ~ as.character(term))) %>%
#   mutate(term = fct_relevel(term, "Number of samples", after = 11)) %>%
#   arrange(term) %>%
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table4_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` + 
#         `p.value_1,1-dichloroethane` + `p.value_HCFC-22` + 
#         `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.4) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE)
# 
# Table5_key <- Table4_key %>%
#   bind_rows(data.frame(term = c("result", "p.value"), 
#                        outcome = c("Exceeded ≥1 HRL", "Exceeded ≥1 HRL"), 
#                        what = c("Percent change (95% CI)", "P-value")))
# 
# Table5_format <- table5_w_allpov %>%
#   filter(pov_var_model=="mdi") %>%
#   left_join(hlvl_mod_result, by = "term") %>% 
#   ungroup() %>%
#   rename(
#     `p.value_>=1 target contaminant over health guideline` = p.value, 
#     `result_>=1 target contaminant over health guideline` = result) %>%
#   select(term, contains(">=1 UCMR"), 
#          `result_>=1 target contaminant over health guideline`, 
#          `p.value_>=1 target contaminant over health guideline`,
#          contains("1,4-dioxane"), contains("1,1-dichloro"), 
#          contains("HCFC-22"), contains("PFAS")) %>%
#   mutate(term = factor(term, levels = unique(table5_w_allpov$term))) %>%
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table4_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` +
#           `p.value_1,1-dichloroethane` + `p.value_HCFC-22` +
#           `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.2) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE)
# 
# Table6_format <- det_mod_evrdet_size %>%
#   filter(!is.na(term)) %>%
#   pivot_wider(names_from = Size, values_from = c(p.value, result, nobs)) %>% 
#   select(term, result_L, p.value_L, 
#          result_S, p.value_S) %>% 
#   mutate(term = factor(term, levels = unique(table5_w_allpov$term))) %>%
#   arrange(term) %>%
#   flextable() %>%
#   add_header_row(top = TRUE, 
#                  values = c("", 
#                             "Large water system (N = 4,030)", 
#                             "", 
#                             "Small water system (N = 778)", 
#                             "")) %>%
#   set_header_labels(term = "", 
#                     result_L = "Percent change (95% CI)", 
#                     p.value_L = "P-value", 
#                     result_S = "Percent change (95% CI)", 
#                     p.value_S = "P-value") %>%
#   merge_h(i = 1, part = 'header') %>%
#   theme_vanilla() %>%
#   autofit() %>%
#   vline(part = 'all', j = c(1, 3))
# 
# # poverty
# TableS3_format <- table5_w_allpov %>%
#   filter(pov_var_model=="perc_pov_ppl") %>%
#   left_join(hlvl_mods_otherses, by = c("term", 
#                                        "pov_var_model" = "ses_var")) %>% 
#   ungroup() %>%
#   rename(
#     `p.value_>=1 target contaminant over health guideline` = p.value, 
#     `result_>=1 target contaminant over health guideline` = result) %>%
#   select(term, contains(">=1 UCMR"), 
#          `result_>=1 target contaminant over health guideline`, 
#          `p.value_>=1 target contaminant over health guideline`,
#          contains("1,4-dioxane"), contains("1,1-dichloro"), 
#          contains("HCFC-22"), contains("PFAS")) %>%
#   mutate(term = factor(term, levels = unique(table5_w_allpov$term))) %>%
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table4_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` +
#           `p.value_1,1-dichloroethane` + `p.value_HCFC-22` +
#           `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.2) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE)
# 
# # homeownership
# TableS4_format <- table5_w_allpov %>%
#   filter(pov_var_model=="perc_hmown") %>%
#   left_join(hlvl_mods_otherses, by = c("term", 
#                                        "pov_var_model" = "ses_var")) %>% 
#   ungroup() %>%
#   rename(
#     `p.value_>=1 target contaminant over health guideline` = p.value, 
#     `result_>=1 target contaminant over health guideline` = result) %>%
#   select(term, contains(">=1 UCMR"), 
#          `result_>=1 target contaminant over health guideline`, 
#          `p.value_>=1 target contaminant over health guideline`,
#          contains("1,4-dioxane"), contains("1,1-dichloro"), 
#          contains("HCFC-22"), contains("PFAS")) %>%
#   mutate(term = factor(term, levels = unique(table5_w_allpov$term))) %>%
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table4_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` +
#           `p.value_1,1-dichloroethane` + `p.value_HCFC-22` +
#           `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.2) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE)
# 
# # perc uninsured
# TableS5_format <- table5_w_allpov %>%
#   filter(pov_var_model=="perc_uninsur") %>%
#   left_join(hlvl_mods_otherses, by = c("term", 
#                                        "pov_var_model" = "ses_var")) %>% 
#   ungroup() %>%
#   rename(
#     `p.value_>=1 target contaminant over health guideline` = p.value, 
#     `result_>=1 target contaminant over health guideline` = result) %>%
#   select(term, contains(">=1 UCMR"), 
#          `result_>=1 target contaminant over health guideline`, 
#          `p.value_>=1 target contaminant over health guideline`,
#          contains("1,4-dioxane"), contains("1,1-dichloro"), 
#          contains("HCFC-22"), contains("PFAS")) %>%
#   mutate(term = factor(term, levels = unique(table5_w_allpov$term))) %>%
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table4_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` +
#           `p.value_1,1-dichloroethane` + `p.value_HCFC-22` +
#           `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.2) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE)
# 
# doc <- read_docx() %>%
#   body_add_par("Table 4. Crude logistic regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table4_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 5. Multiple regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table5_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 6. Multiple regression model assessing likelihood of detecting a target contaminant, stratified by PWS size.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table6_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S3. Multiple regression models using unidimensional poverty measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS3_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Figure S1. Correlogram of covariates used in multiple regression models. Spearman's p statistic was used to measure associations. *** p-value < 0.001; ** p-value < 0.01; * p-value < 0.05.") %>%
#   body_add_par("") %>%
#   body_add_gg(value = Figure1corr, style = "centered", width = 10, height = 6, scale = 1.5) %>%
#   body_end_section_landscape(w = 8.5, h = 11)
# 
# #print(doc, target = paste0("results/output/UCMRsummary_tables_reg_", Sys.Date(), ".docx"))  
# 
# ## once all tables and figures have been decided, use doc2 to compile all tables/figs in order
# ## still need to find and store each object separately 
# ## may be more efficient to do this in another script
# 
# doc2 <- read_docx() %>%
#   body_add_par("Table 1. Characteristics of PWSs in the study sample (n = 4,808) and median demographic values of the counties they serve.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table1_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 2. Detection frequencies of UCMR3 target contaminants.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table2_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 3. Comparison of mean-county level demographics between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table3_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 4. Crude logistic regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table4_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 5. Multiple regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table5_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 6. Multiple regression model assessing likelihood of detecting a target contaminant, stratified by PWS size.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table6_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S1. Comparison of mean-county univariate SES indicators between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS1_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S2. Comparison of PWS detection frequencies between systems in tribes, territories, or in a U.S. state or D.C.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS2_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S3. Multiple regression models using unidimensional poverty measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS3_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S4. Multiple regression models using unidimensional homeownership measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS4_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S5. Multiple regression models using unidimensional uninsurance rate measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS5_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Figure S1. Correlogram of covariates used in multiple regression models. Spearman's p statistic was used to measure associations. *** p-value < 0.001; ** p-value < 0.01; * p-value < 0.05.") %>%
#   body_add_par("") %>%
#   body_add_gg(value = Figure1corr, style = "centered", width = 10, height = 6, scale = 1.5) %>%
#   body_end_section_landscape(w = 8.5, h = 11)
# 
# # print(doc2, target = paste0("results/output/UCMRsummary_all_tables_and_figs_", Sys.Date(), ".docx"))  
# 

### AM'S WORK ZONE ######


## How does changing the direction of the MDI values affect the coefs?
## How does changing the reference group from L to S in Size affect the coefs?
## Answers: 1) it changes the directions only; 2) note that percentage change
## are the estimates reported in the paper. To calculate the percentage change 
## of the referent, convert PC to an OR and take the reciprocal of the OR, and 
## finally, reconvert it back to a PC.

# processed %>%
#   pivot_longer(cols = c(hlvlchem, starts_with("contam_")),
#                names_to = "contam.pfas", 
#                values_to = "detchem") %>%
#   mutate(contam.pfas = str_remove(contam.pfas, "contam_")) %>%
#   pivot_longer(cols = c("mdi", "perc_hmown", "perc_pov_ppl", "perc_uninsur"), 
#                names_to = "sesvar", 
#                values_to = "sesval") %>%
#   filter(sesvar=="mdi" & contam.pfas == "evrdet") %>%
#   mutate(sesval = sesval*-1) %>%
#   mutate(Size = factor(Size, levels = c("S", "L"))) %>%
#   group_by(contam.pfas, sesvar) %>%
#   nest() %>%
#   mutate(base = paste("detchem ~ perc_hisp_any + perc_black_nohisp + 
#                                            propurban + as.factor(Size) + 
#                                            as.factor(source_type) + 
#                                            WWTP_ML_km2 + n_samples + sesval + 
#                              (1|stateabbr)")) %>%
#   mutate(specformula = case_when(str_detect(contam.pfas,"evrdet|hlvlchem") ~ paste(base, "+ bin_TRI"), 
#                                  contam.pfas == "1,4-dioxane" ~ paste(base, "+ `bin_14DIOXANE`"), 
#                                  contam.pfas == "1,1-dichloroethane" ~ paste(base, "+ `bin_CHLORINATED_SOLVENTS`"), 
#                                  contam.pfas == "HCFC-22" ~ paste(base, "+ bin_CFCs"), 
#                                  contam.pfas == "PFAS" ~ paste(base, "+  airportMFTA_bin + n_epastewardship_bin"), 
#                                  TRUE ~ "999")) %>%
#   {stopifnot(nrow(filter(., specformula == "999"))==0); .;} %>%
#   mutate(glmer1 = map(data, ~lme4::glmer(specformula, 
#                                          data = ., 
#                                          family = binomial))) %>% 
#   mutate(tidyglmer1 = map(glmer1, broom.mixed::tidy)) %>%
#   mutate(tidyglmer2 = map(tidyglmer1, mod2df)) %>%
#   select(-data, -glmer1, -tidyglmer1, -specformula) %>%
#   unnest(tidyglmer2)


# # What should be considered a "sample"?
# # def 1: sample point name, date, and contaminant
# # def 2: sample point name, date, and sample ID
# # def 3: sample point name, date, only
# 
# n_samples_dat <- ucmr3_targetsamples %>% 
#   group_by(PWSID, SamplePointName, CollectionDate, Contaminant) %>% 
#   summarise(n = n()) %>% 
#   group_by(PWSID) %>%
#   summarise(n_samples = sum(n))
# 
# # note that I think this definition was used for Table2
# n_samples_dat2 <- ucmr3_targetsamples %>%
#   group_by(PWSID, SamplePointName, CollectionDate, SampleID) %>% 
#   summarise(n = n()) %>% 
#   group_by(PWSID) %>%
#   summarise(n_samples = n())
# 
# n_samples_dat3 <- ucmr3_targetsamples %>%
#   group_by(PWSID, SamplePointName, CollectionDate) %>% 
#   summarise(n_samples = n()) %>% 
#   group_by(PWSID) %>%
#   summarise(n_samples = n())
# 
# comp_sample_def <- n_samples_dat %>% 
#   rename('sampdef1' = n_samples) %>%
#   left_join(n_samples_dat2 %>% rename('sampdef2' = n_samples)) %>%
#   left_join(n_samples_dat3 %>% rename('sampdef3' = n_samples))
# 
# aa <- ucmrdf.mod %>%
#   left_join(comp_sample_def, 
#             by = "PWSID") 
# 
# cor.test(aa$perc_hisp_any, aa$sampdef3, method = 'spearman')
# 
# aa %>% 
#   filter(contam.pfas == "evrdet") %>%
#   pivot_longer(cols = contains("sampdef")) %>%
#   ggplot(aes(x = log(perc_hisp_any), y = log(value))) + 
#   geom_point() + 
#   facet_wrap(~name)
# 
# ggplot(comp_sample_def, 
#        aes(x = log(n_samples_sampID), y = log(n_samples_sampNameDate))) + 
#   geom_point() + 
#   labs(x = "Log(samples - old definition)", 
#        y = "Log(samples - current definition)")
# 
# # n_samples_dat2 <- n_samples_dat %>%
# #   group_by(PWSID) %>%
# #   summarise(contam.pfas = "evrdet", 
# #             n_samples = sum(n_samples))
# # 
# # n_samples_dat <- n_samples_dat %>%
# #   bind_rows(n_samples_dat2)
# 
# 
# 
# comp_sample_def %>%
#   left_join(old_n_samples2 %>% select(PWSID, n_samples_old)) %>% 
#   left_join(PWSchar) %>% 
#   filter(!is.na(Size)) %>%
#   ggplot(aes(x = log(n_samples_old), y = log(n_samples_sampNameDate))) + 
#   geom_point() + 
#   labs(x = "Log(samples - old definition)", 
#        y = "Log(samples - current definition)")
# 
# check <- n_samples_dat2 %>%
#   left_join(old_n_samples2 %>% select(PWSID, n_samples_old)) %>% 
#   left_join(PWSchar) %>% 
#   filter(!is.na(Size)) %>%
#   mutate(logOld = log(n_samples_old), 
#          logNew = log(n_samples), 
#          check = n_samples_old/n_samples) %>%
#   distinct()
# 
# check %>%
#   mutate(check2 = case_when(check < 3 ~ "<3", 
#                             check == 3 | check < 9 ~ "<3-9", 
#                             check == 9 ~ "9", 
#                             check > 9 ~ ">9")) %>%
#   group_by(check2, Size, source_type) %>%
#   count() 
# 
# ucmrdf.mod2 %>%
#   filter(contam.pfas == "HCFC-22") %>%
#   select(starts_with("perc"), 
#          starts_with("WWTP"), 
#          land.area, mdi, n_WWTP, propurban, n_samples) %>%
#   gather(-n_samples, -PWSID,
#          key = "some_var_name", value = "some_value_name") %>%
#   ggplot(aes(x = some_value_name, y = n_samples)) + 
#   geom_point() + 
#   facet_wrap(~some_var_name, scales = 'free')
# 
# sample_size_dat <- ucmrdf.mod2 %>%
#   filter(contam.pfas == "evrdet") %>%
#   select(n_samples, PWSID, Size, source_type)
# 
# sample_size_dat %>% 
#   ungroup() %>%
#   summarise(mean_samples = mean(n_samples), 
#             sd = sd(n_samples), 
#             var = var(n_samples), 
#             median = median(n_samples),
#             quart = paste0(quantile(n_samples, 0.25), ", ",
#                            quantile(n_samples, 0.75)))
# 
# sample_size_dat %>% 
#   group_by(Size) %>% 
#   summarise(mean_samples = mean(n_samples), 
#             sd = sd(n_samples), 
#             var = var(n_samples), 
#             median = median(n_samples),
#             quart = paste0(quantile(n_samples, 0.25), ", ",
#                           quantile(n_samples, 0.75)))
# 
# sample_size_dat %>% 
#   group_by(source_type)  %>% 
#   summarise(mean_samples = mean(n_samples), 
#             sd = sd(n_samples), 
#             var = var(n_samples), 
#             median = median(n_samples),
#             quart = paste0(quantile(n_samples, 0.25), ", ",
#                            quantile(n_samples, 0.75)))
#   
# sample_size_dat %>% gather(-n_samples, -PWSID,
#          key = "some_var_name", value = "some_value_name") %>%
#   group_by(some_value_name) %>%
#   mutate(n_systems = n(), 
#          some_value_name = paste0(some_value_name, 
#                                   " (n=", 
#                                   n_systems, 
#                                   ")")) %>%
#   ggplot(aes(x = some_value_name, y = log(n_samples))) + 
#   geom_jitter(width = 0.2, col = 'grey65') + 
#   geom_boxplot(width = 0.3) + 
#   # stat_summary(aes(y = log(n_samples), label = round((..y..), 1)),
#   #              fun = median, geom = "text", col = 'red',
#   #              position = position_nudge(x = 0.45)) +
#   # stat_summary(aes(y = log(n_samples), label = round((..y..), 1)),
#   #           fun = mean, geom = "text", col = 'blue',
#   #           position = position_nudge(x = 0.45, y = 0.3)) +
#   facet_wrap(~some_var_name, scales = 'free') +
#   labs(x = "", y = "Log(Number of samples)", 
#        title = "Total number of target contaminant samples collected")









########################## GARBAGE <3 ###########################################

# AM workspace
colnames(ucmrdf.mod)
test_50 <- left_join(ucmrdf.mod, TRI_counties_bin_50, by = c("GEO.id2" = "FIPS")) %>%
  mutate(evr_fac = ifelse(is.na(evr_fac), 0, 1)) %>%
  mutate(propurban = propurban*100)
  # select(contam.pfas, detchem, bin_TRI, evr_fac)
test_5 <- left_join(ucmrdf.mod, TRI_counties_bin_5, by = c("GEO.id2" = "FIPS")) %>%
  mutate(evr_fac = ifelse(is.na(evr_fac), 0, 1)) %>%
  mutate(propurban = propurban*100)

test_50 %>% filter(contam.pfas == 'evrdet') %>% select(bin_TRI) %>% ungroup() %>% summarise(sum = sum(bin_TRI))
test_50 %>% filter(contam.pfas == 'evrdet') %>% select(evr_fac) %>% ungroup() %>% summarise(sum = sum(evr_fac))
test_5 %>% filter(contam.pfas == 'evrdet') %>% select(evr_fac) %>% ungroup() %>% summarise(sum = sum(evr_fac))

# check
# test %>% filter(bin_TRI == 1) %>% select(GEO.id2, evr_fac)

# crude
glm(detchem ~ bin_TRI, test_50 %>% filter(contam.pfas == 'evrdet'), family = 'binomial') %>% tidy() %>% mod2df()
glm(detchem ~ evr_fac, test_50 %>% filter(contam.pfas == 'evrdet'), family = 'binomial') %>% tidy() %>% mod2df()
glm(detchem ~ evr_fac, test_5 %>% filter(contam.pfas == 'evrdet'), family = 'binomial') %>% tidy() %>% mod2df()

# adjusted
adj_mod_crude <- glm(detchem ~ perc_hisp_any + perc_black_nohisp + propurban +
      mdi + as.factor(Size) + as.factor(source_type)+ bin_TRI + WWTP_ML_km2 + 
        n_epastewardship_bin + airportMFTA_bin, 
    data = test_50 %>% filter(contam.pfas == 'evrdet'), 
    family = 'binomial') %>% tidy(exp = T, conf.int = T, conf.level = 0.99) %>%
  mutate(model = 'original')
adj_mod_50 <- glm(detchem ~ perc_hisp_any + perc_black_nohisp + propurban +
      mdi + as.factor(Size) + as.factor(source_type)+ evr_fac + WWTP_ML_km2 + 
        n_epastewardship_bin + airportMFTA_bin, 
    data = test_50 %>% filter(contam.pfas == 'evrdet'), 
    family = 'binomial') %>% tidy(exp = T, conf.int = T, conf.level = 0.99) %>%
  mutate(model = '50_km_model')
adj_mod_5 <- glm(detchem ~ perc_hisp_any + perc_black_nohisp + propurban +
      mdi + as.factor(Size) + as.factor(source_type)+ evr_fac + WWTP_ML_km2 + 
        n_epastewardship_bin + airportMFTA_bin, 
    data = test_5 %>% filter(contam.pfas == 'evrdet'), 
    family = 'binomial') %>% tidy(exp = T, conf.int = T, conf.level = 0.99) %>%
  mutate(model = '5_km_model')

df <- bind_rows(adj_mod_crude, adj_mod_50, adj_mod_5) %>%
  mutate(term = case_when(term == 'evr_fac' ~ 'bin_TRI', TRUE ~ term)) %>%
  mutate(model = factor(model, levels = c('original', '5_km_model', '50_km_model')))

ggplot(df %>% filter(term != '(Intercept)'), aes(x = estimate, y = term)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, width = 0.2)) + 
  facet_wrap(~model) + 
  geom_vline(xintercept = 1, col = 'red') + 
  theme_bw() + 
  labs(x = "Odds ratio", y = "")
  # theme(axis.text.x = element_text(angle = 90))

# convert tidy model output to presentable data

# mod2df <- function(mod){
#   
#   #convert to percent change and add significance indicator
#   mod %>% mutate(perc.change = round((exp(estimate)-1)*100, digits = 1),
#                  lo.ci = round((exp(estimate - 1.96*std.error)-1)*100, digits = 1),
#                  hi.ci = round((exp(estimate + 1.96*std.error)-1)*100, digits = 1), 
#                  result = paste0("", as.character(format(perc.change, nsmall = 1, digits = 2)), " ", "(", 
#                                  as.character(format(lo.ci, nsmall = 1, digits = 2)),
#                                  ", ", as.character(format(hi.ci, nsmall = 1, digits = 2)), ")")) %>%
#     mutate(result = case_when((p.value < .05 & p.value > .01) ~ paste0(result, "*"),
#                               (p.value < .01 & p.value > .001) ~ paste0(result, "**"),
#                               (p.value < .001) ~ paste0(result, "***"),
#                               TRUE ~ result)) %>%
#     select(-c(estimate:hi.ci)) %>% 
#     pivot_wider(names_from = term, values_from = result)
#   
# }

# mod2df <- function(mod){
#   
#   #convert to percent change and add significance indicator
#   mod %>% mutate(perc.change = round((exp(estimate)-1)*100, digits = 1),
#                  lo.ci = round((exp(estimate - 1.96*std.error)-1)*100, digits = 1),
#                  hi.ci = round((exp(estimate + 1.96*std.error)-1)*100, digits = 1), 
#                  result = paste0("", as.character(format(perc.change, nsmall = 1, digits = 2)), " ", "(", 
#                                  as.character(format(lo.ci, nsmall = 1, digits = 2)),
#                                  ", ", as.character(format(hi.ci, nsmall = 1, digits = 2)), ")")) %>%
#     select(-c(estimate:statistic, perc.change:hi.ci)) %>% 
#     pivot_wider(names_from = term, values_from = c(result, p.value))
#   
#}



# modtidy <- function(mod){
#   
#   #convert to percent change and add significance indicator
#   mod %>% 
#     filter(term != "(Intercept)") %>% 
#     mutate(p.value = case_when((p.value < .05 & p.value > .01) ~ "*",
#                                (p.value < .01 & p.value > .001) ~ "**",
#                                (p.value < .001) ~ "***",
#                                TRUE ~ ""))
#   
# }

# 
# 
# 
# # multi_var_result_df <- bind_rows(multi_var_result_list, .id = "id") %>% 
# #   pivot_longer(-id, names_to = "pov_var_model") %>% 
# #   filter(value$term != "(Intercept)") 
# # 
# 
# 
# model_14diox <- function(dat){
#   (glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
#              as.factor(Size)+as.factor(source_type) + WWTP_ML_km2 + `bin_14DIOXANE`,
#            data = dat, family = binomial()))
# }
# 
# model_11dichlor <- function(dat){
#   (glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
#              as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ `bin_CHLORINATED_SOLVENTS`,
#            data = dat , family = binomial()))
# }
# 
# model_hcfc <- function(dat){
#   (glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
#              as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ bin_CFCs,
#            data = dat , family = binomial()))
# 
# }
# 
# model_pfas <- function(dat){
#   (glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
#              as.factor(Size)+as.factor(source_type) + WWTP_ML_km2  +  airportMFTA_bin + n_epastewardship_bin,
#            data = dat, family = binomial()))
# }
# 
# 
# 
# #pov variable 
# pov_var <- c("mdi", "perc_pov_ppl", "perc_uninsur", "perc_hmown")
# 
# # run all variables through the model one at a time by contaminant
# multi_list <- list()
# multi_all <- data.frame()
# 
# 
# for(i in unique(ucmrdf.mod$contam.pfas)) {
#   det_mod_df <- ucmrdf.mod %>%
#     filter(contam.pfas == i)
#   
#   result <- model(det_mod_df)}
#   
#   multi_list[[i]] <- mod2df(tidy(result)) %>% 
#     bind_rows(data.frame(n = length(result$residuals), term = "nobs")) %>% 
#     mutate(contam.pfas = i)
#   
#   
# }
# 
# 
# for(i in unique(ucmrdf.mod$contam.pfas)) {
#   det_mod <- ucmrdf.mod %>%
#     filter(contam.pfas == i)
#   
#   if(det_mod$contam.pfas == "evrdet"){
#     result <- model_evrdet(det_mod)
#   }
#   if(det_mod$contam.pfas == "1,4-dioxane"){
#     result <- model_14diox(det_mod)
#   }
#   if(det_mod$contam.pfas == "1,1-dichloroethane"){
#     result <- model_11dichlor(det_mod)
#   }
#   if(det_mod$contam.pfas == "HCFC-22"){
#     result <- model_hcfc(det_mod)
#   }
#   if(det_mod$contam.pfas == "PFAS"){
#     result <- model_pfas(det_mod)
#   }
#     
#   multi_list[[i]] <- mod2df(tidy(result)) %>% 
#     bind_rows(data.frame(n = length(result$residuals), term = "nobs")) %>% 
#     mutate(contam.pfas = i)
# 
# }
# 
# multi_all <- bind_rows(multi_list) %>% 
#   mutate(p.value = round(p.value, 4),
#          term = case_when(term %in% c("bin_CFCs", "bin_14DIOXANE", 
#                                       "bin_CHLORINATED_SOLVENTS", "bin_TRI", 
#                                       "n_epastewardship_bin") ~ "bin_IndFac",
#                           TRUE ~ term)) %>% 
#   filter(term != "(Intercept)") %>% 
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
#                                                       "1,1-dichloroethane", "HCFC-22", "PFAS"),
#                               labels = c(">=1 UCMR Detection", "1,4-dioxane",
#                                          "1,1-dichloroethane", "HCFC-22", 
#                                          "PFAS"))) %>% 
#   pivot_wider_spec(spec2) %>% 
#   mutate(term = case_when(term == "perc_hisp_any" ~ "Percent Hispanic",
#                           term == "perc_black_nohisp" ~ "Percent Black",
#                           term == "perc_pov_ppl" ~ "Percent Poverty",
#                           term == "perc_hmown" ~  "Percent Homeownership",
#                           term == "propurban" ~ "Percent Urban Households",
#                           term == "mdi" ~ "Percent Deprived",
#                           term == "perc_uninsur" ~ "Percent Uninsured",
#                           term == "as.factor(Size)S" ~ "Size (Small)",
#                           term == "as.factor(source_type)GU" ~ "Water Source (GU)",
#                           term == "as.factor(source_type)GW" ~ "Water Source (GW)",
#                           term == "as.factor(source_type)SW" ~ "Water Source (SW)",
#                           term == "as.factor(source_type)MX" ~ "Water Source (MX)",
#                           term == "as.factor(source_type)MIX" ~ "Water Source (MIX)",
#                           term == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
#                           term == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
#                           term == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
#                           term == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
#                           term == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
#                           term == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
#                           term == "bin_TRI" ~ "Any TRI facility",
#                           term == "bin_IndFac" ~ ">=1 Industrial facility present",
#                           term == "nobs" ~ "Number of observations in model",
#                           TRUE ~ term)) %>% 
#   mutate(term = factor(term, levels = c("Number of observations in model","Percent Hispanic", 
#                                         "Percent Black","Percent Urban Households", 
#                                         "Percent Deprived","Size (Small)", 
#                                         "Water Source (SW)", "Water Source (MIX)",
#                                         ">=1 Industrial facility present", 
#                                         "WWTP total flow per area (million L per km^2)",
#                                         "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)"))) %>% 
#   arrange(term)



# 
# reg_dem_evrdet_hlvlchem<- glm(hlvlchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
#                         as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
#                       data = det_mod_evrdet, family = binomial())
# 
# clean_reg_dem_evrdet_hlvlchem <- mod2df(tidy(reg_dem_evrdet_hlvlchem)) %>% 
#   bind_rows(data.frame(n = length(reg_dem_evrdet_hlvlchem$residuals), term = "nobs")) %>% 
#   mutate(contam.pfas = "hlvlchem")
# 
# 
# write_csv(mod_allcontam, "results/output/all contaminants demo and sources regression model_withpvalue.csv")
# 



### stratify by system size

model_fun_demo <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+propurban+mdi+as.factor(source_type), 
           data = dat, family = binomial()))
}
model_fun_sources <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+propurban+mdi+as.factor(source_type)+ 
             bin_TRI + WWTP_ML_km2, 
           data = dat, family = binomial()))
}

det_mod_evrdet_size <- ucmrdf.mod %>%
  filter(contam.pfas == "evrdet") %>% 
  group_by(Size) %>% 
  nest() %>% 
  mutate(new_result1 = purrr::map(data, model_fun_demo),
         new_result2 = purrr::map(data, model_fun_sources)) %>% 
  mutate(`1` = purrr::map(new_result1, mod2df),
         `2` = purrr::map(new_result2, mod2df)) %>% 
  select(-data, -new_result1, -new_result2) %>% 
  pivot_longer(names_to = "model", values_to = "result", cols = c(`1`:`2`)) %>% 
  unnest(result) %>%
  pivot_longer(names_to = "var", values_to = "result", c(-Size, -model)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = c(model, Size), values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "as.factor(source_type)MIX" ~ "Water Source (MIX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         TRUE ~ var)) %>% 
  relocate(`2_L`, .after = "1_S")

# write_csv(det_mod_evrdet_size, "results/output/Any UCMR detection model stratified by system size.csv")






spec2 <- bind_rows(multi_list) %>% 
  build_wider_spec(names_from = contam.pfas, values_from = c(result, p.value, n)) %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS"))) %>% 
  arrange(contam.pfas)


mod_allcontam <- bind_rows(clean_reg_dem_diox, clean_reg_dem_11dichl, clean_reg_dem_hcfc,
                           clean_reg_dem_pfas, clean_reg_dem_evrdet, clean_reg_dem_evrdet_hlvlchem) %>% 
  mutate(p.value = round(p.value, 4),
         term = case_when(term %in% c("bin_CFCs", "bin_14DIOXANE", 
                                      "bin_CHLORINATED_SOLVENTS", "bin_TRI", 
                                      "n_epastewardship_bin") ~ "bin_IndFac",
                          TRUE ~ term)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  pivot_wider_spec(spec2) %>% 
  mutate(term = case_when(term == "perc_hisp_any" ~ "Percent Hispanic",
                          term == "perc_black_nohisp" ~ "Percent Black",
                          term == "perc_pov_ppl" ~ "Percent Poverty",
                          term == "perc_hmown" ~  "Percent Homeownership",
                          term == "propurban" ~ "Percent Urban Households",
                          term == "mdi" ~ "Percent Deprived",
                          term == "perc_uninsur" ~ "Percent Uninsured",
                          term == "as.factor(Size)S" ~ "Size (Small)",
                          term == "as.factor(source_type)GU" ~ "Water Source (GU)",
                          term == "as.factor(source_type)GW" ~ "Water Source (GW)",
                          term == "as.factor(source_type)SW" ~ "Water Source (SW)",
                          term == "as.factor(source_type)MX" ~ "Water Source (MX)",
                          term == "as.factor(source_type)MIX" ~ "Water Source (MIX)",
                          term == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                          term == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                          term == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                          term == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                          term == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                          term == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                          term == "bin_TRI" ~ "Any TRI facility",
                          term == "bin_IndFac" ~ ">=1 Industrial facility present",
                          term == "nobs" ~ "Number of observations in model",
                          TRUE ~ term)) %>% 
  mutate(term = factor(term, levels = c("Number of observations in model","Percent Hispanic", 
                                        "Percent Black","Percent Urban Households", 
                                        "Percent Deprived","Size (Small)", 
                                        "Water Source (SW)", "Water Source (MIX)",
                                        ">=1 Industrial facility present", 
                                        "WWTP total flow per area (million L per km^2)",
                                        "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)"))) %>% 
  arrange(term)




### 1,4-DIOXANE ###
det_mod_diox <- ucmrdf.mod %>%
  filter(contam.pfas == "1,4-dioxane") 


reg_dem_diox <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
                      as.factor(Size)+as.factor(source_type) + WWTP_ML_km2 + `bin_14DIOXANE`, 
                    data = det_mod_diox, family = binomial())



clean_reg_dem_diox <- mod2df(tidy(reg_dem_diox)) %>% 
  bind_rows(data.frame(n = length(reg_dem_diox$residuals), term = "nobs")) %>% 
  mutate(contam.pfas = "1,4-dioxane")


### 1,1-DICHLOROETHANE ###
det_mod_11dichl <- ucmrdf.mod %>%
  filter(contam.pfas == "1,1-dichloroethane")


reg_dem_11dichl  <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
                          as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ `bin_CHLORINATED_SOLVENTS`, 
                        data = det_mod_11dichl , family = binomial())

clean_reg_dem_11dichl <- mod2df(tidy(reg_dem_11dichl))  %>% 
  bind_rows(data.frame(n = length(reg_dem_11dichl$residuals), term = "nobs")) %>% 
  mutate(contam.pfas = "1,1-dichloroethane")


### HCFC ###
det_mod_hcfc <- ucmrdf.mod %>%
  filter(contam.pfas == "HCFC-22")


reg_dem_hcfc  <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
                       as.factor(Size)+as.factor(source_type) + WWTP_ML_km2+ bin_CFCs, 
                     data = det_mod_hcfc , family = binomial())

clean_reg_dem_hcfc <- mod2df(tidy(reg_dem_hcfc)) %>% 
  bind_rows(data.frame(n = length(reg_dem_hcfc$residuals), term = "nobs")) %>% 
  mutate(contam.pfas = "HCFC-22")


# PFAS ###
det_mod_pfas<- ucmrdf.mod %>%
  filter(contam.pfas == "PFAS")


reg_dem_pfas <- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+mdi+
                      as.factor(Size)+as.factor(source_type) + WWTP_ML_km2  +  airportMFTA_bin + n_epastewardship_bin, 
                    data = det_mod_pfas, family = binomial())

clean_reg_dem_pfas <- mod2df(tidy(reg_dem_pfas)) %>% 
  bind_rows(data.frame(n = length(reg_dem_pfas$residuals), term = "nobs")) %>% 
  mutate(contam.pfas = "PFAS")

# evrdet ###

det_mod_evrdet<- ucmrdf.mod %>%
  filter(contam.pfas == "evrdet")


reg_dem_evrdet<- glm(detchem ~ perc_hisp_any + perc_black_nohisp+propurban+ mdi+
                       as.factor(Size)+as.factor(source_type)+ bin_TRI + WWTP_ML_km2,
                     data = det_mod_evrdet, family = binomial())

clean_reg_dem_evrdet<- mod2df(tidy(reg_dem_evrdet)) %>% 
  bind_rows(data.frame(n = length(reg_dem_evrdet$residuals), term = "nobs")) %>% 
  mutate(contam.pfas = "evrdet")


spec2 <- bind_rows(clean_reg_dem_diox, clean_reg_dem_11dichl, clean_reg_dem_hcfc,
                   clean_reg_dem_pfas, clean_reg_dem_evrdet, clean_reg_dem_evrdet_hlvlchem) %>% 
  build_wider_spec(names_from = contam.pfas, values_from = c(result, p.value, n)) %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  arrange(contam.pfas)


mod_allcontam <- bind_rows(clean_reg_dem_diox, clean_reg_dem_11dichl, clean_reg_dem_hcfc,
                           clean_reg_dem_pfas, clean_reg_dem_evrdet, clean_reg_dem_evrdet_hlvlchem) %>% 
  mutate(p.value = round(p.value, 4),
         term = case_when(term %in% c("bin_CFCs", "bin_14DIOXANE", 
                                      "bin_CHLORINATED_SOLVENTS", "bin_TRI", 
                                      "n_epastewardship_bin") ~ "bin_IndFac",
                          TRUE ~ term)) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
                                                      "1,1-dichloroethane", "HCFC-22", "PFAS", "hlvlchem"),
                              labels = c(">=1 UCMR Detection", "1,4-dioxane",
                                         "1,1-dichloroethane", "HCFC-22", 
                                         "PFAS", ">=1 over a health guideline"))) %>% 
  pivot_wider_spec(spec2) %>% 
  mutate(term = case_when(term == "perc_hisp_any" ~ "Percent Hispanic",
                          term == "perc_black_nohisp" ~ "Percent Black",
                          term == "perc_pov_ppl" ~ "Percent Poverty",
                          term == "perc_hmown" ~  "Percent Homeownership",
                          term == "propurban" ~ "Percent Urban Households",
                          term == "mdi" ~ "Percent Deprived",
                          term == "perc_uninsur" ~ "Percent Uninsured",
                          term == "as.factor(Size)S" ~ "Size (Small)",
                          term == "as.factor(source_type)GU" ~ "Water Source (GU)",
                          term == "as.factor(source_type)GW" ~ "Water Source (GW)",
                          term == "as.factor(source_type)SW" ~ "Water Source (SW)",
                          term == "as.factor(source_type)MX" ~ "Water Source (MX)",
                          term == "as.factor(source_type)MIX" ~ "Water Source (MIX)",
                          term == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                          term == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                          term == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                          term == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                          term == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                          term == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                          term == "bin_TRI" ~ "Any TRI facility",
                          term == "bin_IndFac" ~ ">=1 Industrial facility present",
                          term == "nobs" ~ "Number of observations in model",
                          TRUE ~ term)) %>% 
  mutate(term = factor(term, levels = c("Number of observations in model","Percent Hispanic", 
                                        "Percent Black","Percent Urban Households", 
                                        "Percent Deprived","Size (Small)", 
                                        "Water Source (SW)", "Water Source (MIX)",
                                        ">=1 Industrial facility present", 
                                        "WWTP total flow per area (million L per km^2)",
                                        "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)"))) %>% 
  arrange(term)


########################## 
########################## 
########################## 

model_building1 <- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                         perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+mdi+
                         as.factor(Size)+as.factor(source_type), 
                       data = det_mod_evrdet, family = binomial())


model_building2 <- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                         perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+mdi+
                         as.factor(Size), 
                       data = det_mod_evrdet, family = binomial())

mod_comp_build <- bind_rows(tidy(model_building1) %>% mutate(version = 1), tidy(model_building2) %>% mutate(version = 2)) %>% 
  group_by(term) %>% 
  summarize(estimate1 = estimate[which(version ==1)],
            estimate2 = estimate[which(version == 2)],
            diff = case_when(estimate1 > estimate2 ~ "decrease",
                             estimate1 < estimate2 ~ "increase"),
            increase = case_when(estimate1+(.1*abs(estimate1)) < estimate2 ~ "greater than 10% increase"),
            decrease = case_when(estimate1 - (.1*abs(estimate1)) > estimate2 ~ "greater than 10% decrease"))



clean_models_comp <- function(dat){
  dat %>% 
    pivot_longer(names_to = "var", values_to = "result", c(-contam.pfas, -model)) %>%
    mutate(result = case_when(is.na(result) ~ "",
                              TRUE ~ result)) %>% 
    pivot_wider(names_from = model, values_from = result) %>% 
    mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                           var == "perc_black_nohisp" ~ "Percent Black",
                           var == "perc_pov_ppl" ~ "Percent Poverty",
                           var == "perc_hmown" ~  "Percent Homeownership",
                           var == "propurban" ~ "Percent Urban Households",
                           var == "mdi" ~ "MDI Rate",
                           var == "perc_uninsur" ~ "Percent Uninsured",
                           var == "as.factor(Size)S" ~ "Size (Small)",
                           var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                           var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                           var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                           var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                           var == "as.factor(source_type)MIX" ~ "Water Source (MIX)",
                           var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                           var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                           var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                           var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                           var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                           var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                           var == "bin_TRI" ~ "Any TRI facility",
                           TRUE ~ var)) 
}


clean_mod_all <- bind_rows(clean_reg_dem_11dichl, clean_reg_dem_diox, clean_reg_dem_hcfc, clean_reg_dem_pfas,
                           clean_reg_dem_evrdet) %>% 
  clean_models_comp()

write_csv(clean_mod_all, "results/output/logistic regression model.csv")




mod_comp_14diox <- bind_rows(clean_reg_dem_diox %>% mutate(model = "1"), 
                             clean_reg_dem_diox2  %>% mutate(model = "2"),
                             clean_reg_dem_diox3  %>% mutate(model = "3"),
                             clean_reg_dem_diox4  %>% mutate(model = "4")) %>% 
  pivot_longer(names_to = "var", values_to = "result", c(-model, -contam.pfas)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = model, values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         var == "pov_status_co" ~ ">20% of county below pov line",
                         TRUE ~ var))

write_csv(mod_comp_14diox, "results/output/1,4-Dioxane regression comparison model.csv")

mod_comp_11dichl<- bind_rows(clean_reg_dem_11dichl %>% mutate(model = "1"), 
                        clean_reg_dem_11dichl2  %>% mutate(model = "2"),
                        clean_reg_dem_11dichl3  %>% mutate(model = "3"),
                        clean_reg_dem_11dichl4 %>% mutate(model = "4")) %>% 
  pivot_longer(names_to = "var", values_to = "result", c(-model, -contam.pfas)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = model, values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         var == "pov_status_co" ~ ">20% of county below pov line",
                         TRUE ~ var))

write_csv(mod_comp_11dichl, "results/output/1,1-dichloroethane regression comparison model.csv")

mod_comp_hcfc<- bind_rows(clean_reg_dem_hcfc %>% mutate(model = "1"), 
                          clean_reg_dem_hcfc2  %>% mutate(model = "2"),
                          clean_reg_dem_hcfc3  %>% mutate(model = "3"),
                          clean_reg_dem_hcfc4  %>% mutate(model = "4")) %>% 
  pivot_longer(names_to = "var", values_to = "result", c(-model, -contam.pfas)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = model, values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         var == "pov_status_co" ~ ">20% of county below pov line",
                         TRUE ~ var))

write_csv(mod_comp_hcfc, "results/output/HCFC-22 regression comparison model.csv")


mod_comp_pfas <- bind_rows(clean_reg_dem_pfas %>% mutate(model = "1"), 
                           clean_reg_dem_pfas2  %>% mutate(model = "2"),
                           clean_reg_dem_pfas3  %>% mutate(model = "3"),
                           clean_reg_dem_pfas4  %>% mutate(model = "4")) %>% 
  pivot_longer(names_to = "var", values_to = "result", c(-model, -contam.pfas)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = model, values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         var == "pov_status_co" ~ ">20% of county below pov line",
                         TRUE ~ var))

write_csv(mod_comp_pfas, "results/output/PFAS regression comparison model.csv")




#### STRATIFY BY SYSTEM SIZE

model_fun <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+mdi+
             as.factor(source_type), 
           data = dat, family = binomial()))
}
model_fun2 <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+
             as.factor(source_type), 
           data = dat, family = binomial()))
}
model_fun3 <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+perc_uninsur+
             as.factor(source_type), 
           data = dat, family = binomial()))
}
model_fun4 <- function(dat){
  tidy(glm(detchem ~
             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+pov_status_co+
             as.factor(source_type), 
           data = dat, family = binomial()))
}

det_mod_evrdet_size<- ucmrdf.mod %>%
  filter(contam.pfas == "evrdet")%>% 
  group_by(sys.size) %>% 
  nest() %>% 
  mutate(new_result1 = purrr::map(data, model_fun),
         new_result2 = purrr::map(data, model_fun2),
         new_result3 = purrr::map(data, model_fun3),
         new_result4 = purrr::map(data, model_fun4)) %>% 
  mutate(`1` = purrr::map(new_result1, mod2df),
         `2` = purrr::map(new_result2, mod2df),
         `3` = purrr::map(new_result3, mod2df),
         `4` = purrr::map(new_result4, mod2df)) %>% 
  select(-data, -new_result1, -new_result2, -new_result3, -new_result4) %>% 
  pivot_longer(names_to = "model", values_to = "result", cols = c(`1`:`4`)) %>% 
  unnest(result) %>%
  pivot_longer(names_to = "var", values_to = "result", c(-sys.size, -model)) %>% 
  mutate(result = case_when(is.na(result) ~ "",
                            TRUE ~ result)) %>% 
  pivot_wider(names_from = c(model, sys.size), values_from = result) %>% 
  mutate(var = case_when(var == "perc_hisp_any" ~ "Percent Hispanic",
                         var == "perc_black_nohisp" ~ "Percent Black",
                         var == "perc_pov_ppl" ~ "Percent Poverty",
                         var == "perc_hmown" ~  "Percent Homeownership",
                         var == "propurban" ~ "Percent Urban Households",
                         var == "mdi" ~ "MDI Rate",
                         var == "perc_uninsur" ~ "Percent Uninsured",
                         var == "as.factor(Size)S" ~ "Size (Small)",
                         var == "as.factor(source_type)GU" ~ "Water Source (GU)",
                         var == "as.factor(source_type)GW" ~ "Water Source (GW)",
                         var == "as.factor(source_type)SW" ~ "Water Source (SW)",
                         var == "as.factor(source_type)MX" ~ "Water Source (MX)",
                         var == "WWTP_ML_km2" ~ "WWTP total flow per area (million L per km^2)",
                         var == "bin_14DIOXANE" ~ "1,4-Dioxane TRI facility (>=1 in county)",
                         var == "bin_CHLORINATED_SOLVENTS" ~ "Chlorinated Solvent TRI facility (>=1 in county)",
                         var == "bin_CFCs" ~ "CFC TRI facility (>=1 in county)",
                         var == "airportMFTA_bin" ~ "Certified airports (>=1 in county) or Military fire-training areas (>=1 in county)",
                         var == "n_epastewardship_bin" ~ "Major PFAS industrial facilities (>=1 in county)",
                         TRUE ~ var))


write_csv(det_mod_evrdet_size, "results/output/Any UCMR detection model stratified by system size.csv")

# decided against
reg_dem_evrdet2<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+
                        as.factor(Size)+as.factor(source_type), 
                      data = det_mod_evrdet, family = binomial())

reg_dem_evrdet3<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+perc_uninsur+
                        as.factor(Size)+as.factor(source_type), 
                      data = det_mod_evrdet, family = binomial())
reg_dem_evrdet4<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+pov_status_co+
                        as.factor(Size)+as.factor(source_type), 
                      data = det_mod_evrdet, family = binomial())

reg_dem_evrdet5<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+propurban+mdi+
                        as.factor(Size)+as.factor(source_type), 
                      data = det_mod_evrdet, family = binomial())

reg_dem_evrdet6<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+propurban+mdi+perc_hmown+
                        as.factor(Size)+as.factor(source_type) + as.factor(bin_TRI) + WWTP_ML_km2, 
                      data = det_mod_evrdet, family = binomial())

reg_dem_evrdet7<- glm(detchem ~# WWTP_ML_km2 + `bin_14DIOXANE` +  airportMFTA_bin + n_epastewardship_bin + 
                        perc_hisp_any+perc_black_nohisp+propurban+mdi+perc_hmown+
                        as.factor(Size)+as.factor(source_type), 
                      data = det_mod_evrdet, family = binomial())

clean_reg_dem_evrdet2 <- mod2df(tidy(reg_dem_evrdet2)) %>% 
  mutate(contam.pfas = "evrdet")

clean_reg_dem_evrdet3 <- mod2df(tidy(reg_dem_evrdet3)) %>% 
  mutate(contam.pfas = "evrdet")

clean_reg_dem_evrdet4 <- mod2df(tidy(reg_dem_evrdet4)) %>% 
  mutate(contam.pfas = "evrdet")

clean_reg_dem_evrdet5 <- mod2df(tidy(reg_dem_evrdet5)) %>% 
  mutate(contam.pfas = "evrdet")

clean_reg_dem_evrdet6 <- mod2df(tidy(reg_dem_evrdet6)) %>% 
  mutate(contam.pfas = "evrdet")

clean_reg_dem_evrdet7 <- mod2df(tidy(reg_dem_evrdet7)) %>% 
  mutate(contam.pfas = "evrdet")







# STOP HERE -----------------------------------------------------------

#everything below has not been reviewed recently 



# ucmrdf.test_pfas_corr <- ucmrdf.test_pfas[ucmrdf.test_pfas$contam.pfas=="PFAS" &
#                                           !is.na(ucmrdf.test_pfas$geography),] %>%
#   select("PWSID", "contam.pfas", "geography", "perc_hisp_any", "propurban", 
#          "perc_hmown","perc_black_nohisp", "perc_pov_ppl", "mdi", 
#          "n_epastewardship_bin", "n_MFTA_bin", "n_airports_bin",
#          "WWTP_ML_km2") %>%
#   unique() %>% 
#   rename("Percent Hispanic" = "perc_hisp_any",
#          "Percent Black" = "perc_black_nohisp",
#          "Percent Urban" = "propurban",
#          "Percent Homeownership" = "perc_hmown",
#          "Percent Poverty" = "perc_pov_ppl")

# cor.matrix_pfas <- cor(ucmrdf.test_pfas_corr[,4:12], method = "spearman",
#                   use = "complete.obs")

# 
# melt(get_upper_tri(cor.matrix_pfas)) %>%
# ggplot(aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile() + 
#   geom_text(aes(label = round(value,2))) + 
#   scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
#                        low = "#29af7f", high =  "#b8de29", mid = "white", 
#                        name = "Cor value") + 
#   scale_x_discrete(position = "top") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size=12),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         legend.text = element_text(size=12)) +
#   xlab("")+
#   ylab("")

#ggsave("results/output/PFAS spearman corr plot.png", width = 15, height = 10, units = "in")



# 
# ucmrdf.test_tri_corr <- ucmrdf.mod[ucmrdf.mod$contam.pfas=="1,4-dioxane",] %>%
#   filter(!PWSID %in% empty_test_chem) %>% 
#   select("PWSID", "contam.pfas", "perc_hisp_any", "propurban", "mdi",
#          "perc_hmown","perc_black_nohisp", "perc_pov_ppl", "WWTP_ML_km2",
#          "bin_14DIOXANE", "bin_CHLORINATED_SOLVENTS", "bin_111TRICHLOROETHANE", "bin_ETHYLIDENE_DICHLORIDE",
#          "bin_CHLORODIFLUOROMETHANE", "bin_DICHLORODIFLUOROMETHANE", "bin_CFCs") %>%
#   mutate(WWTP_ML_km2 = case_when(is.na(WWTP_ML_km2) ~ 0,
#                                  TRUE ~ WWTP_ML_km2)) %>% 
#   unique() %>% 
#   rename("Percent Hispanic" = "perc_hisp_any",
#          "Percent Black" = "perc_black_nohisp",
#          "Percent Urban" = "propurban",
#          "Percent \nHomeownership" = "perc_hmown",
#          "Percent Poverty" = "perc_pov_ppl",
#          "MDI Rate" = "mdi", 
#          "Has Chlorinated \nSolvents" = bin_CHLORINATED_SOLVENTS,
#          "Has \n1,1,1-Trichloroethane" = "bin_111TRICHLOROETHANE",
#          "Has \n1,1-Dichloroethane" = "bin_ETHYLIDENE_DICHLORIDE",
#          "Has HCFC-22" = bin_CHLORODIFLUOROMETHANE,
#          "Has 1,4-Dioxane" = bin_14DIOXANE,
#          "Has CFCs" = bin_CFCs,
#          "Has CFC-12" = bin_DICHLORODIFLUOROMETHANE)
# 
# cor.matrix_tri <- cor(ucmrdf.test_tri_corr[,3:16], method = "spearman",
#                   use = "complete.obs") 
# 
# melt(get_upper_tri(cor.matrix_tri)) %>%
# ggplot(aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile() + 
#   geom_text(aes(label = round(value,2))) + 
#   #scale_fill_binned(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), type = "viridis") + 
#   scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1), 
#                     low = "#29af7f", high =  "#b8de29", mid = "white",
#                     name = "Cor value") + 
#   scale_x_discrete(position = "top") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.text.y = element_text(size=12),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         legend.text = element_text(size=12)) +
#   xlab("")+
#   ylab("")


################################################################################
# 4a. PFAS PRELIMINARY MODELS AND HISTOGRAMS ######
################################################################################
# 
# 
# ### variables to include in model 
# modcols_pfas <- c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any", "mdi",
#              "perc_hmown", "propurban", "Size", "WS.GW_SW_CODE", "source_type","n_WWTP",
#              "WWTP_ML_km2", "n_epastewardship", "n_epastewardship_bin",
#              "n_MFTA", "n_MFTA_bin", "n_airports", "n_airports_bin","WWTP_totalflow_mgd")
# 
# 
# # run all variables through the model one at a time by contaminant
# crude_list_pfas <- list()
# crude_all_pfas <- data.frame()
# 
# for(i in 1:length(modcols_pfas)) {
#   
#   #run model
#   crude_ind <- ucmrdf.test_pfas %>% 
#     filter(contam.pfas == "PFAS") 
#   
#   mod_crude <- tidy(glm(paste("detchem ~", modcols_pfas[[i]]), data = crude_ind, family = binomial())) %>% 
#     modtidy()
# 
#     # mutate(result = map(model, modtidy)) %>% 
#     # mutate(result_modify = map(model, crude2df)) %>% 
#     # unnest(cols = c(result, result_modify)) %>% 
#     # select(-data, -model)
#     # 
#   crude_list_pfas[[i]] <- mod_crude
#   
# }
# 
# crude_all_pfas <- bind_rows(crude_list_pfas) 
# 
# crude_table_pfas <- crude_all_pfas %>% 
#   mutate(result_raw = case_when(estimate < 0 ~ paste0("-", p.value),
#                                 estimate > 0 ~ paste0("+", p.value))) %>% 
#   select(term, result_raw)
# 
# write_csv(crude_table_pfas, "results/output/PFAS crude regressions.csv")
# 
# ################################################################################
# # 4b. PFAS REGRESSIONS######
# ################################################################################
# 
# 
# 
# ##### For now, because of NAs in "geography" and several contaminants in "contam.pfas",
# ##### create an interim dataframe for all regressions with only the necessary columns to keep the number of observations constant
# pfasdata_reg <- ucmrdf.test_pfas[ucmrdf.test_pfas$contam.pfas =="PFAS",] %>% 
#   select("PWSID","contam.pfas", "geography","fullstate", "region_usgs", "Size","WS.GW_SW_CODE",
#          "perc_hisp_any", "propurban", "perc_hmown", "mdi", "source_type",
#          "land.area","perc_black_nohisp", "n_WWTP", "n_epastewardship", "n_epastewardship_bin",
#          "n_MFTA", "n_MFTA_bin", "n_airports", "n_airports_bin", "perc_pov_ppl","WWTP_totalflow_mgd", 
#          "WWTP_ML_km2", "detchem", "hlvlchem")
# 
# ##### brief looks at associations between outcomes and each point source variable
# # WWTPs
# 
# # # univariate regressions; note: all are log odds below
# # reg_quartile_WWTP <- glm(detchem ~ as.factor(n_quartile_WWTP), data=pfasdata_reg,
# #                          family="binomial") # significant increase in odds only with 1 vs 2nd quartile, but not 1st vs
# # # other quartiles; "continuous" control of quartiles are not significant
# # summary(reg_quartile_WWTP)
# 
# reg_WWTP <- glm(detchem ~ n_WWTP, data=pfasdata_reg, family="binomial") 
# # each increase in WWTPs within the county is positively associated
# summary(reg_WWTP)
# 
# reg_WWTP_MLkm2 <- glm(detchem ~ WWTP_ML_km2, data=pfasdata_reg,
#                       family="binomial") # association with flow per area of county
# summary(reg_WWTP_MLkm2)
# 
# # reg_WWTP_MLkm2_quartile <- glm(detchem ~ as.factor(MLkm2_quartile_WWTP), data=pfasdata_reg,
# #                                family="binomial") # quartiles of flow/area (compared to Q1) are positively associated with detection
# # # proceed with using continuous control of flow over number of WWTPs
# # summary(reg_WWTP_MLkm2_quartile)
# 
# ## MFTAs
# tapply(pfasdata_reg$detchem, pfasdata_reg$n_MFTA,mean)
# 
# reg_MFTA <- glm(detchem ~ n_MFTA, data=pfasdata_reg,
#                 family="binomial")
# summary(reg_MFTA)
# 
# ## EPA Stewardship
# tapply(pfasdata_reg$detchem, pfasdata_reg$n_epastewardship, mean)
# 
# reg_EPAS <- glm(detchem ~ n_epastewardship, data=pfasdata_reg,
#                 family="binomial")
# summary(reg_EPAS)
# 
# ## Airports
# tapply(pfasdata_reg$detchem, pfasdata_reg$n_airports, mean)
# 
# reg_airports <- glm(detchem ~ n_airports, data=pfasdata_reg,
#                     family="binomial")
# summary(reg_airports)
# 
# # preliminary regressions using only demo and only point source data
# reg_dem <- glm(detchem ~ as.factor(fullstate)+as.factor(Size)+as.factor(WS.GW_SW_CODE)+
#                  perc_hisp_any+perc_black_nohisp+perc_hmown+perc_pov_ppl+propurban,
#                data=pfasdata_reg, 
#                family="binomial") # demo model with region fixed effect
# summary(reg_dem)
# 
# # preliminary adjusted model (demographics, system (including state), and point source covariates)
# adj_model_detchem <- glm(detchem ~ WWTP_ML_km2+n_epastewardship_bin + n_MFTA_bin + n_airports_bin +
#                            perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+
#                            as.factor(fullstate)+as.factor(Size)+as.factor(WS.GW_SW_CODE),
#                          data = pfasdata_reg,
#                          family="binomial")
# 
# summary(adj_model_detchem)
# exp(adj_model_detchem$coefficients)
# 
# # adjusted model without state or region_usgs
# adj_model_detchem2 <- glm(detchem ~ WWTP_ML_km2+n_epastewardship_bin+n_MFTA_bin+n_airports_bin+
#                             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+perc_pov_ppl+
#                             as.factor(Size)+as.factor(WS.GW_SW_CODE),
#                           data = pfasdata_reg,
#                           family="binomial")
# 
# summary(adj_model_detchem2)
# exp(adj_model_detchem2$coefficients)
# 
# # indicator combining airports and MFTA binary indicators based on co-linearity between the two
# pfasdata_reg$airportMFTA_bin <- with(pfasdata_reg, ifelse((n_airports_bin == 1 | n_MFTA_bin == 1), 1,0))
# 
# adj_model_detchem3 <- glm(detchem ~ WWTP_ML_km2+n_epastewardship_bin+airportMFTA_bin+
#                             perc_hisp_any+perc_black_nohisp+perc_hmown+propurban+mdi + perc_pov_ppl+
#                             as.factor(Size)+as.factor(source_type),
#                           data = pfasdata_reg,
#                           family="binomial")
# 
# summary(adj_model_detchem3)
# 
# tidy_reg_dem_pfas <- modtidy(tidy(adj_model_detchem3))
# clean_reg_dem_pfas <- mod2df(tidy(adj_model_detchem3)) %>% 
#   mutate(contam.pfas = "PFAS")





##################################################################################################
#### 7. CRUDE ANALYSIS ####
##################################################################################################

### variables to include in model 
modcols <- c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any",
                       "perc_hmown", "propurban", "Size", "WS.GW_SW_CODE", "n.samp", "region_census", "region_usgs")


crude_mod <- function(dat, key){
  tidy(glm(paste("key ~", modcols[[i]]), data = dat, family = binomial()))
  
}

# run all variables through the model one at a time by contaminant
crude_list <- list()
stront_list <- list()


for(i in 1:length(modcols)) {
  
  #run model
  crude_all <- ucmrdf.test %>% 
    gather(resp, key, detchem:evrovrhlvl) %>% 
    mutate(contam.pfas = case_when(resp == "evrovrhlvl" ~ "any",
                                   TRUE ~ contam.pfas)) %>% 
    group_by(contam.pfas, resp) %>%
    nest() %>% 
    subset((contam.pfas %in% ucmrchemtest$contam.pfas[ucmrchemtest$detection.test == "Y"] & resp == "detchem") |
             (contam.pfas %in% c(ucmrchemtest$contam.pfas[ucmrchemtest$healthlvl.test == "Y"], "PFAS") & resp == "hlvlchem") |
             (resp == "evrovrhlvl")) %>% 
    mutate(model = map(data, crude_mod)) %>%
    mutate(result = map(model, mod2df)) %>% 
    unnest(contam.pfas, resp, result, .drop = TRUE) %>%
    mutate(resp = case_when(resp == "detchem" ~ "Detection (Y/N)",
                            resp == "hlvlchem" ~ "Health level (over/not)",
                            resp == "evrovrhlvl" ~ "Sys ever over Health level (over/not)"))
  
  
  crude_list[[i]] <- crude_all
  
  strontcrude <- lm(paste("avg.strontium ~", modcols[[i]]), data = strontiumavgs) %>%
    tidy() %>% 
    mod2df() %>%
    mutate(Model = "[crude] Continuous",
           contam.pfas = "strontium")
  stront_list[[i]] <- strontcrude
  
}

# paste together
ucmrcrude_all <- bind_rows(crude_list, stront_list) %>%
  split(f = .$contam.pfas)


for(i in names(ucmrcrude_all)){
  detchem <- i
  ucmrcrude_indvdf <- ucmrcrude_all[[i]]
  write_csv(ucmrcrude_indvdf, paste("ProjectTechnical files/UCMR3 data/R/results/preliminary/crude regressions/ucmrcrude_", detchem,"2.27.19.csv"))
}




##################################################################################################
### 8. SYSTEM CHARACTERISTICS ####
##################################################################################################


demobreakdown <- function(dat) {
  dat %>%
    summarize(
      n.sys = length(unique(PWSID)),
      #n.ppl = sum(WS.POPULATION_SERVED_COUNT),
      med.black = round(median(perc_black_nohisp, na.rm = TRUE),1), 
      med.hisp = round(median(perc_hisp_any, na.rm = TRUE),1),
      med.pov = round(median(perc_pov_ppl, na.rm = TRUE),1),
      med.hmown = round(median(perc_hmown, na.rm = TRUE),1),
      med.urb = round(median(propurban, na.rm = TRUE),1),
      prop.small = round(length(unique(PWSID[which(Size == "S")]))/length(Size)*100, 2),
      prop.SW = round(length(unique(PWSID[which(WS.GW_SW_CODE == "SW")]))/length(WS.GW_SW_CODE)*100, 2)
    )
}

region_census_brkdwn <- ucmrdf.test %>% group_by(region_census) %>%
  demobreakdown() %>% mutate(brktype = "region_census")

region_usgs_brkdwn <- ucmrdf.test %>% group_by(region_usgs) %>%
  demobreakdown() %>% mutate(brktype = "region_usgs")

overall_brkdwn <- ucmrdf.test %>% demobreakdown() %>% mutate(brktype = "overall")

sys.size_brkdwn <- ucmrdf.test %>% group_by(Size) %>%
  demobreakdown() %>% mutate(brktype = "Size")

source_brkdwn <- ucmrdf.test %>% group_by(WS.GW_SW_CODE) %>%
  demobreakdown() %>% mutate(brktype = "Source Water")

all_brkdwn <- bind_rows(overall_brkdwn, region_usgs_brkdwn, region_census_brkdwn,sys.size_brkdwn, source_brkdwn)

write_csv(all_brkdwn, "ProjectTechnical files/UCMR3 data/R/results/preliminary/system characteristics by category.csv")


ucmrdf.test %>% gather(modcol, value, n.samp:propurban) %>%
  ggplot(aes(x = region, y = value)) + 
  geom_boxplot() + facet_wrap(~modcol, scales = "free")


##############################################################
### dump from PFAS demographic analysis #####
##############################################################



#### create table 2 for hlvlchem and evr ovr hlvlchem

ovrhlvlsumm <-
  ucmrdf.test %>%
  subset(contam.pfas %in% c(ucmrchemtest$contam.pfas[ucmrchemtest$healthlvl.test == "Y"], "PFAS")) %>%
  select(hlvlchem, contam.pfas, PWSID, perc_black_nohisp, perc_pov_ppl, perc_hisp_any, perc_hmown, propurban) %>%
  gather(variable, value, perc_black_nohisp:propurban) %>%
  group_by(contam.pfas, variable) %>%
  summarize(
    ovrall.avg = mean(value),
    underhlvl.avg = mean(value[which(hlvlchem == 0)]),
    ovrhlvl.avg = mean(value[which(hlvlchem == 1)]),
    underhlvl.var = var(value[which(hlvlchem == 0)]),
    ovrhlvl.var = var(value[which(hlvlchem == 1)]),
    underhlvl.n = length(unique(PWSID[which(hlvlchem == 0)])),
    ovrhlvl.n = length(unique(PWSID[which(hlvlchem == 1)]))
  ) %>%
  mutate(welch.t = round(((ovrhlvl.avg - underhlvl.avg)/sqrt((ovrhlvl.var/ovrhlvl.n) + (underhlvl.var/underhlvl.n))), 3),
         df = round(((ovrhlvl.var/ovrhlvl.n + underhlvl.var/underhlvl.n)^2/((ovrhlvl.var^2)/(ovrhlvl.n^2*(ovrhlvl.n-1)) + (underhlvl.var^2)/(underhlvl.n^2*(underhlvl.n-1)))), 3),
         pval = round(((2*pt(abs(welch.t), df, lower=FALSE))), 3))

write_csv(ovrhlvlsumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by exceedence of UCMR HLVL_03.01.19.csv")

evrovrhlvlsumm <-
  ucmrdf.test %>%
  subset(contam.pfas %in% c(ucmrchemtest$contam.pfas[ucmrchemtest$healthlvl.test == "Y"], "PFAS")) %>%
  select(evrovrhlvl, contam.pfas, PWSID, perc_black_nohisp, perc_pov_ppl, perc_hisp_any, perc_hmown, propurban) %>%
  gather(variable, value, perc_black_nohisp:propurban) %>%
  group_by(variable) %>%
  summarize(
    ovrall.avg = mean(value),
    underhlvl.avg = mean(value[which(evrovrhlvl == 0)]),
    ovrhlvl.avg = mean(value[which(evrovrhlvl == 1)]),
    underhlvl.var = var(value[which(evrovrhlvl == 0)]),
    ovrhlvl.var = var(value[which(evrovrhlvl == 1)]),
    underhlvl.n = length(unique(PWSID[which(evrovrhlvl == 0)])),
    ovrhlvl.n = length(unique(PWSID[which(evrovrhlvl == 1)]))
  ) %>%
  mutate(welch.t = round(((ovrhlvl.avg - underhlvl.avg)/sqrt((ovrhlvl.var/ovrhlvl.n) + (underhlvl.var/underhlvl.n))), 3),
         df = round(((ovrhlvl.var/ovrhlvl.n + underhlvl.var/underhlvl.n)^2/((ovrhlvl.var^2)/(ovrhlvl.n^2*(ovrhlvl.n-1)) + (underhlvl.var^2)/(underhlvl.n^2*(underhlvl.n-1)))), 3),
         pval = round(((2*pt(abs(welch.t), df, lower=FALSE))), 3))


write_csv(evrovrhlvlsumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by any exceedence of UCMR HLVL (not chem specific)_03.01.19.csv")



#pfas detect summary stat

PFASdetsys <- unique(ucmr3$PWSID[!is.na(ucmr3$AnalyticalResultsValue) & ucmr3$AnalyticalResultsValue > 0 & ucmr3$contam.pfas == "PFAS"])
PFASdet.p1 <- unique(ucmr3[ucmr3$contam.pfas == "PFAS", c("PWSID", "Size")])
PFASdet.p1$detchem <- ifelse(PFASdet.p1$PWSID %in% PFASdetsys, 1, 0)
PFASdet.p2 <- merge(PFASdet.p1, nsampcontam[nsampcontam$contam.pfas == "PFAS",], by = "PWSID")
PFASdet <- merge(PFASdet.p2, alldemo, by = "PWSID")
PFASdet$state <- substr(PFASdet$PWSID, 1, 2)
PFASdet$sys.size <- relevel(as.factor(PFASdet$sys.size), ref = "V. Small")
PFASdet$propurban <- PFASdet$propurban*100

colstomelt <- colnames(PFASdet)[!colnames(PFASdet) %in% c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any", "perc_hmown", "propurban")]

meltpfasdet <- melt(PFASdet, id.vars = colstomelt)

pfasdetsumm <-
  group_by(meltpfasdet, variable) %>%
  summarize(
    ovrall.avg = mean(value),
    notdet.avg = mean(value[which(detchem == 0)]),
    det.avg = mean(value[which(detchem == 1)]),
    notdet.var = var(value[which(detchem == 0)]),
    det.var = var(value[which(detchem == 1)]),
    notdet.n = length(value[which(detchem == 0)]),
    det.n = length(value[which(detchem == 1)])
  )

pfasdetsumm$welch.t <- with(pfasdetsumm,(det.avg - notdet.avg)/sqrt((det.var/det.n) + (notdet.var/notdet.n)))

pfasdetsumm$df <- with(pfasdetsumm, (det.var/det.n + notdet.var/notdet.n)^2/((det.var^2)/(det.n^2*(det.n-1)) + (notdet.var^2)/(notdet.n^2*(notdet.n-1))))

pfasdetsumm$pval <- with(pfasdetsumm, 2*pt(abs(welch.t), df, lower=FALSE))

#write.csv(pfasdetsumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by detection of PFAS_02.08.19.csv", row.names = FALSE)


#now dioxane, vanadium, strontium summary statistics by detection 
dioxanesys <- unique(ucmr3$PWSID[!is.na(ucmr3$AnalyticalResultsValue) & ucmr3$AnalyticalResultsValue > 0.35 & ucmr3$contam.pfas == "1,4-dioxane"])
dioxanedf <- unique(ucmr3[ucmr3$contam.pfas == "1,4-dioxane", c("PWSID", "Size")])
dioxanedf$ovrdioxane <- ifelse(dioxanedf$PWSID %in% dioxanesys, 1, 0)
dioxane.demo <- merge(dioxanedf, alldemo, by = "PWSID")
dioxane.demo$state <- substr(dioxane.demo$PWSID, 1, 2)
dioxane.demo$sys.size <- relevel(as.factor(dioxane.demo$sys.size), ref = "V. Small")
dioxane.demo$propurban <- dioxane.demo$propurban*100

colstomelt <- colnames(dioxane.demo)[!colnames(dioxane.demo) %in% c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any", "perc_hmown", "propurban")]

meltdioxanedemo <- melt(dioxane.demo, id.vars = colstomelt)

ovrdioxanesumm <-
  group_by(meltdioxanedemo, variable) %>%
  summarize(
    ovrall.avg = mean(value),
    underdioxane.avg = mean(value[which(ovrdioxane == 0)]),
    ovrdioxane.avg = mean(value[which(ovrdioxane == 1)]),
    underdioxane.var = var(value[which(ovrdioxane == 0)]),
    ovrdioxane.var = var(value[which(ovrdioxane == 1)]),
    underdioxane.n = length(value[which(ovrdioxane == 0)]),
    ovrdioxane.n = length(value[which(ovrdioxane == 1)])
  )

ovrdioxanesumm$welch.t <- with(ovrdioxanesumm,(ovrdioxane.avg - underdioxane.avg)/sqrt((ovrdioxane.var/ovrdioxane.n) + (underdioxane.var/underdioxane.n)))

ovrdioxanesumm$df <- with(ovrdioxanesumm, (ovrdioxane.var/ovrdioxane.n + underdioxane.var/underdioxane.n)^2/((ovrdioxane.var^2)/(ovrdioxane.n^2*(ovrdioxane.n-1)) + (underdioxane.var^2)/(underdioxane.n^2*(underdioxane.n-1))))

ovrdioxanesumm$pval <- with(ovrdioxanesumm, 2*pt(abs(welch.t), df, lower=FALSE))

#write.csv(ovrdioxanesumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by exceedence of UCMR dioxane_02.08.19.csv", row.names = FALSE)

vanadiumsys <- unique(ucmr3$PWSID[!is.na(ucmr3$AnalyticalResultsValue) & ucmr3$AnalyticalResultsValue > 21 & ucmr3$contam.pfas == "vanadium"])
vanadiumdf <- unique(ucmr3[ucmr3$contam.pfas == "vanadium", c("PWSID", "Size")])
vanadiumdf$ovrvanadium <- ifelse(vanadiumdf$PWSID %in% vanadiumsys, 1, 0)
vanadium.demo <- merge(vanadiumdf, alldemo, by = "PWSID")
vanadium.demo$state <- substr(vanadium.demo$PWSID, 1, 2)
vanadium.demo$sys.size <- relevel(as.factor(vanadium.demo$sys.size), ref = "V. Small")
vanadium.demo$propurban <- vanadium.demo$propurban*100

colstomelt <- colnames(vanadium.demo)[!colnames(vanadium.demo) %in% c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any", "perc_hmown", "propurban")]

meltvanadiumdemo <- melt(vanadium.demo, id.vars = colstomelt)

ovrvanadiumsumm <-
  group_by(meltvanadiumdemo, variable) %>%
  summarize(
    ovrall.avg = mean(value),
    undervanadium.avg = mean(value[which(ovrvanadium == 0)]),
    ovrvanadium.avg = mean(value[which(ovrvanadium == 1)]),
    undervanadium.var = var(value[which(ovrvanadium == 0)]),
    ovrvanadium.var = var(value[which(ovrvanadium == 1)]),
    undervanadium.n = length(value[which(ovrvanadium == 0)]),
    ovrvanadium.n = length(value[which(ovrvanadium == 1)])
  )

ovrvanadiumsumm$welch.t <- with(ovrvanadiumsumm,(ovrvanadium.avg - undervanadium.avg)/sqrt((ovrvanadium.var/ovrvanadium.n) + (undervanadium.var/undervanadium.n)))

ovrvanadiumsumm$df <- with(ovrvanadiumsumm, (ovrvanadium.var/ovrvanadium.n + undervanadium.var/undervanadium.n)^2/((ovrvanadium.var^2)/(ovrvanadium.n^2*(ovrvanadium.n-1)) + (undervanadium.var^2)/(undervanadium.n^2*(undervanadium.n-1))))

ovrvanadiumsumm$pval <- with(ovrvanadiumsumm, 2*pt(abs(welch.t), df, lower=FALSE))

#write.csv(ovrvanadiumsumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by exceedence of UCMR vanadium_02.08.19.csv", row.names = FALSE)

strontiumsys <- unique(ucmr3$PWSID[!is.na(ucmr3$AnalyticalResultsValue) & ucmr3$AnalyticalResultsValue > 1500 & ucmr3$contam.pfas == "strontium"])
strontiumdf <- unique(ucmr3[ucmr3$contam.pfas == "strontium", c("PWSID", "Size")])
strontiumdf$ovrstrontium <- ifelse(strontiumdf$PWSID %in% strontiumsys, 1, 0)
strontium.demo <- merge(strontiumdf, alldemo, by = "PWSID")
strontium.demo$state <- substr(strontium.demo$PWSID, 1, 2)
strontium.demo$sys.size <- relevel(as.factor(strontium.demo$sys.size), ref = "V. Small")
strontium.demo$propurban <- strontium.demo$propurban*100

colstomelt <- colnames(strontium.demo)[!colnames(strontium.demo) %in% c("perc_black_nohisp", "perc_pov_ppl", "perc_hisp_any", "perc_hmown", "propurban")]

meltstrontiumdemo <- melt(strontium.demo, id.vars = colstomelt)

ovrstrontiumsumm <-
  group_by(meltstrontiumdemo, variable) %>%
  summarize(
    ovrall.avg = mean(value),
    understrontium.avg = mean(value[which(ovrstrontium == 0)]),
    ovrstrontium.avg = mean(value[which(ovrstrontium == 1)]),
    understrontium.var = var(value[which(ovrstrontium == 0)]),
    ovrstrontium.var = var(value[which(ovrstrontium == 1)]),
    understrontium.n = length(value[which(ovrstrontium == 0)]),
    ovrstrontium.n = length(value[which(ovrstrontium == 1)])
  )

ovrstrontiumsumm$welch.t <- with(ovrstrontiumsumm,(ovrstrontium.avg - understrontium.avg)/sqrt((ovrstrontium.var/ovrstrontium.n) + (understrontium.var/understrontium.n)))

ovrstrontiumsumm$df <- with(ovrstrontiumsumm, (ovrstrontium.var/ovrstrontium.n + understrontium.var/understrontium.n)^2/((ovrstrontium.var^2)/(ovrstrontium.n^2*(ovrstrontium.n-1)) + (understrontium.var^2)/(understrontium.n^2*(understrontium.n-1))))

ovrstrontiumsumm$pval <- with(ovrstrontiumsumm, 2*pt(abs(welch.t), df, lower=FALSE))

#write.csv(ovrstrontiumsumm, "ProjectTechnical files/UCMR3 data/R/results/preliminary/summary statistics by exceedence of UCMR strontium_02.08.19.csv", row.names = FALSE)




############################################
#### 10. Spearman correlation coefficient #####
##########################################


### edited 8.28.18 by ABH
### adding Spearman correlation coefficient for all systems

cndemo <- read.csv("Casey Foundation/data analysis/data/R scraps/sdwis cnserv demo and syschars.csv")
stateregs <- read.csv("Casey Foundation/data analysis/data/originals/Geography data/US state regions.csv")




