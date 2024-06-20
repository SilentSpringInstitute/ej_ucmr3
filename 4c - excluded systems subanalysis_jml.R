### AUTHOR: AM
### WRITTEN: 2023-02-18
### LAST REVIEW: 2023-03-05
### WRITTEN IN: R version: 4.2.2
### Purpose: Analyze detection frequencies among 105 excluded systems in tribes
### and territories

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)
source("1 - UCMR loading and processing.R")

# load script 4 so that we can remove the PWSs with missing MDIs
source("4a - UCMR Summary.R")

# drop PWSIDs that are missing MDI 
ucmr3_targetsamples_included <- ucmr3_targetsamples %>% 
  filter(!PWSID %in% missing_mdi)

ucmr_detcode_included <- ucmr_detcode %>% 
  filter(!PWSID %in% missing_mdi)

#### 6. rerun above but on excluded systems ####################################

# clean UCMR data: add column that groups PFAS into one contaminant, add geo info, 
# filter to chems in ucmrchemtest (from 2019 plans) and then to only 1,4-dioxane, 1-1,dichloroethane, and PFAS (for 2020 plans)
ucmr3_targetsamples_exclude <- 
  ucmr3_raw %>% 
  mutate(contam.pfas = case_when(Contaminant %in% c("PFBS", "PFHpA", "PFOA", "PFOS", "PFNA", "PFHxS") ~ "PFAS",
                                 TRUE ~ Contaminant),
         state = substr(PWSID,1,2),
         sys.sampid = paste0(PWSID, "-", SampleID)) %>% 
  mutate(detchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue > 0) ~ 1,
                             TRUE ~ 0),
         hlvlchem = case_when((!is.na(AnalyticalResultsValue) & AnalyticalResultsValue >= .35 & contam.pfas == "1,4-dioxane") ~ 1,
                              (!is.na(AnalyticalResultsValue) & AnalyticalResultsValue >= 6.14 & contam.pfas == "1,1-dichloroethane") ~ 1,
                              TRUE ~ 0)) %>%
  filter((state %in% c("NN", "PR", "01", "05", "06", "08", "09", "10", "GU", "MP", "VI", "AS"))) %>% 
  filter(contam.pfas %in% c("1,4-dioxane", "1,1-dichloroethane", "PFAS", "HCFC-22")) %>% 
  filter(MonitoringRequirement == "AM")

# how many PWSIDs are from tribes or territories?
ucmr3_targetsamples_exclude %>%
  mutate(tribe_or_terr = case_when(state %in% c("01", "05", "06", "08", "09", "10", "NN") ~ "tribe", 
                                   TRUE ~ "territory")) %>%
  group_by(tribe_or_terr) %>%
  summarise(length(unique(PWSID)))

#get IDs of systems in UCMR (for other scripts)
ucmrsys_exclude <- unique(ucmr3_targetsamples_exclude$PWSID)
# write_csv(as.data.frame(ucmrsys_exclude), "results/output/excluded_ucmr_systems_IDs.csv")

# get PFAS detection frequencies for excluded systems
pfospfoasum_exclude <- ucmr3_targetsamples_exclude %>% 
  subset(contam.pfas == "PFAS") %>% 
  group_by(sys.sampid, PWSID, contam.pfas) %>% 
  summarize(sum.pfospfoa = sum(AnalyticalResultsValue[which(Contaminant %in% c("PFOA", "PFOS"))], na.rm = TRUE),
            sum.detchem = case_when(sum(detchem) >= 1 ~ 1, 
                                    TRUE ~ 0),
            sum.hlvlchem = case_when(sum.pfospfoa >= 0.07 ~ 1, 
                                     TRUE ~ 0)) %>% 
  left_join(ucmr3_targetsamples_exclude %>%
              select(PWSID:SampleID, SampleEventCode:sys.sampid)) %>%
  unique() 

# drop all PFAS and then merge in pfoapfossum
ucmr3_exclude <- ucmr3_targetsamples_exclude %>% 
  filter(!contam.pfas %in% c("PFAS")) %>% 
  bind_rows(pfospfoasum_exclude %>%
              select(-sum.pfospfoa) %>% 
              rename(detchem = sum.detchem,
                     hlvlchem = sum.hlvlchem)) %>% 
  group_by(PWSID, contam.pfas) %>%
  mutate(n.samp = length(unique(sys.sampid)),
         n.date = length(unique(CollectionDate)),
         n.sampfac = length(unique(FacilityID))) %>% 
  filter(contam.pfas %in% c("PFAS", "1,1-dichloroethane", "1,4-dioxane", "HCFC-22"))


#get detection by PWS

# get 1 or 0 for various tests with one row per system and contaminant
ucmr_detcode_exclude <- ucmr3_exclude %>% 
  group_by(PWSID) %>% 
  mutate(n_watertype = length(unique(FacilityWaterType)),
         source_type = case_when(FacilityWaterType == "MX" ~ "MIX",
                                 FacilityWaterType == "GU" ~ "MIX",
                                 TRUE ~ FacilityWaterType),
         source_type = case_when(n_watertype > 1 ~ "MIX",
                                 TRUE ~ source_type)) %>%
  group_by(PWSID, contam.pfas, Size, source_type) %>%
  summarize(detchem = case_when(sum(detchem) >= 1 ~ 1,
                                TRUE ~ 0),
            hlvlchem = case_when(sum(hlvlchem) >= 1 ~ 1,
                                 TRUE ~ 0)) %>% 
  ungroup() %>% 
  evrdet()


# PWS detection frequencies --------------------------------------------

# summarize detection frequiences among included systems
det_freq_grouped <- ucmr_detcode_included %>% 
  group_by(contam.pfas) %>% 
  summarize(N = length(unique(PWSID)), 
            Ndet = length(unique(PWSID[which(detchem == "1")])), 
            detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2), 
            Nhlvl = length(unique(PWSID[which(hlvlchem == "1")])),
            hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2))

# summarize detection frequiences among excluded systems
det_freq_contaminant_exclude <- ucmr_detcode_exclude %>% 
  mutate(state = substr(PWSID, 1, 2)) %>%
  mutate(tribe_or_terr = case_when(state %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
                                   TRUE ~ "territory")) %>%
  #bind_rows(ucmr_detcode_exclude %>% mutate(tribe_or_terr = "both")) %>%
  group_by(contam.pfas, tribe_or_terr) %>%
  summarize(N = length(unique(PWSID)), 
            Ndet = length(unique(PWSID[which(detchem == "1")])), 
            detfreq_sys = round(length(unique(PWSID[which(detchem == "1")]))/length(unique(PWSID))*100, 2), 
            Nhlvl = length(unique(PWSID[which(hlvlchem == "1")])),
            hlvl_freq = round(length(unique(PWSID[which(hlvlchem == "1")]))/length(unique(PWSID))*100,2)) %>% 
  # merge with det frequencies among included systems
  bind_rows(det_freq_grouped %>% mutate(tribe_or_terr = "stateside")) %>%
  rename(Contaminant = contam.pfas)

### JML check (3/23):
JL_check <- ucmr_detcode_exclude %>% 
  mutate(state = substr(PWSID, 1, 2)) %>%
  mutate(tribe_or_terr = case_when(state %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
                                   TRUE ~ "territory")) %>%
  #bind_rows(ucmr_detcode_exclude %>% mutate(tribe_or_terr = "both")) %>%
  group_by(contam.pfas, tribe_or_terr) %>%
  summarize(N = n(), 
            Ndet = sum(detchem == "1"),
            detfreq_sys = round(Ndet / N * 100, 2), 
            Nhlvl = sum(hlvlchem == "1"),
            hlvl_freq = round(Nhlvl / N * 100, 2))
### end of check: all looks good this way

ucmr3_exclude
# write_csv(det_freq_contaminant_exclude,
#           paste0("results/output/excluded_ucmr_systems_freq_summary_",
#                  Sys.Date(), ".csv"))

########### ARCHIVED BY AM ON 2023-03-05 ###################################### 

# TableS2_format <- det_freq_contaminant_exclude %>%
#   mutate(Contaminant = case_when(Contaminant == "evrdet" ~ "≥1 UCMR contaminant", 
#                                  TRUE~Contaminant)) %>%
#   mutate(Contaminant = factor(Contaminant, 
#                               levels = c("≥1 UCMR contaminant", "1,4-dioxane",
#                                          "1,1-dichloroethane", 
#                                          "HCFC-22", 
#                                          "PFAS"))) %>%
#   arrange(Contaminant) %>%
#   flextable() %>%
#   autofit() %>%
#   #valign(valign = "top", part = "body") %>%
#   add_header_row(
#     top = T, 
#     values = c("Contaminant", 
#                "Territory or tribe", 
#                "Total number of systems", 
#                "Detected", 
#                "",
#                "Exceeded a health reference level", 
#                "")) %>%
#   set_header_labels(Contaminant = "", 
#                     N = "",
#                     tribe_or_terr = "",
#               Ndet = "N", 
#               detfreq_sys = "%", 
#               Nhlvl = "N",
#               hlvl_freq = "%") %>%
#   merge_v(j = 1) %>%
#   merge_h(i = 1:2, part = 'header') %>%
#   theme_vanilla() %>%
#   align(align = 'left', part = 'all')
# 
# # write_csv(det_freq_contaminant_exclude,
# #           paste0("results/output/excluded_ucmr_systems_freq_summary_",
# #                  Sys.Date(), ".csv"))
# 
# # calculate proportions among the cases (PWS detecting each contaminant)
# 
# terrtribes_amongdetsys <- ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                             TRUE ~ "territory")) %>%
#   bind_rows(ucmr_detcode_exclude %>% mutate(cohort = 'both tribes and terr')) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   # filter(detchem == 1) %>%
#   group_by(cohort, contam.pfas) %>%
#   summarise(n_count = length(unique(PWSID)), 
#             n_detected = length(unique(PWSID[which(detchem == "1")]))) %>%
#   group_by(contam.pfas) %>% 
#   mutate(n_total_detected = sum(n_detected), 
#          perc_represented = n_detected/n_total_detected) %>%
#   mutate(contam.pfas = factor(contam.pfas, levels = c("evrdet", "1,4-dioxane", 
#                                                       "1,1-dichloroethane", 
#                                                       "HCFC-22", "PFAS")))
# 
# terrtribes_amongdetsys_table <- terrtribes_amongdetsys %>%
#   mutate(var_name = paste0(contam.pfas, " (N=", n_total_detected, ")")) %>%
#   select(-contam.pfas, -n_total_detected, -n_count) %>%
#   mutate(perc_represented = round(100*perc_represented, 2)) %>%
#   pivot_wider(id_cols = cohort, 
#               names_from = var_name,
#               values_from = perc_represented) %>%
#   select(cohort, starts_with("evr"), 
#          starts_with("1,4-dioxane"), 
#          starts_with("1,1-dichlo"), 
#          starts_with("HCFC-22"), 
#          starts_with("PFAS"))
# 
# # write_csv(terrtribes_amongdetsys_table,
# #           paste0("results/output/exclvsincl_sys_caserepresentation_",
# #                  Sys.Date(), ".csv"))
# 
# ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                             TRUE ~ "territory")) %>%
#   # bind_rows(ucmr_detcode_exclude %>% mutate(cohort = 'both tribes and terr')) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   # filter(detchem == 1) %>%
#   group_by(cohort) %>%
#   summarise(n_count = length(unique(PWSID))) %>%
#   ungroup() %>%
#   mutate(n_total_detected = sum(n_count), 
#          perc_represented = round(100*n_count/n_total_detected,2))
# 
# mod_dat <- ucmr_detcode_exclude %>%
#   mutate(cohort = case_when(substr(PWSID, 1, 2) %in% c("01", "05", "06", "08", "09", "10") ~ "tribe", 
#                             TRUE ~ "territory")) %>%
#   bind_rows(ucmr_detcode_included %>% mutate(cohort = "stateside")) %>%
#   mutate(system_included = case_when(cohort == "stateside" ~ "included", 
#                                      TRUE ~ "excluded"), 
#          system_included = factor(system_included, levels = c("included", 
#                                                               "excluded")))
# 
# my_mod_function <- function(data){
#   glm(detchem ~ cohort, data = data, family = 'binomial')
# }
# 
# modresult1 <- mod_dat %>%
#   group_by(contam.pfas) %>%
#   nest() %>%
#   mutate(mod = map(data, my_mod_function), 
#          result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod) %>% 
#   pivot_wider(id_cols = term, 
#               names_from = contam.pfas, 
#               values_from = c(p.value, result))
# 
# my_mod_function2 <- function(data){
#   glm(detchem ~ system_included, data = data, family = 'binomial')
# }
# 
# modresult2 <- mod_dat %>%
#   group_by(contam.pfas) %>%
#   nest() %>%
#   mutate(mod = map(data, my_mod_function2), 
#          result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod) %>% 
#   pivot_wider(id_cols = term, 
#               names_from = contam.pfas, 
#               values_from = c(p.value, result))
# 
# excludedsys_model_table <- modresult1 %>% bind_rows(modresult2) %>%
#   select(term, 
#          ends_with("evrdet"), 
#          ends_with("1,4-dioxane"), 
#          ends_with("1,1-dichloroethane"), 
#          ends_with("HCFC-22"), 
#          ends_with("PFAS"))
# 
# # write_csv(excludedsys_model_table,
# #           paste0("results/output/exclvsincl_sys_model_",
# #                  Sys.Date(), ".csv"))
# 
# 
# modhlvl1 <- glm(hlvlchem ~ system_included, data = mod_dat %>%
#                   filter(contam.pfas == "evrdet"), family = 'binomial')
# summary(modhlvl1)
# 
# modhlvl1 <- glm(hlvlchem ~ cohort*Size + source_type, data = mod_dat %>%
#                   filter(contam.pfas == "evrdet"), family = 'binomial')
# summary(modhlvl1)
# 
# my_mod_function3 <- function(data){
#   glm(hlvlchem ~ cohort, data = data, family = 'binomial')
# }
# 
# mod_dat %>%
#   filter(contam.pfas == "evrdet") %>%
#   nest(data = everything()) %>%
#   mutate(mod = map(data, my_mod_function3), 
#          result_modify = map(mod, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, nobs)) %>% 
#   select(-data, -mod)
# 
# #install.packages('MatchIt')
# library(MatchIt)
# 
# dat <- mod_dat %>% filter(contam.pfas == "evrdet")
# dat$Size <- as.factor(dat$Size); dat$source_type <- as.factor(dat$source_type)
# matchit <- matchit(system_included ~ Size + source_type, 
#                    data = dat, 
#                    method = "nearest", 
#                    ratio = 1)  
# summary(matchit)
# plot(matchit, type = 'jitter', interactive = FALSE)
# df.match <- match.data(matchit)[1:ncol(dat)]
# df.match %>%
#   select(PWSID) %>%
#   left_join(mod_dat, by = "PWSID") %>%
#   group_by(contam.pfas) %>%
#   nest() %>%
#   mutate(mod = map(data, ~glm(hlvlchem ~ system_included, data = .x, family = 'binomial')),
#          mod2 = map(data, ~glm(detchem ~ system_included, data = .x, family = 'binomial')),
#          result_modify = map(mod, crude2df), 
#          result_modify2 = map(mod2, crude2df))  %>% 
#   mutate(nobs = purrr::map(mod, get_residuals)) %>% 
#   unnest(cols = c(result_modify, result_modify2, nobs), 
#          names_sep = ".", 
#          names_repair = "unique") %>% 
#   select(-data, -mod) %>% View()
# 
# library(flextable)
# library(officer)

