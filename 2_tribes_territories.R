# DATE STARTED: 2023-03-14
# AUTHOR: Aaron Maruzzo
# PURPOSE: Analyze water systems serving US territories or tribal areas
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# start here: 
source("1_combine_process.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# US tribes and US territories
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Water systems serving counties with missing MDI rates were excluded from the main analyses. 
# Most of these systems were systems serving US tribes or territories. 
# To explore possible ej issues, we compared detection and exceedance frequencies
# (proportions of water systems with detects/exceedences) between 2 groups: 
#   (1) systems serving US territories or tribes 
#   (2) systems serving US states and DC
# We used Fisher's Exact tests due to low numbers of detects. Results are 
# reported in the supplement. 

# Start with data frame object "dat_ucmr3." This object is the pre-processed 
# version of "dat_clean", which was used throughout the main analysis in other scripts. The 
# difference is that dat_ucmr3 contains data for water systems prior to 
# removing systems with missing data. 

str(dat_ucmr3)

# Count the number of systems in tribes and territories overall. Check  if 
# all these systems collected samples for target contaminants.
#
## Total: 108 systems
##  Note: 3 Puerto Rico systems never collected samples for target contaminants.
## Total, excluding systems without samples: 105 systems. 
## Most systems (69 out of 105) were in Puerto Rico. There was a total of 76 
## systems serving US territories in the UCMR3 and 29 systems serving tribal areas.

dat_tt <- dat_ucmr3 %>% filter(state_status %in% c("tribe", "territory"))
dat_tt2 <- dat_tt %>% filter(!is.na(det_any))
stopifnot(nrow(setdiff(dat_tt, dat_tt2)) == 3)
# dat_tt2 %>% count(state) 
# dat_tt2 %>% count(state_status)  

# Pivot longer. 

dat_tt3 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value")

# Detection frequencies  -----------------------------------------------------

## contamination-specific frequencies among tribal PWSs (n=29) and territorial 
## PWSs (n=76). Frequencies were calculated for systems separated by whether 
## they were in a tribal area or U.S. territory, and combined. 
det_freq_tt <- dat_tt3 %>% 
  bind_rows(dat_tt3 %>% mutate(state_status = "overall")) %>% 
  group_by(state_status, outcome) %>% 
  summarise(n = n(), 
            detect = sum(value == 1),
            det_freq = 100*detect/n())

## contamination-specific frequencies among PWSs in the main analysis (n=4808).
det_freq_main <- dat_clean %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  mutate(state_status = "State or D.C.") %>%
  group_by(state_status, outcome) %>% 
  summarise(n = sum(!is.na(value)),  
            detect = sum(value == 1, na.rm = T),
            det_freq = 100*detect/n())

## combine datasets 
det_freq_maintt <- bind_rows(det_freq_main, det_freq_tt)
det_freq_maintt %>% pivot_wider(id_cols = c(state_status, n), 
                                names_from = outcome, values_from = det_freq)


## detection freqs of target contaminants among territories/tribal systems
# tribal OR territory combined
t2 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)),  # NA = system did not sample for contaminant
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "Tribal or Territorial PWS")


# make a dataset that has the main US-based water systems sample, and the 
# tribal/territorial water systems

dat_tt_withUS <- dat_clean %>% 
  bind_rows(dat_tt) %>%
  mutate(state_status_2 = ifelse(state_status %in% c("tribe", "territory"), 
                                 "TT", "US"))
dat_tt_withUS

nrow(dat_tt_withUS) #4913 (U.S. PWSs + excluded systems)
dat_clean %>% bind_rows(dat_tt) %>% group_by(state_status) %>% count()
# 4808 states, 76 territories, 29 tribes

dat_clean %>% bind_rows(dat_tt) %>% group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

dat_clean %>% bind_rows(dat_tt) %>% 
  filter(det_any == 1) %>% 
  group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

dat_clean %>% bind_rows(dat_tt) %>% 
  filter(det_diox == 1) %>% 
  group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

# conduct Fisher's Exact test for each outcome
# comparison is frequencies of detections TT (tribal or territory PWS) vs. US (other PWS) 
temp <- dat_tt_withUS %>%
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  nest() %>%
  mutate(contTabl = map(data, ~table(.x$value, .x$state_status_2))) %>%
  mutate(test = map(contTabl, ~fisher.test(.)), 
         test = map(test, ~tidy(.))) %>% 
  unnest(test)

T4 <- bind_rows(t1, t3) %>% 
  left_join(temp %>% distinct(outcome, p.value)
  ) %>%
  mutate(p.value = ifelse(p.value < 0.001, "< 0.001", as.character(signif(p.value, 2)))) %>%
  relocate(state_status, .after = outcome) %>%
  arrange(outcome, state_status) %>%
  mutate(yes = paste0(yes, " (", det_freq, ")"), 
         state_status = paste0(state_status, " (n = ", n, ")")) %>%
  select(-det_freq, -n) 
T4

# Save progress.

# write.csv(T4, paste0("outputs/", Sys.Date(), " - comparing TT vs US systems.csv"))


# Disproportionality  -----------------------------------------------------

# Over-representation analysis 

# function 
make_clean_value <- function(n, freq){paste0(n, " (", freq, ")")}

# occurrence in UCMR3 dataset
overall_UCMR <- dat_tt_withUS %>% 
  count(state_status) %>%
  mutate(total = sum(n), freq = signif(100*n/total, 2)) %>%
  mutate(overall_prevalence = make_clean_value(n, freq))
overall_UCMR

# occurrence among PWSs with detections
prev_among_detected <- dat_tt_withUS %>%
  select(PWSID, det_dca, det_diox, det_pfas, det_hcfc, state_status, state_status_2) %>%
  pivot_longer(cols = starts_with("det_"), names_to = "chemical", values_to = "value") %>%
  filter(value == 1) %>%
  count(chemical, state_status) %>%
  complete(chemical, state_status, fill=list(n = 0)) %>%
  group_by(chemical) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = signif(100*n/total, 2)) %>%
  mutate(clean_value = make_clean_value(n, freq)) %>%
  mutate(chemical = paste0(chemical, " (n=", total, ")"))
prev_among_detected

# Simplify and pivot wider for table:
prev_among_detected_2 <- prev_among_detected %>%
  pivot_wider(id_cols = state_status, names_from = chemical, values_from = clean_value)
prev_among_detected_2

# combine tables and format
comb_overrepresentn <- overall_UCMR %>%
  select(overall_prevalence) %>%
  bind_cols(prev_among_detected_2) %>%
  relocate(overall_prevalence, .after = state_status)
comb_overrepresentn

# write.csv(comb_overrepresentn, 
#           paste0("outputs/", Sys.Date(), " - overrepresentation TT vs US systems.csv"))


# Archive -----------------------------------------------------------------

# combined <- dat_tt2 %>% bind_rows(dat_clean %>% 
#                                     mutate(state_status = "State or D.C."))
# 
# long <- combined %>% 
#   pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
#                names_to = "outcome", values_to = "value")
# 
# nested <- long %>%
#   group_by(outcome) %>% 
#   nest()
