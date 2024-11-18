# DATE STARTED: 2023-03-14
# AUTHOR: Aaron Maruzzo
# PURPOSE: Analyze water systems serving US territories or tribal areas
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# start here: 
# source("1_combine_process.R")

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
stopifnot(nrow(dat_ucmr3)==4923)

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

stopifnot(nrow(dat_tt3) == nrow(dat_tt2)*6)

# Detection frequencies  -----------------------------------------------------

# Calculate the number of systems in US tribes and territories that detected 
# a target contaminant. Repeat for all six outcomes. Use bind_row() to create 
# an "overall" category, which combined US tribes and territories. 

det_freq_tt <- dat_tt3 %>% 
  bind_rows(dat_tt3 %>% mutate(state_status = "overall")) %>% 
  group_by(state_status, outcome) %>% 
  summarise(n = n(), 
            detect = sum(value == 1),
            det_freq = 100*detect/n())

# Calculate the number of systems in the U.S. and D.C. that detected a 
# target contaminant. Repeat for all six outcomes. 
# Note: should be the same frequencies reported in Table 1.

det_freq_main <- dat_clean %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  mutate(state_status = "State or D.C.") %>%
  group_by(state_status, outcome) %>% 
  summarise(n = sum(!is.na(value)),  
            detect = sum(value == 1, na.rm = T),
            det_freq = 100*detect/n())

# Combine the detection frequency tables.

det_freq_maintt <- bind_rows(det_freq_main, det_freq_tt)

# For statistical tests (Exact tests), create a data frame object that 
# has all US PWSs. This is exactly the same as dat_ucmr3.

dat_tt_withUS <- dat_clean %>% 
  bind_rows(dat_tt) %>%
  mutate(state_status_2 = ifelse(state_status %in% c("tribe", "territory"), 
                                 "TT", "US"))

setdiff(dat_ucmr3, dat_tt_withUS %>% select(-state_status_2))
table(dat_tt_withUS$state_status_2)

# Conduct Fisher's Exact tests. Test for differences in detection frequencies 
# between US (main) systems versus excluded systems (tribes and territories). 
# Alpha = 0.05. n1=4815 and n2=108.

exact_results <- dat_tt_withUS %>%
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  nest() %>%
  mutate(contTabl = map(data, ~table(.x$value, .x$state_status_2))) %>%
  mutate(test = map(contTabl, ~fisher.test(.)), 
         test = map(test, ~tidy(.))) %>% 
  unnest(test)

# view results here:
# exact_results %>% select(-data, -contTabl) %>% view()

# Extract p-values for table. Format p-values, then combine with combined 
# detection frequency table. Sort by outcome first, then by area. Export.

exact_results2 <- exact_results %>% 
  select(outcome, p.value) %>% 
  mutate(p.value = format.pval(p.value, 
                               digits = 1, 
                               eps = 0.001, 
                               nsmall =2))

exact_results3 <- det_freq_maintt %>% 
  left_join(exact_results2, by = 'outcome') %>%
  filter(state_status != "overall") %>%
  relocate(outcome, 1) %>%
  arrange(outcome, state_status)

# Save progress.

# write.csv(exact_results3,
#           paste0("results/SuppTable. DFs bw US tribes, territories, and mainland US_",
#                  Sys.Date(), ".csv"))


# Over-representation analysis -------------------------------------------------

# Over-representation analysis looks at the proportion of water systems in US tribes and 
# territories among the overall population (UCMR3 systems) and compares
# them to the proportion of systems in US tribes/territories among 
# systems that detected an unregulated contaminant.

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
#           paste0("results/SuppTable. Overrepresentation_", 
#                  Sys.Date(), ".csv"))


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
