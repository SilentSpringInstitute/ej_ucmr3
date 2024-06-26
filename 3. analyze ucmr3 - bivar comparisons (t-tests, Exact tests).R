
# If starting from here: run script 2
source("2. create main datasets.R")

library(tidyverse)
library(flextable)
library(ggplot2)
library(broom)
library(ggh4x)

# dat_clean   = df of 4808 PWSs (restricted to PWSs w/ MDI data)
dat_clean

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FIGURE 1 AND TABLE. Unequal variances t-tests -----------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Comparison of mean-county univariate SES indicators between PWSs with and without
#+ detection of unregulated contaminants and with or without exceedances of
#+ health reference levels. Unequal variances t-test used to assess 
#+ differences in averages.

tRes <- dat_clean %>% 
  select(PWSID, det_any, viol_any, 
         mdi_rate, perc_hmown, perc_pov_ppl, perc_uninsur, 
         perc_hisp_any, perc_black_nohisp, perc_urban) %>%
  pivot_longer(cols = c(mdi_rate, starts_with("perc_"))) %>%
  group_by(name) %>%
  summarise(tRes_det = t.test(value[det_any == 0], value[det_any == 1], var.equal = FALSE)$p.value, 
            tRes_viol = t.test(value[viol_any == 0], value[viol_any == 1], var.equal = FALSE)$p.value) 
tRes

demoMeans <- dat_clean %>% 
  mutate(det_any = ifelse(det_any == "1", "detected UC", "did not detect UC")) %>%
  mutate(viol_any = ifelse(viol_any == "1", "exceeded HRL", "did not exceed HRL")) %>%
  bind_rows(dat_clean %>% mutate(det_any = "overall", viol_any = "overall")) %>% 
  select(PWSID, det_any, viol_any, 
         mdi_rate, perc_hmown, perc_pov_ppl, perc_uninsur, 
         perc_hisp_any, perc_black_nohisp, perc_urban) %>%
  pivot_longer(cols = c(mdi_rate, starts_with("perc_"))) %>%
  pivot_longer(cols = c(det_any, viol_any), names_to = "outcome", values_to = "value_out") %>%
  group_by(name, outcome, value_out) %>%
  summarise(n = paste("n =", n()), mean = mean(value), sd = sd(value), se = sd/sqrt(n())) 
demoMeans

####
### Figure for paper ### 
####

#+ https://teunbrand.github.io/ggh4x/reference/guide_axis_nested.html

custom_stars <- function(x){
  case_when(x < 0.001 ~ "**", 
            x >= 0.001 & x < 0.05 ~ "*",
            x >= 0.05 & x < 0.10 ~ "+",
            x >= 0.10 ~ " ")
}

#### PLOT ####

## Compares average % Hispanic, % Black, % deprived, and % urban among 
## water systems with and without an unregulated contaminant detection, 
## and among water systems with and without an unregulated contaminant 
## exceedance. 


ggplot(demoMeans %>%
         left_join(tRes %>% 
                     pivot_longer(cols = starts_with("tRes"), names_to = "outcome") %>%
                     mutate(outcome = ifelse(outcome == "tRes_det", "det_any", "viol_any")) %>%
                     rename(p.value = value) %>%
                     mutate(stars = custom_stars(p.value))) %>%
         filter(value_out != "overall") %>% 
         rename(demovar = name, 
                outcome = outcome, 
                outcome_level = value_out) %>%
         mutate(outcome_level = ifelse(str_detect(outcome_level, "not"), 
                                       "No", "Yes")) %>%
         filter(demovar %in% c("mdi_rate", "perc_black_nohisp", 
                               "perc_hisp_any", "perc_urban")) %>%
         mutate(demovar = factor(demovar, 
                                 levels = c("perc_hisp_any", 
                                            "perc_black_nohisp", 
                                            "mdi_rate", 
                                            "perc_urban"), 
                                 labels = c("Percent Hispanic", 
                                            "Percent non-Hispanic Black", 
                                            "Percent deprived", 
                                            "Percent urban households"))) %>%
         mutate(outcome = factor(outcome, 
                                 levels = c("det_any", "viol_any"), 
                                 labels = str_wrap(c("Target contaminant detected",
                                                     "Health-reference level exceeded")))) %>%
         mutate(stars = ifelse(outcome_level == "No", "", stars), 
                stars = ifelse(stars == " ", "n.s.", stars)) %>%
         mutate(formatted_p = ifelse(p.value < 0.001, "p < 0.001", 
                                     paste("p =", round(p.value, 2))), 
                formatted_p = ifelse(outcome_level == "No", "", formatted_p)), 
       aes(x = interaction(outcome_level, outcome), 
           y = mean, 
           fill = interaction(outcome_level, outcome))) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0.25,
                position = 'dodge') + 
  geom_col(width = 0.9,
           position = position_dodge(width = 1.5),
           color = 'black', 
           linewidth = 0.3) + 
  scale_x_discrete(guide = 'axis_nested', labels = function(x) str_wrap(x, width = 15)) + 
  geom_segment(aes(x = 1,
                   xend = 1,
                   y = as.integer(interaction(outcome_level, outcome)) + 2, 
                   yend = as.integer(interaction(outcome_level, outcome)) + 2)) + 
  geom_text(aes(label = stars), 
            size = 4, 
            position = position_nudge(y = 6)) +
  #geom_text(aes(label = formatted_p), size = 4, position = position_nudge(y = 7)) +
  facet_wrap(~demovar, 
             strip.position = 'left',
             scales = 'free') + 
  scale_fill_manual(name = "", 
                    values = c("#9bccc6", "#50a6a6", '#c2a5cf', '#7b3294')) + 
  facetted_pos_scales(y = list(
    demovar %in% c("Percent Hispanic", "Percent non-Hispanic Black", "Percent deprived") ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 30)), 
    demovar == "Percent urban households" ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0,110))
  )) + 
  labs(x = "", y = "") + 
  theme_bw() + 
  theme(text = element_text(size = 12),
        #aspect.ratio = 5/4,
        ggh4x.axis.nestline = element_blank(), 
        panel.grid = element_blank(),
        legend.position = 'none', 
        strip.placement = 'outside', 
        strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text.y.left = element_text(size = 10))

# ggsave(plot = last_plot(),
#        paste0("outputs/Fig 1. Mean Demo Comparisons_",
#               Sys.Date(),
#               ".pdf"), 
#        scale = 1.3,
#        height = 5, 
#        width = 5)

#### TABLES ####

fdemoMeans <- demoMeans %>%
  pivot_wider(id_cols = name, 
              names_from = c(outcome, value_out, n), 
              values_from = c(mean), 
              names_glue = "{outcome}_{value_out}_{n}", 
              names_sort = TRUE) %>%
  left_join(tRes %>%
              mutate_if(is.numeric, ~ifelse(. < 0.001, "< 0.001", paste(round(., 2))))) %>% 
  select(name, 
         starts_with("det_any_"), 
         starts_with("tRes_det"), 
         starts_with("viol_any_"), 
         starts_with("tRes_viol"))
fdemoMeans

# write.csv(fdemoMeans, paste0("outputs/", Sys.Date(),
#                              " - mean demos and pws, t-test results.csv"))

# fdemoMeans %>%
#   flextable() %>%
#   colformat_num(digits = 2) %>% 
#   #merge_v(j = 1:2) %>%
#   separate_header(split = "_") %>%
#   theme_box() %>%
#   autofit() %>%
#   set_table_properties(layout = "autofit")


# df of just p-values 
# means2 %>%
#   distinct(name, outcome, value) %>%
#   rename(p_value = value) %>%
#   pivot_wider(names_from = outcome, names_prefix = "p_", values_from = p_value)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TRI facility and demographics among COUNTIES -------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This section introduces a new analysis: evaluate TRI facility distributions
# among counties (not water systems) to test if counties with TRI facilities have
# higher %His, %Black, %deprived, %urban, etc.

# The main dataset, dat_clean, does not have a column of county_ids. 
# This is makes a list of PWSIDs and their associated counties. 
# pwsid_fips comes from script '2. create main datasets.R'
pwsid_county_link <- fips_cn15 %>% select(PWSID, GEO.id2) %>% rename(county_id = GEO.id2)
pwsid_county_link

pwsid_county_link %>% group_by(PWSID) %>% count() %>% arrange(-n)

# The lines below join county served info with each PWSID and counts the number of counties
# in the UCMR sample.

county_ids_vec <- dat_clean %>%
  left_join(pwsid_county_link) %>%
  distinct(PWSID, county_id) %>%
  pull(county_id) %>%
  unique()
county_ids_vec

length(county_ids_vec) #1718 counties in our sample

# dat_clean %>%
#   left_join(pwsid_county_link) %>%
#   distinct(PWSID, county_id, state) %>%
#   count(state) %>%
#   arrange(-n) %>%
#   head()

### How many counties in our sample had a TRI facility? 

# A better dataset for this analysis is dataframe 'cn15', which comes from script
# '1. demo processing.R'. 

cn15
colnames(cn15)
# variables with the prefix "bin_fac" are binary variables (0/1) indicating 
# whether a TRI facility is present or absent.

# cn15 has (nearly) one row per county.
nrow(cn15) #3143 
distinct(cn15) %>% nrow() #3142 
## gloucester county, new jersey (GEO.id2 == 34015) is duplicated
cn15 %>% count(GEO.id2) %>% filter(n > 1)
cn15 %>% filter(GEO.id2 == "34015") %>% distinct()
# removing duplicate gloucester county, new jersey
cn15 <- cn15 %>% distinct()

# check if cn15 has one line per county.
stopifnot(nrow(cn15) == nrow(distinct(cn15)))

# restrict to counties in the main UCMR3 sample.
cn15.1 <- cn15 %>% filter(GEO.id2 %in% county_ids_vec)

cn15.1 %>%
  # add bin_fac_pfas (any major industry, MFTA, or airport present = 1, absent = 0)
  group_by(GEO.id2) %>% 
  mutate(bin_fac_pfas = ifelse(sum(src_epa_present == 1) > 0 |
                                 n_MFTA > 0 |
                                 n_airports > 0, 
                               1, 0)) %>%
  ungroup() %>% 
  # pivot_longer outcomes and variables of interest
  pivot_longer(cols = starts_with("bin_fac")) %>%
  filter(name == "bin_fac_any") %>%
  count(value)

# 131 out of 1718 (7.6%) of counties had a TRI facility

### Conduct significance tests
# The next sets of code chunks conduct unequal variance t-tests. I created 
# cn15.2 to add an outcome of interest (proximity to PFAS pollution sources, 
# 'bin_fac_pfas') and copied and pasted code for WWTP flow and % homeownership, 
# which was not included in the original cn15 dataset.
# 
# With cn15.2, I created a longform dataset and nested dataframes to perform 
# the stat tests.
# 
# The t-test results are stored in 'source_demo_comparison'.

cn15.2 <- cn15.1 %>%
  
  # add bin_fac_pfas (any present MFTA OR airport)
  group_by(GEO.id2) %>% 
  mutate(bin_fac_pfas = ifelse(sum(src_epa_present == 1) > 0 |
                                 n_MFTA > 0 |
                                 n_airports > 0, 1, 0)) %>%
  
  # add WWTP flow 
  ungroup() %>% 
  mutate(WWTP_totalflow_mgd = replace_na(WWTP_totalflow_mgd, 0)) %>%
  mutate(adj_wwtp_flow_mgd_per_sqmeters = WWTP_totalflow_mgd/land.area, 
         adj_wwtp_flow = (3.78541*1e6)*adj_wwtp_flow_mgd_per_sqmeters) %>% 
  mutate(wwtp_flow_bin = ifelse(adj_wwtp_flow >= median(adj_wwtp_flow), 
                                ">= median WWTP flow", "< median WWTP flow")) %>%
  
  # add perc_hmown
  mutate(perc_hmown = 100*owned.house/all.house14)
  
source_demo_comparison <- cn15.2 %>%
  
  # Pivot outcomes and variables of interest
  pivot_longer(cols = starts_with("bin_fac")) %>%
  pivot_longer(cols =
                 c(perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban, 
                   perc_hmown, perc_pov_ppl, perc_uninsur), 
               names_to = "demovar", values_to = "demovalue") %>%
  
  # conduct test
  group_by(name, demovar) %>%
  nest() %>%
  mutate(test = map(data, ~t.test(demovalue ~ value, data = ., var.equal = FALSE))) %>%
  mutate(test_clean = map(test, ~tidy(.))) %>%
  ungroup() %>%
  
  # format statistical test results
  unnest(test_clean) %>% 
  mutate(p_stars = gtools::stars.pval(p.value)) %>%
  select(name, demovar, estimate, estimate1, estimate2, p.value, p_stars)

source_demo_comparison

## checking
cn15.2 %>% group_by(bin_fac_diox) %>% summarise(n = n(), mean = mean(perc_hmown))
# looks good 

## double-checking calculations with bin_fac_any and perc_hisp_any
# cn15.2 %>%
#   group_by(bin_fac_any) %>%
#   summarise(n = n(), mean_hisp = mean(perc_hisp_any)) 
# t.test(perc_hisp_any ~ bin_fac_any, data = cn15.2)$p.value # 0.151
# t.test(perc_hisp_any ~ bin_fac_any, data = cn15.2)
# 
# source_demo_comparison %>%
#   filter(name == "bin_fac_any" & demovar == "perc_hisp_any")
# looks good!

#### TABLES ####
# formatting t-test results into exportable format
fsource_demo_comp <- source_demo_comparison %>%
  rename(p.stars = p_stars) %>%
  mutate(p.value = ifelse(p.value < 0.001, "< 0.001", paste(round(p.value, 2)))) %>%
  pivot_wider(id_cols = demovar, 
              names_from = name, 
              names_vary = "slowest",
              names_glue = "{name}__{.value}",
              names_sort = TRUE, 
              values_from = c(estimate1, estimate2, p.value, p.stars))
fsource_demo_comp

# write.csv(fsource_demo_comp, paste0("outputs/", Sys.Date(),
#                              " - mean demos and TRI facs, t-test results.csv"))

# fsource_demo_comp %>% 
#   flextable() %>% 
#   autofit() %>% 
#   separate_header(split = '__') %>%
#   theme_vanilla()

#### PLOT ####
ggplot(source_demo_comparison %>%
         filter(demovar %in% c('mdi_rate', 'perc_black_nohisp', 'perc_hisp_any', 'perc_urban')) %>%
         pivot_longer(cols = c(estimate1, estimate2), names_to = 'value_out', values_to = "mean") %>%
         left_join(cn15.2 %>%
                     pivot_longer(cols = starts_with("bin_fac")) %>%
                     pivot_longer(cols =
                                    c(perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban, 
                                      perc_hmown, perc_pov_ppl, perc_uninsur), 
                                  names_to = "demovar", values_to = "demovalue") %>%
                     group_by(name, demovar, value) %>%
                     summarise(n = n(), mean = mean(demovalue), sd = sd(demovalue), se = sd/sqrt(n)) %>%
                     mutate(value_out = ifelse(value == 0, "estimate1", "estimate2")) %>%
                     distinct(name, demovar, value_out, mean, se)
           
         ) %>%
         #left_join(test) %>%
         mutate(demovar = factor(demovar, 
                                 levels = c('perc_hisp_any', 'perc_black_nohisp', 
                                            'mdi_rate', 'perc_urban'), 
                                 labels = c("Percent Hispanic", "Percent non-Hispanic Black", 
                                            "Percent deprived", "Percent urban households"))) %>%
         
         rename(outcome = name) %>%
         mutate(outcome = factor(outcome, levels = c("bin_fac_any", 
                                                     "bin_fac_diox", 
                                                     "bin_fac_cfc", 
                                                     "bin_fac_chlor_solv", 
                                                     "bin_fac_pfas"), 
                                 labels = c("Any TRI facility", 
                                            "1,4-dioxane facility", 
                                            "CFC facility", 
                                            "Chlorinated solvent facility",
                                            "PFAS airport, MFTA, or major industrial source"))) %>%
         mutate(value_out = factor(value_out, 
                                   levels = c("estimate1", "estimate2"), 
                                   labels = c("No", "Yes"))) %>%
         mutate(p_stars = ifelse(value_out == "No", "", p_stars)), 
       aes(x = value_out, y = mean, fill = value_out)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = 'dodge') + 
  geom_bar(stat = 'identity', color = 'black', linewidth = 0.3) +
  geom_text(aes(label = p_stars), vjust = -0.5) + 
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  facet_grid(demovar ~ outcome, switch = "both",  labeller = label_wrap_gen(15), scales = "free_y")  + 
  facetted_pos_scales(y = list(
    demovar %in% c("Percent Hispanic", "Percent non-Hispanic Black", "Percent deprived") ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 25)), 
    demovar == "Percent urban households" ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0,110))
  )) + 
  scale_fill_manual(values = c("#abd9e9", "#2c7bb6"))+ 
  theme_bw() + 
  labs(x = "", 
       y = "") + 
  theme(text = element_text(size = 12),
        #aspect.ratio = 10/9,
        ggh4x.axis.nestline = element_blank(), 
        panel.spacing.y = unit(0.8, "lines"),
        panel.grid = element_blank(),
        legend.position = 'none', 
        strip.placement = 'outside', 
        strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text.y.left = element_text(size = 10)
        ) 

# ggsave(plot = last_plot(),
#        filename = paste0("outputs/", Sys.Date(), " - demographics and TRI facility.pdf"),
#        width = 7, height = 6)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # TRIBES AND TERRITORIES ----------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ A total of 105 PWSs were excluded from the analysis because of missing MDI data. 
#+ Of these, the majority were PWSs in tribal lands and U.S. territories. This 
#+ analysis compares unregulated contaminant detection and violation frequencies 
#+ between tribal/territorial PWSs and mainland PWSs. D.C. is considered a mainland
#+ PWS. 
#+ 
#+ This produces a table that summarizes detection and violation frequencies and 
#+ a Fisher's Exact Test to test whether there were differences in the number of 
#+ PWSs with det/viol in tribal OR territorial lands vs. in the mainland. 
#+ 
#+ See Mok et al. for info on tribal PWSs in the UCMR3.


dat_ucmr3
dat_ucmr3 %>% colnames()
# get the PWSs that were removed from the main sample due to missing MDI
setdiff(dat_ucmr3, dat_clean) %>% count(state)
setdiff(dat_ucmr3, dat_clean) %>% count(state_status)
# restrict to PWSs in tribes and territories
dat_tt <- dat_ucmr3 %>% filter(state_status %in% c("tribe", "territory"))

## check
dat_tt %>% count(state_status, state)
nrow(dat_tt) # 108
nrow(dat_tt %>% filter(state_status == "tribe")) #29
nrow(dat_tt %>% filter(state_status == "territory")) #79

# 3 PWSs (all in PR) never collected samples for target contaminants
ids_nvr_collectd <- dat_tt %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>%
  filter(is.na(value)) %>%
  distinct(PWSID) %>%
  pull(PWSID)
ids_nvr_collectd

# check with raw UCMR 3 data:
# ucmr3 %>% 
#   filter(PWSID %in% ids_nvr_collectd) %>% view()

# Remove PWSIDs that never collected samples for target contaminants
dat_tt <- dat_tt %>% filter(!PWSID %in% ids_nvr_collectd)

## overall main UCMR sample
t1 <- dat_clean %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
                           names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "State or D.C.")
t1

## overall tribe or territory
t2 <- dat_tt %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "Tribal or Territorial PWS")
t2

## tribe and territory separated
t3 <- dat_tt %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  ## adding state_status to stratify by territory or tribe
  group_by(state_status, outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1))
t3





# bind_rows(t1, t3)

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

comb_overrepresentn %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla()

#### TABLE #####
# write.csv(T4, paste0("outputs/", Sys.Date(), " - comparing TT vs US systems.csv"))

# flextable? 
# T4 %>%
#   flextable() %>%
#   autofit() %>% 
#   set_header_labels(
#     outcome = "Outcome", 
#     
#   )

## END OF SCRIPT.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ARCHIVE -----------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# theme(panel.spacing.y = unit(0.5, "lines"),
#       strip.background = element_blank(),
#       strip.placement = "outside")
# # Start plotting here:
# ggplot(aes(x = interaction(value_out, outcome), 
#            y = mean, 
#            fill = interaction(outcome, value_out))) +
#   #geom_bar(stat = 'identity', position = 'dodge') + 
#   geom_col(position = position_dodge(0.5)) + 
#   scale_x_discrete(guide = 'axis_nested') + 
#   
#   # Add stars
#   geom_text(aes(label = p_stars), size = 5, position = position_nudge(y = 5)) +
#   
#   # Facet 
#   facet_wrap(~demovar, strip.position = 'left', scales = 'free') 
# 
# # Adjust outcome category names and stars
# filter(value_out %in% c('0', '1')) %>% 
#   mutate(stars = ifelse(stars == " ", "n.s.", stars)) %>%
#   mutate(stars = ifelse(value_out == 0, "", stars)) %>%
#   mutate(value_out = factor(value_out, levels = c("0", "1"), labels = c("No", "Yes"))) %>%
#   
#   # Adjust outcome names
#   mutate(outcome = factor(outcome, 
#                           levels = c("det_any", "viol_any"), 
#                           labels = str_wrap(c("Target contaminant detected",
#                                               "Health-reference level exceeded"), 25))) %>%
#   
#   # Start plotting here:
#   ggplot(aes(x = interaction(value_out, outcome), 
#              y = mean, 
#              fill = interaction(outcome, value_out))) +
#   #geom_bar(stat = 'identity', position = 'dodge') + 
#   geom_col(position = position_dodge(0.5)) + 
#   scale_x_discrete(guide = 'axis_nested') + 
#   
#   # Add stars
#   geom_text(aes(label = stars), size = 5, position = position_nudge(y = 5)) +
#   
#   # Facet 
#   facet_wrap(~name, strip.position = 'left', scales = 'free') + 
#   
#   # Themes and other adjustments
#   scale_fill_manual(name = "", values = c("#9bccc6",'#c2a5cf',  "#50a6a6", '#7b3294')) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
#   labs(x = "", y = "") + 
#   theme_bw() + 
#   theme(text = element_text(size = 12),
#         aspect.ratio = 3/5,
#         ggh4x.axis.nestline = element_blank(), 
#         panel.grid = element_blank(),
#         legend.position = 'none', 
#         strip.placement = 'outside', 
#         strip.background = element_rect(fill = 'white', color = 'white'), 
#         strip.text.y.left = element_text(size = 10))
# 
# 
# 
# cn15 %>%
#   filter(GEO.id2 %in% county_ids_vec) %>%
#   pivot_longer(cols = starts_with("bin_fac")) %>%
#   mutate(value = as.factor(value)) %>%
#   pivot_longer(cols = c(perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban), 
#                names_to = "demovar", values_to = "demovalue") %>%
#   select(name, value, demovar, demovalue) %>%
#   left_join(source_demo_comparison) %>%
#   mutate(value = ifelse(value == 0, "Absent", "Present")) %>% 
#   mutate(demovar = factor(demovar, 
#                           levels = c("perc_black_nohisp", 
#                                      "perc_hisp_any", 
#                                      "mdi_rate", 
#                                      "perc_urban"), 
#                           labels = c("% Black", "% Hispanic", 
#                                      "% deprived", "% urban households"))) %>%
#   mutate(name = case_when(name == "bin_fac_any" ~ "Any TRI facility", 
#                           name == "bin_fac_diox" ~ "Dioxin facility", 
#                           name == "bin_fac_chlor_solv" ~ "Chlorinated solvent facility", 
#                           name == "bin_fac_cfc" ~ "CFC facility")) %>%
#   ggplot(aes(x = value, y = demovalue)) +
#   geom_jitter(alpha = 0.5, color = 'lightgrey') + 
#   geom_boxplot(aes(color = value)) + 
#   geom_text(aes(x = 1.6, y = 80, label = p_stars), 
#             size = 5, hjust = 1, vjust = 1) + 
#   # geom_violin(aes(color = value)) + 
#   facet_grid(demovar ~ name, switch = "y") +
#   scale_color_manual(name = "", values = c("darkgreen", "darkblue")) + 
#   labs(x = "", y = "") +
#   theme_bw() + 
#   theme(legend.position = "none", 
#         axis.text = element_text(size = 15), 
#         strip.text = element_text(size = 13))

# select(-stars) %>%
#   filter(value_out != 2)  %>% 
#   rename(p.value = value) %>%
#   pivot_wider(id_cols = name, 
#               names_from = c(outcome, value_out), 
#               values_from = c(mean, p.value),
#               names_sort = TRUE,
#               names_glue = "{outcome}_{value_out}_{.value}")%>%
#   view()
# 
# colnames(crude_results)
# crude_results$name %>% unique()
# temp$model[[1]] %>% view()
# 
# , 
#          perc_estimate = map_chr(model, ~.x$fmt_perc_change[.x$term == "predictor"]), 
#          p_stars = map_chr(model, ~.x$p_stars[.x$term == "predictor"]), 
#   ) %>%
#   select(-data, -model)
# 
# 
# dat_clean %>%
#   group_by(det_any) %>%
#   summarise(n = n(), 
#             det_any_freq = 100*sum(det_any == 1)/n,
#             exceed_any_freq = 100*sum(viol_any == 1)/n,
#             pop_served = sum(pop_served, na.rm = T),  # 5 PWSs w/o info on pop served
#             mean_samp = paste0(round(mean(n_samples), 1), " (", round(sd(n_samples), 1), ")"), 
#             median_hisp = calc_and_format_quantiles(perc_hisp_any), 
#             median_no_hisp_black = calc_and_format_quantiles(perc_black_nohisp), 
#             median_deprived = calc_and_format_quantiles(mdi_rate), 
#             median_urban_household = calc_and_format_quantiles(perc_urban)
#             )
# 
# dat_clean %>%
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
#   group_by(name) %>%
#   summarise(n_rows = n(), 
#             n_PWSID = sum(!is.na(value)), 
#             n_det = sum(value == 1, na.rm = T), 
#             detfreq = 100*n_det/n_PWSID, 
#             samps = mean(n_samples))
# 
# # 00. LIBRARIES -----------------------------------------------------------
# library(tidyverse)
# library(ggplot2)
# library(forcats)
# library(lubridate)
# library(broom)
# library(gtools)
# 
# ana <- dat_ucmr3 %>% filter(!is.na(mdi_rate))
# excluded_ana <- setdiff(dat_ucmr3, ana)
# excluded_ana %>% count(state)
# summary(ana)
# 
# add_stars <- 
# 
# 
# ana %>%
#   group_by(det_any, size) %>% 
#   summarise(n = n(), 
#             across(starts_with('det_'), ~sum(. == 1)),
#             across(starts_with('viol_'), ~sum(. == 1)), 
#             mdi_rate = median(mdi_rate),
#             across(starts_with('perc_'), ~median(., na.rm = T)))
# 
# ggplot(ana, aes(x = state, fill = pws_type)) + 
#   geom_histogram(stat = 'count', position = 'dodge')
# 
# # Plot medians by demographs
# ana %>%
#   pivot_longer(cols = c(det_any, viol_any), 
#                names_to = 'outcome', 
#                values_to = 'out') %>%
#   group_by(outcome, out) %>%
#   summarise(n = n(), 
#             mdi_rate = median(mdi_rate),
#             across(starts_with('perc_'), ~median(., na.rm = T))) %>%
#   pivot_longer(cols = c(mdi_rate, starts_with('perc_')),
#                names_to = 'demo',
#                values_to = 'level') %>%
#   ggplot(aes(x = outcome, y = level, fill = out)) +
#   geom_bar(stat = 'identity', position = 'dodge') + 
#   facet_wrap(~demo)
# 
# ana %>% 
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
#                names_to = "outcome", 
#                values_to = "y") %>%
#   group_by(outcome) %>%
#   nest() %>%
#   # Build out the regression model here:
#   mutate(m1 = map(data, ~glm(y ~ mdi_rate + n_samples + perc_black_only, 
#                              data = ., family = 'binomial')), 
#          m1_tidy = map(m1, tidy, exponentiate = T)) %>%
#   unnest(m1_tidy) %>%
#   select(-data, -m1) %>% 
#   mutate(stars = stars.pval(p.value))

