# DATE STARTED: 2024-01-16
# AUTHOR: Aaron Maruzzo
# PURPOSE: Explore distribution of % Hispanic and outcomes by US region
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Start here (if not already run):
# source("1_combine_process.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Overview
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Were there any differences in contamination or demographics by region? 
# 
# This script explored the distribution of unregulated contaminant detection, 
# point sources, and demographics by region. Statistics from this script were reported
# in the discussion. 

state_region_df <- data.frame(state = state.abb, region = state.region)
state_region_df

# Add DC systems as South

rowDC <- tibble(state = "DC", region = "South")

state_region_df <- bind_rows(state_region_df, rowDC)

# det_any
dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(num_water_sys = n(), 
            sys_with_any_detect = sum(det_any == 1), 
            sys_with_any_detect_freq = 100*sys_with_any_detect/num_water_sys)

# det_diox
dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(num_water_sys = n(), 
            sys_with_any_detect = sum(det_diox == 1, na.rm = T), 
            sys_with_any_detect_freq = 100*sys_with_any_detect/num_water_sys)

# det_dca
dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(num_water_sys = n(), 
            sys_with_any_detect = sum(det_dca == 1, na.rm = T), 
            sys_with_any_detect_freq = 100*sys_with_any_detect/num_water_sys)

# det_hcfc
dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(num_water_sys = n(), 
            sys_with_any_detect = sum(det_hcfc == 1, na.rm = T), 
            sys_with_any_detect_freq = 100*sys_with_any_detect/num_water_sys)

# det_pfas
dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(num_water_sys = n(), 
            sys_with_any_detect = sum(det_pfas == 1, na.rm = T), 
            sys_with_any_detect_freq = 100*sys_with_any_detect/num_water_sys)

# summary statistics of the following vars by region:
# proportion of systems linked to a TRI facility 
# proportion of systems that were MIX systems
# median % hispanic, % nhb, % urban
# average wastewater flow

dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(n = n(), 
            n_present_any = sum(n_fac_any == 1), 
            freq = round(100*n_present_any/n, 0),
            freq_MIX = 100*sum(pws_type == "MX")/n,
            # mean_black = mean(perc_black_nohisp),
            median_black = median(perc_black_nohisp),
            median_black = round(median_black, 0),
            median_hisp = median(perc_hisp_any),
            median_urban = median(perc_urban),
            mean_wwtp = round(mean(adj_wwtp_flow),2)
            )


# Archive -----------------------------------------------------------------

# max(dat_clean$perc_hisp_any)
# max(dat_clean$perc_black_nohisp)
# 
# colnames(dat_clean)
# ggplot(dat_clean, aes(x = perc_black_nohisp, y = n_samples)) + 
#   geom_point()
# ggplot(dat_clean, aes(x = perc_hisp_any, y = n_samples)) + geom_point()
# 
# ggplot(dat_clean, aes(x = det_any, y = mdi_rate)) + geom_boxplot()
# ggplot(dat_clean, aes(x = det_any, y = perc_hisp_any)) + geom_boxplot()
# ggplot(dat_clean, aes(x = det_any, y = perc_black_nohisp)) + geom_boxplot()
# 
# ggplot(dat_clean, aes(x = pws_type, y = perc_hisp_any)) + geom_boxplot()
# ggplot(dat_clean, aes(x = pws_type, y = perc_black_nohisp)) + geom_boxplot()
# ggplot(dat_clean, aes(x = pws_type, y = mdi_rate)) + geom_boxplot()
# 
# ggplot(dat_clean, aes(x = pws_type, y = n_samples)) + geom_boxplot()
# ggplot(dat_clean, aes(x = pws_type, y = n_samples)) + geom_boxplot()
# 

# ggplot(dat_clean, aes(x = perc_hisp_any, y = perc_black_nohisp)) + geom_point()
# ggplot(dat_clean, aes(x = perc_hisp_any, y = mdi_rate)) + geom_point()
# 
# length(unique(allsdwis3$PWSID)) == nrow(allsdwis3)
# allsdwis3 %>% count(PRIMACY_AGENCY_CODE)
# allsdwis3 %>% mutate(in_ucmr3 = ifelse(PWSID %in% dat_clean$PWSID, "yes", "no")) %>%
#   group_by(PRIMACY_AGENCY_CODE, in_ucmr3) %>%
#   count() %>%
#   group_by(PRIMACY_AGENCY_CODE) %>%
#   mutate(state_total = sum(n)) %>%
#   ungroup() %>%
#   mutate(freq = 100*n/state_total) %>%
#   arrange(-freq)

# dat_test <- dat_clean %>% left_join(state_region_df)
# dat_test
# 
# library(lme4)
# glmer(det_any ~ perc_black_nohisp + (1|state), data = dat_clean, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# glmer(det_any ~ perc_black_nohisp + region + (1|state), data = dat_test, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# 
# glmer(det_any ~ perc_hisp_any + (1|state), data = dat_clean, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# glmer(det_any ~ n_fac_any + adj_wwtp_flow + (1|state), data = dat_clean, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# glmer(det_any ~ perc_hisp_any + n_fac_any + adj_wwtp_flow + (1|state), data = dat_clean, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
# glmer(det_any ~ perc_hisp_any + n_fac_any + adj_wwtp_flow + pws_type + (1|state), data = dat_clean, family = 'binomial') %>% 
#   broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
