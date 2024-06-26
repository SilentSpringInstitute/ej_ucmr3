
colnames(dat_clean)
ggplot(dat_clean, aes(x = perc_black_nohisp, y = n_samples)) + 
  geom_point()
ggplot(dat_clean, aes(x = perc_hisp_any, y = n_samples)) + geom_point()

ggplot(dat_clean, aes(x = det_any, y = mdi_rate)) + geom_boxplot()
ggplot(dat_clean, aes(x = det_any, y = perc_hisp_any)) + geom_boxplot()
ggplot(dat_clean, aes(x = det_any, y = perc_black_nohisp)) + geom_boxplot()

ggplot(dat_clean, aes(x = pws_type, y = perc_hisp_any)) + geom_boxplot()
ggplot(dat_clean, aes(x = pws_type, y = perc_black_nohisp)) + geom_boxplot()
ggplot(dat_clean, aes(x = pws_type, y = mdi_rate)) + geom_boxplot()

ggplot(dat_clean, aes(x = pws_type, y = n_samples)) + geom_boxplot()
ggplot(dat_clean, aes(x = pws_type, y = n_samples)) + geom_boxplot()

# compare prevalence of facilities in the South compared to other regions (or to the U.S.)
state_region_df <- data.frame(state = state.abb, region = state.region)
state_region_df

max(dat_clean$perc_hisp_any)
max(dat_clean$perc_black_nohisp)

dat_clean %>%
  left_join(state_region_df) %>%
  group_by(region) %>%
  summarise(n = n(), 
            n_present_any = sum(n_fac_any == 1), 
            freq_MIX = 100*sum(pws_type == "MX")/n,
            freq = 100*n_present_any/n, 
            mean_black = mean(perc_black_nohisp),
            median_black = median(perc_black_nohisp),
            median_hisp = median(perc_hisp_any),
            median_urban = median(perc_urban),
            mean_wwtp = mean(adj_wwtp_flow))

# * Add explanation to 
# * Add explanation 
#+ 1) Narrow range of %NHB than %Hisp
#+ 2) Region with highest %NHB also is region with lowest WW and lowest freq sources

#+ 

ggplot(dat_clean, aes(x = perc_hisp_any, y = perc_black_nohisp)) + geom_point()
ggplot(dat_clean, aes(x = perc_hisp_any, y = mdi_rate)) + geom_point()

length(unique(allsdwis3$PWSID)) == nrow(allsdwis3)
allsdwis3 %>% count(PRIMACY_AGENCY_CODE)
allsdwis3 %>% mutate(in_ucmr3 = ifelse(PWSID %in% dat_clean$PWSID, "yes", "no")) %>%
  group_by(PRIMACY_AGENCY_CODE, in_ucmr3) %>%
  count() %>%
  group_by(PRIMACY_AGENCY_CODE) %>%
  mutate(state_total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = 100*n/state_total) %>%
  arrange(-freq)

dat_test <- dat_clean %>% left_join(state_region_df)
dat_test


library(lme4)
glmer(det_any ~ perc_black_nohisp + (1|state), data = dat_clean, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
glmer(det_any ~ perc_black_nohisp + region + (1|state), data = dat_test, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)


glmer(det_any ~ perc_hisp_any + (1|state), data = dat_clean, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
glmer(det_any ~ n_fac_any + adj_wwtp_flow + (1|state), data = dat_clean, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
glmer(det_any ~ perc_hisp_any + n_fac_any + adj_wwtp_flow + (1|state), data = dat_clean, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
glmer(det_any ~ perc_hisp_any + n_fac_any + adj_wwtp_flow + pws_type + (1|state), data = dat_clean, family = 'binomial') %>% 
  broom.mixed::tidy(exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
