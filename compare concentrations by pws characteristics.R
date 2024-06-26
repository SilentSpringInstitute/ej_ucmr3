
colnames(ucmr3.2)

length(dat_clean$PWSID)

ucmr3.0 %>%
  group_by(PWSID, Size, FacilityWaterType, Contaminant) %>%
  summarise(n = n(), mean_lvl = mean(AnalyticalResultValue, na.rm=T)) %>%
  ungroup() %>%
  arrange(-mean_lvl) %>%
  filter(PWSID %in% dat_clean$PWSID) %>%
  distinct(PWSID, FacilityWaterType) %>%
  count(PWSID) %>%
  arrange(-n)

ucmr3.0 %>% select(PWSID, Size, Contaminant, AnalyticalResultValue)
dat_clean %>% select(PWSID, size, pws_type) %>%
  left_join(ucmr3.0 %>% 
              select(PWSID, Size, Contaminant, AnalyticalResultValue), 
            by = c("PWSID", "size" = "Size")) %>%
  group_by(PWSID, size, pws_type, Contaminant) %>%
  summarise(n = n(), mean_lvl = mean(AnalyticalResultValue, na.rm=T)) %>%
  filter(str_detect(Contaminant, "PF|HCFC|1,1-dichloro|1,4-dioxane")) %>%
  group_by(size, Contaminant) %>%
  summarise(n=n(), 
            perc90=quantile(mean_lvl, 0.90, na.rm =T), 
            perc75=quantile(mean_lvl, 0.75, na.rm=T),
  ) %>%
  pivot_wider(id_cols = Contaminant, names_from = size, values_from = c(perc90, perc75))

ucmr3.0 %>%
  group_by(PWSID, Size, FacilityWaterType, Contaminant) %>%
  summarise(n = n(), mean_lvl = mean(AnalyticalResultValue, na.rm=T)) %>%
  arrange(-mean_lvl) %>%
  filter(PWSID %in% dat_clean$PWSID) %>%
  filter(str_detect(Contaminant, "PF|HCFC|1,1-dichloro|1,4-dioxane")) %>%
  group_by(Size, Contaminant) %>%
  summarise(n=n(), 
            perc90=quantile(mean_lvl, 0.90, na.rm =T), 
            #perc75=quantile(mean_lvl, 0.75, na.rm=T),
            ) %>%
  pivot_wider(id_cols = c(Contaminant), 
              names_from = c(Size, n), 
              values_from = perc90) %>%
  mutate(small_is_ = ifelse(S > L, "more", "less or equal"))

dat_clean
