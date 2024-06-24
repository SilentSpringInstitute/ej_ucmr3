library(janitor)

allsdwis_dc %>% 
  mutate(in_UCMR3 = if_else(PWSID %in% ucmr_detcode$PWSID, "yes", "no")) %>%
  tabyl(in_UCMR3) %>%
  adorn_pct_formatting()

# https://enviro.epa.gov/enviro/EF_METADATA_HTML.sdwis_page?p_column_name=PRIMARY_SOURCE_CODE

table(allsdwis_dc$WS.GW_SW_CODE)
table(allsdwis_dc$WS.PRIMARY_SOURCE_CODE)

source1 <- allsdwis_dc %>%
  select(PWSID, WS.PRIMARY_SOURCE_CODE) %>%
  mutate(definition = case_when(
    WS.PRIMARY_SOURCE_CODE == "GW" ~ "groundwater", 
    WS.PRIMARY_SOURCE_CODE == "SW" ~ "surface water", 
    WS.PRIMARY_SOURCE_CODE == "GU" ~ "groundwater under the influence of surface water", 
    WS.PRIMARY_SOURCE_CODE == "GUP" ~ "purchased ground water under influence of surface water source", 
    WS.PRIMARY_SOURCE_CODE == "GWP" ~ "groundwater purchased", 
    WS.PRIMARY_SOURCE_CODE == "SWP" ~ "surface water purchased", 
  )) %>%
  mutate(source_type = case_when(
    WS.PRIMARY_SOURCE_CODE %in% c("GW", "GWP") ~ "GW", 
    WS.PRIMARY_SOURCE_CODE %in% c("SW", "SWP") ~ "SW", 
    WS.PRIMARY_SOURCE_CODE %in% c("GU", "GUP") ~ "MIX"
  ))

source1 %>% tabyl(source_type) %>% adorn_pct_formatting()

source1 %>% 
  mutate(in_UCMR3 = if_else(PWSID %in% ucmr_detcode$PWSID, "yes", "no")) %>%
  tabyl(in_UCMR3, source_type)

source2 <- ucmr_detcode %>%
  distinct(PWSID, source_type)

source3 <- source1 %>%
  left_join(source2 %>% 
              rename(source_in_paper = source_type))

source4 <- source3 %>% 
  filter(!is.na(source_in_paper)) %>%
  mutate(same_or_diff = 
           if_else(source_type == source_in_paper, 
                   "same", "diff")) 

source4 %>%
  count(same_or_diff)

# Size ---------

colnames(allsdwis_dc)
summary(allsdwis_dc$WS.POPULATION_SERVED_COUNT)

size1 <- allsdwis_dc %>%
  mutate(size = if_else(
    WS.POPULATION_SERVED_COUNT >= 10000, 
    "L", "S"
  )) %>%
  select(PWSID, size)

table(size1$size, exclude = "ifany")

size2 <- ucmr_detcode %>%
  distinct(PWSID, Size)

size3 <- size1 %>%
  left_join(size2 %>% 
              rename(size_in_paper = Size)) 

size4 <- size3 %>%
  count(size, size_in_paper) %>%
  mutate(size = if_else(is.na(size), "not_in_SDWIS", size)) %>%
  mutate(size_in_paper = if_else(is.na(size_in_paper), 
                                 "not_in_paper", 
                                 size_in_paper))

size4 %>%
  mutate(ore_total = sum(n)) %>%
  group_by(size) %>%
  mutate(size_total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = 100*n/size_total)

size4 %>%
  filter(size_in_paper %in% c("L", "S")) %>%
  mutate(ore_total = sum(n)) %>%
  group_by(size) %>%
  mutate(size_total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = 100*n/size_total)

hist(allsdwis_dc$WS.POPULATION_SERVED_COUNT)
allsdwis_dc %>% ggplot(aes(x=WS.POPULATION_SERVED_COUNT)) + geom_histogram()
min(allsdwis_dc$WS.POPULATION_SERVED_COUNT,na.rm=T)
