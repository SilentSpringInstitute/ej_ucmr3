library(tidyverse)
library(janitor)

sdwis2013 <- read_xlsx("raw/Water System Detail_20240624.xlsx")

colnames <- as.character(as.vector(sdwis2013[4,]))
colnames <- str_to_lower(colnames)
colnames <- str_replace_all(colnames, " ", "_")
colnames

sdwis2013 <- sdwis2013[-(1:4),]
colnames(sdwis2013) <- colnames

sys_in_ucmr3 <- unique(ucmr3_raw$PWSID)
length(sys_in_ucmr3) #5401 water systems

sys_in_ucmr3 <- unique(ucmr_detcode$PWSID)
length(sys_in_ucmr3) #4815 water systems <- USE THIS

# Start analysis here ---------------------------------------------------------

# Of the total number of water systems in SDWIS, how many were part of the UCMR3? 
# total number = 150,332 systems 
# in the ucmr3 = 4,814
# NOTE: 
#  - one system is missing (MS0130025). WHY? It was coded as inactive status. 

setdiff(sys_in_ucmr3, sdwis2013$pws_id)
# check Amanda's SDWIS downloads
allsdwis %>% filter(PWSID == "MS0130025")

sdwis2013 %>%
  mutate(in_ucmr3 = if_else(pws_id %in% sys_in_ucmr3, "yes", "no")) %>%
  tabyl(in_ucmr3) %>%
  adorn_totals()

sdwis2013 %>%
  mutate(in_ucmr3 = if_else(pws_id %in% sys_in_ucmr3, "yes", "no")) %>%
  group_by(in_ucmr3) %>% 
  summarise(total_pop_served = sum(as.numeric(population_served_count))) %>%
  ungroup() %>%
  mutate(total_pop = sum(total_pop_served), 
         freq = 100*total_pop_served/total_pop)

# size 

# systems in SDWIS
size1 <- sdwis2013 %>% 
  mutate(size_cat = case_when(
    as.numeric(population_served_count) > 10000 ~ "L", 
    as.numeric(population_served_count) > 3300 & 
      as.numeric(population_served_count) <= 10000 ~ "S", 
    as.numeric(population_served_count) >= 0 & 
      as.numeric(population_served_count) <= 3300 ~ "verySm",
    TRUE ~ "oops"
  ))

# check 1: no missing size categories
stopifnot(nrow(filter(size1, size_cat == "oops"))==0)

# systems in the paper
size2 <- ucmr_detcode %>% distinct(PWSID, Size)

size3 <- size1 %>%
  left_join(
    size2 %>% 
      mutate(in_ucmr3 = "yes") %>%
      rename(size_in_paper = Size), 
    by = c("pws_id" = "PWSID")
  ) %>%
  # if a system is in size2 but not in size1, then that system was 
  # not in the ucmr3 paper and merged with a blank ("NA")
  mutate(in_ucmr3 = if_else(is.na(in_ucmr3), "no", in_ucmr3))

size4 <- size3 %>% 
  mutate(same_or_different = case_when(
    in_ucmr3 == "yes" & size_cat == size_in_paper ~ "same", 
    in_ucmr3 == "yes" & size_cat != size_in_paper ~ "different", 
    in_ucmr3 == "no" ~ "-9999", 
    TRUE ~ "oops"
  )) 

size4 %>% 
  filter(in_ucmr3 == "yes") %>% 
  tabyl(same_or_different)

size4 %>%
  filter(same_or_different == "different") %>%
  count(size_cat, size_in_paper)

size3 %>%
  filter(size_cat == "L") %>%
  filter(primacy_type == "State") %>%
  tabyl(in_ucmr3) %>%
  adorn_totals()

size3 %>% 
  filter(size_cat == "S") %>%
  tabyl(in_ucmr3)

size1 %>% tabyl(size_cat)

# source water type 

unique(sdwis2013$primary_source)

source1 <- sdwis2013 %>%
  mutate(pws_type = case_when(
    primary_source == "Ground water" ~ "GW", 
    primary_source == "Ground water purchased" ~ "GW",
    primary_source == "Surface water purchased" ~ "SW", 
    primary_source == "Surface water" ~ "SW", 
    primary_source == "Unknown Primary Source" ~ "UNK", 
    primary_source == "Groundwater under influence of surface water" ~ "MIX", 
    primary_source == "Purchased ground water under influence of surface water source" ~ "MIX", 
    TRUE ~ "oops"
  ))

source1 %>% count(primary_source, pws_type) 

source1 %>% count(pws_type) %>% mutate(total = sum(n), freq = 100*n/total)

source2 <- ucmr_detcode %>% distinct(PWSID, source_type)

source3 <- source1 %>%
  select(pws_id, primary_source, pws_type) %>%
  left_join(source2 %>% 
              mutate(in_ucmr3 = "yes") %>%
              rename(pws_type_in_ucmr3 = source_type), 
            by = c("pws_id" = "PWSID"))

source4 <- source3 %>% 
  mutate(in_ucmr3 = if_else(is.na(in_ucmr3), "no", in_ucmr3)) %>%
  mutate(same_or_different = case_when(
    in_ucmr3 == "yes" & pws_type == pws_type_in_ucmr3 ~ "same", 
    in_ucmr3 == "yes" & pws_type != pws_type_in_ucmr3 ~ "different", 
    in_ucmr3 == "no" ~ "-9999", 
    TRUE ~ "oops"
  )) 

source4 %>% 
  filter(in_ucmr3 == "yes") %>% 
  tabyl(same_or_different)

source4 %>% 
  filter(in_ucmr3 == "yes") %>% 
  filter(same_or_different == "different") %>%
  count(primary_source, pws_type_in_ucmr3)

sys_id_small <- size4 %>% filter(size_cat == "S") %>% pull(pws_id) %>% unique()
length(sys_id_small) # 5,157 systems are small
100*length(sys_id_small) / nrow(size4)

source5 <- source4 %>%
  filter(pws_id %in% sys_id_small) %>%
  count(in_ucmr3, pws_type) %>%
  group_by(in_ucmr3) %>%
  mutate(total = sum(n), 
         freq = 100*n/total) %>%
  ungroup()

source5 %>%
  mutate(n = paste0(n, " (", round(freq,1), ")")) %>%
  pivot_wider(id_cols = pws_type, names_from = in_ucmr3, values_from = n)

table(source2$source_type)

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
