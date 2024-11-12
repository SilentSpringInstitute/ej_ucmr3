# DATE STARTED: 2024-06-28
# AUTHOR: Aaron Maruzzo
# PURPOSE: Explore distribution of UCMR3 systems relative to EPA SDWIS systems
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

#===============================================================================
# start here
#===============================================================================

# If not already in the working environment, run:
# source("3. analyze ucmr3 - bivar comparisons (t-tests, Exact tests).R")
# to identify which counties were included in the paper

library(tidyverse)
library(patchwork)
library(janitor)

# Counties --------------------------------------------------------------------

# census data 
cn15 
# vector of counties linked to a UCMR 3 
length(county_ids_vec) #1718

# check if cn15 has one line per county.
stopifnot(nrow(cn15) == nrow(distinct(cn15)))

# calculate the median and IQR for perc hispanic, perc black, perc urban, and perc deprived
# between counties that were linked to a UCMR 3 (n=1718) versus not linked 
# (n=1424)

cn15 %>% 
  mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
  group_by(in_paper) %>%
  summarise(
    n = n(), 
    med_hisp_any = median(perc_hisp_any), 
    iqr_hisp = IQR(perc_hisp_any),
    med_black = median(perc_black_nohisp), 
    iqr_black = IQR(perc_black_nohisp),
    med_mdi = median(mdi_rate, na.rm=T), 
    iqr_mdi = IQR(mdi_rate, na.rm=T),
    med_urban = median(perc_urban),
    iqr_urban = IQR(perc_urban)
  )

#===============================================================================
# plot 
#===============================================================================

## this function outputs a plot of counties stratified by whether they were 
## linked to a UCMR 3 system (x-axis) and % demographic. overlays a red line 
## and value corresponding to the median value in % demographic. 
## users assign a demographic value ("demo"), which refers to a column in "cn15" 
## data

make_county_plot <- function(demo){
  cn15 %>%
  mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
  group_by(in_paper) %>%
  mutate(in_paper = paste0(in_paper, "\nN=", length(in_paper))) %>%
  ungroup() %>% 
  ggplot(aes(x = in_paper, y = !!enquo(demo))) + 
  geom_jitter(shape = 1, 
              color = "grey75") +
  stat_summary(fun = "median",
               fun.min = "median",
               fun.max= "median", 
               size= 0.3, 
               geom = "crossbar", 
               color = "red") + 
  stat_summary(fun = "median",
               colour = "red", 
               size = 4,
               geom = "text", 
               aes(label = after_stat(round(y, 1))),
               position = position_nudge(x = 0.1, y = 10)) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) + 
  labs(x = "") + 
  theme_bw() +
  theme(title = element_text(size = 9))
}

# make plots
p1 <- make_county_plot(demo = perc_hisp_any) + labs(y = "Percent", title = "Percent Hispanic")
p2 <- make_county_plot(demo = perc_black_nohisp)  + labs(y = "", title = "Percent NH Black")
p3 <- make_county_plot(demo = perc_urban) + labs(y = "", title = "Percent urban")
p4 <- make_county_plot(demo = mdi_rate) + labs(y = "", title = "Percent deprived")

# define axis titles and axes
axis_title <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) + geom_blank() +
  theme_void() + 
  theme(axis.title.x = element_text()) + 
  labs(x = "County linked to a UCMR 3 water system")

# combine plots together
plot <- (p1 + p2 + p3 + p4) + plot_layout(nrow = 1) 
plot <- patchworkGrob(plot)
x.label <- grid::textGrob("County linked to a UCMR 3 water system", vjust = -1)
gridExtra::grid.arrange(plot, bottom = x.label)

# # PWS types in the UCMR 3 -----------

sys_in_ucmr3 <- dat_clean %>% filter(!is.na(det_pfas)) %>% pull(PWSID)
length(sys_in_ucmr3) #4920

## import data
## from EPA SDWIS 2013 Q4
sdwis2013 <- read_excel("raw/Water System Detail_20240624.xlsx")

## clean column names
clean_colnames <- sdwis2013[4,] %>% pivot_longer(cols = everything()) %>% pull(value)
clean_colnames <- str_to_lower(clean_colnames)
clean_colnames <- str_replace_all(clean_colnames, " ", "_")
sdwis2013_clean <- sdwis2013[-c(1:4),]
colnames(sdwis2013_clean) <- clean_colnames

df_sys_ucmr3 <- sdwis2013_clean %>%
  filter(pws_id %in% sys_in_ucmr3)

allsdwis3 %>% filter(PWSID %in% sys_in_ucmr3) %>% count(PWS_TYPE_CODE) %>% 
  mutate(sum = sum(n), freq = 100*n/sum)

allsdwis3 %>% 
  filter(PWSID %in% sys_in_ucmr3) %>%
  left_join(dat_clean %>%
              select(PWSID, size)) %>%
  # mutate(size = if_else(WS.POPULATION_SERVED_COUNT > 10000, "Large", "Small")) %>%
  count(size) %>%
  mutate(sum = sum(n), freq = 100*n/sum)
  
# the SDWIS data is missing one system
## this was registered as an "inactive" system in SDWIS, and the downloaded
## data only included active systems
nrow(df_sys_ucmr3) != length(sys_in_ucmr3)
setdiff(sys_in_ucmr3, df_sys_ucmr3$pws_id)  # MS0130025
sdwis2013_clean %>% filter(pws_id == "MS0130025") # empty
# it is in the other downloaded data 
allsdwis3 %>% filter(PWSID == "MS0130025")
newrow <- allsdwis3 %>% filter(PWSID == "MS0130025")
# length(colnames(newrow)) == length(colnames(sdwis2013_clean))
newrow1 <- newrow %>% select(PWSID, PWS_NAME, PWS_TYPE_CODE, WS.POPULATION_SERVED_COUNT, WS.GW_SW_CODE)
# 
df_pws <- sdwis2013_clean %>% select(pws_id, pws_name, pws_type, population_served_count, primary_source)
colnames(newrow1) <- colnames(df_pws)
newrow1 <- newrow1 %>% mutate(pws_type = if_else(pws_type=="CWS","Community water system","999"))
newrow1 <- newrow1 %>% mutate(primary_source = if_else(primary_source=="GW", "Ground water", "999"))
stopifnot(newrow1$pws_type != "999" | newrow1$primary_source != "999")
df_pws <- df_pws %>% mutate(population_served_count = as.double(population_served_count))
df_pws2 <- df_pws %>% bind_rows(newrow1)
nrow(df_pws2)  # 150333

df_pws2

# categorize by size
df_pws3 <- df_pws2 %>% 
  left_join(dat_ucmr3 %>% select(c("pws_id"="PWSID"), size))

# filter for PWSIDs in study sample
df_pws3 <- df_pws3 %>% 
  filter(pws_id %in% dat_clean$PWSID)

df_pws3 %>% count(size)

## check with ucmr3 
# dat.check <- dat_clean %>% select(PWSID, size) %>% rename(ucmrsize = size)
# df_pws3.check <- df_pws3 %>% left_join(dat.check, by = c("pws_id"="PWSID"))
# df_pws3.check %>% 
#   filter(pws_id %in% sys_in_ucmr3) %>%
#   mutate(mismatch = case_when(
#     size == "Large" & ucmrsize == "L" ~ "ok", 
#     size == "Small" & ucmrsize == "S" ~ "ok", 
#     TRUE ~ paste0("mismatch, SDWIS: ", size, " UCMR3: ", ucmrsize)
#   )) %>%
#   count(mismatch)
# df_pws3 %>%
#   filter(size == "Large") %>%
#   filter(pws_type %in% c("Community water system", "Non-Transient non-community system"))

# nrow(df_pws3)
# df_ucmr3_oretype %>% mutate(sum = sum(n), freq = 100*n/sum, freq = round(freq,0))
# df_all_oretype %>% mutate(sum = sum(n), freq = 100*n/sum, freq = round(freq,0))
# df_pws3 %>% count(size)

df_all_oretype <- df_pws3 %>% count(pws_type)
df_all_pws <- df_pws3 %>% group_by(size) %>% count(pws_type)
df_ucmr3_pws <- df_pws3 %>% filter(pws_id %in% sys_in_ucmr3) %>% group_by(size) %>% count(pws_type)
df_ucmr3_oretype <- df_pws3 %>% filter(pws_id %in% sys_in_ucmr3) %>% count(pws_type)

df_all_pws2 <- df_all_pws %>% mutate(total = sum(n), freq = 100*n/total, freq = if_else(freq > 10, round(freq, 0), round(freq, 1)))
df_all_pws2 <- df_all_pws2 %>% mutate(n = paste0(prettyNum(n, big.mark=","), " (", freq, ")")) %>% select(-total, -freq)
df_all_pws2

df_all_oretype2 <- df_all_oretype %>% mutate(total = sum(n), freq = 100*n/total, freq = if_else(freq > 10, round(freq, 0), round(freq, 1)))
df_all_oretype2 <- df_all_oretype2 %>% mutate(n = paste0(prettyNum(n, big.mark=","), " (", freq, ")")) %>% select(-total, -freq)
df_all_oretype2

df_ucmr3_oretype2 <- df_ucmr3_oretype %>% mutate(total = sum(n), freq = 100*n/total, freq = if_else(freq > 10, round(freq, 0), round(freq, 1)))
df_ucmr3_oretype2 <- df_ucmr3_oretype2 %>% mutate(n = paste0(prettyNum(n, big.mark=","), " (", freq, ")")) %>% select(-total, -freq)
df_ucmr3_oretype2

df_ucmr3_pws2 <- df_ucmr3_pws %>% mutate(total = sum(n), freq = 100*n/total, freq = if_else(freq > 10, round(freq, 0), round(freq, 1)))
df_ucmr3_pws2 <- df_ucmr3_pws2 %>% mutate(n = paste0(prettyNum(n, big.mark=","), " (", freq, ")")) %>% select(-total, -freq)
df_ucmr3_pws2

df_top <- df_all_pws2 %>% left_join(df_ucmr3_pws2, by = c('size', 'pws_type'))
df_bottom <- df_all_oretype2 %>% left_join(df_ucmr3_oretype2, by = c('pws_type'))

df_representative <- df_top %>% rbind(df_bottom)

df_total1 <- df_pws %>% mutate(pws_type = "total") %>% count(pws_type)
df_total2 <- df_pws3 %>% filter(pws_id %in% sys_in_ucmr3) %>% mutate(pws_type = "total") %>% count(pws_type)
df_total3 <- df_total1 %>% left_join(df_total2, by = "pws_type")

df_representative2 <- df_representative %>% bind_rows(df_total3 %>% mutate(n.x = as.character(n.x), 
                                                     n.y = as.character(n.y)))
df_representative3 <- df_representative2 %>%
  rename(num_sys_sdwis = n.x, num_sys_ucmr3 = n.y) %>%
  mutate(size = replace_na(size, ""), 
         num_sys_ucmr3 = replace_na(num_sys_ucmr3, "0 (0)"))

df_representative3

write.csv(df_representative3, 
          "results/Suppl Table. Num of systems in SDWIS versus UCMR3 study.csv")

df_pws$primary_source %>% unique()

allsdwis3 %>%
  filter(PWSID %in% sys_in_ucmr3) %>%
  count(PWS_TYPE_CODE) %>%
  mutate(freq = 100*n/sum(n))

sdwis2013_clean %>%
  filter(pws_id %in% sys_in_ucmr3) %>%
  mutate(size = if_else(population_served_count >= 10000, "L", "S")) %>%
  count(pws_type)

allsdwis3 %>% filter(PWS_ACTIVITY_CODE == "A")

unique(sdwis2013_clean$activity_status)

sdwis2013_clean %>% 
  mutate()

# filter(PWSID %in% dat_clean$PWSID) %>%
  mutate(size = if_else(WS.POPULATION_SERVED_COUNT >= 10000, "L", "S")) %>%
  count(size, PWS_TYPE_CODE)

summary(dat_clean$perc_urban)
hist(dat_clean$perc_urban)

dat_clean %>%
  mutate(is_zero = if_else(perc_urban == 0, 'yes', 'no')) %>%
  count(is_zero)
