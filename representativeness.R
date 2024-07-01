# 6/26/24 
# AM 
# Compare representative-ness 

# Start here ------------------------------------------------------------------
# 
# If not already in the working environment, run:
# source("3. analyze ucmr3 - bivar comparisons (t-tests, Exact tests).R")
# to identify which counties were included in the paper

library(tidyverse)
library(patchwork)
library(janitor)

# Counties --------------------------------------------------------------------

cn15 
county_ids_vec 

str(cn15)
# check if cn15 has one line per county.
stopifnot(nrow(cn15) == nrow(distinct(cn15)))

cn15 %>% 
  filter(GEO.id2 %in% county_ids_vec) %>%
  mutate(any_urbanized = if_else(perc_urban > 0, "yes", "no")) %>%
  tabyl(any_urbanized)

cn15 %>% 
  mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
  tabyl(in_paper) %>%
  adorn_totals()

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

p1 <- make_county_plot(demo = perc_hisp_any) + labs(y = "Percent", title = "Percent Hispanic")
p2 <- make_county_plot(demo = perc_black_nohisp)  + labs(y = "", title = "Percent NH Black")
p3 <- make_county_plot(demo = perc_urban) + labs(y = "", title = "Percent urban")
p4 <- make_county_plot(demo = mdi_rate) + labs(y = "", title = "Percent deprived")

axis_title <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) + geom_blank() +
  theme_void() + 
  theme(axis.title.x = element_text()) + 
  labs(x = "County linked to a UCMR 3 water system")

plot <- (p1 + p2 + p3 + p4) + plot_layout(nrow = 1) 
plot <- patchworkGrob(plot)
x.label <- grid::textGrob("County linked to a UCMR 3 water system", vjust = -1)
gridExtra::grid.arrange(plot, bottom = x.label)

# PWS types in the UCMR 3 -----------

sys_in_ucmr3 <- dat_ucmr3 %>% filter(!is.na(det_pfas)) %>% pull(PWSID)
length(sys_in_ucmr3) #4920
sdwis2013 <- read_excel("raw/Water System Detail_20240624.xlsx")

clean_colnames <- sdwis2013[4,] %>% pivot_longer(cols = everything()) %>% pull(value)
clean_colnames <- str_to_lower(clean_colnames)
clean_colnames <- str_replace_all(clean_colnames, " ", "_")

sdwis2013_clean <- sdwis2013[-c(1:4),]
colnames(sdwis2013_clean) <- clean_colnames
sdwis2013_clean

dat_clean %>% 
  tabyl(size) %>%
  adorn_totals()

colnames(allsdwis3)

sdwis2013_clean %>% 
  filter(pws_id %in% sys_in_ucmr3) %>%
  nrow()
  
allsdwis3 %>% 
  filter(PWSID %in% sys_in_ucmr3) %>%
  nrow()

sdwis2013_clean %>% 
  filter(pws_id %in% sys_in_ucmr3) %>%
  mutate(size = if_else(population_served_count >= 10000, "L", "S")) %>%
  count(pws_type)

allsdwis3 %>% 
  filter(PWSID %in% dat_clean$PWSID) %>%
  mutate(size = if_else(WS.POPULATION_SERVED_COUNT >= 10000, "L", "S")) %>%
  count(size, PWS_TYPE_CODE)

summary(dat_clean$perc_urban)
hist(dat_clean$perc_urban)

dat_clean %>%
  mutate(is_zero = if_else(perc_urban == 0, 'yes', 'no')) %>% 
  count(is_zero)
