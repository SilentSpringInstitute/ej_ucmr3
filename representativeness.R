# 6/26/24 
# AM 
# Compare representative-ness 

# Start here ------------------------------------------------------------------
# 
# If not already in the working environment, run:
# source("3. analyze ucmr3 - bivar comparisons (t-tests, Exact test).R")
# to identify which counties were included in the paper

library(tidyverse)
library(patchwork)
library(janitor)

# Water systems ---------------------------------------------------------------

# Data sources: 
# Active PWSs in 2013 Q4 
# Active and inactive PWSs in 2013 Q4
# Our main dataset (dat_clean)

# https://echo.epa.gov/tools/data-downloads/sdwa-download-summary

# Systems = Active (A), Changed from public to non-public (N), Merged with another system (M)

sdwis2013 <- read_xlsx("raw/Water System Detail_20240624.xlsx")

colnames <- as.character(as.vector(sdwis2013[4,]))
colnames <- str_to_lower(colnames)
colnames <- str_replace_all(colnames, " ", "_")
colnames

sdwis2013 <- sdwis2013[-(1:4),]
colnames(sdwis2013) <- colnames

dim(sdwis2013)
str(sdwis2013)

unique(sdwis2013$activity_status)

# Counties --------------------------------------------------------------------

cn15 
county_ids_vec 

str(cn15)
# check if cn15 has one line per county.
stopifnot(nrow(cn15) == nrow(distinct(cn15)))

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
               aes(label = after_stat(y)),
               position = position_nudge(x = 0.4, y = 5)) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) + 
  theme_bw() +
  theme(title = element_text(size = 9))
}

p1 <- make_county_plot(demo = perc_hisp_any) + labs(x = "", y = "Percent", title = "Percent Hispanic")
p2 <- make_county_plot(demo = perc_black_nohisp)  + labs(x = "", y = "", title = "Percent NH Black")
p3 <- make_county_plot(demo = perc_urban) + labs(x = "", y = "", title = "Percent urban")
p4 <- make_county_plot(demo = mdi_rate) + labs(x = "", y = "", title = "Percent deprived")

p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
