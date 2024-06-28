# 6/26/24 
# AM 
# Compare representativeness 

# Water systems ---------------------------------------------------------------

# Data sources: 
# Active PWSs in 2013 Q4 
# Active and inactive PWSs in 2013 Q4
# Our main dataset (dat_clean)

dat_clean


# Counties --------------------------------------------------------------------

cn15 
county_ids_vec 

str(cn15)
# check if cn15 has one line per county.
stopifnot(nrow(cn15) == nrow(distinct(cn15)))

# restrict to counties in the main UCMR3 sample.
cn15.1 <- cn15 %>% filter(GEO.id2 %in% county_ids_vec)

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

boxplot(cn15$perc_urban)

make_county_plot <- function(demo){
  cn15 %>%
  mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
    group_by(in_paper) %>%
  mutate(in_paper = paste0(in_paper, "\nN=", length(in_paper))) %>%
  ggplot(aes(x = in_paper, y = !!enquo(demo))) + 
  geom_jitter(shape = 1, 
              color = "grey75") +
  stat_summary(fun.y = "median",
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

library(patchwork)
p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
