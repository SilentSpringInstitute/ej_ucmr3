# DATE STARTED: 2024-06-28
# AUTHOR: Aaron Maruzzo
# PURPOSE: Explore distribution of UCMR3 system types (eg CWS/NTNCWS)  
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Supplemental Table 1. Distribution of systems by system type according to SDWIS
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Systems were classified by system size (depending on size of service population) and 
# by system type (depending on the nature of service populations). Systems 
# were classified in SDWIS, not the UCMR. System types were either community water 
# systems (CWS), transient non-community water systems (TNCWS), or 
# non-transient non-community water systems (NTNCWS).
# 
# We searched for system types for 4808 systems based on active systems listed in SDWIS on 2013 quarter 4: 
# https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/sdwis_fed_reports_public/200
# and compared the breakdown of CWS/TNCWS/NTNCWS by size among systems 
# in the UCMR3 and systems overall. Original files are available upon request. 

# start here: 
source("1_combine_process.R")

# Load SDWIS data ---------------------------------------------------------

sdwis2013 <- read_excel("raw/Water System Detail_20240624.xlsx")

# Clean column names.

clean_colnames <- sdwis2013[4,] %>% pivot_longer(cols = everything()) %>% pull(value)
clean_colnames <- str_to_lower(clean_colnames)
clean_colnames <- str_replace_all(clean_colnames, " ", "_")
sdwis2013_clean <- sdwis2013[-c(1:4),]
colnames(sdwis2013_clean) <- clean_colnames

stopifnot(nrow(sdwis2013_clean)==150332) # total number of water systems

# head(sdwis2013_clean)

# One system was not found in SDWIS ("MS0130025"). This system was classified
# as a community water system (CWS) serving 492 people according to the 
# other SDWIS data file used in previous scripts. According to SDWIS this 
# system was inactive during the time period. Bind a new row manually for 
# this one system.

setdiff(dat_clean$PWSID, sdwis2013_clean$pws_id)
newrow <- allsdwis3 %>%
  filter(PWSID == "MS0130025")

newrow1 <- tibble(pws_id = "MS0130025", 
       pws_name = newrow$PWS_NAME, 
       # pws_type = newrow$PWS_TYPE_CODE,
       # Use full name to be consistent with SDWIS
       pws_type = "Community water system",
       population_served_count = newrow$WS.POPULATION_SERVED_COUNT)

sdwis <- sdwis2013_clean %>%
  mutate(pws_type = as.character(pws_type), 
         population_served_count = as.numeric(population_served_count)) %>%
  bind_rows(newrow1)

# Categorize the size of systems. Systems were defined as large if they 
# were classified as large systems in the UCMR or, if not, served
# greater than 10,000 customers. Small systems were either classified as 
# small systems in the UCMR or, if not, served equal to or less than 
# 10,000 customers. 

sdwis1 <- sdwis %>%
  left_join(dat_clean %>% 
              select(PWSID, size), by = c("pws_id"="PWSID"))

sdwis2 <- sdwis1 %>%
  mutate(size = case_when(
    !is.na(size) ~ size, 
    is.na(size) & population_served_count > 10000 ~ "L", 
    is.na(size) & population_served_count <= 10000 ~ "S", 
    TRUE ~ "oops"
  ))

# Count -------------------------------------------------------------------

# Calculate total number of systems in SDWIS and UCMR3. The UCMR3 total must 
# be the same as in the paper (n=4808).

tot1 <- sdwis2 %>%
  summarise(n_sdwis = n(), 
            size = "total", pws_type = "total")

tot2 <- sdwis2 %>% 
  filter(pws_id %in% dat_clean$PWSID) %>%
  summarise(n_ucmr3 = n(), 
            size = "total", pws_type = "total")

tot3 <- tot1 %>% left_join(tot2)

# Calculate number of systems stratified by size and PWS type.

sdwis3 <- sdwis2 %>%
  group_by(size, pws_type) %>%
  summarise(n_sdwis = n())

sdwis4 <- sdwis2 %>%
  filter(pws_id %in% dat_clean$PWSID) %>%
  group_by(size, pws_type) %>%
  summarise(n_ucmr3 = n())

tab <- sdwis4 %>% right_join(sdwis3) %>% arrange(size)
tab1 <- tab %>% mutate(n_ucmr3 = replace_na(n_ucmr3, 0))

tab1

# Calculate number of systems stratified by PWS type only.

sdwis5 <- sdwis2 %>%
  group_by(pws_type) %>%
  summarise(n_sdwis = n())

sdwis6 <- sdwis2 %>%
  filter(pws_id %in% dat_clean$PWSID) %>%
  group_by(pws_type) %>%
  summarise(n_ucmr3 = n())

tab2 <- sdwis6 %>% right_join(sdwis5)
tab3 <- tab2 %>% mutate(n_ucmr3 = replace_na(n_ucmr3, 0))
tab3 <- tab3 %>% mutate(size = "overall")

# Combine into one table.

tab4 <- bind_rows(tab1, tab3, tot3)

# write.csv(tab4, 
#           "results/Suppl Table. Num of systems in SDWIS versus UCMR3 study.csv")

# Archive -----------------------------------------------------------------
 
# # If not already in the working environment, run:
# # source("3. analyze ucmr3 - bivar comparisons (t-tests, Exact tests).R")
# # to identify which counties were included in the paper
# 
# library(tidyverse)
# library(patchwork)
# library(janitor)
# 
# # Counties 
# # census data 
# cn15 
# # vector of counties linked to a UCMR 3 
# length(county_ids_vec) #1718
# 
# # check if cn15 has one line per county.
# stopifnot(nrow(cn15) == nrow(distinct(cn15)))
# 
# # calculate the median and IQR for perc hispanic, perc black, perc urban, and perc deprived
# # between counties that were linked to a UCMR 3 (n=1718) versus not linked 
# # (n=1424)
# 
# cn15 %>% 
#   mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
#   group_by(in_paper) %>%
#   summarise(
#     n = n(), 
#     med_hisp_any = median(perc_hisp_any), 
#     iqr_hisp = IQR(perc_hisp_any),
#     med_black = median(perc_black_nohisp), 
#     iqr_black = IQR(perc_black_nohisp),
#     med_mdi = median(mdi_rate, na.rm=T), 
#     iqr_mdi = IQR(mdi_rate, na.rm=T),
#     med_urban = median(perc_urban),
#     iqr_urban = IQR(perc_urban)
#   )
# 
# # plot 
# ## this function outputs a plot of counties stratified by whether they were 
# ## linked to a UCMR 3 system (x-axis) and % demographic. overlays a red line 
# ## and value corresponding to the median value in % demographic. 
# ## users assign a demographic value ("demo"), which refers to a column in "cn15" 
# ## data
# 
# make_county_plot <- function(demo){
#   cn15 %>%
#   mutate(in_paper = if_else(GEO.id2 %in% county_ids_vec, "yes", "no")) %>%
#   group_by(in_paper) %>%
#   mutate(in_paper = paste0(in_paper, "\nN=", length(in_paper))) %>%
#   ungroup() %>% 
#   ggplot(aes(x = in_paper, y = !!enquo(demo))) + 
#   geom_jitter(shape = 1, 
#               color = "grey75") +
#   stat_summary(fun = "median",
#                fun.min = "median",
#                fun.max= "median", 
#                size= 0.3, 
#                geom = "crossbar", 
#                color = "red") + 
#   stat_summary(fun = "median",
#                colour = "red", 
#                size = 4,
#                geom = "text", 
#                aes(label = after_stat(round(y, 1))),
#                position = position_nudge(x = 0.1, y = 10)) + 
#   scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) + 
#   labs(x = "") + 
#   theme_bw() +
#   theme(title = element_text(size = 9))
# }
# 
# # make plots
# p1 <- make_county_plot(demo = perc_hisp_any) + labs(y = "Percent", title = "Percent Hispanic")
# p2 <- make_county_plot(demo = perc_black_nohisp)  + labs(y = "", title = "Percent NH Black")
# p3 <- make_county_plot(demo = perc_urban) + labs(y = "", title = "Percent urban")
# p4 <- make_county_plot(demo = mdi_rate) + labs(y = "", title = "Percent deprived")
# 
# # define axis titles and axes
# axis_title <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) + geom_blank() +
#   theme_void() + 
#   theme(axis.title.x = element_text()) + 
#   labs(x = "County linked to a UCMR 3 water system")
# 
# # combine plots together
# plot <- (p1 + p2 + p3 + p4) + plot_layout(nrow = 1) 
# plot <- patchworkGrob(plot)
# x.label <- grid::textGrob("County linked to a UCMR 3 water system", vjust = -1)
# gridExtra::grid.arrange(plot, bottom = x.label)