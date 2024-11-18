# DATE STARTED: 2023-03-14
# AUTHOR: Aaron Maruzzo
# PURPOSE: Conduct t-tests for tables and figures
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# start here: 
# source("1_combine_process.R")

library(tidyverse)
library(ggplot2)
library(broom)
library(ggh4x)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------------------------------------------------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script created two figures in the paper. Figure 1 is a bar plot of 
# the average demographic characteristic of counties, stratified by whether 
# or not the county was linked to an industrial source of unregulated 
# contaminants. Figure 2 is a bar plot that compared average 
# demographics between systems that detected one or more unregulated contaminant
# versus not, and between systems that exceeded one or more health-reference
# concentration for PFOA, PFOS, 1-4-dioxane, or 1,1-dichloroethane, versus not.
# The main demographic characteristics of interest were % Hispanic, % non-Hispanic Black, 
# % deprived, and % urban. Potential industrial sources included the presence of a facility 
# that reported emissions of 1,4-dioxane, a facility that reported HCFC-22 or CFC-12
# emissions, or a facility that reported chlorinated solvent emissions. 
# Potential sources of PFAS include the presence of a PFAS airport, military 
# fire-training area (MFTA), or an EPA stewardship site.
# 
# We used unequal variance t-tests to compare averages between groups. We 
# reported averages and the results of t-tests in the supplement. Code in this 
# script produced tables in the supplement.
#
# The script above downloads processed data. It sources the script that loads the original 
# UCMR3 dataset into the working environment (1__ucmr3_process.R) as well as 
# various county-level demographic datasets (1__demo_process.R). See original 
# scripts for further info. 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Counties served by water systems in the UCMR3 -------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compare average levels of demographics between: 
#  * counties that had >=1 criteria TRI facility vs not
#  * counties that had >=1 1,4-dioxane TRI facility vs not 
#  * counties that had >=1 CFC TRI facility vs not
#  * counties that had >=1 chlorinated solvent facility vs not
#  * counties that had >=1 major PFAS industry, AFFF-certified airport, or a MFTA
# Demographics of interest: 
#  * Percent Hispanic
#  * Percent non-Hispanic Black 
#  * Percent deprived 
#  * Percent urban
#  * Percent of people in poverty (for sensitivity check of using % deprived)
#  * Percent of people who are uninsured (for sensitivity)
#  * Percent of homeownership (for sensitivity)

# Start with data frame object "fips_cn15."
# This object comes from "1__demo_process.R". See script for further info. 
# This object is a linked dataset that combined water systems to the counties served. 
# Each row should be distinct combination of a system and a county. For each pair, the dataset  
# has columns with the demographics of interest, with the exception of 
# percent homeownership. It also has columns of binary variables (prefix "bin_") 
# indicating the presence of potential sources. Because fips_cn15 links ALL systems 
# downloaded from SDWIS, we first restrict the dataset to include only 
# systems that are part of the analysis.

head(fips_cn15)

# Rename "GEO.id2" to "county_id."

pwsid_county_link <- fips_cn15 %>% 
  filter(PWSID %in% dat_clean$PWSID) %>% 
  rename(county_id = GEO.id2) %>%
  mutate(perc_hmown = 100*owned.house/all.house14)

# Check for uniqueness.

stopifnot(nrow(pwsid_county_link %>% count(PWSID, county_id) %>% filter(n > 1))==0)

# Counties were duplicated in the data since it can match with many systems. 
# For example, Calhoun County, Alabama has 4 different water systems that 
# sampled for target contaminants in the UCMR3.
# Use distinct() to capture unique combinations of county IDs, the demographic
# variables of interest, and the presence of potential sources.

county_dat <- pwsid_county_link %>% 
  distinct(county_id,
           # demographics 
           perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban, 
           perc_pov_ppl, perc_uninsur, perc_hmown,
           # potential sources
           n_fac_any, n_fac_diox, n_fac_cfc, n_fac_chlor_solv, 
           n_MFTA, n_airports, src_epa_present) 
county_dat

# Check for duplicates. Each row is one unique county. 
stopifnot(nrow(county_dat) == length(unique(county_dat$county_id)))

# Check for missing data. 
stopifnot(county_dat %>% filter(if_any(everything(), is.na)) %>% nrow() == 0)

# How many counties were matched to a UCMR 3 PWS overall? 1720 counties. 
nrow(county_dat)

# Create a new single column variable indicating whether either an MFTA facility, 
# AFFF-certified airport, or an EPA stewardship facility was present in the 
# county. Pivot everything long to make processing easier. 
# y = name of demographic variable of interest (eg percent deprived, perc Hisp, etc.)
# x = name of point source of interest (eg presence of any TRI facility)

county_dat1 <- county_dat %>%
  pivot_longer(cols = c(mdi_rate, starts_with("perc_")), 
               names_to = "y", 
               values_to = "yi") %>%
  mutate(bin_any_pfas = if_else(
    n_MFTA > 0 | n_airports > 0 | src_epa_present > 0, 
    1, 0
  )) %>%
  pivot_longer(cols = c(starts_with("n_fac_"), "bin_any_pfas"), 
               names_to = "x", 
               values_to = "xi")

# Nest data frame by the names of the long data. Two columns represent 
# the names of the demographic variables and point sources of interest, 
# and a third column is a list of data frames. This makes it easier to apply 
# a function to the list-column. Each data frame in the list-column is 1718 rows, 
# representing the counties.

county_dat2 <- county_dat1 %>%
  group_by(x, y) %>%
  nest()

# Define a function to loop over the list-column that calculates the 
# average demographic level (yi) for each group of xi (dichotomous, 1/0). 
# The function also conducts the unequal variance t-tests and returns the 
# p-value from those tests.

func_mean_demo <- function(dat, xi){
  # y is the column of outcomes. y is grouped.
  dat.1 <- dat %>%
    group_by(xi > 0) %>%
    summarise(
      n = n(), 
      mean = mean(yi), 
      sd = sd(yi)
    ) %>%
    ungroup() %>% 
    mutate(freq = 100*n/sum(n), .after = n)
  
  t <- t.test(dat$yi[dat$xi == 0], dat$yi[dat$xi > 0], var.equal = FALSE)
  
  dat.1 <- dat.1 %>% mutate(p = t$p.value)
  dat.1 <- dat.1 %>% mutate(p_star = gtools::stars.pval(p))

  return(dat.1)
}

# t-test: average perc deprived in counties with criteria TRI vs. none
# example: 
# func_mean_demo(dat = county_dat2$data[[1]], xi = xi)

# Apply the t-test function over the nested data.

county_results <- county_dat2 %>%
  mutate(res = map(data, ~func_mean_demo(dat = .x, xi = xi))) %>%
  select(-data) %>%
  unnest(res)

county_results

# Save progress.

# write.csv(county_results, paste0("results/SuppTable. Mean demos and TRI facs_",
#                                  Sys.Date(), ".csv"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure 1 ----------------------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Prepare data for plotting. Calculate standard errors, format p-values, and 
# define factor and labels that display in the plot. Note: in defining 
# factor levels of "y", we ignored demographic variables used for sensitivity 
# analyses (ie, ignored percent poverty, percent homeownership, percent uninsured).

county_ready2plot <- county_results %>%
  rename(xi = `xi > 0`) %>%
  mutate(
    se = sd/sqrt(n),
    p1 = case_when(
      xi == "FALSE" & p >= 0.001 ~ as.logical(NA),
      xi == "TRUE" & p < 0.001 ~ "p<0.001", 
      xi == "TRUE" & p >= 0.001 ~ paste0("p=", format(round(p, 2), nsmall=2))),
    y = factor(y,
               levels = c("perc_hisp_any", "perc_black_nohisp", "mdi_rate", "perc_urban"), 
               labels = c("Percent Hispanic", 
                          "Percent non-Hispanic Black",
                          "Percent deprived",
                          "Percent urban")), 
    x = factor(x, 
               levels = c("n_fac_any", "n_fac_diox", "n_fac_cfc", "n_fac_chlor_solv", "bin_any_pfas"), 
               labels = c("Any criteria facility", 
                          "1,4-dioxane facility", 
                          "CFC facility", 
                          "Chlorinated solvent facility", 
                          "PFAS airport, MFTA, or major industrial source")), 
    xi = factor(xi, 
                levels = c("FALSE", "TRUE"), 
                labels = c("No", "Yes")))

# Plot. Use custom functions from ggh4x library to assist in defining scales for 
# each facet and nesting labels.
# https://teunbrand.github.io/ggh4x/reference/guide_axis_nested.html

ggplot(county_ready2plot %>% filter(!is.na(y)), 
       aes(x = xi, y = mean, fill = xi)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = 'dodge') + 
  geom_bar(stat = 'identity', color = 'black', linewidth = 0.3) +
  geom_text(aes(label = p1), 
            position = position_stack(vjust = 1.4),
            size = 2) + 
  facet_grid(y ~ x, switch = "both",  labeller = label_wrap_gen(15), scales = "free_y")  + 
  facetted_pos_scales(y = list(
    y %in% c("Percent Hispanic", "Percent non-Hispanic Black") ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 25)), 
    y == "Percent deprived" ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 35)),
    y == "Percent urban" ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0,110))
  )) + 
  scale_fill_manual(values = c("#abd9e9", "#2c7bb6"))+ 
  theme_bw() + 
  labs(x = "", 
       y = "") + 
  theme(text = element_text(size = 12),
        ggh4x.axis.nestline = element_blank(), 
        panel.spacing.y = unit(0.8, "lines"),
        panel.grid = element_blank(),
        legend.position = 'none', 
        strip.placement = 'outside', 
        strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text.y.left = element_text(size = 10)
  ) 

# Save progress.

# ggsave(plot = last_plot(),
#        filename = paste0("results/Figure 1. Demographics and TRI facility_",
#                          Sys.Date(), ".pdf"),
#        width = 7, height = 6)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Average customers served by water systems ----------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compare average levels of demographics between: 
#  * systems that detected >=1 target contaminant vs none
#  * systems that exceeded >=1 health-reference concentration vs none
# Target contaminant referred to 1,4-dioxane, HCFC-22, 1,1-dichlorethane, or any PFAS in the UCMR3. 
# Health benchmarks referred to 2017 EPA values for 1,4-dioxane, 1,1-dichloroethane, PFOA, and PFOS.
# Demographics of interest: 
#  * Percent Hispanic
#  * Percent non-Hispanic Black 
#  * Percent deprived 
#  * Percent urban
#  * Percent of people in poverty (for sensitivity check of using % deprived)
#  * Percent of people who were uninsured (for sensitivity)
#  * Percent of homeownership (for sensitivity)

# Start with data frame object "dat_clean." See 1_combine_process.R. Select 
# relevant columns of interest for the analysis and pivot longer.

demo_pws_data <- dat_clean %>% 
  select(PWSID, det_any, viol_any, 
         mdi_rate, perc_hmown, perc_pov_ppl, perc_uninsur, 
         perc_hisp_any, perc_black_nohisp, perc_urban) %>%
  pivot_longer(cols = c(mdi_rate, starts_with("perc_")), 
               names_to = "demo_variable", 
               values_to = "demo_value") 

# Check: 4808 systems * 7 demographic variables
stopifnot(nrow(demo_pws_data)==4815*7)

# str(demo_pws_data)

# For each demographic variable name (eg "Percent Hispanic"), calculate the average
# level among systems overall, among systems that detected any target contaminants (det_any==1), 
# among systems that had no detections (det_any==0), among systems that 
# exceeded any health-reference conc (viol_any==1), and among systems that 
# did not exceed any health-reference conc (viol_any==0). Conduct a t-test
# to compare average levels between groups and return the p-value.

demo_pws_summary <- demo_pws_data %>%
  group_by(demo_variable) %>%
  summarise(
    all_n = length(unique(PWSID)), 
    ore_avg = mean(demo_value), 
    ore_sd = sd(demo_value),
    n_det0 = sum(det_any == 0),
    mean_det0 = mean(demo_value[det_any == 0]), 
    sd_det0 = sd(demo_value[det_any == 0]), 
    n_det1 = sum(det_any == 1), 
    mean_det1 = mean(demo_value[det_any == 1]), 
    sd_det1 = sd(demo_value[det_any == 1]),
    p_det = t.test(demo_value[det_any == 0], demo_value[det_any == 1], var.equal = FALSE)$p.value, 
    n_viol0 = sum(viol_any == 0), 
    mean_viol0 = mean(demo_value[viol_any == 0]), 
    sd_viol0 = sd(demo_value[viol_any == 0]),
    n_viol1 = sum(viol_any == 1),
    mean_viol1 = mean(demo_value[viol_any == 1]), 
    sd_viol1 = sd(demo_value[viol_any == 1]),
    p_viol = t.test(demo_value[viol_any == 0], demo_value[viol_any == 1], var.equal = FALSE)$p.value)

demo_pws_summary

# check by hand 
# calculate mean demographic values
# demo_pws_means <- demo_pws_data %>% group_by(det_any, demo_variable) %>% summarise(n = n(), mean = mean(demo_value), sd = sd(demo_value))
# demo_pws_means
# 
# demo_pws_means %>% pivot_wider(id_cols = demo_variable, names_from = c(det_any, n), names_glue = "detany_{det_any}_n{n}", values_from = mean)

# Save progress.

# write.csv(demo_pws_summary, paste0("results/Mean demographic levels by PWS outcomes_",
#                                    Sys.Date(), 
#                                    ".csv"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure 2 ----------------------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Description:
#
# x-axis : systems stratified by 2 groups w 2 outcomes each (detect/not; exceed/not)
#          show as nested x-axis.
# y-axis : average % demographic variable (with 1 standard error). Four demographic 
#          variables (% Hisp, % NH Black, % deprived, % urban).
# in plot: p-value on the right-most bar of each bar grouping indicate whether 
#          difference in means was significant in t-tests.
#
# color palette: 
pal <- c("#9bccc6", "#50a6a6", '#c2a5cf', '#7b3294')

# get means 
means <- demo_pws_summary %>% 
  select(demo_variable, starts_with("mean")) %>%
  pivot_longer(cols = starts_with("mean"), names_prefix = "mean_", names_to = "x", values_to = "means")

# get standard deviation
sd <- demo_pws_summary %>% 
  select(demo_variable, starts_with("sd")) %>% 
  pivot_longer(cols = starts_with("sd"), names_prefix = "sd_", names_to = "x", values_to = "sd") 

# get p-values
p <- demo_pws_summary %>% 
  select(demo_variable, starts_with("p")) %>% 
  pivot_longer(cols = starts_with("p_"), names_prefix = "p_", names_to = "x", values_to = "p") %>%
  mutate(x = paste0(x, "1"))

# get n's
n <- demo_pws_summary %>% 
  select(demo_variable, starts_with("n_")) %>% 
  pivot_longer(cols = starts_with("n_"), names_prefix = "n_", names_to = "x", values_to = "n")

# Combine pieces into a format for plotting.

demo_pws_data4plot <- means %>% 
  left_join(sd) %>% 
  left_join(p) %>% 
  left_join(n)

demo_pws_data4plot

# Create stars using gtools package. Calculate standard error. Create a grouping
# variable (called "x_group") to nest x-axis labels in the plot. This grouping 
# variable was either the detection outcome or the exceedance outcome.

demo_pws_data_ready2plot <- demo_pws_data4plot %>% 
  mutate(p_stars = gtools::stars.pval(p), 
         p_stars = if_else(str_detect(x, "1") & p_stars == " ", "n.s.", p_stars),
         p1 = case_when(
           p < 0.001 ~ "p<0.001", 
           p >= 0.001 ~ paste0("p=", format(round(p, 2), nsmall=2))),
         se = sd/sqrt(n), 
         x_group = if_else(str_detect(x, "det"), "det", "viol")) %>%
  rename(y = means) %>%
  select(x_group, demo_variable, n, x, y, se, sd, p, p1, p_stars)

# Ready labels.

demo_pws_data_ready2plot <- demo_pws_data_ready2plot %>% 
  filter(demo_variable %in% c("perc_hisp_any", "perc_black_nohisp", 
                              "perc_urban", "mdi_rate")) %>%
  mutate(demo_variable = factor(demo_variable, 
                                levels = c("perc_hisp_any", 
                                           "perc_black_nohisp", 
                                           "mdi_rate", 
                                           "perc_urban"), 
                                labels = c("Percent Hispanic", 
                                           "Percent non-Hispanic Black", 
                                           "Percent deprived", 
                                           "Percent urban")), 
         x_group = if_else(x_group == "det", "Target contaminant detected", "Health-reference level exceeded"), 
         x = if_else(str_detect(x, "0"), "No", "Yes"), 
         x_interact = interaction(x, x_group), 
         x_interact = factor(x_interact, 
                             levels = c("No.Target contaminant detected", 
                                        "Yes.Target contaminant detected", 
                                        "No.Health-reference level exceeded", 
                                        "Yes.Health-reference level exceeded")) 
  )
        
# Plot.

ggplot(demo_pws_data_ready2plot, 
       aes(x = x_interact, 
           y = y, 
           fill = x_interact)) + 
  geom_errorbar(aes(
                    ymin = y - se, 
                    ymax = y + se
                    ), 
                width = 0.25,
                position = 'dodge') + 
  geom_col(width = 0.9,
           position = position_dodge(width = 1.5),
           color = 'black', 
           linewidth = 0.3) +
  geom_text(aes(label = p1), 
            size = 4, 
            position = position_nudge(y = c(0,5,0,5,0,5,0,5,0,5,0,5,0,15,0,15))) +
  facet_wrap(~demo_variable, 
             strip.position = 'left',
             scales = 'free') +
  facetted_pos_scales(y = list(
    demo_variable %in% c("Percent Hispanic", "Percent non-Hispanic Black", "Percent deprived") ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 30)), 
    demo_variable == "Percent urban" ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0,110))
  )) + 
  scale_x_discrete(guide = 'axis_nested', 
                   labels = function(x) str_wrap(x, width = 15)) + 
  scale_fill_manual(name = "", 
                    values = c("#9bccc6", "#50a6a6", '#c2a5cf', '#7b3294')) +
  labs(x = "", y = "") + 
  theme_bw() + 
  theme(text = element_text(size = 12),
        ggh4x.axis.nestline = element_blank(), 
        panel.grid = element_blank(),
        legend.position = 'none', 
        strip.placement = 'outside', 
        strip.background = element_rect(fill = 'white', color = 'white'), 
        strip.text.y.left = element_text(size = 10))
      
# ggsave(plot = last_plot(),
#        paste0("results/Fig 2. Compare average demographics between water systems_", 
#               Sys.Date(), ".pdf"),
#        scale = 1.3,
#        height = 5,
#        width = 5)

# Archive -----------------------------------------------------------------

# test method from:
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7600
# test <- demo_pws_data %>% filter(demo_variable == "perc_hisp_any") 
# x <- test[test$det_any==1,]$demo_value
# y <- test[test$det_any==0,]$demo_value
# out <- t.test(x, y, var.equal = FALSE)
# str(out)
# out$estimate 
# str(out$estimate)
# out$estimate[1]
