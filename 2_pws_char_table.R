# DATE STARTED: 2023-03-13
# AUTHOR: Aaron Maruzzo
# PURPOSE: Creates table summary of PWS in the study (Table 2 in paper)
# LATEST REVISION: 2024-11-12
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 2. Baseline characteristics of public water systems -------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Grouping types of UCMR3 systems (appears in the first column of the table in this order): 
#   * Overall (total N=4815 US PWSs)
#   * Systems that detected a target contaminant 
#   * Systems that did not detect a target contaminant 
#   * Systems that exceeded a health-reference conc for PFOA, PFOS, 1,4-dioxane, or 1,1-DCA
#   * Systems that did not exceed a health-reference conc for PFOA, PFOS, 1,4-dioxane, or 1,1-DCA
#   * Large systems 
#   * Small systems 
#   * Groundwater (GW) systems 
#   * Surface water (SW) systems 
#   * Systems that use GW under the influence of SW or a combination of GW/SW sources (MIX) 
# 
# Variables of interest (2nd-9th columns) are: 
#   * Number of systems
#   * Total population served (sum of reported populations from SDWIS data)
#   * Proportion of systems with any target contaminant detects
#   * Proportion of systems with any exceedance of a health-based reference conc for 
#       PFOA, PFOS, 1,4-dioxane, or 1,1-DCA.
#   * Mean (and std. deviation) number of samples collected during the UCMR 3 
#   * Median (and Q1-Q3) percent Hispanic among counties served 
#   * Median (and Q1-Q3) percent non-Hispanic Blank among counties served
#   * Median (and Q1-Q3) percent deprived among counties served 
#   * Median (and Q1-Q3) percent urban among counties served

# Start here (if not already run; left un-commented to source this script independently):
source("1_combine_process.R")

# stopifnot(nrow(dat_clean)==4808) # this was increased when updating MDI to 2010-2014 averages
stopifnot(nrow(dat_clean)==4815) # total number of systems in the study

# str(dat_clean)
# colnames(dat_clean)

# Function ----------------------------------------------------------------

calc_and_format_quantiles <- function(col){
  paste0(format(round(median(col),1), nsmall = 1), " (",
         format(round(quantile(col, 0.25), 1), nsmall = 1), ", ",
         format(round(quantile(col, 0.75), 1), nsmall = 1), ")")
}

# Overall summary ---------------------------------------------------------

# line 62 sums the total number of population served and required 
# na.rm=T. 5 systems do not have population served counts from 
# SDWIS and were coded as "NA" to denote missingness.

ore <- dat_clean %>%
  summarise(n = n(), 
            det_any_freq = 100*sum(det_any == 1)/n,
            exceed_any_freq = 100*sum(viol_any == 1)/n,
            pop_served = sum(pop_served, na.rm = T)/1e6, # in millions  
            mean_samp = paste0(round(mean(n_samples), 1), " (", round(sd(n_samples), 1), ")"), 
            median_hisp = calc_and_format_quantiles(perc_hisp_any), 
            median_no_hisp_black = calc_and_format_quantiles(perc_black_nohisp), 
            median_deprived = calc_and_format_quantiles(mdi_rate), 
            median_urban_household = calc_and_format_quantiles(perc_urban))

# checks with consistency
# median(dat_clean$perc_hisp_any); IQR(dat_clean$perc_hisp_any)
# median(dat_clean$perc_black_nohisp); IQR(dat_clean$perc_black_nohisp)
# median(dat_clean$mdi_rate); IQR(dat_clean$mdi_rate)
# median(dat_clean$perc_urban); IQR(dat_clean$perc_urban)

# Lines below groups by variables in the vector my_vars and does the 
# same operations as the lines above and for different groupings (eg small systems only). 

tab <- list()

my_vars <- c("det_any", "viol_any", "size", "pws_type")

for (var in my_vars) {
  
  temp <- dat_clean %>% 
    group_by_at(var) %>%
    summarise(n = n(), 
              det_any_freq = 100*sum(det_any == 1)/n,
              exceed_any_freq = 100*sum(viol_any == 1)/n,
              pop_served = sum(pop_served, na.rm = T)/1e6, #in millions  
              mean_samp = paste0(round(mean(n_samples), 1), " (", round(sd(n_samples), 1), ")"), 
              median_hisp = calc_and_format_quantiles(perc_hisp_any), 
              median_no_hisp_black = calc_and_format_quantiles(perc_black_nohisp), 
              median_deprived = calc_and_format_quantiles(mdi_rate), 
              median_urban_household = calc_and_format_quantiles(perc_urban)
    )
  
  # Store result 
  tab[[var]] <- temp
}

# Bind outputs of the list into a data frame object.

tab1 <- bind_rows(tab)
tab1

# Bind tab1 with the overall summary results.

tab2 <- bind_rows(tab1, ore %>% mutate(overall = 'overall'))

# Make export ready.

tab3 <- tab2 %>% 
  pivot_longer(cols = c(det_any, viol_any, size, pws_type, overall)) %>%
  arrange(factor(name, levels = c('overall', 'det_any', 'viol_any',  'size', 'pws_type'))) %>%
  filter(!is.na(value)) %>%
  relocate(c(name, value), 1:2)

tab3 

# write.csv(tab3, paste0("results/Table 1. PWS Characteristics_", Sys.Date(), ".csv"))
