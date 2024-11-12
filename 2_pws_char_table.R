
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE 1. Characteristics of PWSs in study --------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ Headings are: Cat Level (e.g., yes/no, large/small, or for overall NA), Pop served, 
#+ % of PWSs with >=1 det and >=1 viol, mean number of samples and sd, 
#+ median and quartile proportions of Hispanic, non-His Black, deprived, and urban
#+ households

colnames(dat_clean)

dat_clean %>% filter(is.na(pop_served))

calc_and_format_quantiles <- function(col){
  paste0(format(round(median(col),1), nsmall = 1), " (",
         format(round(quantile(col, 0.25), 1), nsmall = 1), ", ",
         format(round(quantile(col, 0.75), 1), nsmall = 1), ")")
}


ore <- dat_clean %>%
  summarise(n = n(), 
            det_any_freq = 100*sum(det_any == 1)/n,
            exceed_any_freq = 100*sum(viol_any == 1)/n,
            pop_served = sum(pop_served, na.rm = T)/1e6, # in millions  
            # na.rm = T required ^ since 5 PWSs w/o info on pop served
            mean_samp = paste0(round(mean(n_samples), 1), " (", round(sd(n_samples), 1), ")"), 
            median_hisp = calc_and_format_quantiles(perc_hisp_any), 
            median_no_hisp_black = calc_and_format_quantiles(perc_black_nohisp), 
            median_deprived = calc_and_format_quantiles(mdi_rate), 
            median_urban_household = calc_and_format_quantiles(perc_urban))
ore

table1 <- list()

my_vars <- c("det_any", "viol_any", "size", "pws_type")

for (var in my_vars) {
  
  # Group by one of the variables and summarise
  temp_result <- dat_clean %>% 
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
  table1[[var]] <- temp_result
}

table1

final_table1 <- bind_rows(table1)
final_table1

final_table2 <- bind_rows(final_table1, ore %>% mutate(overall = 'overall'))

final_table3 <- final_table2 %>% 
  pivot_longer(cols = c(det_any, viol_any, size, pws_type, overall)) %>%
  arrange(factor(name, levels = c('overall', 'det_any', 'viol_any',  'size', 'pws_type'))) %>%
  filter(!is.na(value)) %>%
  relocate(c(name, value), 1:2)
final_table3 

# write.csv(final_table3, paste0("outputs/Table 1. PWS Characteristics_", Sys.Date(), ".csv"))
