

df <- adjglmermods %>%
  select(-effect, -group, -base) %>%
  filter(!str_detect(term, "Intercept")) %>%
  clean_output()

df2 <- df %>%
  mutate(perc_change = as.numeric(str_extract(result, "-?\\d+\\.?\\d*")), 
         low_perc_change = as.numeric(str_extract(result, "(?<=\\()[+-]?\\d+(\\.\\d+)?(?=,)")), 
         high_perc_change = as.numeric(str_extract(result, "(?<=, )[+-]?\\d+(\\.\\d+)?(?=\\))"))) %>% 
  # we don't need the result column anymore
  select(-result)

# we only care about sociodemographic vars

sdvars <- c("Hispanic", "Black", "urban", "Percent")

unique(df2$contam.pfas)

contam.pfas_levels <- c("Detected ≥1 target contaminant", "Exceeded ≥1 HRL", "1,4-dioxane", "1,1-dichloroethane", 
                        "HCFC-22", "PFAS") 
# good first-pass
coefs_diffSESmeasures_plot <- df2 %>%
  # great! they all have 'percent' in their names 
  filter(str_detect(term, "Percent")) %>%
  ungroup() %>%
  
  # change term name for SES vars to "Percent SES variable"
  mutate(term = if_else(str_detect(term, "poverty|home|deprived|uninsur"), "Percent SES variable", 
                          as.character(term))) %>%
  
  # factor term
  mutate(term = factor(term, levels = c("Percent Hispanic", 
                                        "Percent Black, non-Hispanic", 
                                        "Percent SES variable", 
                                        "Percent urban households"))) %>%
  
  # clean sesvar 
  mutate(sesvar = case_when(sesvar == "mdi" ~ "Percent Deprived", 
                            sesvar == "perc_hmown" ~ "Percent home ownership", 
                            sesvar == "perc_pov_ppl" ~ "Percent poverty", 
                            sesvar == "perc_uninsur" ~ "Percent Uninsured")) %>%
  mutate(sesvar = factor(sesvar, levels = c("Percent Deprived", "Percent poverty", 
                                            "Percent home ownership", "Percent Uninsured"))) %>%
  
  # clean contam.pfas 
  mutate(contam.pfas = case_when(contam.pfas == "evrdet" ~ "Detected ≥1 target contaminant", 
                                 contam.pfas == "hlvlchem" ~ "Exceeded ≥1 HRL",
                                 #str_detect(contam.pfas, "dichlor") ~ "1,1-dichlor-\noethane",
                                 TRUE ~ as.character(contam.pfas))) %>%
  mutate(contam.pfas = factor(contam.pfas, levels = contam.pfas_levels)) %>%
  
  
  ggplot(aes(x = perc_change, y = fct_rev(sesvar))) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'grey75') + 
  geom_point(size = 0.3) + 
  geom_pointrange(size = 0.3, aes(xmin = low_perc_change, xmax = high_perc_change)) +
  facet_grid(term~contam.pfas, 
             labeller = label_wrap_gen(10)) + 
  # formatting
  labs(x = "Percent Change (95% CI)", y = "") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

# ggsave(plot = coefs_diffSESmeasures_plot, filename = paste0("Sociodem_coefs_diff_SES_", Sys.Date(), ".png"))
