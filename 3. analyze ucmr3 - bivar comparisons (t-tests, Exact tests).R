library(tidyverse)
library(flextable)
library(ggplot2)
library(broom)
library(ggh4x)

## If you are running code within this repo, download the relevant data from 
## script 2.

## ============================================================================
## download the data 
## ============================================================================
## "dat_clean" is the data of 4,808 UCMR 3 public water systems. Each system 
## is categorized by system size and source water type. Each system was previously
## linked to county-level data (perc Hispanic, perc Black, perc urban, perc deprived, 
## wastewater flow, the presence of any TRI, presence of any 1,4-d TRI, presence
## of any CFC TRI, presence of any chlorinated solvent TRI, presence of any 
## major PFAS industry, and presence of an AFFF-certified airport or MFTA. 
## A system classified as "present" = is linked to >=1 county with >=1 facility 
# 
## "fips_cn15" is data combining all PWS-county combinations identified in SDWIS 
## with demographic data, like perc Hispanic. 
## fips_cn15 contains PWSs that are NOT in the UCMR 3
source("2. create main datasets.R")

stopifnot(nrow(dat_clean) == 4808)

## ============================================================================
## unequal variance t-test
## 
## comparing average levels of demographics between (a) counties that 
## had (a) >=1 criteria TRI fac vs not; (b) 1,4-dioxane TRI fac; (c) CFC TRI fac; 
## (d) Chlorinated solvent fac; (e) major PFAS industry or PFAS airport/MFTA
## ============================================================================

pwsid_county_link <- fips_cn15 %>% 
  # filter for systems that were part of the UCMR 3
  filter(PWSID %in% dat_clean$PWSID) %>% 
  rename(county_id = GEO.id2)

# check for duplicate entries of PWS-county combinations
stopifnot(nrow(pwsid_county_link %>% count(PWSID, county_id) %>% filter(n > 1))==0)

## counties are duplicated in the data since it matched counties 
## with water systems. systems may serve >1 counties and 
## multiple systems can be in the same county
## use distinct

county.data <- pwsid_county_link %>% 
  distinct(county_id, perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban, 
         n_fac_any, n_fac_diox, n_fac_cfc, n_fac_chlor_solv, 
         n_MFTA, n_airports, src_epa_present) 
county.data

# each row is one unique county
stopifnot(nrow(county.data) == length(unique(county.data$county_id)))

# no missing data 
stopifnot(county.data %>% filter(if_any(everything(), is.na)) %>% nrow() == 0)

# how many counties were matched to a UCMR 3 PWS overall?
## 1718 counties 
nrow(county.data)

county.data1 <- county.data %>%
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

# nest dataframe, each group is one outcome (Y1=has facility, Y0=no fac) and 
# one demographic x
county.data2 <- county.data1 %>%
  group_by(x, y) %>%
  nest()

# function to apply to nested dataframes
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
func_mean_demo(dat = county.data2$data[[1]], xi = xi)

# 
county_res <- county.data2 %>%
  mutate(res = map(data, ~func_mean_demo(dat = .x, xi = xi))) %>%
  select(-data) %>%
  unnest(res)
county_res

## save progress

write.csv(county_res, paste0("results/Suppl. Mean demos and TRI facs.csv"))

## ============================================================================
## figure 1
## ============================================================================

county_ready2plot <- county_res %>%
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

ggplot(county_ready2plot, 
       aes(x = xi, y = mean, fill = xi)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25, position = 'dodge') + 
  geom_bar(stat = 'identity', color = 'black', linewidth = 0.3) +
  geom_text(aes(label = p1), 
            position = position_stack(vjust = 1.4),
            size = 2) + 
  facet_grid(y ~ x, switch = "both",  labeller = label_wrap_gen(15), scales = "free_y")  + 
  facetted_pos_scales(y = list(
    y %in% c("Percent Hispanic", "Percent non-Hispanic Black", "Percent deprived") ~ 
      scale_y_continuous(expand = c(0,0), limits = c(0, 25)), 
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

## save progress

ggsave(plot = last_plot(),
       filename = paste0("results/Figure 1. Demographics and TRI facility.pdf"),
       width = 7, height = 6)

## ============================================================================
## unequal variance t-test
## 
## comparing average levels of demographics between (a) water systems that 
## detected >=1 target contaminant versus 0 contaminants and (b) water systems 
## that exceeded >=1 EPA health benchmark level for PFAS, 1,4-d, or 1,1-DCA
## ============================================================================

# select relevant columns of interest and pivot to a long dataframe
demo_pws_data <- dat_clean %>% 
  select(PWSID, det_any, viol_any, 
         mdi_rate, perc_hmown, perc_pov_ppl, perc_uninsur, 
         perc_hisp_any, perc_black_nohisp, perc_urban) %>%
  pivot_longer(cols = c(mdi_rate, starts_with("perc_")), 
               names_to = "demo_variable", 
               values_to = "demo_value") 

# peek at data
str(demo_pws_data)

## test method http://www.sthda.com/english/wiki/wiki.php?id_contents=7600
# test <- demo_pws_data %>% filter(demo_variable == "perc_hisp_any") 
# x <- test[test$det_any==1,]$demo_value
# y <- test[test$det_any==0,]$demo_value
# out <- t.test(x, y, var.equal = FALSE)
# str(out)
# out$estimate 
# str(out$estimate)
# out$estimate[1]

## there are 4 outcomes of interest: detect UC (det1), did not detect (det0), 
## exceed EPA health benchmark (viol1), did not exceed (viol0). 
# for each outcome, calculate the number of systems, the average demographic measures in 
# each level of demo_variable. then, test for differences in average demographic 
# levels comparing systems with detections and exceedances (2 outcome groups).

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

## check by hand 
# calculate mean demographic values
# demo_pws_means <- demo_pws_data %>% group_by(det_any, demo_variable) %>% summarise(n = n(), mean = mean(demo_value), sd = sd(demo_value))
# demo_pws_means
# 
# demo_pws_means %>% pivot_wider(id_cols = demo_variable, names_from = c(det_any, n), names_glue = "detany_{det_any}_n{n}", values_from = mean)

# export 
write.csv(demo_pws_summary, paste0("results/", Sys.Date()," Mean demographic levels by PWS outcomes.csv"))

## ============================================================================
## figure 2
## ============================================================================
## useful links: 
# https://teunbrand.github.io/ggh4x/reference/guide_axis_nested.html

## plot description
# x-axis: systems stratified by 2 groups w 2 outcomes each (detect/not; exceed/not)
#  show as nested x-axis 
# y-axis: average % {demographic factor} +/- 1 sd
# in plot: p-value stars on the right-most bar in each group indicating whether 
#   the difference in means was significant in t-tests
# color palette: c("#9bccc6", "#50a6a6", '#c2a5cf', '#7b3294')

# get means 
means <- demo_pws_summary %>% 
  select(demo_variable, starts_with("mean")) %>%
  pivot_longer(cols = starts_with("mean"), names_prefix = "mean_", names_to = "x", values_to = "means")

# calculated averages for 7 demographic factors * 4 total outcomes = 28
## show 4 main demographic factors of interest (perc Hisp, perc Bl, perc deprived, perc_urban)
stopifnot(nrow(means) == 28)

# get standard deviation
sd <- demo_pws_summary %>% 
  select(demo_variable, starts_with("sd")) %>% 
  pivot_longer(cols = starts_with("sd"), names_prefix = "sd_", names_to = "x", values_to = "sd") 

# get p-values
p <- demo_pws_summary %>% 
  select(demo_variable, starts_with("p")) %>% 
  pivot_longer(cols = starts_with("p_"), names_prefix = "p_", names_to = "x", values_to = "p") %>%
  # to help joining later
  mutate(x = paste0(x, "1"))

# get n's
n <- demo_pws_summary %>% 
  select(demo_variable, starts_with("n_")) %>% 
  pivot_longer(cols = starts_with("n_"), names_prefix = "n_", names_to = "x", values_to = "n")

# combine into a format for plotting
plot.data <- means %>% 
  left_join(sd) %>% 
  left_join(p) %>% 
  left_join(n)
plot.data

# create stars, calculate std error, and create a grouping x-axis variable 
# called "x_group"
demo_pws_data_ready2plot <- plot.data %>% 
  mutate(p_stars = gtools::stars.pval(p), 
         p_stars = if_else(str_detect(x, "1") & p_stars == " ", "n.s.", p_stars),
         p1 = case_when(
           p < 0.001 ~ "p<0.001", 
           p >= 0.001 ~ paste0("p=", format(round(p, 2), nsmall=2))),
         se = sd/sqrt(n), 
         x_group = if_else(str_detect(x, "det"), "det", "viol")) %>%
  select(x_group, demo_variable, n, x, y, se, sd, p, p1, p_stars)

# ready labels 
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
        
## plot 
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
      
ggsave(plot = last_plot(),
       paste0("results/Fig 2. Compare average demographics between water systems.pdf"),
       scale = 1.3,
       height = 5,
       width = 5)

#==============================================================================
# tribes and u.s. territories
#==============================================================================

# 105 water systems were excluded from the main analysis that were missing MDI, 
# and other census data, and were serving tribal area or U.S. territories. 
# to explore possible ej issues, we compared detection and exceedance frequencies
# (proportions of water systems with detects/exceedences) between 2 groups: 
# systems serving U.S. territories or tribes versus systems serving areas in the
# main analysis (U.S. states and D.C.). We used Fisher's Exact test due to 
# low numbers of detects among a relatively lower number of tribal/territorial 
# systems detecting/exceeding contams

## begin here 
str(dat_ucmr3)

# number of systems excluded, by "state" (specific U.S. territory/tribal area. 
# tribal areas are designated with numbers) or "state status" (tribe or territory)
## total = 108** 
## **3 systems (all in Puerto Rico) never collected samples for target contams
dat_tt <- dat_ucmr3 %>% filter(state_status %in% c("tribe", "territory"))
dat_tt %>% count(det_any)
dat_tt2 <- dat_tt %>% filter(!is.na(det_any))
dat_tt2 %>% count(state) # most (69 systems) were in Puerto Rico
dat_tt2 %>% count(state_status) # 76 territories, 29 tribal PWS

## detection freqs of target contaminants among systems in the main analysis
t1 <- dat_clean %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
                           names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "State or D.C.")

## detection freqs of target contaminants among territories/tribal systems
# tribal OR territory combined
t2 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "Tribal or Territorial PWS")

# tribal OR territory separate
t3 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  ## adding state_status to stratify by territory or tribe
  group_by(state_status, outcome) %>% 
  summarise(n = sum(!is.na(value)), # NAs mean no sampling done.
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1))
t3

# make a dataset that has the main US-based water systems sample, and the 
# tribal/territorial water systems

dat_tt_withUS <- dat_clean %>% 
  bind_rows(dat_tt) %>%
  mutate(state_status_2 = ifelse(state_status %in% c("tribe", "territory"), 
                               "TT", "US"))
dat_tt_withUS

nrow(dat_tt_withUS) #4913 (U.S. PWSs + excluded systems)
dat_clean %>% bind_rows(dat_tt) %>% group_by(state_status) %>% count()
# 4808 states, 76 territories, 29 tribes

dat_clean %>% bind_rows(dat_tt) %>% group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

dat_clean %>% bind_rows(dat_tt) %>% 
  filter(det_any == 1) %>% 
  group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

dat_clean %>% bind_rows(dat_tt) %>% 
  filter(det_diox == 1) %>% 
  group_by(state_status) %>% count() %>%
  ungroup() %>% mutate(N = sum(n), freq = 100*n/N)

# conduct Fisher's Exact test for each outcome
# comparison is frequencies of detections TT (tribal or territory PWS) vs. US (other PWS) 
temp <- dat_tt_withUS %>%
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  nest() %>%
  mutate(contTabl = map(data, ~table(.x$value, .x$state_status_2))) %>%
  mutate(test = map(contTabl, ~fisher.test(.)), 
         test = map(test, ~tidy(.))) %>% 
  unnest(test)

T4 <- bind_rows(t1, t3) %>% 
  left_join(temp %>% distinct(outcome, p.value)
            ) %>%
  mutate(p.value = ifelse(p.value < 0.001, "< 0.001", as.character(signif(p.value, 2)))) %>%
  relocate(state_status, .after = outcome) %>%
  arrange(outcome, state_status) %>%
  mutate(yes = paste0(yes, " (", det_freq, ")"), 
         state_status = paste0(state_status, " (n = ", n, ")")) %>%
  select(-det_freq, -n) 
T4

# Over-representation analysis 

# function 
make_clean_value <- function(n, freq){paste0(n, " (", freq, ")")}

# occurrence in UCMR3 dataset
overall_UCMR <- dat_tt_withUS %>% 
  count(state_status) %>%
  mutate(total = sum(n), freq = signif(100*n/total, 2)) %>%
  mutate(overall_prevalence = make_clean_value(n, freq))
overall_UCMR

# occurrence among PWSs with detections
prev_among_detected <- dat_tt_withUS %>%
  select(PWSID, det_dca, det_diox, det_pfas, det_hcfc, state_status, state_status_2) %>%
  pivot_longer(cols = starts_with("det_"), names_to = "chemical", values_to = "value") %>%
  filter(value == 1) %>%
  count(chemical, state_status) %>%
  complete(chemical, state_status, fill=list(n = 0)) %>%
  group_by(chemical) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(freq = signif(100*n/total, 2)) %>%
  mutate(clean_value = make_clean_value(n, freq)) %>%
  mutate(chemical = paste0(chemical, " (n=", total, ")"))
prev_among_detected

# Simplify and pivot wider for table:
prev_among_detected_2 <- prev_among_detected %>%
  pivot_wider(id_cols = state_status, names_from = chemical, values_from = clean_value)
prev_among_detected_2

# combine tables and format
comb_overrepresentn <- overall_UCMR %>%
  select(overall_prevalence) %>%
  bind_cols(prev_among_detected_2) %>%
  relocate(overall_prevalence, .after = state_status)
comb_overrepresentn

# write.csv(comb_overrepresentn, 
#           paste0("outputs/", Sys.Date(), " - overrepresentation TT vs US systems.csv"))

comb_overrepresentn %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla()

#### TABLE #####
# write.csv(T4, paste0("outputs/", Sys.Date(), " - comparing TT vs US systems.csv"))

# flextable? 
# T4 %>%
#   flextable() %>%
#   autofit() %>% 
#   set_header_labels(
#     outcome = "Outcome", 
#     
#   )

## END OF SCRIPT.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ARCHIVE -----------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# theme(panel.spacing.y = unit(0.5, "lines"),
#       strip.background = element_blank(),
#       strip.placement = "outside")
# # Start plotting here:
# ggplot(aes(x = interaction(value_out, outcome), 
#            y = mean, 
#            fill = interaction(outcome, value_out))) +
#   #geom_bar(stat = 'identity', position = 'dodge') + 
#   geom_col(position = position_dodge(0.5)) + 
#   scale_x_discrete(guide = 'axis_nested') + 
#   
#   # Add stars
#   geom_text(aes(label = p_stars), size = 5, position = position_nudge(y = 5)) +
#   
#   # Facet 
#   facet_wrap(~demovar, strip.position = 'left', scales = 'free') 
# 
# # Adjust outcome category names and stars
# filter(value_out %in% c('0', '1')) %>% 
#   mutate(stars = ifelse(stars == " ", "n.s.", stars)) %>%
#   mutate(stars = ifelse(value_out == 0, "", stars)) %>%
#   mutate(value_out = factor(value_out, levels = c("0", "1"), labels = c("No", "Yes"))) %>%
#   
#   # Adjust outcome names
#   mutate(outcome = factor(outcome, 
#                           levels = c("det_any", "viol_any"), 
#                           labels = str_wrap(c("Target contaminant detected",
#                                               "Health-reference level exceeded"), 25))) %>%
#   
#   # Start plotting here:
#   ggplot(aes(x = interaction(value_out, outcome), 
#              y = mean, 
#              fill = interaction(outcome, value_out))) +
#   #geom_bar(stat = 'identity', position = 'dodge') + 
#   geom_col(position = position_dodge(0.5)) + 
#   scale_x_discrete(guide = 'axis_nested') + 
#   
#   # Add stars
#   geom_text(aes(label = stars), size = 5, position = position_nudge(y = 5)) +
#   
#   # Facet 
#   facet_wrap(~name, strip.position = 'left', scales = 'free') + 
#   
#   # Themes and other adjustments
#   scale_fill_manual(name = "", values = c("#9bccc6",'#c2a5cf',  "#50a6a6", '#7b3294')) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
#   labs(x = "", y = "") + 
#   theme_bw() + 
#   theme(text = element_text(size = 12),
#         aspect.ratio = 3/5,
#         ggh4x.axis.nestline = element_blank(), 
#         panel.grid = element_blank(),
#         legend.position = 'none', 
#         strip.placement = 'outside', 
#         strip.background = element_rect(fill = 'white', color = 'white'), 
#         strip.text.y.left = element_text(size = 10))
# 
# 
# 
# cn15 %>%
#   filter(GEO.id2 %in% county_ids_vec) %>%
#   pivot_longer(cols = starts_with("bin_fac")) %>%
#   mutate(value = as.factor(value)) %>%
#   pivot_longer(cols = c(perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban), 
#                names_to = "demovar", values_to = "demovalue") %>%
#   select(name, value, demovar, demovalue) %>%
#   left_join(source_demo_comparison) %>%
#   mutate(value = ifelse(value == 0, "Absent", "Present")) %>% 
#   mutate(demovar = factor(demovar, 
#                           levels = c("perc_black_nohisp", 
#                                      "perc_hisp_any", 
#                                      "mdi_rate", 
#                                      "perc_urban"), 
#                           labels = c("% Black", "% Hispanic", 
#                                      "% deprived", "% urban households"))) %>%
#   mutate(name = case_when(name == "bin_fac_any" ~ "Any TRI facility", 
#                           name == "bin_fac_diox" ~ "Dioxin facility", 
#                           name == "bin_fac_chlor_solv" ~ "Chlorinated solvent facility", 
#                           name == "bin_fac_cfc" ~ "CFC facility")) %>%
#   ggplot(aes(x = value, y = demovalue)) +
#   geom_jitter(alpha = 0.5, color = 'lightgrey') + 
#   geom_boxplot(aes(color = value)) + 
#   geom_text(aes(x = 1.6, y = 80, label = p_stars), 
#             size = 5, hjust = 1, vjust = 1) + 
#   # geom_violin(aes(color = value)) + 
#   facet_grid(demovar ~ name, switch = "y") +
#   scale_color_manual(name = "", values = c("darkgreen", "darkblue")) + 
#   labs(x = "", y = "") +
#   theme_bw() + 
#   theme(legend.position = "none", 
#         axis.text = element_text(size = 15), 
#         strip.text = element_text(size = 13))

# select(-stars) %>%
#   filter(value_out != 2)  %>% 
#   rename(p.value = value) %>%
#   pivot_wider(id_cols = name, 
#               names_from = c(outcome, value_out), 
#               values_from = c(mean, p.value),
#               names_sort = TRUE,
#               names_glue = "{outcome}_{value_out}_{.value}")%>%
#   view()
# 
# colnames(crude_results)
# crude_results$name %>% unique()
# temp$model[[1]] %>% view()
# 
# , 
#          perc_estimate = map_chr(model, ~.x$fmt_perc_change[.x$term == "predictor"]), 
#          p_stars = map_chr(model, ~.x$p_stars[.x$term == "predictor"]), 
#   ) %>%
#   select(-data, -model)
# 
# 
# dat_clean %>%
#   group_by(det_any) %>%
#   summarise(n = n(), 
#             det_any_freq = 100*sum(det_any == 1)/n,
#             exceed_any_freq = 100*sum(viol_any == 1)/n,
#             pop_served = sum(pop_served, na.rm = T),  # 5 PWSs w/o info on pop served
#             mean_samp = paste0(round(mean(n_samples), 1), " (", round(sd(n_samples), 1), ")"), 
#             median_hisp = calc_and_format_quantiles(perc_hisp_any), 
#             median_no_hisp_black = calc_and_format_quantiles(perc_black_nohisp), 
#             median_deprived = calc_and_format_quantiles(mdi_rate), 
#             median_urban_household = calc_and_format_quantiles(perc_urban)
#             )
# 
# dat_clean %>%
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_"))) %>%
#   group_by(name) %>%
#   summarise(n_rows = n(), 
#             n_PWSID = sum(!is.na(value)), 
#             n_det = sum(value == 1, na.rm = T), 
#             detfreq = 100*n_det/n_PWSID, 
#             samps = mean(n_samples))
# 
# # 00. LIBRARIES -----------------------------------------------------------
# library(tidyverse)
# library(ggplot2)
# library(forcats)
# library(lubridate)
# library(broom)
# library(gtools)
# 
# ana <- dat_ucmr3 %>% filter(!is.na(mdi_rate))
# excluded_ana <- setdiff(dat_ucmr3, ana)
# excluded_ana %>% count(state)
# summary(ana)
# 
# add_stars <- 
# 
# 
# ana %>%
#   group_by(det_any, size) %>% 
#   summarise(n = n(), 
#             across(starts_with('det_'), ~sum(. == 1)),
#             across(starts_with('viol_'), ~sum(. == 1)), 
#             mdi_rate = median(mdi_rate),
#             across(starts_with('perc_'), ~median(., na.rm = T)))
# 
# ggplot(ana, aes(x = state, fill = pws_type)) + 
#   geom_histogram(stat = 'count', position = 'dodge')
# 
# # Plot medians by demographs
# ana %>%
#   pivot_longer(cols = c(det_any, viol_any), 
#                names_to = 'outcome', 
#                values_to = 'out') %>%
#   group_by(outcome, out) %>%
#   summarise(n = n(), 
#             mdi_rate = median(mdi_rate),
#             across(starts_with('perc_'), ~median(., na.rm = T))) %>%
#   pivot_longer(cols = c(mdi_rate, starts_with('perc_')),
#                names_to = 'demo',
#                values_to = 'level') %>%
#   ggplot(aes(x = outcome, y = level, fill = out)) +
#   geom_bar(stat = 'identity', position = 'dodge') + 
#   facet_wrap(~demo)
# 
# ana %>% 
#   pivot_longer(cols = c(starts_with("det_"), starts_with("viol_")), 
#                names_to = "outcome", 
#                values_to = "y") %>%
#   group_by(outcome) %>%
#   nest() %>%
#   # Build out the regression model here:
#   mutate(m1 = map(data, ~glm(y ~ mdi_rate + n_samples + perc_black_only, 
#                              data = ., family = 'binomial')), 
#          m1_tidy = map(m1, tidy, exponentiate = T)) %>%
#   unnest(m1_tidy) %>%
#   select(-data, -m1) %>% 
#   mutate(stars = stars.pval(p.value))

