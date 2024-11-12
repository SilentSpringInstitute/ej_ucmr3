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

# filter for systems that were part of the UCMR 3
# rename GEO.id2 to county_id
# calculate percent homeownership as proportion of 
pwsid_county_link <- fips_cn15 %>% 
  filter(PWSID %in% dat_clean$PWSID) %>% 
  rename(county_id = GEO.id2) %>%
  mutate(perc_hmown = 100*owned.house/all.house14)

pwsid_county_link %>% 
  select(county_id, perc_hmown, perc_pov_ppl, perc_urban, perc_hisp_any)

# check for duplicate entries of PWS-county combinations
stopifnot(nrow(pwsid_county_link %>% count(PWSID, county_id) %>% filter(n > 1))==0)

## counties are duplicated in the data since it can match with many systems. 
## for example, "calhoun county, alabama" has 41 different water systems and 
## 4 of them were part of the UCMR 3.
## Use distinct() to capture unique combinations of county id's and the corresponding
## demographic values, then check for duplicates.

# pwsid_county_link %>% 
#   count(county_id) %>% 
#   filter(n > 1)
# 
# fips_cn15 %>% filter(GEO.id2 == "01015") %>% view()

county.data <- pwsid_county_link %>% 
  distinct(county_id, perc_hisp_any, perc_black_nohisp, mdi_rate, perc_urban, 
           perc_pov_ppl, perc_uninsur, perc_hmown,
         n_fac_any, n_fac_diox, n_fac_cfc, n_fac_chlor_solv, 
         n_MFTA, n_airports, src_epa_present) 
county.data

# check for duplicates - each row is one unique county
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

unique(county.data1$y)

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
# example: 
# func_mean_demo(dat = county.data2$data[[1]], xi = xi)

# apply the t-test function over the nested data (county.data2)
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

# prepare data for plotting. calculate standard errors, format p-values, and 
# define factor and labels that will display in the plot
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

# plot
county_ready2plot %>% view()
ggplot(county_ready2plot %>% filter(!is.na(y)), 
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

## calculated averages for 7 demographic factors * 4 total outcomes = 28
# show 4 main demographic factors of interest (perc Hisp, perc Bl, perc deprived, perc_urban)
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
  rename(y = means) %>%
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

## brief description
# Some water systems were excluded from the main analysis that were missing MDI, 
# and other census data, and were serving tribal areas or U.S. territories. 
# To explore possible ej issues, we compared detection and exceedance frequencies
# (proportions of water systems with detects/exceedences) between 2 groups: 
# systems serving U.S. territories or tribes versus systems serving areas in the
# main analysis (U.S. states and D.C.). We used Fisher's Exact test due to 
# low numbers of detects among tribal/territorial PWSs

## begin here 
# dat_ucmr3 precedes dat_clean in data cleaning scripts
# dat_ucmr3 contains data for PWSs **before** restriction
str(dat_ucmr3)

## number of systems excluded
## total = 108
## 3 systems (all in Puerto Rico) never collected samples for target contams
dat_tt <- dat_ucmr3 %>% filter(state_status %in% c("tribe", "territory"))
dat_tt2 <- dat_tt %>% filter(!is.na(det_any))
stopifnot(nrow(setdiff(dat_tt, dat_tt2)) == 3)

## number of systems excluded by "state" (specific U.S. territory/tribal area. 
# tribal areas are designated with numbers) or "state status" (tribe or territory)
dat_tt2 %>% count()  # 105 systems overall
dat_tt2 %>% count(state)  # most (69 systems) were in Puerto Rico
dat_tt2 %>% count(state_status)  # 76 territories, 29 tribal PWS

# make long data (easier format)
dat_tt3 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value")

## contamination-specific frequencies among tribal PWSs (n=29) and territorial 
## PWSs (n=76). Frequencies were calculated for systems separated by whether 
## they were in a tribal area or U.S. territory, and combined. 
det_freq_tt <- dat_tt3 %>% 
  bind_rows(dat_tt3 %>% mutate(state_status = "overall")) %>%  # for overall category
  group_by(state_status, outcome) %>% 
  summarise(n = n(), 
            detect = sum(value == 1),
            det_freq = 100*detect/n())

## contamination-specific frequencies among PWSs in the main analysis (n=4808).
det_freq_main <- dat_clean %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
                           names_to = "outcome", values_to = "value") %>% 
  mutate(state_status = "State or D.C.") %>%
  group_by(state_status, outcome) %>% 
  summarise(n = sum(!is.na(value)),  # NA = system did not sample for contaminant
            detect = sum(value == 1, na.rm = T),
            det_freq = 100*detect/n())

## combine datasets 
det_freq_maintt <- bind_rows(det_freq_main, det_freq_tt)
det_freq_maintt %>% pivot_wider(id_cols = c(state_status, n), 
                                names_from = outcome, values_from = det_freq)

combined <- dat_tt2 %>% bind_rows(dat_clean %>% 
                        mutate(state_status = "State or D.C."))

long <- combined %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value")

nested <- long %>%
  group_by(outcome) %>% 
  nest()

nested$data[[1]] %>% view()
## detection freqs of target contaminants among territories/tribal systems
# tribal OR territory combined
t2 <- dat_tt2 %>% 
  pivot_longer(cols = c(starts_with("det_"), "viol_any"), 
               names_to = "outcome", values_to = "value") %>% 
  group_by(outcome) %>% 
  summarise(n = sum(!is.na(value)),  # NA = system did not sample for contaminant
            yes = sum(value == 1, na.rm = T),
            det_freq = round(100*sum(value == 1, na.rm = T)/n, 1)) %>%
  mutate(state_status = "Tribal or Territorial PWS")


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


comb_overrepresentn %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla()

## export
# write.csv(T4, paste0("outputs/", Sys.Date(), " - comparing TT vs US systems.csv"))

# write.csv(comb_overrepresentn, 
#           paste0("outputs/", Sys.Date(), " - overrepresentation TT vs US systems.csv"))
