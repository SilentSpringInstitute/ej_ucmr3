# DATE STARTED: 2021-07-06
# AUTHOR: Amanda Hernandez, Jahred Liddie
# PURPOSE: Conduct Spearman's p correlations
# LATEST REVISION: 2024-11-13
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Start here:
# source("1_combine_process.R")

library(corrplot)
library(RColorBrewer)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview -------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This script calculated correlation coefficients between pairs of variables 
# used in multiple regression models. The results of this script 
# were provided in the supplement as a figure (correlelogram). We used 
# Spearman's rank correlation to determine correlation coefficients and reported
# coefficients in the figure.
# 
# Warning: This script autosaves a plot with appropriate margins to a folder 
# called "outputs" in the working directory. The plot is not visible in the 
# console otherwise. 
#
# The variables were:
#  * % Hispanic, 
#  * % NH Black,
#  * % deprived 
#  * % homeownership
#  * % uninsured
#  * % poverty 
#  * % urban 
#  * System size (large or small) 
#  * Groundwater source (GW) 
#  * Surface water source (SW)
#  * Combination of GW and SW sources (MIX)
#  * Number of samples 
#  * Normalized wastewater effluent flow (in million L/km2)
#  * Presence of >=1 TRI facility reporting emissions of one or more of 1,4-dioxane, 
#      HCFC-22, CFC-12, 1,1-DCA, or 1,1,1-TCE between 2010-2015
#  * Presence of >=1 TRI facility reporting emissions of 1,4-d only
#  * Presence of >=1 TRI facility reporting emissions of HCFC-22 or CFC-12 only
#  * Presence of >=1 TRI facility reporting emissions of 1,1-DCA or 1,1,1-TCE only
#  * Presence of >=1 military fire training area (MFTA)
#  * Presence of >=1 AFFF-certified airport 
#  * Presence of >=1 EPA Stewardship facility 

primaryCols <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate",  "perc_urban",
                 
                 "perc_hmown", "perc_uninsur", "perc_pov_ppl", 
                 
                 "pws_type", "size", "n_samples", 
                 
                 "adj_wwtp_flow", 
                 
                 "n_fac_any_bin", "n_fac_diox_bin", "n_fac_cfc_bin", "n_fac_chlor_solv_bin", 
                 "n_MFTA_bin", "n_airports_bin", "src_epa_present_bin")

# In data frame object "dat_clean", there are three levels of system type:
# GW, SW, or MIX. Pivot these wider and create binary variables (1s/0s). 

stopifnot(nrow(dat_clean)==4815)

dat_short <- dat_clean %>% 
  select(PWSID, all_of(primaryCols)) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = pws_type, values_from = value, values_fill = 0) %>%
  relocate(c(MX, GW, SW), .after = size)

# Make data for correlation test by removing the ID column ("PWSID") and 
# ensuring all variables are of class double.

dat4corr <- dat_short %>% select(-PWSID) %>% mutate_all(as.double)

# Conduct Spearman's test.

corRes <- cor(dat4corr, method = 'spearman')
corRes

# Fix row names to make labels for figure.
# rownames(corRes)

new_rownames <- 
  c("Percent Hispanic", 
    "Percent Black, non-Hispanic", 
    "Percent deprived", 
    "Percent urban households",
    "Percent home ownership", 
    "Percent uninsured", 
    "Percent poverty", 
    "System size", 
    "Source water: MX", 
    "Source water: GW", 
    "Source water: SW", 
    "Number of samples", 
    "WWTP total flow per area", 
    "Any TRI facility", 
    "Any 1,4-dioxane TRI facility", 
    "Any CFC TRI facility", 
    "Any chlorinated solvent TRI facility", 
    "MFTA present", 
    "Airport present", 
    "Major PFAS industrial facility present")

rownames(corRes) <- new_rownames
colnames(corRes) <- new_rownames

# Get p-values (not reported). This can be added to the figure if needed.

corPval <- corrplot::cor.mtest(corRes, conf.level = 0.95)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure S1: Correlelogram ----------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(corrplot)
library(RColorBrewer)

# start plot here:
plot.new()

# save a file in the outputs folder
png(file = paste0("results/", Sys.Date(), " - correlelogram.png"), 
    width = 10, height = 7.5, units='in', res = 300)

# set margins
par(omi=c(0,0,0,0), mgp=c(0,0,0),mar=c(0,0,0,0))

# plot using corrplot (from corrplot package) and COL2 (from RColorBrewer)
corrplot(corRes,
         type = 'lower',
         col = COL2("PRGn"), # sets color scheme
         mar = c(0,0,0,0),
         oma = c(0,0,0,0),
         omi = c(0,0,0,0),
         mgp = c(0,0,0),
         addCoef.col = 'grey10',# color of coefficients 
         tl.col = 'black', # makes axis text black
         number.cex = 0.7, # sets size of estimates
         method = 'square', # sets shape
         tl.cex = 0.8,
         #p.mat = corPval$p, # adds p-values
         #tl.srt = 45,
         #insig = 'blank',
         #col = prgn_palette[c(-1, -11)], 
         win.asp = 2/3,
         )

# end plot here:
dev.off()

# Archive -------------------------------------------------------------

# pdf(file = paste0("outputs/", Sys.Date(), " - correlelogram.pdf"))
# jpeg(file = paste0("outputs/", Sys.Date(), " - correlelogram.jpg"), 
#     width = 500, height = 500, res = 400)

# par(mar = c(par("mar")[1], par("mar")[2], 0, 0))
# par(omi = c(0,0,0,0), mgp = c(0,0,0), mar = c(0,0,0,0), family = "D")
# par(mfrow=c(1,1),cex=1,cex.lab = 0.75,cex.main=0.2,cex.axis=0.2)

# 
# dat_clean %>% colnames()
# dat_clean %>% select(ends_with("bin") & contains("fac")) %>% summarise(across(everything(), ~sum(.==1)))
# # hetcor(dat_short %>% select(-PWSID) %>% mutate(n_samples = as.double(n_samples)))
# # ggpairs(dat_short %>% select(-PWSID) %>% mutate(n_samples = as.double(n_samples))
# # )
# # str(dat_short)
# 
# colnames(cn15)
# cn15 %>% select(bin_fac_cfc) %>% unique() 
# cn15 %>%
#   # identify counties in the main study sample 
#   filter(GEO.id2 %in% county_ids_vec) %>%
#   select(bin_fac_cfc) %>%
#   count(bin_fac_cfc)
# 
# cn15 %>% count(geography, bin_fac_cfc) %>% filter(bin_fac_cfc == 1)
# dat_clean %>% count(state, n_fac_cfc_bin) %>% arrange(-n) %>% filter(n_fac_cfc_bin == 1)
# d <- cn15 %>%
#   # identify counties in the main study sample 
#   filter(GEO.id2 %in% county_ids_vec) %>%
#   select(WWTP_totalflow_mgd, land.area, perc_hisp_any, perc_black_nohisp, perc_urban, mdi_rate) %>%
#   mutate(adj_wwtp_flow_mgd_per_sqmeters = WWTP_totalflow_mgd/land.area, 
#          adj_wwtp_flow = (3.78541*1e6)*adj_wwtp_flow_mgd_per_sqmeters) %>%
#   select(-WWTP_totalflow_mgd, -land.area, -adj_wwtp_flow_mgd_per_sqmeters) 
# 
# colnames(d)
# 
# ggplot(d %>%
#          pivot_longer(cols = c(starts_with("perc"), starts_with("mdi"))), 
#        aes(x = value, y = log(adj_wwtp_flow))) + 
#   geom_point() + 
#   facet_wrap(~name)
# 
# 
# colnames(dat_clean)
# ggplot(dat_clean %>%
#          select(starts_with("perc"), starts_with("mdi"), adj_wwtp_flow) %>%
#          pivot_longer(cols = c(starts_with("perc"), starts_with("mdi"))), 
#        aes(x = value, y = log(adj_wwtp_flow))) + 
#   geom_point() + 
#   geom_smooth(method = 'lm')  + 
#   facet_wrap(~name)
# 
# 
# 
# cor.test(d$adj_wwtp_flow, d$perc_hisp_any, method = 'spearman', exact = FALSE)
# 
# 
# rcorr(as.matrix(d), type = 'spearman')
# contcols
# rcorr(as.matrix(dat_clean %>%
#                   select(all_of(contcols), starts_with("mdi"), adj_wwtp_flow)), 
#       type = 'spearman')$r %>%
#   corrplot(method = 'color', type = 'lower')
