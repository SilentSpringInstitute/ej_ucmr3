
# library(polycor)
# library(GGally)
# library(tidyverse)
library(corrplot)
library(RColorBrewer)
# library(Hmisc)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Correlations -------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+ This script produces correlation statistics among covariates used in
#+ the multiple regression models. The covariates are: % Hispanic, % NH Black, MDI, 
#+ % homeownership, % uninsured, % poverty, % urban, system size (L or S, one category), 
#+ source water (GW, SW, or MIX, each separately), number of samples, WWTP flow, 
#+ presence of source terms (any TRI fac, diox TRI, CFCs TRI, CFC-12 TRI, HCFC-22 TRI, 
#+ chlorinated solvents TRI, 1,1,-trichloroethane TRI, 1,1-dichloroethane TRI, 
#+ MFTA present, airport present, EPA stewardship fac present).
#+ 
#+ This script makes a correlelogram with Spearman's p statistics.

colnames(dat_clean)
primaryCols <- c("perc_hisp_any", "perc_black_nohisp", "mdi_rate",  "perc_urban",
                 
                 "perc_hmown", "perc_uninsur", "perc_pov_ppl", 
                 
                 "pws_type", "size", "n_samples", 
                 
                 "adj_wwtp_flow", 
                 
                 "n_fac_any_bin", "n_fac_diox_bin", "n_fac_cfc_bin", "n_fac_chlor_solv_bin", 
                 "n_MFTA_bin", "n_airports_bin", "src_epa_present_bin")
primaryCols

dat_short <- dat_clean %>% 
  select(PWSID, all_of(primaryCols)) %>%
  # make binary columns of pws_type (3 levels) 
  mutate(value = 1) %>%
  pivot_wider(names_from = pws_type, values_from = value, values_fill = 0) %>%
  relocate(c(MX, GW, SW), .after = size)
#select(PWSID, GW, MX, SW)



# make data for correlation test, also change column names here
temp <- dat_short %>% select(-PWSID) %>% mutate_all(as.double)

# conduct Spearman's test 
corRes <- cor(temp, method = 'spearman')
corRes

# fix names for figure
rownames(corRes)
new_rownames <- 
  c("Percent Hispanic", 
    "Percent Black, non-Hispanic", 
    "Percent deprived", 
    "Percent urban households",
    "Percent home ownership", 
    "Percent uninsured", 
    "Percent poverty", 
    "System size", 
    "Source water: MX", "Source water: GW", "Source water: SW", 
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
corRes

# get p-values for corr statistics
corPval <- cor.mtest(corRes, conf.level = 0.95)
corPval

cols <- brewer.pal(3, "PRGn")

######## This is the plot #####

plot.new()
#pdf(file = paste0("outputs/", Sys.Date(), " - correlelogram.pdf"))
# jpeg(file = paste0("outputs/", Sys.Date(), " - correlelogram.jpg"), 
#     width = 500, height = 500, res = 400)
png(file = paste0("outputs/", Sys.Date(), " - correlelogram.png"), 
    width = 10, height = 7.5, units='in', res = 300)

# par(mar = c(par("mar")[1], par("mar")[2], 0, 0))
par(omi=c(0,0,0,0), mgp=c(0,0,0),mar=c(0,0,0,0))
# par(omi = c(0,0,0,0), mgp = c(0,0,0), mar = c(0,0,0,0), family = "D")
# par(mfrow=c(1,1),cex=1,cex.lab = 0.75,cex.main=0.2,cex.axis=0.2)

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
         #col = prgn_palette[c(-1, -11)], # sets color scheme
         win.asp = 2/3,
         )

dev.off()
dev.off()

## End of script. 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Archive -------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
