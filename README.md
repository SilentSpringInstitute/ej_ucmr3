# ej_ucmr3

Code associated with the UCMR3 paper entitled "Socioeconomic disparities in exposures to PFAS and other unregulated industrial drinking water contaminants in U.S. public water systems"

Libraries used 
  * tidyverse
  * janitor
  * readxl
  * ggplot2
  * ggh4x
  * corrplot
  * RColorBrewer
  * lme4
  * gtools
  * broom
  * broom.mixed
  * margins

Scripts in this repo were grouped into series and intended to run in order (series 1->4). 

Outputs were saved in a folder ("results/") created in a local directory. 

**Note: "1_combine_process.R" creates a data frame object called "dat_clean" that is used in subsequent series (2-4). "dat_clean" consists of 4815 rows (total number of water systems) and multiple columns corresponding to each system's results (eg, detection of any PFAS, "det_pfas", response: 1/0) and a summary profile of the county (or counties) they serve (eg, % Hispanic, response: continuous). 

"1_combine_process.R" must be sourced in series 2-4.

 - Series 1: processing scripts.

Scripts compiled downloaded datasets of results from the Third Unregulated Contaminant Monitoring Rule (2013-2015), county-level information from U.S. Census Bureau and American Community Surveys, and suspected sources of unregulated contaminants including wastewater and facilities reporting emissions to the Toxics Release Inventory (TRI). 

 - Series 2: univariate analyses.

Scripts include code that calculated detection frequencies (Table 1) and characterized baseline characteristics of water systems (Table 2). Script 2_pws_compare_SDWIS compared distributions of large and small systems in the UCMR3 with SDWIS systems by system type (Table S1). The analysis on US tribes and territories (Table S10-S11) is also is in this series.

 - Series 3: bivariate analyses. 

Scripts include code used to compare the average demographic measures between counties with and without point sources (Figures 2) and average demographic measures between systems with and without detections (Figure 3). Script 3_bivar_hisp_US_regions.R preliminarily assessed differences in demographics and point sources by US regions (mentioned in discussion). Script 3_correlation.R determined correlations between covariates used in regression models (Figure S1).

 - Series 4: multivariate (regression) analyses.

Scripts estimated associations between the presence of unregulated contaminants and sociodemographic variables, PWS characteristics, and suspected sources with logistic regressions (Table 3 [crude and adjusted], Table 4 [stratified]). Supplemental sensitivity regressions and a marginal analysis were also part of the series.
