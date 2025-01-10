# Socioeconomic disparities in exposures to PFAS and other unregulated industrial drinking water contaminants in U.S. public water systems

## README

## Libraries used 
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

## Organization
Scripts in this repo were grouped into series and intended to run in order (series 1->4). 

## Data inputs
Inputs were downloaded from public databases from various US EPA and US Census Bureau websites from 2017-2024. Additionals available upon request to schaider@silentspring.org.

Raw data were cleaned and combined in "1_combine_process.R". 
Clean data were used in subsequent series (2-4). 
Clean data consisted of 4815 systems (rows) and information about the counties served and UCMR3 results (various columns). 

## Description of series

 - Series 1: processing scripts.

Scripts compiled downloaded datasets of results from the Third Unregulated Contaminant Monitoring Rule (2013-2015), county-level information from U.S. Census Bureau and American Community Surveys, and suspected sources of unregulated contaminants including wastewater and facilities reporting emissions to the Toxics Release Inventory (TRI). 

 - Series 2: univariate analyses.

Scripts calculated detection frequencies (Table 1) and defined baseline characteristics of water systems (Table 2). In addition, script 2_pws_compare_SDWIS.R compared proportions of large and small systems in the UCMR3 with SDWIS systems by system type (Table S1). Script 2_tribes_territories.R looked at US tribes and territories (Table S10-S11).

 - Series 3: bivariate analyses. 

Scripts compared average demographic levels between counties with and without point sources (Figures 2) and average demographic measures between drinking water systems with and without detections (Figure 3). Script 3_bivar_hisp_US_regions.R assessed differences in demographics and point sources by US regions, which were included in the Discussion section. Script 3_correlation.R determined correlations between all covariates used in regression models (Figure S1).

 - Series 4: multivariate (regression) analyses.

Scripts tested associations between the presence of unregulated contaminants and sociodemographic variables, PWS characteristics, and suspected sources with logistic regression models (see Table 3 [crude and adjusted] and Table 4 [stratified]). Supplemental sensitivity regression models and a marginal analysis were included.
