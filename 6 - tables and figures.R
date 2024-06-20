### AUTHOR: AM
### STARTED: 2023-03-02
### WRITTEN IN: R version 4.2.2
### Purpose: Compile UCMR3 tables and figures

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)
getwd()

source("1 - UCMR loading and processing.R")
source("Janet functions May 2015_AH.r")
source("4a - UCMR summary.R")
source("4a - UCMR summary_det freqs.R")
source("4a - UCMR summary_pairwise_comp.R")
source("4b - UCMR analysis.R")

# Need to run these separately. The previous code throws an error and stops sourcing.
source("4c - excluded systems subanalysis.R")
source("6 - new supp figure.R")


library(tidyverse)
library(officer)
library(flextable)

## Functions

# takes regression tables as input 
clean_for_Table <- function(mod, show_pval = FALSE, show_all_results = FALSE){
  
  mod <- mod  %>%
    arrange(term) %>%
    select(term, 
           contains(">=1 UCMR"), contains("health"),
           contains("1,4-dioxane"), contains("1,1-dichloroethane"), 
           contains("HCFC-22"), contains("PFAS"), 
           -contains("nobs"), -sesvar) %>% 
    pivot_longer(cols = -term, 
                 names_to = c(".value", "group"), 
                 names_sep = "_") %>% 
    filter(!is.na(result))

  if(show_pval == TRUE){
    mod <- mod %>% 
      mutate(coef_formatted = paste(result, "\n", p.value)) %>%
      mutate(coef_formatted = ifelse(str_detect(coef_formatted, "Inf"),
                                     NA, coef_formatted)) %>%
      # 2023-03-26 change column names here
      mutate(group = case_when(str_detect(group, "UCMR Detection") ~ "Detected ≥1 target contaminant", 
                               str_detect(group, "health") ~ "Exceeded ≥1 HRL", 
                               str_detect(group, "1,4-dioxane") ~ "1,4-dioxane", 
                               str_detect(group, "1,1-dichloroethane") ~ "1,1-dichloroethane", 
                               str_detect(group, "HCFC-22") ~ "HCFC-22", 
                               str_detect(group, "PFAS") ~ "PFAS")) %>%
      pivot_wider(id_cols = term, 
                  names_from = group, 
                  values_from = coef_formatted) 
  }
  
  if(show_pval == FALSE){
    mod <- mod %>% 
      mutate(coef_formatted = case_when(p.value < 0.001 ~ paste0(result, "**"),
                                        p.value < 0.050 ~ paste0(result, "*"),
                                        p.value <= 0.100 ~ paste0(result, "+"),
                                        p.value >= 0.100 ~ paste0(result, ""))) %>% 
      # 2023-03-26 change column names here
      mutate(group = case_when(str_detect(group, "UCMR Detection") ~ "Detected ≥1 target contaminant", 
                               str_detect(group, "health") ~ "Exceeded ≥1 HRL", 
                               str_detect(group, "1,4-dioxane") ~ "1,4-dioxane", 
                               str_detect(group, "1,1-dichloroethane") ~ "1,1-dichloroethane", 
                               str_detect(group, "HCFC-22") ~ "HCFC-22", 
                               str_detect(group, "PFAS") ~ "PFAS")) %>%
      pivot_wider(id_cols = term, 
                  names_from = group, 
                  values_from = coef_formatted)
  }
  if(show_all_results == TRUE){return(mod)}
  if(show_all_results == FALSE){
    mod <- mod %>% 
      filter(!str_detect(term, "samples|TRI|Certified|facility"))
    return(mod)}
}

make_key <- function(dat, type = "unstratified"){
  if(type == "strat"){
    ab <- data.frame(term = colnames(cladjglmermods_strat2)) %>%
      mutate(size = ifelse(str_detect(term, "L"), "Large water system (N = 4,030)",
                           "Small water system (N = 778)"), 
             result = ifelse(str_detect(term, "result"), "Percent change (95% CI)", 
                             "p-value"))  %>%
      filter(term != "term")
    return(ab)
  }
  if(type == "unstratified"){
    aa <- data.frame(term = colnames(dat))  %>%
      mutate(outcome = case_when(str_detect(term, "UCMR Detection") ~ "Detected ≥1 target contaminant", 
                                 str_detect(term, "health") ~ "Exceeded ≥1 HRL", 
                                 str_detect(term, "1,4-dioxane") ~ "1,4-dioxane", 
                                 str_detect(term, "1,1-dichloroethane") ~ "1,1-dichloroethane", 
                                 str_detect(term, "HCFC-22") ~ "HCFC-22", 
                                 str_detect(term, "PFAS") ~ "PFAS"))  %>%
      #mutate(outcome2 = "Percent change (95% CI)")  %>%
      filter(term != "term")
    return(aa)
  }
}

format_reg_table <- function(table, mod_type = "adj"){
  
  signif.code.notes <- c("Signif code: *** p < 0.001; ** p < 0.01; * p < 0.05; + p < 0.10")
  
  Note <- c("Note: HRL = health reference level; HCFC-22 = hydrofluorocarbon-22; PFAS = per- or polyfluoroalkyl 
  substance; GW = groundwater; SW = surface water; MIX = groundwater
  under the influence of surface water, or system using a combination 
  of groundwater and surface water; WWTP = wastewater treatment plant; -- = Not applicable.")
  Note <- str_replace_all(Note, "\n", " ")  %>% str_squish()
  Note1 <- as_paragraph(Note)
  
  Adj.Note <- c(" Adjusted logistic mixed-effects regression models include adjustments for 
the number of samples collected, the presence of relevant point source locations 
within serviced counties, and a random state intercept. For detected ≥1 target
contaminant and exceeded ≥1 HRL, models were adjusted for TRI facility
(present or absent); for 1,4-dioxane, 1,1-dichloroethane, and HCFC-22, models
were adjusted for TRI facility emitting the specific contaminant; and for PFAS, 
the model was adjusted for certain airports and presence of a PFAS industrial 
source. Percent change = (e^β – 1) x 100.")
  Adj.Note <- str_replace_all(Adj.Note, "\n", " ") %>% str_squish()
  Adj.Note1 <- as_paragraph(Adj.Note)
  
  n_columns <- ncol(table)
  
  # if(mod_type == "adj"){mapping_key <- Table4_key}
  # if(mod_type == "strat"){mapping_key <- Table5_key}
  # if(mod_type == "crude"){mapping_key <- Table3_key2}
  
  table <- table %>%
    
    # standard template
    flextable() %>%
    colformat_char(na_str = "--") %>%
    add_header_row(values = c("", "Percent Change (95% CI)"), 
                   colwidths = c(1, n_columns-1)) %>%
    align(align = "center", part = "all") %>%
    align(align = "left", part = "all", j = 1) %>%
    set_table_properties(layout = "autofit")
  
  if(mod_type %in% c("adj", "strat")){
   table <- table %>% 
      # add_footer_lines(values = par2) %>%
      add_footer_lines(as_paragraph(signif.code.notes)) %>%
      add_footer_lines(Note1) %>%
      footnote(i=1, j=2, value = Adj.Note1,
               ref_symbols = c("a"), part = "header", inline = FALSE) %>%
      footnote(i=5, j=5, value = as_paragraph(c(" Only large PWSs detected 1,1-dichloroethane.")), 
               ref_symbols = c("b"), part = "body", inline = FALSE) %>% 
     font(fontname = "Times", part = 'all') %>%
     fontsize(size = 10, part = 'all') %>%
     bold(bold = TRUE, part = 'header') %>%
     padding(padding.bottom = 0, padding.top = 0, part = "footer")
   
   return(table)
  }
  if(mod_type == "crude"){
    table <- table %>% 
      # add_footer_lines(values = par2) %>%
      add_footer_lines(as_paragraph(signif.code.notes)) %>%
      add_footer_lines(Note1) %>%
      # footnote(i=1, j=2, value = Adj.Note1,
      #          ref_symbols = c("a"), part = "header", inline = FALSE) %>%
      footnote(i=5, j=5, value = as_paragraph(c(" Only large PWSs detected 1,1-dichloroethane.")), 
               ref_symbols = c("b"), part = "body", inline = FALSE) %>% 
      font(fontname = "Times", part = 'all') %>%
      fontsize(size = 10, part = 'all') %>%
      bold(bold = TRUE, part = 'header') %>%
      padding(padding.bottom = 0, padding.top = 0, part = "footer")
    
    return(table)
  }
  
  # archived 2023-03-26
  # table %>%
  #   colformat_char(na_str = "--") %>%
  #   set_header_df(mapping = mapping_key, key = "term") %>%
  #   theme_vanilla() %>% 
  #   #bold(i = 2, part = 'header', bold = FALSE) %>%
  #   vline(part = 'all', j = c(seq(1, 7, by = 1))) %>%
  #   #set_table_properties(layout = 'autofit') %>%
  #   width(j = 1, width = 1.5) %>%
  #   width(j= 2:7, width = 1.3) %>% 
  #   footnote(i=5, j=5, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
  #            ref_symbols = c("a"), part = "body", inline = FALSE) %>%
  #   font(fontname = "Times", part = 'all') %>%
  #   fontsize(size = 10, part = 'all')
}



################################################################################
#  MAIN TABLES/FIGURES REFERENCED IN PAPER  ####
################################################################################

# this comes from script 4a - UCMR summary.R
all_brkdwn2 <- all_brkdwn %>%
  left_join(size_detfreq %>% 
              bind_rows(source_detfreq) %>%
              bind_rows(overall_detfreq) %>%
              select(n.sys, n.ppl, det_freq, hlvl_freq),
            by = c('n.sys', 'n.ppl'))

Table1_key <- data.frame(colname = colnames(all_brkdwn2))

Table1_formatted <- all_brkdwn2 %>% 
  filter(!str_detect(brktype, "SDWA")) %>% 
  select(-contains("W System"), -contains("small PWSs")) %>%
  mutate(brktype2 = case_when(str_detect(brktype, "Detected") ~ "Detected a target contaminant", 
                              str_detect(brktype, "Over federal") ~ "Exceeded a health-reference level", 
                              str_detect(brktype, "System Size") ~ "System Size", 
                              str_detect(brktype, "Source Water") ~ "Source Water", 
                              TRUE ~ "Overall")) %>%
  mutate(n.ppl = round(n.ppl/1e6, 1)) %>%
  mutate_at(c("det_freq", "hlvl_freq"), round, 1) %>% 
  mutate(brktype = case_when(str_detect(brktype, "0") ~ "No", 
                             str_detect(brktype, "1-") ~ "Yes", 
                             str_detect(brktype, "(L)") ~ "Large", 
                             brktype == "System Size (S)" ~ "Small", 
                             str_detect(brktype, "(GW)") ~ "GW", 
                             str_detect(brktype, "(MIX)") ~ "MIX", 
                             str_detect(brktype, "SW") ~ "SW", 
                             str_detect(brktype, "Overall") ~ "")) %>%
  select(brktype2, brktype, n.sys, n.ppl, 
         det_freq, hlvl_freq, everything(), 
         -contains("Poverty"), -contains("Uninsured"), 
         -contains("Homeownersip")) %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit() %>%
  merge_v(j = 1) %>%
  colformat_num(na_str = "--") %>%
  set_header_labels(brktype2 = "", 
                    brktype = "", 
                    n.sys = "Number of PWSs", 
                    `Percent small PWSs` = "Small system (%)", 
                    `Percent GW Systems` = "GW system (%)", 
                    `Percent SW Systems` = "SW system (%)", 
                    `Median MDI Rate (Q1, Q3)` = "Median Depravity (Q1, Q3)",
                    n.ppl = "Population served (in millions)", 
                    det_freq = "% of systems with ≥1 detection", 
                    hlvl_freq = "% of systems with ≥1 HRL exceedance", 
                    `Median Percent Hispanic (Q1, Q3)` = "Median % Hispanic (Q1, Q3)", 
                    `Median MDI Rate (Q1, Q3)` = "Median % deprived (Q1, Q3)", 
                    `Median Percent Black (Q1, Q3)` = "Median % Black (Q1, Q3)", 
                    `Median Percent Urban (Q1, Q3)` = "Median % Urban (Q1, Q3)"
                    ) %>%
  # width(j = 1, width = 1)  %>% # NOT REALLY DOING ANYTHING with set_layout_*
  # width(j = 7:11, width = 1.6)  %>%
  align(align = 'center', j = 2:11, part = 'all') %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  set_table_properties(layout = 'autofit')

# this comes from script 1
Table2_key <- data.frame(contam.pfas = c("1,4-dioxane", "1,1-dichloroethane", 
                                         "HCFC-22", "PFAS", "PFOA", "PFOS", "PFHpA", 
                                         "PFHxS", "PFNA", "PFBS")) %>%
  mutate(reporting.limit = case_when(contam.pfas == "1,4-dioxane" ~ 70, 
                                     contam.pfas == "1,1-dichloroethane" ~ 30, 
                                     contam.pfas == "HCFC-22" ~ 80, 
                                     contam.pfas == "PFAS" ~ as.numeric(NA), 
                                     contam.pfas == "PFOA" ~ 20, 
                                     contam.pfas == "PFOS" ~ 40, 
                                     contam.pfas == "PFHpA" ~ 10, 
                                     contam.pfas == "PFHxS" ~ 30, 
                                     contam.pfas == "PFNA" ~ 20, 
                                     contam.pfas == "PFBS" ~ 90), 
         health.reference.level = case_when(contam.pfas == "1,4-dioxane" ~ 350, 
                                            contam.pfas == "1,1-dichloroethane" ~ 6140, 
                                            str_detect(contam.pfas, "PFOA|PFOS") ~ 70, 
                                            TRUE ~ as.numeric(NA)),
         common.sources = case_when(contam.pfas == "1,4-dioxane" ~ "Solvent production, consumer products", 
                                    contam.pfas == "1,1-dichloroethane" ~ "Solvent production", 
                                    contam.pfas == "HCFC-22" ~ "Refrigerant, low-temperature solvent, and in fluorocarbon resins", 
                                    str_detect(contam.pfas, "PF") ~ "Firefighting foams, consumer products, fluoropolymer coatings"))

Table2_formatted <- ucmr_sum %>%
  filter(contam.pfas!="evrdet") %>%
  left_join(Table2_key, by = "contam.pfas") %>%
  mutate(hlvl_freq = case_when(str_detect(contam.pfas, "PFOA|PFOS") ~ hlvl_freq[contam.pfas=="PFAS"],
                               str_detect(contam.pfas, "PFAS") ~ as.numeric(NA),
                               TRUE ~ hlvl_freq)) %>%
  na_if(0) %>%
  select(contam.pfas, reporting.limit, 
         detfreq_samps, detfreq_sys,
         health.reference.level, 
         hlvl_freq, 
         common.sources) %>%
  flextable() %>%
  colformat_num(na_str = "--") %>%
  set_header_labels(contam.pfas = "Contaminant", 
                    detfreq_samps = "Sample detection frequency (%)", 
                    detfreq_sys = "% of systems with ≥1 detection", 
                    hlvl_freq = "% of systems with ≥1 HRL exceedance") %>%
  merge_v(j = 7) %>%
  merge_at(i = 5:6, j = 5) %>%
  merge_at(i = 5:6, j = 6) %>%
  border_remove() %>%
  theme_vanilla() %>%
  padding(i = 5:10, j = 1, padding.left = 20) %>% 
  set_header_labels(reporting.limit = "Reporting limit (ng/L)", 
                    health.reference.level = "US EPA health-reference level (ng/L)", 
                    common.sources = "Common sources") %>%
  footnote(i=1, j=3, value = as_paragraph(c(" Percent of samples tested that contained each contaminant")), 
           ref_symbols = c("a"), part = "header", inline = FALSE) %>% 
  footnote(i=1, j=4, value = as_paragraph(c(" Percent of PWSs tested that contained each contaminant")), 
           ref_symbols = c("b"), part = "header", inline = FALSE) %>% 
  # footnote(i=1, j=5, value = as_paragraph(c(" US EPA health-reference level (HRL)")), 
  #          ref_symbols = c("c"), part = "header", inline = FALSE) %>%
  footnote(i=1, j=6, value = as_paragraph(c(" Percent of PWSs with at least one HRL exceedance")), 
           ref_symbols = c("d"), part = "header", inline = FALSE) %>%
  autofit() %>%
  width(width = 2) %>%
  width(j = 1, width = 1)  %>%
  align(j = 2:7, align = 'center', part = 'all') %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 11, part = 'all') %>%
  set_table_properties(layout = 'autofit')

# from 4b - UCMR analysis.R script
Figure1 <- p_final

# Table4 now Table3 since previous Table 3 was turned into a figure
# this comes from 4b - UCMR analysis.R script
# Table3_key <- data.frame(term = colnames(crude_table)) %>%
#   mutate(outcome = paste0(case_when(str_detect(term, "UCMR Detection") ~ "Detected ≥1 target contaminant", 
#                              str_detect(term, "over health guideline") ~ "Exceeded ≥1 HRL", 
#                              str_detect(term, "1,4-dioxane") ~ "1,4-dioxane", 
#                              str_detect(term, "1,1-dichloroethane") ~ "1,1-dichloroethane", 
#                              str_detect(term, "HCFC-22") ~ "HCFC-22", 
#                              str_detect(term, "PFAS") ~ "PFAS"), 
#                           "\n Percent change (95% CI)"))

Table3_format <- crude_table %>% 
  filter(!str_detect(term, "ownership|poverty|uninsur")) %>%
  mutate(sesvar = "9999") %>%
  clean_for_Table(show_pval = FALSE, show_all_results = TRUE) %>% 
  format_reg_table(mod_type = "crude")

# Table5 now Table4 since previous Table 3 was turned into a figure
# this comes from 4b - UCMR analysis.R script

cladjglmermods2 <- cladjglmermods %>% 
  ungroup() %>% 
  filter(sesvar == "mdi") %>% 
  filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
  clean_for_Table(show_pval = FALSE, show_all_results = FALSE)

Table4_key <- make_key(cladjglmermods2)

Table4_format <- cladjglmermods2 %>%
  arrange(term) %>%
  format_reg_table()


## Stratified results 

Note <- c("Note: HRL = health reference level; HCFC-22 = hydrofluorocarbon-22; PFAS = per- or polyfluoroalkyl 
  substance; GW = groundwater; SW = surface water; MIX = groundwater
  under the influence of surface water, or system using a combination 
  of groundwater and surface water; WWTP = wastewater treatment plant; -- = Not applicable.")
Note <- str_replace_all(Note, "\n", " ")  %>% str_squish()
Note1 <- as_paragraph(Note)

Strat.Note <- c(" Percent change = (e^β – 1) x 100.")
Strat.Note <- str_replace_all(Strat.Note, "\n", " ") %>% str_squish()
Strat.Note1 <- as_paragraph(Strat.Note)

cladjglmermods_strat2 <- cladjglmermods_strat %>%
  ungroup() %>%
  select(-contam.pfas, -sesvar)

Table5_key <- data.frame(term = colnames(cladjglmermods_strat2)) %>%
  mutate(size = ifelse(str_detect(term, "L"), "Large water system (N = 4,030)",
                       "Small water system (N = 778)"), 
         result = ifelse(str_detect(term, "result"), "Percent change (95% CI)", 
                         "p-value"))  %>%
  filter(term != "term")

Table5_format <- cladjglmermods_strat2 %>% 
  arrange(term) %>%
  select(term, ends_with("L"), ends_with("S")) %>%
  flextable() %>%
  set_header_df(mapping = Table5_key, key = "term") %>%
  merge_h(part = 'header') %>%
  theme_vanilla() %>% 
  vline(part = 'all', j = c(1, 3)) %>%
  width(j = 1, width = 1.5) %>%
  width(j = c(2, 4), width = 2) %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  autofit() %>% 
  add_footer_lines(Note1) %>%
  footnote(i=2, j=3, value = Strat.Note1,
           ref_symbols = c("a"), part = "header", inline = FALSE) %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  bold(bold = TRUE, part = 'header') %>%
  padding(padding.bottom = 0, padding.top = 0, part = "footer")

################################################################################
# SUPPLEMENTAL MATERIALS #########
################################################################################


TableS1_key <- data.frame(colnames = colnames(table3_supplemental_pov)) %>%
  mutate(group = case_when(colnames == "variable" ~ "", 
                           str_detect(colnames, "All PWSs") ~ "All PWSs", 
                           str_detect(colnames, "contaminants detected") ~ "Detected a target contaminant", 
                           str_detect(colnames, "exceeding federal") ~ "Exceeded a health reference level" ), 
         subgroup = case_when(str_detect(colnames, "no") ~ "No", 
                              str_detect(colnames, "with >=1 target") ~ "Yes", 
                              str_detect(colnames, "Comparison") ~ "P-value"))

TableS1_format <- table3_supplemental_pov %>% 
  flextable() %>%
  set_header_df(mapping = TableS1_key, key = "colnames") %>%
  theme_vanilla() %>%
  autofit() %>%
  merge_at(i = 1, j = 3:5, part = 'header') %>%
  merge_at(i = 1, j = 6:8, part = 'header') %>%
  vline(part = 'all', j = c(1, 2, 5)) %>%
  width(j = ~ `Comparison between groups` + `Comparison between groups (hlvlchem)`, 
        width = 1) %>%
  width(j = c(3, 6), width = 1) %>%
  width(j = c(4, 7), width = 1)  %>%
  align(., align = 'center', j = c(2:8), part = 'all') %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 11, part = 'all')

# full crude regression models 
Table_full_crude_formatted <- crude_table %>% 
  filter(!str_detect(term, "ownership|poverty|uninsur")) %>%
  mutate(sesvar = "9999") %>%
  clean_for_Table(show_pval = TRUE, show_all_results = TRUE) %>% 
  format_reg_table(mod_type = "crude")

# full adjusted regression models 
Table_full_adjusted_formatted <- cladjglmermods %>% 
  ungroup() %>% 
  filter(sesvar == "mdi") %>% 
  filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
  clean_for_Table(show_pval = TRUE, show_all_results = TRUE) %>%
  format_reg_table()

# poverty
TableSupp_poverty <- cladjglmermods %>% 
  ungroup() %>%
  filter(sesvar == "perc_pov_ppl") %>%
  clean_for_Table() %>% 
  format_reg_table()

# homeownership
TableSupp_hmown <- 
  cladjglmermods %>% 
  ungroup() %>%
  filter(sesvar == "perc_hmown") %>%
  clean_for_Table() %>% 
  format_reg_table()

# perc uninsured
TableSupp_uninsur <- 
  cladjglmermods %>% 
  ungroup() %>%
  filter(sesvar == "perc_uninsur") %>%
  clean_for_Table() %>%
  format_reg_table()

# we don't need to show all of the results; we just want to show how the main 
# coefficients compare between adding vs. not adding TRI, industrial facilities, etc. 

cladjglmermods3 <- cladjglmermods %>% 
  ungroup() %>% 
  filter(sesvar == "mdi") %>% 
  filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
  clean_for_Table(show_pval = FALSE, show_all_results = FALSE) %>%
  mutate(`Includes potential point source?` = "Y")


cladjglmermods_nosources2 <- cladjglmermods_nosources  %>% 
  filter(sesvar == "mdi") %>% 
  filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
  clean_for_Table(show_pval = FALSE, show_all_results = FALSE) %>%
  mutate(`Includes potential point source?` = "N")

# merge and format! 
TableSupp_comparePointvsnoPoint <- cladjglmermods3 %>%
  bind_rows(cladjglmermods_nosources2) %>% arrange(term) %>%
  relocate(contains("point source"), .after = 1) %>% 
  format_reg_table() %>%
  fit_to_width(max_width = 11) %>%
  #width(width = 0.5, j = 2) %>% 
  merge_v(part = 'body', j = 1) %>%
  fix_border_issues()
  

# tribes and territories 
TableS5_format <- det_freq_contaminant_exclude %>%
  mutate(Contaminant = case_when(Contaminant == "evrdet" ~ "≥1 UCMR contaminant", 
                                 TRUE~Contaminant)) %>%
  mutate(Contaminant = factor(Contaminant, 
                              levels = c("≥1 UCMR contaminant", "1,4-dioxane",
                                         "1,1-dichloroethane", 
                                         "HCFC-22", 
                                         "PFAS"))) %>%
  mutate(Ndetfreq = paste0(Ndet, " (", detfreq_sys, ")"), 
         Nhlvlfreq = paste0(Nhlvl, " (", hlvl_freq, ")")) %>%
  mutate(Nhlvlfreq = ifelse(str_detect(Nhlvlfreq, "NA"), NA, Nhlvlfreq)) %>%
  select(-Ndet, -detfreq_sys, -Nhlvl, -hlvl_freq) %>%
  mutate(tribe_or_terr = case_when(tribe_or_terr == "stateside" ~ "PWS in U.S. State or D.C.", 
                                   tribe_or_terr == "territory" ~ "PWS in U.S. territory", 
                                   tribe_or_terr == "tribe" ~ "Tribal PWS")) %>%
  arrange(Contaminant) %>%
  flextable() %>%
  set_header_labels(Contaminant = "Contaminant", 
                    N = "N",
                    tribe_or_terr = "Tribe or territory",
                    Ndetfreq = "Detected ≥1 target contaminant, n (%)", 
                    Nhlvlfreq = "Exceeded ≥1 HRL, n (%)") %>%
  colformat_char(na_str = "--") %>%
  merge_v(j = 1) %>%
  merge_h(i = 1, part = 'header') %>%
  theme_vanilla() %>%
  align(align = 'left', part = 'all') %>%
  font(fontname = "Times", part = 'all') %>%
  fontsize(size = 11, part = 'all') %>%
  #set_table_properties(layout = 'autofit') %>%
  autofit() %>%
  width(j = 3, width = 1) %>%
  width(j = 4:5, width = 2)


######### COMPILE ALL TABLES AND FIGURES INTO ONE DOCUMENT ####################

# 2023-03-11: Moved TableS2 to end

doc1 <- read_docx() %>%
  body_add_par("Table 1. Characteristics of PWSs in the study sample (n = 4,808) and median demographic values of the counties they serve.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table1_formatted, align = "left") %>%
  #body_add_break() %>%
  body_add_par("Table 2. Reporting limits, detection frequencies, and common sources of UCMR3 target contaminants.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table2_formatted, align = "left") %>%
  body_add_break() %>%
  body_add_par("Figure 1. Comparison of mean-county level demographics between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.  *** p-value < 0.001; ** p-value < 0.01; * p-value < 0.05; (+) p-value < 0.10; n.s. = not significant.") %>%
  body_add_par("") %>%
  body_add_gg(value = p_final, style = "centered", width = 6.12, height = 5.5, scale = 1) %>%
  # body_add_flextable(value = Table3_formatted, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 3. Crude logistic regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table3_format, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 4. Multiple regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table4_format, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 5. Multiple regression model assessing likelihood of detecting a target contaminant, stratified by PWS size.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table5_format, align = "left") %>%
  body_add_break() %>%
  body_end_section_landscape(w = 8.5, h = 11)

print(doc1, target = paste0("results/output/UCMRsummary_MS_tables_and_figs_", Sys.Date(), ".docx"))  

doc2 <- read_docx() %>%
  body_add_par("Table S1. Comparison of mean-county univariate SES indicators between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableS1_format, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S2. Full results of the crude regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table_full_crude_formatted, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S3. Full results of the multiple regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
  body_add_par("") %>%
  body_add_flextable(value = Table_full_adjusted_formatted, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S4. Full results of the multiple regression models, with and without pollution source terms. Models evaluate associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableSupp_comparePointvsnoPoint, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S5. Multiple regression models using unidimensional poverty measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableSupp_poverty, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S6. Multiple regression models using unidimensional homeownership measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableSupp_hmown, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S7. Multiple regression models using unidimensional uninsurance rate measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableSupp_uninsur, align = "left") %>%
  body_add_break() %>%
  body_add_par("Table S8. Comparison of PWS detection frequencies between systems in tribes, territories, or in a U.S. state or D.C.") %>%
  body_add_par("") %>%
  body_add_flextable(value = TableS5_format, align = "left") %>%
  body_add_break() %>%
  body_add_par("Figure S1. Adjusted percent change (95% CI) in UCMR contaminant detections among selected demographics terms using different measures of socioeconomic status.") %>%
  body_add_par("") %>%
  body_add_gg(value = coefs_diffSESmeasures_plot, style = "centered", width = 9, scale = 1) %>%
  body_add_break() %>%
  body_add_par("Figure S2. Correlogram of covariates used in multiple regression models. Spearman's p statistic was used to measure associations.") %>%
  body_add_par("") %>%
  body_add_gg(value = Figure1corr, style = "centered", width = 9.28, height = 5.86, scale = 1.5) %>%
  body_end_section_landscape(w = 8.5, h = 11)

print(doc2, target = paste0("results/output/UCMRsummary_supp_tables_and_figs_", Sys.Date(), ".docx"))  






# ############ ARCHIVED BY AM ON 2023-03-11 #####################################
# 
# doc <- read_docx() %>%
#   body_add_par("Table 1. Characteristics of PWSs in the study sample (n = 4,808) and median demographic values of the counties they serve.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table1_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 2. Detection frequencies of UCMR3 target contaminants.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table2_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 3. Comparison of mean-county level demographics between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table3_formatted, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S1. Comparison of mean-county univariate SES indicators between PWSs with and without detection of unregulated contaminants and with or without exceedances of health reference levels. Unequal variances t-test were used to assess differences.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS1_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S2. Comparison of PWS detection frequencies between systems in tribes, territories, or in a U.S. state or D.C.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS2_format, align = "left") %>%
#   body_add_break() %>%
#   # body_add_par("Table 6. Multiple regression model evaluating associations between participant characteristics and report-type with environmental health knowledge index scores after receiving report-back.") %>%
#   # body_add_par("") %>%
#   # body_add_flextable(value = Table6_format, align = "left") %>%
#   # body_add_break() %>%
#   # body_add_par("Table 7. Proportion of participants who correctly answered each knowledge question at baseline and after report-back, for questions with a baseline percent correct less than 90 percent. McNemar's test was used to test for differences in proportion correct before and after report-back.") %>%
#   # body_add_par("") %>%
#   # body_add_flextable(value = Table7_format, align = "left") %>%
#   # body_add_break() %>%
#   # body_add_par("Table S1. Proportion of participants who correctly answered each knowledge question at baseline and after report-back, stratified by participant race. Frequencies for questions about the MyCHDSReport study are restricted to after report-back.") %>%
#   # body_add_par("") %>%
#   # body_add_flextable(value = TableS1_format, align = "left") %>%
# body_end_section_landscape(w = 8.5, h = 11)
# 
# #print(doc, target = paste0("results/output/UCMRsummary_tables_all_", Sys.Date(), ".docx"))  
# 
# 
# doc <- read_docx() %>%
#   body_add_par("Table 4. Crude logistic regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table4_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 5. Multiple regression models evaluating associations between UCMR contaminant detection, PWS characteristics, and service county-level demographics.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table5_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table 6. Multiple regression model assessing likelihood of detecting a target contaminant, stratified by PWS size.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = Table6_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Table S3. Multiple regression models using unidimensional poverty measures as the SES predictor. All other covariates are identical to the main regression analysis.") %>%
#   body_add_par("") %>%
#   body_add_flextable(value = TableS3_format, align = "left") %>%
#   body_add_break() %>%
#   body_add_par("Figure S1. Correlogram of covariates used in multiple regression models. Spearman's p statistic was used to measure associations. *** p-value < 0.001; ** p-value < 0.01; * p-value < 0.05.") %>%
#   body_add_par("") %>%
#   body_add_gg(value = Figure1corr, style = "centered", width = 10, height = 6, scale = 1.5) %>%
#   body_end_section_landscape(w = 8.5, h = 11)
# 
# #print(doc, target = paste0("results/output/UCMRsummary_tables_reg_", Sys.Date(), ".docx"))  
# 
# ## once all tables and figures have been decided, use doc2 to compile all tables/figs in order
# ## still need to find and store each object separately 
# ## may be more efficient to do this in another script
# 
# Table3_format <- 
#   crude_table %>% 
#   filter(!str_detect(term, "Homeownership|Poverty|Uninsur")) %>% 
#   select(term, 
#          contains(">=1 UCMR"), contains("over health guideline"), 
#          contains("1,4-dioxane"), contains("1,1-dichloroethane"), 
#          contains("HCFC-22"), contains("PFAS"), 
#          -contains("nobs")) %>% 
#   mutate(`result_1,1-dichloroethane` = 
#            ifelse(str_detect(`result_1,1-dichloroethane`,"Inf"), 
#                   as.character(NA), `result_1,1-dichloroethane`)) %>%
#   mutate(`p.value_1,1-dichloroethane` = 
#            ifelse(str_detect(`p.value_1,1-dichloroethane`,"0.96"),
#                   as.character(NA), 
#                   `p.value_1,1-dichloroethane`)) %>%
#   mutate(term = factor(term, levels = c(unique(crude_table$term)))) %>%
#   mutate(term = fct_relevel(term, "Percent Deprived", after = 2)) %>%
#   mutate(term = fct_relevel(term, "Number of samples", after = 11)) %>%
#   arrange(term) %>% 
#   flextable() %>% 
#   colformat_char(na_str = "--") %>%
#   set_header_df(mapping = Table3_key, key = "term") %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1, part = 'header') %>%
#   vline(part = 'all', j = c(seq(1, 14, by = 2))) %>%
#   width(j = ~ `p.value_>=1 UCMR Detection` + `p.value_1,4-dioxane` + 
#           `p.value_1,1-dichloroethane` + `p.value_HCFC-22` + 
#           `p.value_PFAS` + `p.value_>=1 target contaminant over health guideline`,
#         width = 0.4) %>%
#   width(j = ~ term, width = 1) %>%
#   autofit() %>%
#   footnote(i=5, j=8:9, value = as_paragraph(c(" 1,1-dichloroethane was only detected by large PWSs.")), 
#            ref_symbols = c("a"), part = "body", inline = FALSE) %>%
#   font(fontname = "Times", part = 'all') %>%
#   fontsize(size = 11, part = 'all')
# 
# Table3_format
# 
# 
# 
# 
# Table3_formatted <- table3 %>%
#   flextable() %>%
#   autofit() %>%
#   add_header_row(
#     top = TRUE, 
#     values = c("Demographic variable", 
#                "All PWSs", 
#                "Detected a target contaminant", 
#                "",
#                "", 
#                "Exceeded a health reference level", 
#                "",
#                ""
#     )) %>%
#   set_header_labels(
#     variable = "", 
#     `All PWSs` = "", 
#     `PWSs with no target contaminants detected` = "No", 
#     `PWSs with >=1 target contaminant detected` = "Yes", 
#     `Comparison between groups` = "P-value", 
#     `PWSs with no target contaminants exceeding federal guideline` = "No", 
#     `PWSs with >=1 target contaminant exceeding federal guideline` = "Yes", 
#     `Comparison between groups (hlvlchem)` = "P-value" 
#   ) %>%
#   merge_at(i = 1, j = 3:5, part = 'header') %>%
#   merge_at(i = 1, j = 6:8, part = 'header') %>%
#   border_remove() %>%
#   theme_vanilla() %>%
#   vline(part = 'all', j = 2) %>%
#   vline(part = 'all', j = 5) %>%
#   merge_at(i = 1:2, j = 1, part = 'header') %>%
#   merge_at(i = 1:2, j = 2, part = 'header') %>%
#   align(., align = 'center', j = c(2:8), part = 'all') %>%
#   width(j = 1, width = 1)