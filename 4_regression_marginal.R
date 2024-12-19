# DATE STARTED: 2024-06-26
# AUTHOR: Aaron Maruzzo
# PURPOSE: Conduct marginal analysis
# LATEST REVISION: 2024-11-12 
# LATEST VERSION RUN: R version 4.2.2 (2022-10-31 ucrt)

# Start here (if not already run):
# source("1_combine_process.R")

# load libraries 
library(margins)
library(lme4)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Overview ----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The regression in the manuscript is based on a logistic mixed-effects model.
# Reviewer asked to estimate the effect of % Hispanic on the probability on 
# unregulated contaminant detection over some "interesting range" of 
# % Hispanic. 3 ranges tested: from min to max, a one standard deviation increase, and the 
# interquartile range. 

# Estimating marginal effects using det_any as a continuous predictor ----
# We estimated marginal effects by running the model as a continuous variable 
# and using the coefficients (which represent the change in the probability of 
# detection per unit increase). We multiplied the coefficients by the 
# interesting ranges.

min_hisp <- min(dat_clean$perc_hisp_any) # 0% Hispanic 
max_hisp <- max(dat_clean$perc_hisp_any) # 95.7% Hispanic

mean_hisp <- mean(dat_clean$perc_hisp_any) #14.1%
sd_hisp <- sd(dat_clean$perc_hisp_any) #15.5% 
meansd_hisp <- mean_hisp + sd_hisp # 29.6%

q1_hisp <- quantile(dat_clean$perc_hisp_any, 0.25) #3.6% - same as in paper
q3_hisp <- quantile(dat_clean$perc_hisp_any, 0.75) #18.9% - same as in paper
q3_hisp - q1_hisp

# model selected: any detection of target contaminants ("det_any")
mod_jl <- lmer("det_any ~ perc_hisp_any + perc_black_nohisp + mdi_rate + 
             perc_urban + size + pws_type + n_samples + adj_wwtp_flow + n_fac_any_bin + 
             (1|state)",
               data = dat_clean %>% mutate(det_any = as.numeric(det_any)))

summary(mod_jl)
broom.mixed::tidy(mod_jl, conf.level = 0.95, conf.int = TRUE)

coef <- summary(mod_jl)$coefficients[[2]] 
coef
# 0.002993953

## Estimating effects
coef * (max_hisp - min_hisp)
# 0.2865213 = 29% increase over the range of % Hisp
coef * (as.numeric(q3_hisp - q1_hisp))
# 0.04565778 = 5% increase over the one IQR
coef * (sd_hisp)
# 0.0465079 = 5% increase over one-standard dev

## Note- estimating marginal effects for a continuous variable is the same 
## as calculating the average of probability differences between counterfactuals.

# hz1 <- predict(mod_jl, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = min_hisp), 
#               type = "response")
# 
# hz2 <- predict(mod_jl, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = max_hisp), 
#               type = "response")
# 
# min_hisp; max_hisp
# max_hisp - min_hisp
# hist(hz2-hz1, main = "First-order differences")
# mean(hz2-hz1)

# Using margins() package to estimate marginal effects ----

# run logistic mixed-effects model for any unregulated contaminant detection 
mod <- glmer(formula = "det_any ~ perc_hisp_any + perc_black_nohisp + mdi_rate + 
             perc_urban + size + pws_type + n_samples + adj_wwtp_flow + n_fac_any_bin + 
             (1|state)", 
             data = dat_clean, 
             family = binomial)

# summarize average marginal effects
summod <- summary(margins(mod))
summod 

# mean effect coefficient
ave_effect <- summod$AME[summod$factor=="perc_hisp_any"] 
ave_effect
#0.003190683 

# estimating over specific ranges
ave_effect * (max_hisp - min_hisp) * 100
# 30.53484   
ave_effect * (as.numeric(q3_hisp - q1_hisp)) * 100
# 4.865791  
ave_effect * (sd_hisp) * 100
# 4.95639  

# Archive (do not run) ----

# ## Note - again - that these are identical to producing counterfactuals and 
# ## estimating the average difference in odds 
# h1 <- predict(mod, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = min_hisp), 
#               type = "response")
# 
# h2 <- predict(mod, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = max_hisp), 
#               type = "response")
# 
# min_hisp; max_hisp
# max_hisp - min_hisp
# hist(h2-h1, main = "First-order differences")
# mean(h2-h1)
# 
# cplot(mod, "perc_hisp_any", draw = "FALSE")
# dim(mod)
# str(mod)
# ## standard deviation 
# 
# 
# hx1 <- predict(mod, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = mean_hisp), 
#               type = "response")
# 
# hx2 <- predict(mod, 
#               newdata = dat_clean %>% mutate(perc_hisp_any = sd1_hisp), 
#               type = "response")
# 
# 
# mean_hisp; sd1_hisp
# sd1_hisp - mean_hisp
# hist(hx2-hx1, main = "First-order differences")
# mean(hx2-hx1)
# 
# ## q1 to q3 
# 
# hy1 <- predict(mod, 
#                newdata = dat_clean %>% mutate(perc_hisp_any = q1_hisp), 
#                type = "response")
# 
# hy2 <- predict(mod, 
#                newdata = dat_clean %>% mutate(perc_hisp_any = q3_hisp), 
#                type = "response")
# 
# q1_hisp; q3_hisp
# q3_hisp-q1_hisp
# hist(hy2-hy1, main = "First-order differences")
# mean(hy2-hy1)

 