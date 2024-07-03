# 6/26/24 
# AM 
# Marginal analyses


# load libraries 
library(margins)
library(lme4)

# create main datasets
source("2. create main datasets.R")

# Estimating marginal effects (Jahred's method) ----

# The regression in the manuscript is based on a logistic mixed-effects model, 
# using state as a random effect (intercept). Reviewer asked us to estimate 
# the effect of % Hispanic, over some interesting range, on the probability on 
# unregulated contaminant detection. There were 3 interesting ranges: the 
# range from maximum to minimum, a one standard deviation increase, and the 
# interquartile range. 
# 
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

mod_jl <- lmer("det_any ~ perc_hisp_any + perc_black_nohisp + mdi_rate + 
             perc_urban + size + pws_type + n_samples + adj_wwtp_flow + n_fac_any_bin + 
             (1|state)",
               data = dat_clean %>% mutate(det_any = as.numeric(det_any)))

summary(mod_jl)
broom.mixed::tidy(mod_jl, conf.level = 0.95, conf.int = TRUE)

coef <- summary(mod_jl)$coefficients[[2]] 
coef
# 0.00333653

## Estimating effects
coef * (max_hisp - min_hisp)
# 0.319306 = 31.9% increase in probability over the range
coef * (as.numeric(q3_hisp - q1_hisp))
# 0.05113233 = 5.1% increase over the one IQR
coef * (sd_hisp)
# 0.05186385 = 5.2% increase

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
#0.00344706

# estimating over specific ranges
ave_effect * (max_hisp - min_hisp) * 100
# 32.98836  
ave_effect * (as.numeric(q3_hisp - q1_hisp)) * 100
# 5.282619 
ave_effect * (sd_hisp) * 100
# 5.358195 


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

 