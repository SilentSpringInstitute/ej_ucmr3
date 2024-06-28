# 6/26/24 
# AM
# Evaluate interaction 

library(lme4)
library(broom.mixed)
# install.packages("margins")
# library(margins)

# data 
str(dat_clean)
dim(dat_clean)

## Interaction between system size and source water type ----

# regression formula
formInt <- paste(
  "det_any ~ perc_hisp_any + perc_black_nohisp + mdi_rate +",
  "perc_urban + size*pws_type + n_samples + adj_wwtp_flow",
  "+ n_fac_any_bin + (1|state)",
  collapse = " "
)

formInt

# run model
# This may take a minute to run
m1 <- glmer(formula = formInt, data = dat_clean, family = binomial)

# summarize and output
summary(m1)
tidy(m1, exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)

## Interaction between system size and % deprived -----------

# regression formula
formInt <- paste(
  "det_any ~ perc_hisp_any + perc_black_nohisp +",
  "perc_urban + size*mdi_rate + pws_type + n_samples + adj_wwtp_flow",
  "+ n_fac_any_bin + (1|state)",
  collapse = " "
)

formInt

# run model
# This may take a minute to run
m2 <- glmer(formula = formInt, data = dat_clean, family = binomial)

# summarize and output
summary(m2)
tidy(m2, exponentiate = TRUE, conf.level = 0.95, conf.int = TRUE)
