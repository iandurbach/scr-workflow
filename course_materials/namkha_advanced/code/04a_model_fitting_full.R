# Script # 4a / 5
# ###################################################
# The purpose of this script is to fit a small set of simple secr models
# to the full-survey advanced Namkha analysis.
# Script to run before this one: 03_make_ms_masks.R
# Script to run after this one: 05a_model_results_full.R
# ###################################################

# Packages needed
library(secr)
library(here)

output_file <- here("namkha_advanced", "output", "namkha_advanced_fitted_models_full.RData")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
mask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")

###################################################
# 1. Load capture histories and masks
###################################################

load(capthist_file)
load(mask_file)

# Check the capture history and covariates before fitting models.
summary(full_ch)
names(covariates(full_traps))
names(covariates(mask_model))

###################################################
# 2. Fit a small set of simple candidate models
###################################################

f0 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE
)

f1 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

f2 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_elev, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

f3 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ valley_stream, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

f4 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ cliff + valley_stream, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f1
)

AIC(f0, f1, f2, f3, f4)

###################################################
# 3. Save fitted models for the results script
###################################################

save(
  f0, f1, f2, f3, f4,
  file = output_file
)
