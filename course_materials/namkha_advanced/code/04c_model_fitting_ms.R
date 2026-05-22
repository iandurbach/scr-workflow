# Script # 4c / 5
# ###################################################
# The purpose of this script is to fit a small set of simple secr models
# to the multisession advanced Namkha analysis.
# Script to run before this one: 03_make_ms_masks.R
# Script to run after this one: 05c_model_results_ms.R
# ###################################################

# Packages needed
library(secr)
library(here)

output_file <- here("namkha_advanced", "output", "namkha_advanced_fitted_models_ms.RData")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
msmask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_msmask.RData")

###################################################
# 1. Load capture histories and masks
###################################################

load(capthist_file)
load(msmask_file)

# Check the multisession capture history and covariates before fitting models.
summary(ch_ms)
names(covariates(traps_ms[[1]]))
names(covariates(mask_ms[[1]]))

###################################################
# 2. Fit a small set of simple candidate models
###################################################

ms0 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE
)

ms1 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ std_tri, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE,
  start = ms0
)

ms2 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ std_elev, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE,
  start = ms0
)

ms3 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ 1, lambda0 ~ valley_stream, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE,
  start = ms0
)

ms4 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ std_tri, lambda0 ~ cliff + valley_stream, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE,
  start = ms0
)

# Density varies by session, allowing different abundance in each time block.
# Note this model can't easily be used for extrapolation.
ms5 <- secr.fit(
  ch_ms,
  detectfn = "HHN",
  mask = mask_ms,
  model = list(D ~ session, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_ms),
  verify = FALSE,
  start = ms0
)

AIC(ms0, ms1, ms2, ms3, ms4, ms5)

###################################################
# 3. Save fitted models for the results script
###################################################

save(
  ms0, ms1, ms2, ms3, ms4, ms5,
  file = output_file
)
