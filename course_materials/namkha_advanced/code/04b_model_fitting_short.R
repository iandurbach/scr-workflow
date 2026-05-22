# Script # 4b / 5
# ###################################################
# The purpose of this script is to fit a small set of simple secr models
# to the shortened-survey advanced Namkha analysis.
# Script to run before this one: 03_make_ms_masks.R
# Script to run after this one: 05b_model_results_short.R
# ###################################################

# Packages needed
library(secr)
library(here)

output_file <- here("namkha_advanced", "output", "namkha_advanced_fitted_models_short.RData")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
mask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")

###################################################
# 1. Load capture histories and masks
###################################################

load(capthist_file)
load(mask_file)

# Check the shortened capture history and covariates before fitting models.
summary(short_ch)
names(covariates(short_traps))
names(covariates(mask_model))

###################################################
# 2. Fit a small set of simple candidate models
###################################################

# This analysis uses the shorter survey period, while still retaining the
# elevation-constrained mask and the gorge-aware movement distances.

# Null model: density, encounter rate, and movement scale are constant.
s0 <- secr.fit(
  short_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE
)

# Density varies with terrain ruggedness.
s1 <- secr.fit(
  short_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = s0
)

# Density varies with elevation.
s2 <- secr.fit(
  short_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_elev, lambda0 ~ 1, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = s0
)

# Encounter rate varies by whether the camera is at a cliff.
s3 <- secr.fit(
  short_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ cliff, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = s0
)

# Combined model with ruggedness on density and local topography on encounter rate.
s4 <- secr.fit(
  short_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ cliff + valley_stream, sigma ~ 1),
  details = list(userdist = userd_model),
  verify = FALSE,
  start = s1
)

AIC(s0, s1, s2, s3, s4)

###################################################
# 3. Save fitted models for the results script
###################################################

save(
  s0, s1, s2, s3, s4,
  file = output_file
)
