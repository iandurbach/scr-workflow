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

output_file <- here("namkha_advanced", "output", "namkha_advanced_fitted_models_full_sex.RData")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
mask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")

###################################################
# 1. Load capture histories and masks
###################################################

load(capthist_file)
load(mask_file)

# Check the capture history and covariates before fitting sex-specific models.
summary(full_ch)
names(covariates(full_traps))
names(covariates(mask_model))

###################################################
# 2. Fit a small set of simple candidate models
###################################################

# The fitting mask is the trap-buffer mask after clipping to plausible elevation.
# Custom user distances are used so movement can avoid the gorge barrier.
# ADVANCED ISSUE: hcov = "sex" allows detection parameters to vary by individual
# sex while still retaining animals with unknown sex.

# h2 models only use first two levels of the sex covariate 
# We have explicit Unknown but its the third level so dropping is 
levels(covariates(full_ch)$sex)

# Null model with sex as an individual covariate on detection.
f0 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ h2, sigma ~ 1),
  hcov = "sex",
  details = list(userdist = userd_model),
  verify = FALSE
)

# Density varies with terrain ruggedness.
f1 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ h2, sigma ~ 1),
  hcov = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

# Density varies with elevation.
f2 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_elev, lambda0 ~ h2, sigma ~ 1),
  hcov = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

# Encounter rate varies by whether the camera is in a valley or stream setting.
f3 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ valley_stream + h2, sigma ~ 1),
  hcov = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0
)

# Combined model with ruggedness on density and local topography on encounter rate.
f4 <- secr.fit(
  full_ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ cliff + valley_stream + h2, sigma ~ 1),
  hcov = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f1
)

AIC(f0, f1, f2, f3, f4)

###################################################
# 3. Fit models to subset of known-sex individuals
###################################################

# ADVANCED ISSUE: groups = "sex" estimates separate real parameters by sex, but
# it requires removing individuals whose sex is unknown.
full_ch_known_sex <- subset(full_ch, subset = covariates(full_ch)$sex != "Unknown")
full_ch_known_sex <- shareFactorLevels(full_ch_known_sex)

summary(full_ch, terse = TRUE)
summary(full_ch_known_sex, terse = TRUE)

# Null grouped model for known-sex individuals.
f0g <- secr.fit(
  full_ch_known_sex,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ g, sigma ~ 1),
  groups = "sex",
  details = list(userdist = userd_model),
  verify = FALSE
)

# Grouped model with density varying with terrain ruggedness.
f1g <- secr.fit(
  full_ch_known_sex,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ g, sigma ~ 1),
  groups = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0g
)

# Grouped model with density varying with elevation.
f2g <- secr.fit(
  full_ch_known_sex,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_elev, lambda0 ~ g, sigma ~ 1),
  groups = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0g
)

# Grouped model with encounter rate varying at valley/stream cameras.
f3g <- secr.fit(
  full_ch_known_sex,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ valley_stream + g, sigma ~ 1),
  groups = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0g
)

# Grouped combined model with density and encounter-rate covariates.
f4g <- secr.fit(
  full_ch_known_sex,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ cliff + valley_stream + g, sigma ~ 1),
  groups = "sex",
  details = list(userdist = userd_model),
  verify = FALSE,
  start = f0g
)

# Do not compare hcov and grouped models in the same AIC table because they use
# different data or parameterisations.
AIC(f0, f1, f2, f3, f4)
AIC(f0g, f1g, f2g, f3g, f4g)

###################################################
# 3. Save fitted models for the results script
###################################################

save(
  f0, f1, f2, f3, f4,
  f0g, f1g, f2g, f3g, f4g,
  file = output_file
)
