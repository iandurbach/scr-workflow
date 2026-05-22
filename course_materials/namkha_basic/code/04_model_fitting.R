# Script # 4 / 5
# ###################################################
# The purpose of this script is to fit a small set of simple secr models.
# Models are written out explicitly so it is clear how density and detector
# covariates are added to the workflow.
# Script to run before this one: 02_make_masks.R
# Script to run after this one: 05_model_results.R
# ###################################################

# Packages needed 
library(secr)
library(here)

output_file <- here("namkha_basic", "output", "namkha_basic_fitted_models.RData")
capthist_file <- here("namkha_basic", "output", "namkha_basic_secr_inputs_capthist.RData")
mask_file <- here("namkha_basic", "output", "namkha_basic_secr_inputs_mask.RData")

###################################################
# 1. Load capture histories and masks
###################################################

# Load the capthist object from script 01 and the masks from script 02
load(capthist_file)
load(mask_file)

# Check that the key objects contain the expected covariates
summary(ch)
names(covariates(traps))
names(covariates(mask_model))

###################################################
# 2. Fit a small set of simple candidate models
###################################################

# The fitting mask is the trap-buffer mask, not the Namkha boundary mask.
# This allows activity centres outside Namkha if they are within range of traps.

# Null model: no density or detector covariates
m0 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
  verify = FALSE
)

# Density varies with ruggedness (TRI)
m1 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ 1, sigma ~ 1),
  verify = FALSE,
  start = m0
)

# Density varies with distance to water
m2 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_d2hydro, lambda0 ~ 1, sigma ~ 1),
  verify = FALSE,
  start = m0
)

# Encounter rate varies depending on whether detection is in a valley or stream
m3 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ valley_stream, sigma ~ 1),
  verify = FALSE,
  start = m0
)

# Combined model with a density covariate and a detector covariate
m4 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ valley_stream, sigma ~ 1),
  verify = FALSE,
  start = m1
)

# Combined model with a density covariate and a detector covariate
m5 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri + std_d2hydro, lambda0 ~ cliff + valley_stream, sigma ~ 1),
  verify = FALSE,
  start = m4
)

# Compare fitted models using AIC
AIC(m0, m1, m2, m3, m4, m5)

###################################################
# 3. Save fitted models for the results script
###################################################

save(
  m0, m1, m2, m3, m4, m5,
  file = output_file
)
