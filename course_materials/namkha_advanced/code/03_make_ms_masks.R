# Script # 3 / 5
# ###################################################
# The purpose of this script is to create multisession secr masks for model fitting.
# This is done by cropping the elevation-constrained full mask to polygons around
# traps in each session.
# Covariates have already been added in script 02, so this script is mainly
# about subsetting the full mask to make the session-specific masks needed for
# a multisession analysis.
# Script to run before this one: 02_make_masks.R
# Script to run after this one: 04a_model_fitting_full.R, 04b_model_fitting_short.R, 04c_model_fitting_ms.R
# ###################################################

# Packages needed
library(secr)
library(sf)
library(here)

mask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
output_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_msmask.RData")

# Load the mask and capthist objects created in scripts 01 and 02
load(mask_file)
load(capthist_file)

###################################################
# 1. Create a trap-buffer polygon for each session
###################################################

# ADVANCED ISSUE: in a multisession analysis we need one mask for each session.
# Here we create a polygon around the traps in each session, and then use those
# polygons below to crop the full elevation-constrained mask.
# Each session's mask should be within mask_buffer of the traps for that session.
mask_per_session_sf <- list()
for (i in seq_along(traps_ms)) {
  mask_per_session_sf[[i]] <- st_as_sf(traps_ms[[i]], coords = c("x", "y"), crs = my_crs) %>%
    st_buffer(mask_buffer) %>%
    st_union()
}
names(mask_per_session_sf) <- names(traps_ms)

###################################################
# 2. Crop the elevation-constrained mask for each session
###################################################

# Convert the full elevation-constrained mask to an sf geometry object so we can
# test which mask points fall inside each session polygon.
mask_elev_sf <- st_as_sf(mask_elev, coords = c("x", "y"), crs = my_crs) %>%
  st_geometry()

# Create the list of masks used in the multisession analysis.
mask_ms <- list()
for (i in seq_along(mask_per_session_sf)) {
  # binary indicator of whether mask points are in the session mask polygon
  mask_in <- lengths(st_intersects(mask_elev_sf, mask_per_session_sf[[i]])) > 0
  mask_ms[[i]] <- subset(mask_elev, subset = mask_in)
}
names(mask_ms) <- names(traps_ms)

###################################################
# 3. Create matching user-distance matrices for each session (optional)
###################################################

# This block is only needed if the analysis uses a custom user-distance matrix,
# for example because there is a movement barrier such as the gorge in Namkha.

# Use the same session-specific mask indicator as above to subset the columns
# of the full userdist matrix.
userd_ms <- list()
for (i in seq_along(mask_per_session_sf)) {
  mask_in <- lengths(st_intersects(mask_elev_sf, mask_per_session_sf[[i]])) > 0
  userd_ms[[i]] <- userd_elev[, mask_in, drop = FALSE]
}
names(userd_ms) <- names(traps_ms)

###################################################
# 4. Create masks for combined multisession summaries
###################################################

# It is also useful to have a single mask covering the union of all session
# masks, for example when summarising results across the full multisession study.

# Create a mask polygon that is the union of all session masks
all_sess_masks <- mask_per_session_sf[[1]]
for (i in 2:length(mask_per_session_sf)) {
  all_sess_masks <- st_union(all_sess_masks, mask_per_session_sf[[i]])
}

# Crop the survey-area elevation mask to the union of all session masks.
# This gives a single survey-area mask covering all sessions combined.
mask_survey_area_elev_sf <- st_as_sf(mask_survey_area_elev, coords = c("x", "y"), crs = my_crs)
inside_all_sessions <- lengths(st_intersects(mask_survey_area_elev_sf, all_sess_masks)) > 0
mask_all_sess_survey <- subset(mask_survey_area_elev, subset = inside_all_sessions)

# If using barriers/non-Euclidean movement distances, make the matching userdist 
# object for the combined multisession survey mask (optional)
userd_all_sess_survey <- userd_survey_area_elev[, inside_all_sessions, drop = FALSE]

# Save all multisession mask objects needed later in the workflow
save(
  mask_ms, mask_all_sess_survey,
  mask_per_session_sf, all_sess_masks,
  userd_ms, userd_all_sess_survey,
  file = output_file
)
