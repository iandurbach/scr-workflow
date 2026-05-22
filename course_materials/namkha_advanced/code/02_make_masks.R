# Script # 2 / 5
# ###################################################
# The purpose of this script is to create an secr mask that covers the survey region.
# This script builds a general mask from the union of the Namkha survey area and a
# buffer around the camera locations, then adds spatial covariates to that mask.
# It also creates the elevation-constrained mask and the custom user-distance
# objects needed to handle the gorge barrier in the advanced tutorial.
# Script to run before this one: 01_make_capthist.R
# Script to run after this one: 03_make_ms_masks.R
# ###################################################

# Packages needed
library(dplyr)
library(terra)
library(secr)
library(sf)
library(elevatr)
library(gdistance)
library(here)

capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
output_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")
survey_area_file <- here("namkha_advanced", "data", "survey_area", "Namkha_RM.shp")
gorge_file <- here("namkha_advanced", "data", "survey_area", "namkha_gorge.kml")
tri_file <- here("namkha_advanced", "data", "spatial_covs", "TRI_Namkha.tif")
hydro_file <- here("namkha_advanced", "data", "spatial_covs", "Namkha_hydro.shp")

# Load the traps object created in script 01
load(capthist_file)

# User settings for mask spacing and trap buffer distance
mask_spacing <- 1500
mask_buffer <- 25000
elev_min <- 3000
elev_max <- 5500

###################################################
# 1. Read the survey area and trap data
###################################################

# Read the Namkha boundary and project it to the UTM CRS used in the analysis
survey_area <- st_read(survey_area_file, quiet = TRUE) %>%
  st_zm() %>%
  st_transform(my_crs) %>%
  st_geometry()

# Read the gorge line used later for custom movement distances.
# ADVANCED ISSUE: we will treat this gorge as a movement barrier.
gorge <- st_read(gorge_file, quiet = TRUE) %>%
  st_transform(my_crs) %>%
  st_geometry()

# Turn traps into spatial points and create a buffer around all camera locations
traps_sf <- st_as_sf(full_traps, coords = c("x", "y"), crs = my_crs)
traps_buffer <- traps_sf %>%
  st_buffer(mask_buffer) %>%
  st_union() %>%
  st_geometry()

# The full region is the union of the Namkha boundary and the trap buffer
full_region <- st_union(survey_area, traps_buffer)

###################################################
# 2. Create the full secr mask
###################################################

# Create a regular grid of candidate mask points over the full region
mask_grid <- st_make_grid(
  st_buffer(full_region, 10000),
  cellsize = c(mask_spacing, mask_spacing),
  what = "centers"
)

# Keep only the grid points falling inside the full region
mask_points <- mask_grid[full_region] %>%
  st_as_sf() %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  dplyr::select(x = X, y = Y)

# Convert the point coordinates to a secr mask object
mask <- read.mask(data = mask_points, spacing = mask_spacing)

###################################################
# 3. Add spatial covariates to the mask
###################################################

# Read the TRI raster and project it to the analysis CRS
tri <- rast(tri_file)
tri <- project(tri, y = my_crs)
names(tri) <- "tri"

# Aggregate the raster so its resolution is closer to the mask spacing
aggregate_factor <- max(1, floor(mask_spacing / max(res(tri))))
tri <- aggregate(tri, fact = c(aggregate_factor, aggregate_factor))
mask <- addCovariates(mask, tri)

# Read the watercourse layer and calculate distance from each mask point to water
hydro <- st_read(hydro_file, quiet = TRUE) %>%
  st_transform(crs = my_crs) %>%
  st_union()

mask_sf <- st_as_sf(mask, coords = c("x", "y"), crs = my_crs)
covariates(mask)$d2hydro <- as.numeric(st_distance(mask_sf, hydro))

# ADVANCED ISSUE: read elevation data with elevatr and use it to clip the mask.
# The downloaded raster is cached so this only needs to happen once.
elev_file <- here("namkha_advanced", "output", "cache", "namkha_elevation.tif")
dir.create(dirname(elev_file), recursive = TRUE, showWarnings = FALSE)

if (file.exists(elev_file)) {
  elev <- rast(elev_file)
} else {
  elev_download <- get_elev_raster(
    locations = st_transform(st_as_sf(st_buffer(st_union(full_region), 10000)), 4326),
    z = 10,
    clip = "locations"
  )
  elev <- rast(elev_download)
  writeRaster(elev, elev_file, overwrite = TRUE)
}

elev <- project(elev, y = my_crs)
names(elev) <- "elev"
elev_aggregate_factor <- max(1, floor(mask_spacing / max(res(elev))))
elev <- aggregate(elev, fact = c(elev_aggregate_factor, elev_aggregate_factor))
mask <- addCovariates(mask, elev)

# Print missing-value counts and proportions for mask covariates
covariate_missing <- data.frame(
  covariate = names(covariates(mask)),
  n_missing = sapply(covariates(mask), function(x) sum(is.na(x))),
  prop_missing = sapply(covariates(mask), function(x) mean(is.na(x)))
)
print(covariate_missing)

# Stop if more than 20% missing on any covariate
if (any(covariate_missing$prop_missing > 0.2)) {
  stop("At least one mask covariate has more than 20% missing values.")
}

# Replace any missing covariate values with the mean of that covariate
for (covname in names(covariates(mask))) {
  if (anyNA(covariates(mask)[, covname])) {
    covariates(mask)[, covname][is.na(covariates(mask)[, covname])] <-
      mean(covariates(mask)[, covname], na.rm = TRUE)
  }
}

# scale any numeric covariates, first find mean and std devs over full mask
scaling_df <- data.frame(cov = as.character(), mean = as.numeric(), sd = as.numeric())
for (i in names(covariates(mask))) {
  if (is.numeric(covariates(mask)[, i])) {
    newi <- paste0("std_", i)
    meani <- mean(covariates(mask)[, i], na.rm = TRUE)
    sdi <- sd(covariates(mask)[, i], na.rm = TRUE)
    scaling_df <- rbind.data.frame(scaling_df, data.frame(cov = i, mean = meani, sd = sdi))
    covariates(mask)[, newi] <- (covariates(mask)[, i] - meani) / sdi
  }
}

names(covariates(mask))

###################################################
# 4. Create the masks used later in the workflow
###################################################

# Keep a version of the mask restricted to the survey area
inside_survey_area <- lengths(st_intersects(mask_sf, survey_area)) > 0
mask_survey_area <- subset(mask, subset = inside_survey_area)

# ADVANCED ISSUE: create masks that exclude elevations outside the plausible
# snow leopard activity-centre range used for this tutorial.
inside_elev_range <- covariates(mask)$elev >= elev_min & covariates(mask)$elev <= elev_max
mask_elev <- subset(mask, subset = inside_elev_range)

mask_elev_sf <- st_as_sf(mask_elev, coords = c("x", "y"), crs = my_crs)
inside_survey_area_elev <- lengths(st_intersects(mask_elev_sf, survey_area)) > 0
mask_survey_area_elev <- subset(mask_elev, subset = inside_survey_area_elev)

# For model fitting, keep all elevation-constrained points within the trap buffer,
# including those outside Namkha
inside_buffer_elev <- lengths(st_intersects(mask_elev_sf, traps_buffer)) > 0
mask_model <- subset(mask_elev, subset = inside_buffer_elev)

# Save the trap buffer separately because it is useful for later plotting
all_sess_masks <- traps_buffer

###################################################
# 5. Create custom movement distances across the gorge
###################################################

# ADVANCED ISSUE: the gorge is treated as a barrier to movement using a custom
# user-distance matrix for non-Euclidean movement distances.

# The gorge is narrower than the 1.5 km mask spacing in parts, so we calculate
# movement distances on a finer mask and transfer them back to the modelling mask.
mask_fine_all <- make.mask(
  traps = full_traps,
  buffer = mask_buffer,
  type = "trapbuffer",
  spacing = 200
)

# The same fine mask as above, but with the gorge removed.
mask_fine <- make.mask(
  traps = full_traps,
  buffer = mask_buffer,
  type = "trapbuffer",
  spacing = 200,
  poly = gorge,
  poly.habitat = FALSE,
  keep.poly = FALSE
)

# Plot to compare full and gorge-less masks
plot(mask_fine_all)
plot(mask_fine, add = TRUE, col = "red")

# Calculate shortest paths between traps and mask points rather than Euclidean
# distances, so animals have to travel "around" the gorge rather than through it.
# See ?nedist or the secr non-Euclidean distance vignette for details.
userd <- nedist(full_traps, mask, mask_fine, directions = 16)

# Various subsets used by cropped masks
userd_elev <- userd[, inside_elev_range, drop = FALSE]
userd_model <- userd_elev[, inside_buffer_elev, drop = FALSE]
userd_survey_area_elev <- userd_elev[, inside_survey_area_elev, drop = FALSE]

# Save all mask objects needed by later scripts
save(
  mask, mask_survey_area, mask_elev, mask_survey_area_elev, mask_model,
  userd, userd_elev, userd_model, userd_survey_area_elev,
  mask_spacing, mask_buffer, elev_min, elev_max,
  scaling_df,
  survey_area, traps_buffer, all_sess_masks, full_region, gorge,
  file = output_file
)
