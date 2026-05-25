# Script # 5c / 5
# ###################################################
# The purpose of this script is to summarise fitted models and make simple maps
# of the survey region and predicted activity-centre density for the multisession analysis.
# Script to run before this one: 04c_model_fitting_ms.R
# Script to run after this one: none
# ###################################################

# Packages needed
library(dplyr)
library(secr)
library(sf)
library(ggplot2)
library(MASS)
library(here)

source(here("namkha_basic", "code", "more-utilities.R"))

results_dir <- here("namkha_advanced", "output", "results", "ms")
capthist_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
mask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_mask.RData")
msmask_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_msmask.RData")
models_file <- here("namkha_advanced", "output", "namkha_advanced_fitted_models_ms.RData")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###################################################
# 1. Load fitted models and masks
###################################################

load(capthist_file)
load(mask_file)
load(msmask_file)
load(models_file)

# Omitting ms5 which has D ~ session
fitted_models <- list(ms0 = ms0, ms1 = ms1, ms2 = ms2, ms3 = ms3, ms4 = ms4)

# Mask to use when estimating abundance and density, and maps
extrap_mask <- mask_survey_area_elev

# User-distance matrix matching the extrapolation mask
extrap_mask_userd <- userd_survey_area_elev

# `mask_model` was used for fitting, but abundance is extrapolated to `extrap_mask`
survey_area_km2 <- nrow(extrap_mask) * attr(extrap_mask, "area") / 100

###################################################
# 2. Make a table of AIC, abundance, and density
###################################################

# To use region.N to estimate abundance on a new mask, need to replace the model's
# userdist with the new mask's userdist. Do this for all models so that we can estimate
# abundance for all models.
fitted_models_region <- lapply(fitted_models, function(model) {
  model$details$userdist <- setNames(
    replicate(length(traps_ms), extrap_mask_userd, simplify = FALSE),
    names(traps_ms)
  )
  model
})

# Function to extract abundance estimates from secr's region.N
# abundance = E.N (expected abundance) or R.N (realised abundance)
extract_abundance_ms <- function(model_name, abundance = c("E.N")) {
  rn <- region.N(fitted_models_region[[model_name]], region = extrap_mask)
  bind_rows(lapply(seq_along(rn), function(i) {
    data.frame(
      session = names(rn)[i],
      modelname = model_name,
      estimate = rn[[i]][abundance, "estimate"],
      SE.estimate = rn[[i]][abundance, "SE.estimate"],
      lcl = rn[[i]][abundance, "lcl"],
      ucl = rn[[i]][abundance, "ucl"]
    )
  }))
}

# Estimate abundance for each fitted model over the survey area.
# Add density = abundance / area.
abundance_table <- lapply(names(fitted_models), extract_abundance_ms) %>%
  bind_rows() %>%
  mutate(
    D = estimate / survey_area_km2,
    Dlcl = lcl / survey_area_km2,
    Ducl = ucl / survey_area_km2,
    CV = 100 * SE.estimate / estimate
  )

# Add AIC results
aic_table <- AIC(ms0, ms1, ms2, ms3, ms4, criterion = "AICc") %>%
  mutate(modelname = row.names(.))

# Save the combined summary table
write.csv(
  left_join(abundance_table, aic_table, by = "modelname") %>%
    dplyr::select(session, modelname, model, AIC, AICc, dAICc, AICcwt, estimate, lcl, ucl, D, Dlcl, Ducl, CV),
  file = file.path(results_dir, "namkha_advanced_results_abundance_AIC_ms.csv"),
  row.names = FALSE
)

# Use the model with the lowest AICc for the density surface map
best_model_name <- aic_table$modelname[which.min(aic_table$AICc)]
best_model <- fitted_models[[best_model_name]]

###################################################
# 3. Make a map of the survey region
###################################################

# Convert traps, the survey area boundary, and the gorge to sf objects for plotting
traps_sf <- st_as_sf(full_traps, coords = c("x", "y"), crs = my_crs)
survey_area_sf <- st_as_sf(survey_area)
gorge_sf <- st_as_sf(gorge)

# Plot the Namkha boundary, the trap-buffer region, the gorge, and the camera locations
survey_region_plot <- ggplot() +
  geom_sf(data = survey_area_sf, fill = NA, colour = "black", linewidth = 0.5) +
  geom_sf(data = st_as_sf(all_sess_masks), fill = NA, colour = "firebrick", linewidth = 0.4, linetype = 2) +
  geom_sf(data = gorge_sf, colour = "steelblue4", linewidth = 0.6) +
  geom_sf(data = traps_sf, colour = "black", size = 1.1) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave(file.path(results_dir, "namkha_advanced_survey_region_ms.png"), survey_region_plot, width = 7, height = 5, dpi = 200)

###################################################
# 4. Predict and map density over Namkha
###################################################

# User sets the model to make the map for.
# Usually set this to best_model.
map_model <- best_model

# The multisession model predicts one density surface per session, even when
# predicting over a single reporting mask. Use the first session's surface here.
map_session <- names(traps_ms)[1]

# Predict density over the Namkha survey area
pred_surface <- predictDsurface(map_model, mask = extrap_mask, cl.D = FALSE)
density_cols <- which(names(covariates(pred_surface)) == "D.0")

if (length(density_cols) < 1) {
  stop("No predicted density columns found in pred_surface.")
}

# Convert mask points to square cells so the predicted density is easier to plot
density_cells <- st_as_sf(data.frame(extrap_mask), coords = c("x", "y"), crs = my_crs)
density_cells <- st_buffer(
  density_cells,
  dist = attr(extrap_mask, "spacing") / 2,
  endCapStyle = "SQUARE",
  nQuadSegs = 1
)

# Convert density from animals per hectare to animals per 100 km2
density_cells$D_per_100km2 <- covariates(pred_surface)[, density_cols[1]] * 10000

# Plot predicted density together with the survey boundary, gorge, and trap locations
density_plot <- ggplot() +
  geom_sf(data = density_cells, aes(fill = D_per_100km2), colour = NA, alpha = 0.85) +
  geom_sf(data = survey_area_sf, fill = NA, colour = "black", linewidth = 0.5) +
  geom_sf(data = gorge_sf, colour = "steelblue4", linewidth = 0.6) +
  geom_sf(data = traps_sf, colour = "black", size = 0.8) +
  scale_fill_viridis_c(name = expression("Density / 100 km"^2), option = "D") +
  labs(x = NULL, y = NULL, title = paste("Predicted density from session", map_session)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

# Save survey and density maps
ggsave(
  file.path(results_dir, "namkha_advanced_predicted_density_ms.png"),
  density_plot,
  width = 7,
  height = 5,
  dpi = 200
)

# Save the predicted density surface as a shapefile for later mapping
st_write(
  density_cells %>% dplyr::select(D_per_100km2),
  dsn = file.path(results_dir, "namkha_advanced_predicted_density_ms.shp"),
  delete_layer = TRUE,
  quiet = TRUE
)

###################################################
# 5. Plot effects of any covariate on density
###################################################

# User sets the model to make plot for.
# ms4 includes the density and detection covariates used in these example plots.
covplot_model <- fitted_models[["ms4"]]

# Identify which covariates appear in the density part of the chosen model.
# Session effects are model parameters rather than mask covariates, so only
# retain density covariates that are present in the extrapolation mask.
model_covs <- covplot_model$betanames
dens_covs <- sub("^D\\.", "", model_covs[grepl("^D\\.", model_covs)])
dens_covs <- intersect(dens_covs, names(covariates(extrap_mask)))
dens_covs

# User sets the density covariate to plot.
# Any other density covariates in the same model will be held constant.
focal_cov <- "std_tri"
focal_cov_full_name <- "Terrain ruggedness index"

# Stop if the chosen covariate is not actually used in the density model
if (!(focal_cov %in% dens_covs)) {
  stop("focal_cov must be a density covariate in chosen model.")
}

# Build a sequence of values for the focal covariate across its range in the
# extrapolation mask. These values will form the x-axis of the plot.
newdata <- data.frame(
  focal_values = seq(
    min(covariates(extrap_mask)[, focal_cov], na.rm = TRUE),
    max(covariates(extrap_mask)[, focal_cov], na.rm = TRUE),
    length.out = min(500, nrow(extrap_mask))
  )
)
names(newdata)[1] <- focal_cov

# Keep track of both the model-scale covariate name and the version to display
# on the plot. If the model uses a standardized covariate, the plot can still
# show the original unstandardized values for easier interpretation.
focal_cov_unstd <- sub("^std_", "", focal_cov)
plot_cov <- focal_cov

# If the focal covariate is standardized, also create the original-scale
# version so the x-axis is easier to interpret.
if (startsWith(focal_cov, "std_") && focal_cov_unstd %in% scaling_df$cov) {
  focal_mean <- scaling_df[scaling_df$cov == focal_cov_unstd, "mean"]
  focal_sd <- scaling_df[scaling_df$cov == focal_cov_unstd, "sd"]
  newdata[[focal_cov_unstd]] <- newdata[[focal_cov]] * focal_sd + focal_mean
  plot_cov <- focal_cov_unstd
}

# For any additional density covariates in the model, hold them constant across
# the plot. Continuous covariates are set to their mean and lower-cardinality
# covariates are set to their most common value.
other_dens_covs <- setdiff(dens_covs, focal_cov)

for (covname in other_dens_covs) {
  x <- covariates(extrap_mask)[, covname]
  x <- x[!is.na(x)]
  
  # Use the mean for continuous covariates
  if (is.numeric(x) && length(unique(x)) > 5) {
    newdata[[covname]] <- mean(x)
  } else {
    # Use the mode for categorical or low-cardinality covariates
    x_unique <- unique(x)
    x_mode <- x_unique[which.max(tabulate(match(x, x_unique)))]
    newdata[[covname]] <- x_mode
  }
}

# Predicted densities at different levels of focal covariate.
# Use a temporary mask so predictDsurface can evaluate the density model
# using the same type of object as in the fitted analysis.
pred_mask <- subset(extrap_mask, subset = seq_len(nrow(extrap_mask)) <= nrow(newdata))

# Replace the covariate values in the temporary mask with the values we want
# to predict over
for (covname in names(newdata)) {
  covariates(pred_mask)[, covname] <- newdata[[covname]]
}

# Predict density and convert to animals per 100 km2 for plotting
tmp <- predictDsurface(covplot_model, mask = pred_mask, cl.D = TRUE)
density_cols <- which(names(covariates(tmp)) == "D.0")
lcl_cols <- which(names(covariates(tmp)) == "lcl.0")
ucl_cols <- which(names(covariates(tmp)) == "ucl.0")

if (length(density_cols) < 1 || length(lcl_cols) < 1 || length(ucl_cols) < 1) {
  stop("Expected density prediction columns not found in tmp.")
}

newdata$est <- 10000 * covariates(tmp)[, density_cols[1]]
newdata$lcl <- 10000 * covariates(tmp)[, lcl_cols[1]]
newdata$ucl <- 10000 * covariates(tmp)[, ucl_cols[1]]

# Set an upper y-axis limit for the plot
ymax <- min(max(newdata$est * 2), max(newdata$ucl))

# Extract covariate values on mask and extrapolation region to see if you 
# are extrapolating into new covariate spaces
x_surveyed <- data.frame(xt = c(covariates(mask_model)[, plot_cov]))
x_full <- data.frame(xt = c(covariates(extrap_mask)[, plot_cov]))

# Plot fitted density against the focal covariate. Rugs at the top and bottom
# show where covariate values occur in the fitting mask and full extrapolation
# mask, which helps identify extrapolation beyond the sampled covariate range.
pD <- newdata %>%
  ggplot(aes(x = .data[[plot_cov]], y = est)) +
  geom_line(colour = "black") +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "grey70", colour = NA, alpha = 0.5) +
  geom_rug(data = x_full, inherit.aes = FALSE, aes(x = xt), sides = "b", alpha = 0.1, linewidth = 0.25) +
  geom_rug(data = x_surveyed, inherit.aes = FALSE, aes(x = xt), sides = "t", alpha = 0.1, linewidth = 0.25) +
  labs(x = focal_cov_full_name, y = bquote("Density / 100km"^2)) +
  theme_bw(base_size = 14) +
  coord_cartesian(ylim = c(0, ymax)) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Display the density covariate plot in the plotting window
pD

# Save the density covariate plot for later reference
ggsave(file.path(results_dir, "namkha_advanced_Dcovs_ms.png"), pD, width = 9, height = 3.5, dpi = 300)

###################################################
# 6. Plot effects of any covariates on detection
###################################################

# Identify which covariates appear in the encounter-rate part of the chosen model
lam0_covs <- sub("^lambda0\\.", "", model_covs[grepl("^lambda0\\.", model_covs)])
lam0_covs

# User sets possible values for any lambda0 covariates
# In same order as lam0_covs
# For ms4, c(1, 0) = cliff, c(0, 1) = valley_stream, c(0, 0) = other/baseline
# First baseline plot
lam0_covs_vals <- rep(0, length(lam0_covs))

# Stop if the user has not supplied one value per lambda0 covariate
if (length(lam0_covs_vals) != length(lam0_covs)) {
  stop("lam0_covs_vals must be the same length as lam0_covs.")
}

# Calculate the encounter-rate function for the chosen
# model and the user-specified lambda0 covariate values
ecd <- plot_detfunction(
  model = covplot_model,
  nd = 100,
  nbins = 25,
  lam0_covs_vals = lam0_covs_vals
)

# Plot fitted encounter-rate curve and its confidence interval
ec1 <- ggplot() +
  geom_col(
    data = ecd$hist,
    aes(x = xmid / 1000, y = height),
    width = ecd$hist$width[1] / 1000,
    fill = "white",
    colour = "grey40"
  ) +
  geom_line(data = ecd$detfun, aes(x = d / 1000, y = e.est), linewidth = 1.1) +
  geom_ribbon(data = ecd$detfun, aes(x = d / 1000, ymin = e.lcl, ymax = e.ucl), alpha = 0.2) +
  xlab("Distance (km)") +
  ylab("Encounters per day") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Display the plot in the plotting window
ec1

# Save the detection covariate plot for later reference
ggsave(file.path(results_dir, "namkha_advanced_Ecovs_ms_baseline.png"), ec1, width = 9, height = 4.5, dpi = 300)

# Now cliff
lam0_covs_vals <- c(1, 0)

# Stop if the user has not supplied one value per lambda0 covariate
if (length(lam0_covs_vals) != length(lam0_covs)) {
  stop("lam0_covs_vals must be the same length as lam0_covs.")
}

# Calculate the encounter-rate function for the chosen
# model and the user-specified lambda0 covariate values
ecd <- plot_detfunction(
  model = covplot_model,
  nd = 100,
  nbins = 25,
  lam0_covs_vals = lam0_covs_vals
)

# Plot fitted encounter-rate curve and its confidence interval
ec1 = ggplot() +
  geom_line(data = ecd$detfun, aes(x = d / 1000, y = e.est), linewidth = 1.1) +
  geom_ribbon(data = ecd$detfun, aes(x = d / 1000, ymin = e.lcl, ymax = e.ucl), alpha = 0.2) +
  xlab("Distance (km)") +
  ylab("Encounters per day") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Display the plot in the plotting window
ec1

# Save the detection covariate plot for later reference
ggsave(file.path(results_dir, "namkha_advanced_Ecovs_ms_cliff.png"), ec1, width = 9, height = 4.5, dpi = 300)

# Now valley/stream
lam0_covs_vals <- c(0, 1)

# Stop if the user has not supplied one value per lambda0 covariate
if (length(lam0_covs_vals) != length(lam0_covs)) {
  stop("lam0_covs_vals must be the same length as lam0_covs.")
}

# Calculate the encounter-rate function for the chosen
# model and the user-specified lambda0 covariate values
ecd <- plot_detfunction(
  model = covplot_model,
  nd = 100,
  nbins = 25,
  lam0_covs_vals = lam0_covs_vals
)

# Plot fitted encounter-rate curve and its confidence interval
ec1 = ggplot() +
  geom_line(data = ecd$detfun, aes(x = d / 1000, y = e.est), linewidth = 1.1) +
  geom_ribbon(data = ecd$detfun, aes(x = d / 1000, ymin = e.lcl, ymax = e.ucl), alpha = 0.2) +
  xlab("Distance (km)") +
  ylab("Encounters per day") +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "white")
  )

# Display the plot in the plotting window
ec1

# Save the detection covariate plot for later reference
ggsave(file.path(results_dir, "namkha_advanced_Ecovs_ms_valley_stream.png"), ec1, width = 9, height = 4.5, dpi = 300)
