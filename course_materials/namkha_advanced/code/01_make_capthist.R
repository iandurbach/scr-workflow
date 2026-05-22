# Script # 1 / 5
# ###################################################
# The purpose of this script is to create the input objects for secr.fit.
# This consists of "capthist" objects containing capture histories and trap locations.
# This script reads the Namkha camera and detection data from csv files and
# creates three versions of the secr inputs for the advanced tutorial:
# full survey, shortened survey, and multisession.
# Script to run before this one: none.
# Script to run after this one: 02_make_masks.R
# ###################################################

# Packages needed 
library(dplyr)
library(secr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(here)

# Coordinate reference system used later in the workflow
my_crs <- "+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs"

# File paths used later in the workflow
traps_file <- here("namkha_basic", "data", "traps.csv")
detections_file <- here("namkha_basic", "data", "detections.csv")
output_file <- here("namkha_advanced", "output", "namkha_advanced_secr_inputs_capthist.RData")
fig_dir <- here("namkha_advanced", "output", "fig")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# User settings for the advanced tutorial.
# These control the full-survey occasions used for closure diagnostics and the
# shorter survey window used as one alternative to the full long survey.
full_occ_length <- 14
short_survey_start <- as.Date("2022-12-05")
short_survey_end <- short_survey_start + months(4) + days(5)

###################################################
# 1. Read and prepare camera trap data
###################################################

# Read trap data
cameras <- read.csv(traps_file, stringsAsFactors = FALSE, check.names = FALSE) 

# Removing cameras that were lost or malfunctioned
cameras <- cameras %>%
  filter(Note == "Functional") 

# Convert trap dates to Date format
cameras <- cameras %>%
  mutate(
    start_date = as.Date(start_date, format = "%d/%m/%Y"),
    end_date = as.Date(end_date, format = "%d/%m/%Y")
  ) 

# Define the overall survey period from the active cameras
survey_start <- min(cameras$start_date, na.rm = TRUE)
survey_end <- max(cameras$end_date, na.rm = TRUE)
mid_survey <- survey_start + floor(as.numeric(survey_end - survey_start) / 2)
ms_session_end <- as.Date(mid_survey)

# Make a data frame with all the traps information
# This gets used to create the secr traps object 
# but is also useful to have outside the secr package environment

## Essential traps columns
traps_df <- cameras %>%
  select(
    # essential columns
    session, trapID, x, y, effort,
    # additional covariates
    Topography, Water, Altitude
  )

# Create simple binary trap covariates from the descriptive camera metadata.
# These can be used later as detector covariates in secr models.
traps_df <- traps_df |> 
  mutate(
    cliff = if_else(str_detect(str_to_lower(Topography), "cliff"), 1, 0),
    hill = if_else(str_detect(str_to_lower(Topography), "hill"), 1, 0),
    valley_stream = if_else(str_detect(str_to_lower(Topography), "valley|stream"), 1, 0)
  ) %>%
  as.data.frame()

# Create the secr traps object for the full survey.
# ADVANCED ISSUE: the full survey is longer than a period over which closure can
# reasonably be assumed, so here we will create multiple occasions and calculate usage
# by occasion rather than collapsing everything into a single occasion and using
# total effort.
# We do this only so that we can perform closure tests - it doesn't affect other
# secr results because for count detectors only total counts (and effort) matters

# Do not set binary.usage here because we add the occasion-specific usage matrix
# manually below.
full_traps <- read.traps(
  data = traps_df |> dplyr::select(trapID, x, y),
  detector = "count",
  trapID = "trapID"
)

# To calculate usage we need two things:
# (1) the time interval when each camera was operating
# (2) the time interval covered by each occasion
# We can then work out, for each camera and each occasion, how many days of that
# occasion the camera was actually on.

# Define occasion boundaries across the full survey period.
# Each occasion here is a 14-day interval (set by full_occ_length), except 
# possibly the last one.
full_occ_start <- seq.Date(from = survey_start, to = survey_end, by = paste(full_occ_length, "days"))
full_occ_start <- ymd_hms(paste(full_occ_start, "00:00:00"))
full_occ_end <- c(na.omit(dplyr::lead(full_occ_start - minutes(1))), ymd_hms(paste(survey_end, "23:59:59")))
full_occ_bins <- interval(start = full_occ_start, end = full_occ_end)

# Store the dates each camera was operating as an interval
cameras <- cameras %>%
  mutate(
    camera_on = interval(start = start_date, end = end_date)
  )

# Function to calculate the overlap between one interval and one or more other
# intervals, and return the result as a duration.
# Here int1 will be the operating period for one camera, and int2 will be the
# set of occasion intervals.
# If a camera was not operating in a particular occasion, the overlap is set to 0.
int_overlaps_numeric <- function(int1, int2) {
  x <- lubridate::intersect(int1, int2)@.Data
  x[is.na(x)] <- 0
  as.duration(x)
}

# Calculate camera usage in each occasion.
# Each row of the usage matrix is one camera and each column is one occasion.
# The value we store is the number of days for which that camera was operating
# during that occasion.
usage_full <- matrix(0, nrow = nrow(cameras), ncol = length(full_occ_bins))
for (i in seq_len(nrow(cameras))) {
  usage_full[i, ] <- round(
    as.numeric(int_overlaps_numeric(cameras$camera_on[i], full_occ_bins)) / (24 * 60 * 60)
  )
}

# Add the usage matrix to the secr traps object
usage(full_traps) <- usage_full

# Add trap covariates to the full-survey traps object.
covariates(full_traps)$Topography <- traps_df$Topography
covariates(full_traps)$cliff <- traps_df$cliff
covariates(full_traps)$hill <- traps_df$hill
covariates(full_traps)$valley_stream <- traps_df$valley_stream
covariates(full_traps)$Water <- traps_df$Water
covariates(full_traps)$Altitude <- traps_df$Altitude

# Check
names(covariates(full_traps))

###################################################
# 2. Read and prepare detection data
###################################################

# Read detections, standardise IDs, and keep only detections within the survey period
capts_raw <- read.csv(detections_file, stringsAsFactors = FALSE, check.names = FALSE) 

# Convert detection dates to Date format
capts_raw <- capts_raw |> 
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
  )

# Drop any detections falling outside the survey period defined above
if(nrow(capts_raw |> filter((date < survey_start) | (date > survey_end)))){
  print("Dropping detections outside survey period!")
} else { print("All detections within survey period")}
capts_raw <- capts_raw |> 
  filter(date >= survey_start, date <= survey_end) %>%
  arrange(date, animalID, trapID)

###################################################
# 3. Create and save the full-survey secr capthist object
###################################################

# Create an occasion indicator matching the 14-day full-survey intervals above.
capts_full <- capts_raw %>%
  mutate(
    occasion = floor(as.numeric(date - survey_start) / full_occ_length) + 1
  ) 

# make.capthist needs the standard secr columns only.
# Keep sex because the advanced tutorial later introduces sex-specific models.
capts_full <- capts_full %>%
  dplyr::select(session, animalID, occasion, trapID, sex) %>%
  as.data.frame()

full_ch <- make.capthist(captures = capts_full, traps = full_traps)
verify(full_ch)
summary(full_ch)
summary(full_ch, terse = TRUE)

###################################################
# 4. Create and save the shorter-survey secr capthist object
###################################################

# ADVANCED ISSUE: one way to deal with the long survey is to shorten it to a
# period where closure is more plausible.

# Keep only detections within the shortened survey period
short_capts_raw <- capts_raw %>%
  filter(date >= short_survey_start, date <= short_survey_end)

# Adjust trap effort to the shortened survey period.
# For each camera, the new effort is the overlap between:
# (1) the dates when that camera was operating, and
# (2) the dates of the shortened survey period,
# with a check that it can't be negative (this means zero overlap)
short_traps_df <- cameras %>%
  mutate(
    effort_short = pmax(
      0,
      as.numeric(pmin(end_date, short_survey_end) - pmax(start_date, short_survey_start), units = "days")
    )
  ) 

# Keep the core trap variables we need for the shortened analysis.
# This is the same information used in the basic tutorial, except that effort
# has now been replaced by effort_short.
short_traps_df <- short_traps_df |> 
  dplyr::select(
    # essential columns
    trapID, x, y, effort_short,
    # additional covariates
    Topography, Water, Altitude
  ) 

# Add back the trap covariates created earlier
# These are unchanged by shortening the survey, so we can join them on trapID.
short_traps_df <- short_traps_df |> 
  left_join(
    traps_df %>% dplyr::select(trapID, cliff, hill, valley_stream),
    by = "trapID"
  ) %>%
  as.data.frame()

# Create the secr traps object for the shortened survey, using effort_short as
# the count-detector usage value.
short_traps <- read.traps(
  data = short_traps_df |> dplyr::select(trapID, x, y, effort = effort_short),
  detector = "count",
  trapID = "trapID",
  binary.usage = FALSE
)

# Add trap covariates to the shortened-survey traps object.
covariates(short_traps)$Topography <- short_traps_df$Topography
covariates(short_traps)$cliff <- short_traps_df$cliff
covariates(short_traps)$hill <- short_traps_df$hill
covariates(short_traps)$valley_stream <- short_traps_df$valley_stream
covariates(short_traps)$Water <- short_traps_df$Water
covariates(short_traps)$Altitude <- short_traps_df$Altitude

# make.capthist needs the standard secr columns only.
# The shortened survey is treated as a single occasion.
short_capts <- short_capts_raw %>%
  mutate(occasion = 1L) %>%
  dplyr::select(session, animalID, occasion, trapID, sex) %>%
  as.data.frame()

short_ch <- make.capthist(captures = short_capts, traps = short_traps)
verify(short_ch)
summary(short_ch)
summary(short_ch, terse = TRUE)

###################################################
# 5. Create and save the multisession secr capthist object
###################################################

# ADVANCED ISSUE: another way to deal with the long survey is to split it into
# temporal sessions and run a multisession analysis.

# Divide detections into two sessions based on date
ms_capts <- capts_raw %>%
  mutate(
    session = if_else(date < ms_session_end, 1L, 2L),
    occasion = 1L
  ) %>%
  dplyr::select(session, animalID, occasion, trapID, sex) %>%
  as.data.frame()

# Calculate trap effort separately in each session so that each multisession
# traps object has the correct count-detector usage.
ms_traps_df <- cameras %>%
  mutate(
    effort_s1 = pmax(
      0,
      as.numeric(pmin(end_date, ms_session_end - days(1)) - pmax(start_date, survey_start), units = "days")
    ),
    effort_s2 = pmax(
      0,
      as.numeric(pmin(end_date, survey_end) - pmax(start_date, ms_session_end), units = "days")
    )
  ) 

# Keep the core trap variables we need for the multisession analysis.
# This is the same information used in the basic tutorial, except that effort
# is now represented separately for sessions 1 and 2.
ms_traps_df <- ms_traps_df |> 
  dplyr::select(
    # essential columns
    trapID, x, y, effort_s1, effort_s2,
    # additional covariates
    Topography, Water, Altitude
  ) 

# Add back the trap covariates created earlier
# These are unchanged by splitting into sessions so we can join them on trapID.
ms_traps_df <- ms_traps_df |> 
  left_join(
    traps_df %>% dplyr::select(trapID, cliff, hill, valley_stream),
    by = "trapID"
  )

# Reshape wide to long and add a session indicator.
# Each row is now a trap-session combination.
ms_traps_df <- ms_traps_df |> 
  pivot_longer(
    cols = c(effort_s1, effort_s2),
    names_to = "session",
    values_to = "effort"
  ) %>%
  mutate(
    session = if_else(session == "effort_s1", 1, 2)
  ) %>%
  as.data.frame()

# Create a list of secr traps objects, one for each session
ms_traps_list <- split(ms_traps_df, f = ms_traps_df$session)
traps_ms <- vector("list", length(ms_traps_list))
for (i in seq_along(ms_traps_list)) {
  this_traps_df <- ms_traps_list[[i]]
  traps_ms[[i]] <- read.traps(
    data = this_traps_df %>% dplyr::select(trapID, x, y, effort),
    detector = "count",
    trapID = "trapID",
    binary.usage = FALSE
  )
  
  # Add trap covariates to this session's traps object.
  covariates(traps_ms[[i]])$Topography <- this_traps_df$Topography
  covariates(traps_ms[[i]])$cliff <- this_traps_df$cliff
  covariates(traps_ms[[i]])$hill <- this_traps_df$hill
  covariates(traps_ms[[i]])$valley_stream <- this_traps_df$valley_stream
  covariates(traps_ms[[i]])$Water <- this_traps_df$Water
  covariates(traps_ms[[i]])$Altitude <- this_traps_df$Altitude
}
names(traps_ms) <- c("1", "2")

ch_ms <- make.capthist(captures = ms_capts, traps = traps_ms)
# make.capthist may warn about factor covariates because sex, and possibly trap
# covariates, have to be represented consistently across sessions.
verify(ch_ms)
# Force any factor covariates to use the same levels in all sessions.
ch_ms <- shareFactorLevels(ch_ms, stringsAsFactors = FALSE)
# Check again after harmonising factor levels.
verify(ch_ms)
summary(ch_ms)
summary(ch_ms, terse = TRUE)

###################################################
# 6. Make descriptive plots of capture histories
###################################################

# Midpoint of the survey, shown as a visual reference on plots
# In the advanced tutorial we also show the shortened survey window and the
# split between multisession periods.

# Summarise detections by day for a simple capture-history diagnostic plot
daily_detections <- capts_raw %>%
  mutate(
    ndets = 1,
    first_appearance = !duplicated(animalID)
  ) %>%
  complete(date = seq.Date(survey_start, survey_end, by = "day")) %>%
  mutate(
    ndets = replace_na(ndets, 0),
    first_appearance = replace_na(first_appearance, FALSE)
  ) %>%
  arrange(date) %>%
  mutate(
    cumdets = cumsum(ndets),
    cumn = cumsum(first_appearance)
  ) %>%
  dplyr::select(date, ndets, cumdets, cumn) %>%
  pivot_longer(cols = c(cumn, cumdets), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure, cumn = "New animals", cumdets = "Detections"),
    measure = factor(measure, levels = c("New animals", "Detections"))
  )

# Plot cumulative detections and cumulative number of animals over time
capture_history_plot <- ggplot(daily_detections, aes(x = date, y = value, colour = measure)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = mid_survey, colour = "grey75", linetype = 2) +
  geom_vline(xintercept = short_survey_start, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = short_survey_end, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = ms_session_end, colour = "steelblue4", linetype = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual(values = c("New animals" = "firebrick", "Detections" = "black")) +
  labs(x = NULL, y = "Cumulative encounters", colour = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

# Order cameras by installation date for the trap operation plot
cameras_for_plot <- cameras %>%
  mutate(plot_order = rank(start_date, ties.method = "first")) %>%
  arrange(plot_order)

# Mark the first detection of each animal differently from later recaptures
detections_for_plot <- capts_raw %>%
  mutate(
    detection_type = if_else(!duplicated(animalID), "New animal", "Recapture")
  ) %>%
  left_join(
    cameras_for_plot %>% dplyr::select(trapID = trapID, plot_order),
    by = "trapID"
  )

# Plot when each camera was operating and when detections occurred at that camera
camera_operation_plot <- ggplot(
  cameras_for_plot,
  aes(
    xmin = pmax(start_date, survey_start),
    xmax = pmin(end_date, survey_end),
    y = plot_order
  )
) +
  geom_linerange(linewidth = 0.5) +
  geom_point(
    data = detections_for_plot,
    aes(x = date, y = plot_order, colour = detection_type),
    inherit.aes = FALSE,
    size = 1.8,
    alpha = 0.9
  ) +
  geom_vline(xintercept = short_survey_start, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = short_survey_end, colour = "grey50", linetype = 2) +
  geom_vline(xintercept = ms_session_end, colour = "steelblue4", linetype = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual(values = c("New animal" = "firebrick", "Recapture" = "black")) +
  labs(x = NULL, y = "Camera", colour = NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top"
  )

# Combine the two diagnostic plots into one summary figure
combined_plot <- camera_operation_plot / capture_history_plot + plot_layout(heights = c(3, 1))

# Save the plots for checking and later tutorial use
ggsave(
  filename = file.path(fig_dir, "namkha_capture_history_over_time.png"),
  plot = capture_history_plot,
  width = 8,
  height = 3.5,
  dpi = 200
)

ggsave(
  filename = file.path(fig_dir, "namkha_camera_operation_dates.png"),
  plot = camera_operation_plot,
  width = 8,
  height = 5.5,
  dpi = 200
)

ggsave(
  filename = file.path(fig_dir, "namkha_capture_history_summary.png"),
  plot = combined_plot,
  width = 8,
  height = 7,
  dpi = 200
)

###################################################
# 7. Save key inputs for later scripts
###################################################

# Save key inputs for later scripts
save(
  cameras, traps_df, capts_raw,
  survey_start, survey_end, short_survey_start, short_survey_end, ms_session_end,
  full_occ_length, my_crs,
  full_traps, capts_full, full_ch,
  short_traps_df, short_traps, short_capts_raw, short_capts, short_ch,
  ms_traps_df, traps_ms, ms_capts, ch_ms,
  file = output_file
)
