# Script # 1 / 5
# ###################################################
# The purpose of this script is to create the input objects for secr.fit.
# This consists of a "capthist" object containing capture histories and trap locations.
# This script reads the Namkha camera and detection data from csv files and
# creates a single-session secr "capthist" object.
# Script to run before this one: none.
# Script to run after this one: 02_make_masks.R
# ###################################################

# Packages needed 
library(dplyr)
library(secr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(here)

# Coordinate reference system used later in the workflow
my_crs <- "+proj=utm +zone=44 +datum=WGS84 +units=m +no_defs"

# File paths used later in the workflow
traps_file <- here("namkha_basic", "data", "traps.csv")
detections_file <- here("namkha_basic", "data", "detections.csv")
output_file <- here("namkha_basic", "output", "namkha_basic_secr_inputs_capthist.RData")
fig_dir <- here("namkha_basic", "output", "fig")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

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

# Create covariates based on existing ones if you want
traps_df <- traps_df |> 
  mutate(
    cliff = if_else(str_detect(str_to_lower(Topography), "cliff"), 1, 0),
    hill = if_else(str_detect(str_to_lower(Topography), "hill"), 1, 0),
    valley_stream = if_else(str_detect(str_to_lower(Topography), "valley|stream"), 1, 0),
  ) %>%
  as.data.frame()

# Create the basic secr traps object using effort as usage
traps <- read.traps(
  data = traps_df |> dplyr::select(trapID, x, y, effort),
  detector = "count",
  trapID = "trapID",
  binary.usage = FALSE
)

# Add trap covariates
covariates(traps)$Topography <- traps_df$Topography
covariates(traps)$cliff <- traps_df$cliff
covariates(traps)$hill <- traps_df$hill
covariates(traps)$valley_stream <- traps_df$valley_stream
covariates(traps)$Water <- traps_df$Water
covariates(traps)$Altitude <- traps_df$Altitude

# Check
names(covariates(traps))

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
  filter(date >= survey_start, date <= survey_end) 

# make.capthist needs the standard secr columns only
# remove sex below if unavailable
capts <- capts_raw %>%
  dplyr::select(session, animalID, occasion, trapID, sex) %>%
  as.data.frame()

###################################################
# 3. Create and save the secr capthist object
###################################################

ch <- make.capthist(captures = capts, traps = traps)
verify(ch)
summary(ch)
summary(ch, terse = TRUE)

# Save key inputs for later scripts
save(
  cameras, traps_df, traps, capts_raw, capts, ch,
  survey_start, survey_end, my_crs,
  file = output_file
)

###################################################
# 4. Make descriptive plots of capture histories
###################################################

# Midpoint of the survey, shown as a visual reference on plots
mid_survey <- survey_start + floor(as.numeric(survey_end - survey_start) / 2)

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
  geom_vline(xintercept = mid_survey, colour = "grey50", linetype = 2) +
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
