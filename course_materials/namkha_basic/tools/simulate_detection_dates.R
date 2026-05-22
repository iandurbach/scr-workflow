# Simulates detection data for Namkha

library(secr)

# Load traps file (do not change this)
data_dir <- "namkha_basic/data"
traps_raw_file <- file.path(data_dir, "traps_raw.csv")

# Output detections file. This script only overwrites detections_raw.csv.
detections_raw_file <- file.path(data_dir, "detections_raw.csv")

set.seed(444)

# Simulate plausible detections from a given model
mod = readRDS("../namkha_model_for_workshop.Rds")
simdet = simulate(mod, nsim = 1)

# Compare actual and simulate capture histories
summary(mod$capthist, terse = TRUE)
summary(simdet[[1]], terse = TRUE)

# Simulate plausible detection dates 
min_gap_days <- 2

standardise_camera_id <- function(x) {
  gsub("\\s+", "", x)
}

parse_dmy <- function(x) {
  as.Date(x, format = "%d/%m/%Y")
}

sample_spaced_dates <- function(start_date, end_date, n_dates, min_gap_days) {
  candidate_dates <- seq.Date(start_date, end_date, by = "day")
  max_possible <- floor((length(candidate_dates) - 1) / min_gap_days) + 1

  if (n_dates > max_possible) {
    stop(
      "Cannot place ", n_dates, " detections between ", start_date, " and ",
      end_date, " with a minimum gap of ", min_gap_days, " days.",
      call. = FALSE
    )
  }

  for (attempt in seq_len(1000)) {
    selected_dates <- as.Date(character())

    for (i in seq_len(n_dates)) {
      available_dates <- candidate_dates[
        !vapply(
          candidate_dates,
          function(candidate) any(abs(as.integer(candidate - selected_dates)) < min_gap_days),
          logical(1)
        )
      ]

      if (length(available_dates) == 0) {
        break
      }

      selected_dates <- c(selected_dates, sample(available_dates, 1))
    }

    if (length(selected_dates) == n_dates) {
      return(sort(selected_dates))
    }
  }

  stop("Failed to sample spaced dates after 1000 attempts.", call. = FALSE)
}

make_raw_detections <- function(simulated_capthist) {
  simulated_detections <- as.data.frame(simulated_capthist)
  simulated_detections <- simulated_detections[simulated_detections$Occasion == 1, ]

  animal_ids <- sort(unique(simulated_detections$ID))
  animal_sex <- data.frame(
    ID = animal_ids,
    Sex = sample(
      c("Male", "Female", "Unknown"),
      size = length(animal_ids),
      replace = TRUE,
      prob = c(0.4, 0.4, 0.2)
    )
  )

  simulated_detections <- merge(simulated_detections, animal_sex, by = "ID", all.x = TRUE)

  data.frame(
    Session = 1,
    `Snow Leopard ID` = paste0("SL", simulated_detections$ID),
    Occasion = 1,
    `Date of detection (MDY)` = NA_character_,
    `Camera ID` = simulated_detections$TrapID,
    Sex = simulated_detections$Sex,
    check.names = FALSE
  )
}

traps_raw <- read.csv(traps_raw_file, stringsAsFactors = FALSE, check.names = FALSE)
detections_raw <- make_raw_detections(simdet[[1]])

traps_raw$trap_key <- standardise_camera_id(traps_raw[["Camera ID"]])
traps_raw$install_date <- parse_dmy(traps_raw[["Installation Date"]])
traps_raw$last_op_date <- parse_dmy(traps_raw[["Last Operational Date"]])

trap_dates <- traps_raw[
  !is.na(traps_raw$install_date) & !is.na(traps_raw$last_op_date),
  c("trap_key", "install_date", "last_op_date")
]

detections_raw$trap_key <- standardise_camera_id(detections_raw[["Camera ID"]])

missing_traps <- setdiff(unique(detections_raw$trap_key), trap_dates$trap_key)
if (length(missing_traps) > 0) {
  stop(
    "These detection cameras do not have valid operational dates in traps_raw.csv: ",
    paste(missing_traps, collapse = ", "),
    call. = FALSE
  )
}

detections_raw[["Date of detection (MDY)"]] <- NA_character_

groups <- unique(detections_raw[c("Snow Leopard ID", "trap_key")])

for (i in seq_len(nrow(groups))) {
  group_rows <- which(
    detections_raw[["Snow Leopard ID"]] == groups[["Snow Leopard ID"]][i] &
      detections_raw$trap_key == groups$trap_key[i]
  )

  trap_row <- trap_dates[trap_dates$trap_key == groups$trap_key[i], ]

  if (nrow(trap_row) != 1) {
    stop("Expected one trap row for camera ", groups$trap_key[i], ".", call. = FALSE)
  }

  sampled_dates <- sample_spaced_dates(
    start_date = trap_row$install_date,
    end_date = trap_row$last_op_date,
    n_dates = length(group_rows),
    min_gap_days = min_gap_days
  )

  detections_raw[["Date of detection (MDY)"]][group_rows] <- format(sampled_dates, "%m/%d/%Y")
}

detections_raw <- detections_raw[order(detections_raw[["Snow Leopard ID"]], detections_raw[["Camera ID"]]), ]
detections_raw$trap_key <- NULL

write.csv(detections_raw, detections_raw_file, row.names = FALSE, quote = FALSE)

message("Updated ", detections_raw_file)
