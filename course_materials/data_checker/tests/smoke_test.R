suppressPackageStartupMessages({
  source(file.path("R", "helpers.R"), local = TRUE)
})

traps_raw <- read_input_csv(file.path("data", "traps_raw.csv"))
det_raw <- read_input_csv(file.path("data", "detections_raw.csv"))
traps_bad <- read_input_csv(file.path("data", "traps_bad.csv"))
det_bad <- read_input_csv(file.path("data", "detections_bad.csv"))

traps_map <- suggest_mapping(traps_raw, "traps")
det_map <- suggest_mapping(det_raw, "detections")

stopifnot(traps_map$required_ok, det_map$required_ok)

traps_results <- validate_traps(apply_mapping(traps_raw, traps_map$mapping), "EPSG:32644")
det_results <- validate_detections(apply_mapping(det_raw, det_map$mapping), traps_results)

stopifnot(nrow(traps_results$statuses) >= 1)
stopifnot(nrow(det_results$statuses) >= 1)

bad_traps_results <- validate_traps(traps_bad, "EPSG:32644")
bad_det_results <- validate_detections(det_bad, bad_traps_results)

stopifnot(any(bad_traps_results$statuses$level %in% c("error", "warning")))
stopifnot(any(bad_det_results$statuses$level %in% c("error", "warning")))

cat("Smoke test passed\n")
