library(dplyr)
library(ggplot2)
library(htmltools)
library(leaflet)
library(readr)
library(scales)
library(sf)
library(stringr)
library(tidyr)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

required_columns <- list(
  traps = c("session", "trapID", "effort"),
  detections = c("session", "animalID", "occasion", "trapID")
)

optional_groups <- list(
  traps = list(coord_pairs = list(c("x", "y"), c("lat", "long"), c("latitude", "longitude"))),
  detections = list(optional = c("date"))
)

column_synonyms <- list(
  session = c("session", "survey", "survey session", "year", "period"),
  trapID = c("trapid", "trap id", "cameraid", "camera id", "station", "station id", "detector", "detector id", "camid"),
  x = c("x", "utm easting", "easting", "east", "xcoord", "x coordinate"),
  y = c("y", "utm northing", "northing", "north", "ycoord", "y coordinate"),
  lat = c("lat", "latitude", "gps latitude"),
  long = c("long", "longitude", "lon", "lng", "gps longitude"),
  effort = c("effort", "effort days", "effort day", "days", "trap nights"),
  start_date = c("start date", "installation date", "deploy date", "camera start", "start"),
  end_date = c("end date", "last operational date", "retrieval date", "camera end", "end"),
  animalID = c("animalid", "animal id", "snow leopard id", "individual", "individual id", "id"),
  occasion = c("occasion", "sampling occasion", "occasion id"),
  date = c("date", "detection date", "date of detection", "date of detection mdy")
)

normalize_name <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("\\s+", " ")
}

read_input_csv <- function(path) {
  readr::read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
}

score_name_match <- function(raw_name, target_name) {
  raw_norm <- normalize_name(raw_name)
  target_norm <- normalize_name(target_name)
  synonyms_raw <- if (target_name %in% names(column_synonyms)) column_synonyms[[target_name]] else target_name
  synonyms <- normalize_name(synonyms_raw)
  exact_syn <- any(raw_norm == synonyms)
  contains_syn <- any(str_detect(raw_norm, fixed(target_norm))) || any(str_detect(target_norm, fixed(raw_norm)))
  dist <- min(adist(raw_norm, c(target_norm, synonyms)))
  score <- 0
  if (exact_syn) score <- score + 100
  if (contains_syn) score <- score + 20
  score - dist
}

suggest_mapping <- function(df, dataset_type) {
  raw_names <- names(df)
  mapping <- setNames(rep(NA_character_, length(raw_names)), raw_names)

  canonical_targets <- if (dataset_type == "traps") {
    c("session", "trapID", "x", "y", "lat", "long", "effort", "start_date", "end_date")
  } else {
    c("session", "animalID", "occasion", "trapID", "date")
  }

  target_scores <- lapply(canonical_targets, function(target) {
    scores <- vapply(raw_names, function(raw_name) score_name_match(raw_name, target), numeric(1))
    tibble(
      raw = raw_names,
      target = target,
      score = scores
    ) |> arrange(desc(score))
  })
  names(target_scores) <- canonical_targets

  chosen_raw <- character()
  for (target in canonical_targets) {
    candidate <- target_scores[[target]] |>
      filter(!(raw %in% chosen_raw)) |>
      slice(1)
    if (nrow(candidate) == 1 && candidate$score > 5) {
      mapping[[candidate$raw]] <- target
      chosen_raw <- c(chosen_raw, candidate$raw)
    }
  }

  mapping_tbl <- tibble(
    original = raw_names,
    proposed = unname(mapping),
    exact = normalize_name(raw_names) == normalize_name(unname(mapping))
  )

  if (!"session" %in% mapping_tbl$proposed) {
    mapping_tbl <- bind_rows(
      mapping_tbl,
      tibble(original = "<<create session=1>>", proposed = "session", exact = FALSE)
    )
  }

  required_ok <- all(required_columns[[dataset_type]] %in% mapping_tbl$proposed)
  coord_ok <- TRUE
  if (dataset_type == "traps") {
    coord_ok <- any(vapply(optional_groups$traps$coord_pairs, function(pair) all(pair %in% mapping_tbl$proposed), logical(1)))
  }

  list(
    mapping = mapping_tbl,
    exact_match = all(mapping_tbl$exact[!is.na(mapping_tbl$proposed)]),
    required_ok = required_ok && coord_ok
  )
}

apply_mapping <- function(df, mapping_tbl) {
  synthetic_session <- any(mapping_tbl$original == "<<create session=1>>" & mapping_tbl$proposed == "session")
  rename_vec <- mapping_tbl$original[!is.na(mapping_tbl$proposed)]
  names(rename_vec) <- mapping_tbl$proposed[!is.na(mapping_tbl$proposed)]
  rename_vec <- rename_vec[rename_vec != "<<create session=1>>"]
  renamed <- dplyr::rename(df, !!!rename_vec)
  if (synthetic_session && !"session" %in% names(renamed)) {
    renamed <- renamed |> mutate(session = "1", .before = 1)
  }
  renamed
}

check_numeric_text <- function(values) {
  trimmed <- str_trim(values %||% character())
  idx <- trimmed != "" & !is.na(trimmed)
  non_empty <- trimmed[idx]
  comma_decimal <- str_detect(non_empty, "^[-+]?[0-9]+,[0-9]+$")
  whitespace <- non_empty != values[idx]
  parsed <- suppressWarnings(as.numeric(trimmed))
  invalid <- is.na(parsed[idx]) & non_empty != ""
  list(
    parsed = parsed,
    comma_decimal = any(comma_decimal),
    whitespace = any(whitespace),
    invalid = any(invalid)
  )
}

detect_possible_swaps <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4) {
    return(integer())
  }
  x_ok <- x[ok]
  y_ok <- y[ok]
  center <- c(stats::median(x_ok), stats::median(y_ok))
  scale_x <- stats::mad(x_ok, constant = 1) + 1e-6
  scale_y <- stats::mad(y_ok, constant = 1) + 1e-6
  original <- ((x_ok - center[1]) / scale_x)^2 + ((y_ok - center[2]) / scale_y)^2
  swapped <- ((y_ok - center[1]) / scale_x)^2 + ((x_ok - center[2]) / scale_y)^2
  local_idx <- which(original > stats::quantile(original, 0.95, na.rm = TRUE) & swapped < original / 20)
  which(ok)[local_idx]
}

date_formats <- c(
  "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d/%m/%y", "%m/%d/%y", "%y/%m/%d",
  "%Y%m%d", "%d%m%Y", "%m%d%Y", "%d%m%y", "%m%d%y", "%y%m%d"
)

parse_date_column <- function(values) {
  raw <- str_trim(values %||% character())
  raw[raw == ""] <- NA_character_
  norm <- str_replace_all(raw, "[-.]", "/")

  best_score <- -Inf
  best_format <- NA_character_
  best_dates <- rep(as.Date(NA), length(norm))

  for (fmt in date_formats) {
    candidate <- suppressWarnings(as.Date(norm, format = fmt))
    score <- sum(!is.na(candidate))
    if (score > best_score) {
      best_score <- score
      best_format <- fmt
      best_dates <- candidate
    }
  }

  invalid_examples <- norm[!is.na(norm) & is.na(best_dates)]
  invalid_hint <- if (length(invalid_examples) > 0) {
    bad <- invalid_examples[1]
    if (str_detect(bad, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$")) {
      "Some values look date-like but contain impossible days or months."
    } else {
      "Some values do not match the inferred date format."
    }
  } else {
    NULL
  }

  list(
    parsed = best_dates,
    format = best_format,
    success = sum(!is.na(best_dates)),
    total = sum(!is.na(norm)),
    invalid_examples = unique(invalid_examples)[1:min(5, length(unique(invalid_examples)))],
    invalid_hint = invalid_hint
  )
}

check_positive_integerish <- function(values) {
  trimmed <- str_trim(values)
  num <- suppressWarnings(as.numeric(trimmed))
  if (all(is.na(num))) {
    list(ok = !any(str_detect(trimmed[!is.na(trimmed)], "\\s")), type = "text")
  } else {
    list(ok = all(!is.na(num) & num > 0 & floor(num) == num), type = "numeric")
  }
}

check_no_spaces <- function(values) {
  any(str_detect(values, "\\s"))
}

has_disallowed_punctuation <- function(values, allow_underscore = TRUE) {
  pattern <- if (allow_underscore) "[^A-Za-z0-9_]" else "[^A-Za-z0-9]"
  any(str_detect(values, pattern))
}

five_number_tbl <- function(x) {
  tibble(
    min = min(x, na.rm = TRUE),
    q1 = stats::quantile(x, 0.25, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    q3 = stats::quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

covariate_summary_tbl <- function(df, core_cols) {
  covars <- setdiff(names(df), core_cols)
  if (length(covars) == 0) {
    return(tibble())
  }
  bind_rows(lapply(covars, function(col) {
    vals <- df[[col]]
    trimmed <- str_trim(vals)
    numeric_vals <- suppressWarnings(as.numeric(trimmed))
    if (sum(!is.na(numeric_vals)) == sum(trimmed != "" & !is.na(trimmed))) {
      tibble(
        covariate = col,
        unique_levels = dplyr::n_distinct(trimmed[trimmed != ""], na.rm = TRUE),
        min = min(numeric_vals, na.rm = TRUE),
        max = max(numeric_vals, na.rm = TRUE)
      )
    } else {
      tibble(
        covariate = col,
        unique_levels = dplyr::n_distinct(trimmed[trimmed != ""], na.rm = TRUE),
        min = NA_real_,
        max = NA_real_
      )
    }
  }))
}

status_row <- function(field, level, message) {
  tibble(field = field, level = level, message = message)
}

format_flag_values <- function(x, max_n = 8) {
  vals <- unique(x[!is.na(x) & x != ""])
  if (length(vals) == 0) {
    return("none")
  }
  shown <- vals[seq_len(min(length(vals), max_n))]
  out <- paste(shown, collapse = ", ")
  if (length(vals) > max_n) {
    out <- paste0(out, ", ...")
  }
  out
}

validate_traps <- function(df, crs_input = NULL) {
  statuses <- list()

  session_info <- check_positive_integerish(df$session)
  if (!session_info$ok || any(str_detect(df$session, "\\s"))) {
    statuses <- append(statuses, list(status_row("session", "error", "session must be a positive integer or text with no spaces.")))
  }

  trap_trim <- str_trim(df$trapID)
  dup_ids <- df |>
    mutate(trapID_trim = trap_trim) |>
    count(session, trapID_trim, name = "n") |>
    filter(n > 1)
  if (nrow(dup_ids) > 0) {
    dup_labels <- paste(dup_ids$session, dup_ids$trapID_trim, sep = ":")
    statuses <- append(statuses, list(status_row("trapID", "error", paste("trapID values must be unique within each session. Problem values:", format_flag_values(dup_labels)))))
  }
  bad_trap_ids <- trap_trim[str_detect(trap_trim, "\\s") | str_detect(trap_trim, "[^A-Za-z0-9_]")]
  if (length(bad_trap_ids) > 0) {
    statuses <- append(statuses, list(status_row("trapID", "warning", paste("trapID contains whitespace or punctuation that may not parse reliably in secr. Problem values:", format_flag_values(bad_trap_ids)))))
  }

  coord_pair <- if (all(c("x", "y") %in% names(df))) c("x", "y") else if (all(c("lat", "long") %in% names(df))) c("lat", "long") else if (all(c("latitude", "longitude") %in% names(df))) c("latitude", "longitude") else NULL
  coord_data <- NULL
  if (!is.null(coord_pair)) {
    x_check <- check_numeric_text(df[[coord_pair[1]]])
    y_check <- check_numeric_text(df[[coord_pair[2]]])
    if (x_check$comma_decimal || y_check$comma_decimal) {
      statuses <- append(statuses, list(status_row("coordinates", "error", "Coordinates appear to use commas as decimal separators.")))
    }
    if (x_check$whitespace || y_check$whitespace || x_check$invalid || y_check$invalid) {
      statuses <- append(statuses, list(status_row("coordinates", "error", "Coordinates must parse cleanly as numeric values without extra whitespace.")))
    }
    x <- x_check$parsed
    y <- y_check$parsed
    dup_pos <- duplicated(data.frame(x = x, y = y)) | duplicated(data.frame(x = x, y = y), fromLast = TRUE)
    dup_diff_id <- dup_pos & !duplicated(data.frame(x = x, y = y, trapID = trap_trim))
    if (any(dup_diff_id, na.rm = TRUE)) {
      statuses <- append(statuses, list(status_row("coordinates", "error", paste("Two or more different trap IDs share the same coordinates. Problem values:", format_flag_values(trap_trim[dup_diff_id])))))
    }
    swap_idx <- detect_possible_swaps(x, y)
    if (length(swap_idx) > 0) {
      msg <- paste("Possible swapped coordinates detected for:", paste(trap_trim[swap_idx], collapse = ", "))
      statuses <- append(statuses, list(status_row("coordinates", "warning", msg)))
    }
    coord_data <- tibble(x = x, y = y)
  } else {
    statuses <- append(statuses, list(status_row("coordinates", "error", "No valid coordinate pair was found. Provide x/y or lat/long.")))
  }

  effort_check <- check_numeric_text(df$effort)
  effort <- effort_check$parsed
  if (effort_check$comma_decimal || effort_check$whitespace || effort_check$invalid) {
    statuses <- append(statuses, list(status_row("effort", "error", "effort must parse cleanly as numeric values.")))
  }
  nonpositive_effort <- !is.na(effort) & effort <= 0
  if (any(nonpositive_effort)) {
    statuses <- append(statuses, list(status_row("effort", "error", paste("effort must be strictly positive; zero indicates a non-operational detector that should be fixed before secr. Problem traps:", format_flag_values(trap_trim[nonpositive_effort])))))
  }

  start_dates <- end_dates <- NULL
  if (all(c("start_date", "end_date") %in% names(df))) {
    start_parse <- parse_date_column(df$start_date)
    end_parse <- parse_date_column(df$end_date)
    start_dates <- start_parse$parsed
    end_dates <- end_parse$parsed
    if (start_parse$success < start_parse$total) {
      msg <- paste(c("start_date could not be parsed for all rows.", start_parse$invalid_hint, paste(start_parse$invalid_examples, collapse = ", ")), collapse = " ")
      statuses <- append(statuses, list(status_row("start_date", "error", msg)))
    }
    if (end_parse$success < end_parse$total) {
      msg <- paste(c("end_date could not be parsed for all rows.", end_parse$invalid_hint, paste(end_parse$invalid_examples, collapse = ", ")), collapse = " ")
      statuses <- append(statuses, list(status_row("end_date", "error", msg)))
    }
    if (all(!is.na(start_dates)) && all(!is.na(end_dates))) {
      span <- as.numeric(end_dates - start_dates)
      mismatch <- !is.na(effort) & !is.na(span) & abs(span - effort) > 1
      if (any(mismatch)) {
        statuses <- append(statuses, list(status_row("effort", "warning", paste("effort does not match end_date - start_date for some detectors. Problem traps:", format_flag_values(trap_trim[mismatch])))))
      }
    }
  }

  status_tbl <- bind_rows(statuses)
  if (nrow(status_tbl) == 0) {
    status_tbl <- status_row("traps", "pass", "No issues detected in the traps file checks that were run.")
  }

  session_counts <- df |>
    count(session, name = "n_detectors")

  effort_by_session <- tibble(session = df$session, effort = effort) |>
    filter(!is.na(effort))

  coord_plot_data <- if (!is.null(coord_data)) bind_cols(df |> select(session, trapID), coord_data) else NULL

  timeline_data <- NULL
  if (!is.null(start_dates) && !is.null(end_dates)) {
    timeline_data <- tibble(
      session = df$session,
      trapID = trap_trim,
      trap_session = paste(df$session, trap_trim, sep = "_"),
      start_date = start_dates,
      end_date = end_dates
    ) |>
      arrange(start_date)
  }

  list(
    data = df,
    statuses = status_tbl,
    session_summary = list(
      type = session_info$type,
      unique_values = if (session_info$type == "text") unique(df$session) else NULL
    ),
    trap_summary = list(
      trap_ids = sort(unique(trap_trim)),
      trap_table = tibble(
        trapID = sort(unique(trap_trim))
      ) |>
        mutate(issue = case_when(
          trapID %in% unique(dup_ids$trapID_trim) ~ "duplicate within session",
          trapID %in% unique(bad_trap_ids) ~ "whitespace or punctuation",
          TRUE ~ ""
        )),
      session_counts = session_counts
    ),
    coord_summary = list(
      coord_pair = coord_pair,
      coord_plot_data = coord_plot_data,
      crs_input = crs_input
    ),
    effort_summary = list(
      effort = effort,
      by_session = effort_by_session,
      five_number = if (sum(!is.na(effort)) > 0) effort_by_session |>
        group_by(session) |>
        group_modify(~ five_number_tbl(.x$effort)) |>
        ungroup() else tibble()
    ),
    date_summary = list(
      timeline_data = timeline_data
    ),
    covariate_summary = covariate_summary_tbl(df, c("session", "trapID", coord_pair, "effort", "start_date", "end_date"))
  )
}

normalize_animal_base <- function(x) {
  x |>
    str_trim() |>
    str_remove("\\s*\\(.*\\)$") |>
    str_replace_all("[^A-Za-z0-9_]", "") |>
    str_to_upper()
}

validate_detections <- function(df, traps_results = NULL) {
  statuses <- list()
  missing_traps <- rep(FALSE, nrow(df))
  zero_effort_traps <- rep(FALSE, nrow(df))

  session_info <- check_positive_integerish(df$session)
  if (!session_info$ok || any(str_detect(df$session, "\\s"))) {
    statuses <- append(statuses, list(status_row("session", "error", "session must be a positive integer or text with no spaces.")))
  }

  animal_trim <- str_trim(df$animalID)
  bad_animal_ids <- animal_trim[str_detect(animal_trim, "\\s") | str_detect(animal_trim, "[^A-Za-z0-9_]")]
  if (length(bad_animal_ids) > 0) {
    statuses <- append(statuses, list(status_row("animalID", "error", paste("animalID must not contain whitespace or punctuation. Problem values:", format_flag_values(bad_animal_ids)))))
  }
  unknown_ids <- animal_trim[str_to_lower(animal_trim) %in% c("unknown", "unk", "unsure", "na")]
  if (length(unknown_ids) > 0) {
    statuses <- append(statuses, list(status_row("animalID", "error", paste("animalID contains unknown-style placeholders that secr would treat as real individuals. Problem values:", format_flag_values(unknown_ids)))))
  }
  normalized_base <- normalize_animal_base(animal_trim)
  trailing_notes <- tibble(raw = animal_trim, base = normalized_base) |>
    distinct(raw, base) |>
    count(base, name = "n_variants") |>
    filter(base != "", n_variants > 1)
  variant_ids <- tibble(raw = animal_trim, base = normalized_base) |>
    distinct(raw, base) |>
    semi_join(trailing_notes, by = "base") |>
    pull(raw)
  if (length(variant_ids) > 0) {
    statuses <- append(statuses, list(status_row("animalID", "error", paste("Some animal IDs appear with trailing notes or alternate punctuated forms. Problem values:", format_flag_values(variant_ids)))))
  }

  occasion_check <- check_numeric_text(df$occasion)
  occasion <- occasion_check$parsed
  if (occasion_check$comma_decimal || occasion_check$whitespace || occasion_check$invalid || any(occasion <= 0 | floor(occasion) != occasion, na.rm = TRUE)) {
    statuses <- append(statuses, list(status_row("occasion", "error", "occasion must be a positive integer.")))
  }

  date_parse <- NULL
  if ("date" %in% names(df)) {
    date_parse <- parse_date_column(df$date)
    if (date_parse$success < date_parse$total) {
      msg <- paste(c("date could not be parsed for all rows.", date_parse$invalid_hint, paste(date_parse$invalid_examples, collapse = ", ")), collapse = " ")
      statuses <- append(statuses, list(status_row("date", "error", msg)))
    }
    parsed_dates <- date_parse$parsed
    occ_ranges <- tibble(occasion = occasion, date = parsed_dates) |>
      filter(!is.na(occasion), !is.na(date)) |>
      group_by(occasion) |>
      summarise(start = min(date), end = max(date), .groups = "drop") |>
      arrange(start)
    if (nrow(occ_ranges) > 1 && any(occ_ranges$start[-1] <= occ_ranges$end[-nrow(occ_ranges)])) {
      overlap_ids <- occ_ranges$occasion[-1][occ_ranges$start[-1] <= occ_ranges$end[-nrow(occ_ranges)]]
      statuses <- append(statuses, list(status_row("occasion", "warning", paste("Date ranges for different occasions overlap. Problem occasions:", format_flag_values(overlap_ids)))))
    }
  } else {
    parsed_dates <- NULL
  }

  trap_trim <- str_trim(df$trapID)
  if (!is.null(traps_results)) {
    traps_df <- traps_results$data
    traps_lookup <- traps_df |>
      mutate(trapID_trim = str_trim(trapID), effort_num = suppressWarnings(as.numeric(str_trim(effort)))) |>
      select(session, trapID_trim, effort_num)
    joined <- tibble(session = df$session, trapID_trim = trap_trim) |>
      left_join(traps_lookup, by = c("session", "trapID_trim"))
    missing_traps <- is.na(joined$effort_num)
    if (any(missing_traps)) {
      missing_labels <- paste(df$session[missing_traps], trap_trim[missing_traps], sep = ":")
      statuses <- append(statuses, list(status_row("trapID", "error", paste("Some detections reference a trapID/session combination not found in traps.csv. Problem values:", format_flag_values(missing_labels)))))
    }
    zero_effort_traps <- !is.na(joined$effort_num) & joined$effort_num <= 0
    if (any(zero_effort_traps)) {
      zero_labels <- paste(df$session[zero_effort_traps], trap_trim[zero_effort_traps], sep = ":")
      statuses <- append(statuses, list(status_row("trapID", "error", paste("Some detections occur at traps with effort <= 0. Problem values:", format_flag_values(zero_labels)))))
    }
    trap_sessions <- unique(traps_df$session)
    missing_sessions <- setdiff(unique(df$session), trap_sessions)
    if (length(missing_sessions) > 0) {
      statuses <- append(statuses, list(status_row("session", "error", paste("Detections contain sessions not present in traps.csv. Problem values:", format_flag_values(missing_sessions)))))
    }

    if ("date" %in% names(df) && all(c("start_date", "end_date") %in% names(traps_df))) {
      trap_dates <- tibble(
        session = traps_df$session,
        trapID_trim = str_trim(traps_df$trapID),
        start_date = parse_date_column(traps_df$start_date)$parsed,
        end_date = parse_date_column(traps_df$end_date)$parsed
      )
      det_dates <- tibble(session = df$session, trapID_trim = trap_trim, date = parsed_dates)
      date_joined <- det_dates |>
        left_join(trap_dates, by = c("session", "trapID_trim"))
      outside <- !is.na(date_joined$date) & !is.na(date_joined$start_date) & !is.na(date_joined$end_date) &
        (date_joined$date < date_joined$start_date | date_joined$date > date_joined$end_date)
      if (any(outside)) {
        outside_labels <- paste(df$session[outside], trap_trim[outside], as.character(parsed_dates[outside]), sep = ":")
        statuses <- append(statuses, list(status_row("date", "warning", paste("Some detection dates fall outside the operational period recorded for the trap. Problem values:", format_flag_values(outside_labels)))))
      }
    }
  }

  sex_summary <- tibble()
  other_covariates <- tibble()
  sex_col <- names(df)[normalize_name(names(df)) %in% "sex"]
  if (length(sex_col) > 0) {
    sex_vals <- str_trim(df[[sex_col[1]]])
    bad_sex <- sex_vals[!is.na(sex_vals) & sex_vals != "" & !(str_to_lower(sex_vals) %in% c("male", "female", "m", "f"))]
    if (length(bad_sex) > 0) {
      statuses <- append(statuses, list(status_row("sex", "warning", paste("Sex contains values other than Male/Female or M/F. Problem values:", format_flag_values(bad_sex)))))
    }
    sex_summary <- tibble(level = sex_vals) |>
      filter(!is.na(level), level != "") |>
      count(level, name = "n") |>
      arrange(desc(n), level) |>
      mutate(issue = if_else(!(str_to_lower(level) %in% c("male", "female", "m", "f")), "unexpected sex category", ""))
    other_covariates <- covariate_summary_tbl(df, c("session", "animalID", "occasion", "trapID", "date", sex_col[1]))
  } else {
    other_covariates <- covariate_summary_tbl(df, c("session", "animalID", "occasion", "trapID", "date"))
  }

  status_tbl <- bind_rows(statuses)
  if (nrow(status_tbl) == 0) {
    status_tbl <- status_row("detections", "pass", "No issues detected in the detections file checks that were run.")
  }

  animal_counts <- tibble(animalID = animal_trim) |>
    count(animalID, name = "detections") |>
    arrange(desc(detections), animalID) |>
    mutate(issue = case_when(
      animalID %in% unique(bad_animal_ids) ~ "whitespace or punctuation",
      animalID %in% unique(unknown_ids) ~ "unknown-style placeholder",
      animalID %in% unique(variant_ids) ~ "alternate variant of same base ID",
      TRUE ~ ""
    ))

  sessions_missing_from_detections <- tibble()
  if (!is.null(traps_results)) {
    sessions_missing_from_detections <- tibble(session = setdiff(unique(traps_results$data$session), unique(df$session)))
  }

  list(
    data = df,
    statuses = status_tbl,
    session_summary = list(
      type = session_info$type,
      unique_values = if (session_info$type == "text") unique(df$session) else NULL,
      missing_from_detections = sessions_missing_from_detections
    ),
    animal_summary = animal_counts,
    occasion_summary = list(
      occasion = occasion
    ),
    trap_summary = list(
      trap_ids = sort(unique(trap_trim)),
      trap_table = tibble(trapID = trap_trim) |>
        count(trapID, name = "detections") |>
        arrange(desc(detections), trapID) |>
        mutate(issue = case_when(
          trapID %in% unique(trap_trim[missing_traps %||% FALSE]) ~ "not found in traps.csv",
          trapID %in% unique(trap_trim[zero_effort_traps %||% FALSE]) ~ "effort <= 0 in traps.csv",
          TRUE ~ ""
        ))
    ),
    date_summary = list(
      parsed_dates = parsed_dates
    ),
    sex_summary = sex_summary,
    covariate_summary = other_covariates
  )
}

make_status_box <- function(title, status_tbl, field = NULL) {
  rows <- if (is.null(field)) status_tbl else status_tbl |> filter(.data$field == field | .data$field == title)
  if (nrow(rows) == 0) {
    return(tags$p("No issues detected."))
  }
  tags$ul(
    lapply(seq_len(nrow(rows)), function(i) {
      cls <- c(error = "text-red", warning = "text-orange", pass = "text-green")[rows$level[i]] %||% "text-muted"
      tags$li(class = cls, tags$strong(str_to_title(rows$level[i])), paste(rows$message[i]))
    })
  )
}

make_coord_widget <- function(coord_summary) {
  coord_plot_data <- coord_summary$coord_plot_data
  if (is.null(coord_plot_data)) {
    return(tags$p("No coordinate data available."))
  }
  coord_pair <- coord_summary$coord_pair
  crs_input <- coord_summary$crs_input

  if (all(coord_pair %in% c("lat", "long", "latitude", "longitude"))) {
    lng_col <- if ("long" %in% coord_pair) "y" else "y"
    lat_col <- if ("lat" %in% coord_pair) "x" else "x"
    return(leaflet(coord_plot_data) |>
      addTiles() |>
      addCircleMarkers(~y, ~x, popup = ~paste(session, trapID, sep = " / "), radius = 4))
  }

  if (!is.null(crs_input) && nzchar(str_trim(crs_input))) {
    sf_obj <- tryCatch(
      st_as_sf(coord_plot_data, coords = c("x", "y"), crs = crs_input, remove = FALSE) |>
        st_transform(4326),
      error = function(e) NULL
    )
    if (!is.null(sf_obj)) {
      coords <- st_coordinates(sf_obj)
      map_df <- bind_cols(coord_plot_data, tibble(lng = coords[, 1], lat = coords[, 2]))
      return(leaflet(map_df) |>
        addTiles() |>
        addCircleMarkers(~lng, ~lat, popup = ~paste(session, trapID, sep = " / "), radius = 4))
    }
  }

  ggplot(coord_plot_data, aes(x = x, y = y, color = session, label = trapID)) +
    geom_point(size = 2.5) +
    labs(x = coord_pair[1], y = coord_pair[2]) +
    theme_minimal()
}

make_effort_plot <- function(effort_summary) {
  dat <- effort_summary$by_session
  if (nrow(dat) == 0) {
    return(ggplot() + theme_void() + labs(title = "No effort data available"))
  }
  ggplot(dat, aes(x = effort)) +
    geom_histogram(bins = 15, fill = "#3c8dbc", color = "white") +
    facet_wrap(~session, scales = "free_y") +
    theme_minimal()
}

make_timeline_plot <- function(traps_results, detections_results = NULL) {
  timeline <- traps_results$date_summary$timeline_data
  if (is.null(timeline) || nrow(timeline) == 0) {
    return(ggplot() + theme_void() + labs(title = "No date data available"))
  }

  p <- ggplot(timeline, aes(y = reorder(trap_session, start_date))) +
    geom_segment(aes(x = start_date, xend = end_date, yend = trap_session), linewidth = 0.8, color = "#3c8dbc") +
    labs(x = "Date", y = "TrapID_session") +
    theme_minimal()

  if (!is.null(detections_results) && !is.null(detections_results$date_summary$parsed_dates)) {
    det_df <- detections_results$data |>
      transmute(
        session,
        trapID = str_trim(trapID),
        trap_session = paste(session, trapID, sep = "_"),
        date = detections_results$date_summary$parsed_dates
      ) |>
      filter(!is.na(date))
    if (nrow(det_df) > 0) {
      p <- p + geom_point(data = det_df, aes(x = date, y = trap_session), inherit.aes = FALSE, color = "#dd4b39", alpha = 0.7, size = 1.6)
    }
  }
  p
}

status_counts <- function(status_tbl) {
  status_tbl |>
    count(level, name = "n") |>
    complete(level = c("error", "warning", "pass"), fill = list(n = 0))
}
