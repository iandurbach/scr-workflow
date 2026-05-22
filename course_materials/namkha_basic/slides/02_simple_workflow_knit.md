---
title: Simple SECR workflow
subtitle: Namkha basic tutorial
author: |
  | Snow leopard SECR workflow workshop
  | Ian Durbach and Cornelia Oedekoven
institute: |
  | Centre for Research into Ecological & Environmental Modelling
  | University of St Andrews
fontsize: 10pt
format:
  beamer:
    theme: metropolis
    incremental: false
    pdf-engine: xelatex
    slide-level: 2
    include-in-header: preamble.tex
classoption: t
urlcolor: teal
---



## Session 2 objectives

- create a single-session `capthist` object from camera and detection files
- build habitat masks for model fitting and reporting
- fit a small, interpretable set of `secr` models
- turn fitted models into density, abundance, maps, and checks

\vspace{0.4cm}

The emphasis is the workflow: what each object means, where it is saved, and how the scripts connect.

## The simple workflow

Each script saves the main object needed by the next step:

\begin{tikzpicture}[remember picture, overlay]
\node[xshift=-4.2cm, yshift=-0.8cm, draw=black, fill=blue!10,
      fill opacity = 0.3, text opacity = 1, shape=rectangle,
      rounded corners=1.5] at (current page){
      \begin{minipage}[t][3.1cm][t]{0.19\textwidth}
       \textbf{01}\\
       camera file\\
       detection file\\[0.15cm]
       \texttt{capthist}
      \end{minipage}};
\node[xshift=-1.45cm, yshift=-0.8cm, draw=black, fill=blue!10,
      fill opacity = 0.3, text opacity = 1, shape=rectangle,
      rounded corners=1.5] at (current page){
      \begin{minipage}[t][3.1cm][t]{0.20\textwidth}
       \textbf{02}\\
       survey area\\
       buffer\\
       covariates\\[0.15cm]
       \texttt{mask}
      \end{minipage}};
\node[xshift=1.3cm, yshift=-0.8cm, draw=black, fill=blue!10,
      fill opacity = 0.3, text opacity = 1, shape=rectangle,
      rounded corners=1.5] at (current page){
      \begin{minipage}[t][3.1cm][t]{0.20\textwidth}
       \textbf{04}\\
       density model\\
       detection model\\[0.15cm]
       fitted models
      \end{minipage}};
\node[xshift=4.0cm, yshift=-0.8cm, draw=black, fill=blue!10,
      fill opacity = 0.3, text opacity = 1, shape=rectangle,
      rounded corners=1.5] at (current page){
      \begin{minipage}[t][3.1cm][t]{0.18\textwidth}
       \textbf{05}\\
       AICc\\
       abundance\\
       density\\
       maps
      \end{minipage}};
\draw[->, very thick, mDarkTeal] (-3.2,-0.8) -- (-2.35,-0.8);
\draw[->, very thick, mDarkTeal] (-0.45,-0.8) -- (0.35,-0.8);
\draw[->, very thick, mDarkTeal] (2.25,-0.8) -- (3.05,-0.8);
\end{tikzpicture}

\vspace{4.7cm}

\alert{Key distinction:} fit over where animals could have been detected, then report over the region of inference.

## Capture histories: script 01

`01_make_capthist.R` creates the main observation object for `secr`.

- read and clean camera records
- read detection records
- encode effort as trap usage
- attach trap-level covariates
- create and check a `capthist`
- save the object for later scripts

\vspace{0.25cm}


 \footnotesize


``` r
load("namkha_basic/output/namkha_basic_secr_inputs_capthist.RData")
```

 \normalsize

## Capture histories: camera data to `traps`

Use the trap file to define the detector layout and usage:


 \footnotesize


``` r
cameras <- read.csv(traps_file, stringsAsFactors = FALSE)
cameras <- cameras |>
  filter(Note == "Functional") |>
  mutate(
    start_date = as.Date(start_date, format = "%d/%m/%Y"),
    end_date   = as.Date(end_date,   format = "%d/%m/%Y")
  )

traps <- read.traps(
  data = traps_df |> dplyr::select(trapID, x, y, effort),
  detector = "count",
  trapID = "trapID",
  binary.usage = FALSE
)
```

 \normalsize

## Capture histories: detector covariates

Trap covariates belong to the observation process:


 \footnotesize


``` r
covariates(traps)$Topography    <- traps_df$Topography
covariates(traps)$cliff         <- traps_df$cliff
covariates(traps)$hill          <- traps_df$hill
covariates(traps)$valley_stream <- traps_df$valley_stream
covariates(traps)$Water         <- traps_df$Water
covariates(traps)$Altitude      <- traps_df$Altitude
```

 \normalsize

- use these when encounter rate may differ among camera sites
- here effort is also encoded in the `traps` object

## Capture histories: detection data to `capthist`

Standardise the detections, keep the survey period, then make the capture history:


 \footnotesize


``` r
capts_raw <- read.csv(detections_file, stringsAsFactors = FALSE)

capts_raw <- capts_raw |>
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
  filter(date >= survey_start, date <= survey_end)

capts <- capts_raw |>
  dplyr::select(session, animalID, occasion, trapID, sex)

ch <- make.capthist(captures = capts, traps = traps)
verify(ch)
summary(ch, terse = TRUE)
```

 \normalsize

## Capture histories: descriptive checks

Before fitting models, check that effort and detections are coherent:

\begin{center}
\includegraphics[height=2.65in]{../output/fig/namkha_capture_history_summary.png}
\end{center}

- are detections concentrated at a few cameras?
- do detections occur during active camera periods?
- do new individuals keep appearing late in the survey?

## Habitat masks: script 02

`02_make_masks.R` creates the spatial grids used by the SCR model.

- read the Namkha boundary
- build a trap buffer
- create a regular grid of mask points
- add spatial covariates to the mask
- save separate masks for fitting and reporting

\vspace{0.25cm}

The mask is where activity centres could be, not where cameras were placed.

## Habitat masks: define the region

The full region is the union of the survey boundary and the trap buffer:


 \footnotesize


``` r
mask_spacing <- 1500
mask_buffer  <- 25000

survey_area <- st_read("namkha_basic/data/survey_area/Namkha_RM.shp",
                       quiet = TRUE) |>
  st_zm() |>
  st_transform(my_crs) |>
  st_geometry()

traps_buffer <- traps_sf |>
  st_buffer(mask_buffer) |>
  st_union() |>
  st_geometry()
```

 \normalsize

## Habitat masks: create the mask

Build a regular grid and keep points inside the region:


 \footnotesize


``` r
mask_grid <- st_make_grid(
  st_buffer(full_region, 10000),
  cellsize = c(mask_spacing, mask_spacing),
  what = "centers"
)

mask_points <- mask_grid[full_region] |>
  st_as_sf() |>
  cbind(st_coordinates(.)) |>
  st_drop_geometry() |>
  dplyr::select(x = X, y = Y)

mask <- read.mask(data = mask_points, spacing = mask_spacing)
```

 \normalsize

## Habitat masks: add spatial covariates

Density covariates belong on the mask because they describe possible activity-centre locations:


 \footnotesize


``` r
tri <- rast("namkha_basic/data/spatial_covs/TRI_Namkha.tif")
tri <- project(tri, y = my_crs)
names(tri) <- "tri"
mask <- addCovariates(mask, tri)

covariates(mask)$d2hydro <- as.numeric(st_distance(mask_sf, hydro))
```

 \normalsize

- `mask_model` is used for `secr.fit()`
- `mask_survey_area` is used for abundance and reporting

## Model fitting: script 04

`04_model_fitting.R` fits a small candidate set.

- start with the null model
- add density covariates such as `std_tri` or `std_d2hydro`
- add detector covariates such as `valley_stream`
- keep the candidate set small and interpretable

\vspace{0.25cm}

This script saves the fitted models for the reporting step.

## Model fitting: the null model


 \footnotesize


``` r
m0 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1),
  verify = FALSE
)
```

 \normalsize

- `D` is density
- `lambda0` is baseline encounter rate
- `sigma` controls how fast encounter rate declines with distance

## Model fitting: adding ecological structure


 \footnotesize


``` r
m1 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ 1, sigma ~ 1),
  verify = FALSE,
  start = m0
)

m4 <- secr.fit(
  ch,
  detectfn = "HHN",
  mask = mask_model,
  model = list(D ~ std_tri, lambda0 ~ valley_stream, sigma ~ 1),
  verify = FALSE,
  start = m1
)
```

 \normalsize

## Model fitting: compare models

Use AICc to compare the candidate set after fitting:


 \footnotesize


``` r
AIC(m0, m1, m2, m3, m4, m5, criterion = "AICc")
```

 \normalsize

- does the model converge?
- are parameter estimates sensible?
- does the selected model make ecological sense?

## Estimation and reporting: script 05

`05_model_results.R` turns fitted models into workshop outputs.

- compare models with AICc
- estimate abundance over `mask_survey_area`
- convert abundance to density
- map the survey region and predicted density
- plot covariate effects

## Estimation and reporting: abundance and density


 \footnotesize


``` r
fitted_models <- list(m0 = m0, m1 = m1, m2 = m2,
                      m3 = m3, m4 = m4, m5 = m5)

extrap_mask <- mask_survey_area
rn <- region.N(fitted_models[[model_name]], region = extrap_mask)

aic_table <- AIC(m0, m1, m2, m3, m4, m5, criterion = "AICc")
```

 \normalsize

This separates model fitting from reporting and mapping.

## Estimation and reporting: survey region

\begin{center}
\includegraphics[height=2.45in]{../output/results/namkha_basic_survey_region.png}
\end{center}

Survey boundary, camera locations, and the trap-buffer region used in the workflow.

## Estimation and reporting: predicted density

\begin{center}
\includegraphics[height=2.45in]{../output/results/namkha_basic_predicted_density.png}
\end{center}

This is a model-based prediction over the reporting mask, not a direct map of detections.

## Session 2 take-home points

- check the capture histories before fitting models
- keep the fitting mask and the reporting region conceptually separate
- build a small candidate set with clear ecological meaning
- compare models with AICc, but still inspect the fits
- keep model fitting separate from reporting and mapping
