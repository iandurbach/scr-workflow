# Efficient workflows for SCR analyses

This repo provides a standardised workflow for designing and analysing camera trap surveys of large cats with spatially explicit capture-recapture (SCR) in R package `secr`, and material for a workshop demonstrating the workflow.

## Workshop overview

SCR methods are widely used to estimate the abundance and distribution of wild animal populations, but implementing SCR can be challenging. Available software is very broad in scope and documentation is written for a general audience. Often it is not clear which options and settings should be used for a particular application. Through our work supporting the design and analysis of camera trap surveys for the first Population Assessment of the World’s Snow Leopards (PAWS), we have developed a workflow that can be used to simplify and support many SCR surveys involving camera trap surveys of large mammals. 

This workflow targets two common problem areas: efficient and reproducible conversion of detector outputs (e.g. images) into SECR inputs; and user-friendly software supporting the path from SCR inputs to results on quantities like density and abundance. Each step of the workflow is supported by R scripts that implement the relevant functions of the R package `secr` with user-friendly documentation explaining the steps involved. Supported tasks include: creating the habitat mask; constructing capture histories; model fitting; and interpretation of and prediction from fitted models. Additional scripts support more advanced features like multi-session analyses and spatial covariates. 

The intended outcome of the workshop is to broaden access to SCR methods and to provide researchers with the tools and understanding needed to confidently carry out their own analyses.

## Acknowledgements

1. The workflow is demonstrated using simulated data based on a camera trap survey of snow leopards in Namkha Rural Municipality, Nepal. The real analysis and data is reported in:
  - Lama, R.P., Lama, L.D., Ghale, T.R., Regmi, G.R., and Durbach, I.N. First estimates of snow leopard Panthera uncia density in Northwestern Highland of Nepal. To appear in *Oryx*.
2. Workflow scripts developed by Ian Durbach over years of collaborative work on snow leopard surveys with Snow Leopard Trust.
3. Workshop materials developed by Ian Durbach and Cornelia Oedekoven, with funding provided by a Biotechnology and Biological Sciences Research Council (BBSCR) Impact Acceleration Grant.
4. Workflow scripts make heavy use of functions from R package `secr`:
  - Efford, M. G. (2026). secr: Spatially explicit capture-recapture models. R
  package version 5.4.2. https://CRAN.R-project.org/package=secr

## Repo overview

Repo layout

```text
workshop/
  Dockerfile
  docker-compose.yml
  docker-compose.local.yml
  .env.example
  .gitignore
  .dockerignore
  docker/
  scripts/
  course_materials/
  my_work/
```

- `/course_materials/`: contains all participant material (details below)
- `/scripts/`: contains helper scripts for updating workshop material
- `/my_work/`: for workshops only -- `course_materials/` is mounted read-only into `/srv/workshop/course_materials`, `my_work/` is mounted read-write into `/srv/workshop/my_work`.
- `/docker/`: helper scripts for setting up docker container.

Most users will only need `/course_materials/`.

## Workshop participant instructions

During the workshop, there will be two ways to run the R scripts implementing the workflow.

1. Work locally: Install R, RStudio, packages, download workshop materials and set up 
2. Work remotely: log into and work on our dedicated RStudio Server (details shared on day of workshop)

Each of these are explained below. 

### Local setup

Software required:

- R: <https://cran.rstudio.com/>
- RStudio: <https://posit.co/download/rstudio-desktop>

Workshop material: Either fork and pull repo or 

1. Click on green `<Code>` button, select "Download .zip"
2. Unzip into dedicated `secr` workshop folder
3. Browse to `scr-workflow-main/course_materials` and open (double-click) `scr-project.Rproj`. This opens the project in RStudio.
4. Install required packages by typing the following into the RStudio console: `source("install_workshop_packages.R")` 
5. If step 4 fails, attempt to run the simpler installation script in `scr-workflow-main/course_materials/install_workshop_packages_simple.R` line by line.

### Remote setup

0. Open workshop URL.
1. Log in as your assigned account, e.g. `user01`.
2. Open `~/course_materials`.
3. Copy the full `~/course_materials` folder into `~/my_work/`.
4. Open and run scripts only from `~/my_work/course_materials`.
5. Do not save files into `course_materials`, because it is read-only.

## Updating workshop material

### Material-only updates

Edit files under `course_materials/`, then run:

```bash
scripts/deploy_materials.sh
```

### Image or environment updates

Build and push the published image:

```bash
docker buildx build --platform linux/amd64 -t iandurbach/secr-workflow-rstudio:latest --push .
```

Then update the server:

```bash
ssh -i ~/.ssh/workshop_deploy_key root@WORKSHOP.IP.ADDRESS
cd /scr-workshop
docker compose pull
docker compose down
docker compose up -d
```
