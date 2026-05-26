suppressPackageStartupMessages({
  library(DT)
  library(shiny)
  library(shinydashboard)
})

source(file.path("R", "helpers.R"), local = TRUE)

flagged_dt <- function(data, page_length = 8) {
  tbl <- datatable(data, options = list(pageLength = page_length), rownames = FALSE)
  if ("issue" %in% names(data)) {
    flagged_values <- setdiff(unique(data$issue), "")
    if (length(flagged_values) > 0) {
      tbl <- tbl |>
        formatStyle(
          "issue",
          target = "row",
          backgroundColor = styleEqual(flagged_values, rep("#fff3cd", length(flagged_values))),
          valueColumns = "issue"
        )
    }
  }
  tbl
}

ui <- dashboardPage(
  dashboardHeader(title = "SCR Data Checker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Checker", tabName = "checker", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .text-red { color: #dd4b39; }
        .text-orange { color: #f39c12; }
        .text-green { color: #00a65a; }
        .mapping-actions { margin-top: 10px; margin-bottom: 15px; }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "checker",
        fluidRow(
          tabBox(
            width = 12,
            id = "workflow_tabs",
            tabPanel(
              "Column Names",
              br(),
              box(
                width = 12, title = "Uploads and Column Approval", status = "primary", solidHeader = TRUE,
                fluidRow(
                  column(4, fileInput("traps_file", "Upload traps csv", accept = ".csv")),
                  column(4, fileInput("detections_file", "Upload detections csv", accept = ".csv")),
                  column(4, textInput("crs_input", "Optional CRS", placeholder = "EPSG:32644 or +proj=utm +zone=44 +datum=WGS84 +units=m +no_defs"))
                ),
                fluidRow(
                  column(6, uiOutput("traps_mapping_ui")),
                  column(6, uiOutput("detections_mapping_ui"))
                )
              )
            ),
            tabPanel(
              "Data Checks",
              br(),
              fluidRow(
                box(
                  width = 12, title = "Report", status = "primary", solidHeader = TRUE,
                  uiOutput("checks_gate_ui"),
                  downloadButton("download_report", "Download HTML report")
                )
              ),
              fluidRow(
                box(
                  width = 6, title = "Traps File", status = "primary", solidHeader = TRUE,
                  uiOutput("traps_overview"),
                  tabBox(
                    width = 12,
                    tabPanel("session", uiOutput("traps_session_ui")),
                    tabPanel("trapID", uiOutput("traps_trapid_ui")),
                    tabPanel("Coordinates", uiOutput("traps_coords_ui")),
                    tabPanel("Effort", uiOutput("traps_effort_ui")),
                    tabPanel("Dates", uiOutput("traps_dates_ui")),
                    tabPanel("Covariates", uiOutput("traps_covars_ui"))
                  )
                ),
                box(
                  width = 6, title = "Detections File", status = "primary", solidHeader = TRUE,
                  uiOutput("detections_overview"),
                  tabBox(
                    width = 12,
                    tabPanel("session", uiOutput("det_session_ui")),
                    tabPanel("animalID", uiOutput("det_animal_ui")),
                    tabPanel("occasion", uiOutput("det_occasion_ui")),
                    tabPanel("trapID", uiOutput("det_trapid_ui")),
                    tabPanel("Date", uiOutput("det_date_ui")),
                    tabPanel("Covariates", uiOutput("det_covars_ui"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  traps_approved <- reactiveVal(FALSE)
  traps_rejected <- reactiveVal(FALSE)
  detections_approved <- reactiveVal(FALSE)
  detections_rejected <- reactiveVal(FALSE)

  traps_raw <- reactive({
    req(input$traps_file)
    read_input_csv(input$traps_file$datapath)
  })

  detections_raw <- reactive({
    req(input$detections_file)
    read_input_csv(input$detections_file$datapath)
  })

  observeEvent(input$traps_file, {
    traps_approved(FALSE)
    traps_rejected(FALSE)
  })

  observeEvent(input$detections_file, {
    detections_approved(FALSE)
    detections_rejected(FALSE)
  })

  traps_mapping <- reactive({
    req(traps_raw())
    suggest_mapping(traps_raw(), "traps")
  })

  detections_mapping <- reactive({
    req(detections_raw())
    suggest_mapping(detections_raw(), "detections")
  })

  traps_ready <- reactive({
    isTruthy(input$traps_file) &&
      isTRUE(traps_mapping()$required_ok) &&
      (isTRUE(traps_mapping()$exact_match) || isTRUE(traps_approved()))
  })

  detections_ready <- reactive({
    isTruthy(input$detections_file) &&
      isTRUE(detections_mapping()$required_ok) &&
      (isTRUE(detections_mapping()$exact_match) || isTRUE(detections_approved()))
  })

  observeEvent(traps_mapping(), {
    if (isTRUE(traps_mapping()$exact_match) && isTRUE(traps_mapping()$required_ok)) {
      traps_approved(TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(detections_mapping(), {
    if (isTRUE(detections_mapping()$exact_match) && isTRUE(detections_mapping()$required_ok)) {
      detections_approved(TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$approve_traps_mapping, {
    traps_approved(TRUE)
    traps_rejected(FALSE)
  })

  observeEvent(input$reject_traps_mapping, {
    traps_approved(FALSE)
    traps_rejected(TRUE)
  })

  observeEvent(input$approve_detections_mapping, {
    detections_approved(TRUE)
    detections_rejected(FALSE)
  })

  observeEvent(input$reject_detections_mapping, {
    detections_approved(FALSE)
    detections_rejected(TRUE)
  })

  traps_data <- reactive({
    req(traps_raw(), traps_ready())
    apply_mapping(traps_raw(), traps_mapping()$mapping)
  })

  detections_data <- reactive({
    req(detections_raw(), detections_ready())
    apply_mapping(detections_raw(), detections_mapping()$mapping)
  })

  traps_results <- reactive({
    req(traps_data())
    validate_traps(traps_data(), input$crs_input)
  })

  detections_results <- reactive({
    req(detections_data())
    validate_detections(detections_data(), if (isTRUE(traps_ready())) traps_results() else NULL)
  })

  output$traps_mapping_ui <- renderUI({
    req(traps_mapping())
    mapping <- traps_mapping()
    if (mapping$exact_match && mapping$required_ok) {
      return(tags$p(class = "text-green", "Traps columns match required names exactly."))
    }
    if (traps_approved()) {
      return(tags$p(class = "text-green", "Traps column mapping approved. Checks are now enabled on the Data Checks tab."))
    }
    tagList(
      tags$h4("Traps column mapping"),
      if (!mapping$required_ok) tags$p(class = "text-red", "Required traps columns could not be mapped confidently."),
      DTOutput("traps_mapping_table"),
      div(
        class = "mapping-actions",
        actionButton("approve_traps_mapping", "Approve"),
        actionButton("reject_traps_mapping", "Reject")
      ),
      if (traps_rejected()) tags$p(class = "text-red", "Traps mapping rejected. Please revise the csv and upload again.")
    )
  })

  output$detections_mapping_ui <- renderUI({
    req(detections_mapping())
    mapping <- detections_mapping()
    if (mapping$exact_match && mapping$required_ok) {
      return(tags$p(class = "text-green", "Detections columns match required names exactly."))
    }
    if (detections_approved()) {
      return(tags$p(class = "text-green", "Detections column mapping approved. Checks are now enabled on the Data Checks tab."))
    }
    tagList(
      tags$h4("Detections column mapping"),
      if (!mapping$required_ok) tags$p(class = "text-red", "Required detections columns could not be mapped confidently."),
      DTOutput("detections_mapping_table"),
      div(
        class = "mapping-actions",
        actionButton("approve_detections_mapping", "Approve"),
        actionButton("reject_detections_mapping", "Reject")
      ),
      if (detections_rejected()) tags$p(class = "text-red", "Detections mapping rejected. Please revise the csv and upload again.")
    )
  })

  output$checks_gate_ui <- renderUI({
    if (traps_ready() && detections_ready()) {
      return(tags$p(class = "text-green", "Both files are approved. Detailed checks and report download are available below."))
    }
    pending <- c()
    if (!traps_ready()) pending <- c(pending, "traps column approval")
    if (!detections_ready()) pending <- c(pending, "detections column approval")
    tags$p(class = "text-orange", paste("Complete", paste(pending, collapse = " and "), "on the Column Names tab to unlock the full checks."))
  })

  output$traps_mapping_table <- renderDT({
    req(traps_mapping())
    datatable(traps_mapping()$mapping, options = list(dom = "t", pageLength = 20), rownames = FALSE)
  })

  output$detections_mapping_table <- renderDT({
    req(detections_mapping())
    datatable(detections_mapping()$mapping, options = list(dom = "t", pageLength = 20), rownames = FALSE)
  })

  output$traps_overview <- renderUI({
    if (!isTruthy(input$traps_file)) {
      return(tags$p("Upload a traps csv file to begin."))
    }
    if (!traps_mapping()$required_ok) {
      return(tags$p(class = "text-red", "Traps checks cannot proceed until required columns are mapped."))
    }
    if (!traps_ready()) {
      return(tags$p("Approve the proposed traps column mapping to run checks."))
    }
    counts <- status_counts(traps_results()$statuses)
    tags$p(sprintf("Errors: %d | Warnings: %d", counts$n[counts$level == "error"], counts$n[counts$level == "warning"]))
  })

  output$detections_overview <- renderUI({
    if (!isTruthy(input$detections_file)) {
      return(tags$p("Upload a detections csv file to begin."))
    }
    if (!detections_mapping()$required_ok) {
      return(tags$p(class = "text-red", "Detections checks cannot proceed until required columns are mapped."))
    }
    if (!detections_ready()) {
      return(tags$p("Approve the proposed detections column mapping to run checks."))
    }
    counts <- status_counts(detections_results()$statuses)
    tags$p(sprintf("Errors: %d | Warnings: %d", counts$n[counts$level == "error"], counts$n[counts$level == "warning"]))
  })

  output$traps_session_ui <- renderUI({
    req(traps_results())
    res <- traps_results()
    tagList(
      make_status_box("session", res$statuses, "session"),
      tags$p(sprintf("Data type: %s", res$session_summary$type)),
      if (!is.null(res$session_summary$unique_values)) tags$p(paste("Unique values:", paste(res$session_summary$unique_values, collapse = ", ")))
    )
  })

  output$traps_trapid_ui <- renderUI({
    req(traps_results())
    res <- traps_results()
    tagList(
      make_status_box("trapID", res$statuses, "trapID"),
      DTOutput("traps_session_count_table"),
      DTOutput("traps_trapid_table")
    )
  })

  output$traps_coords_ui <- renderUI({
    req(traps_results())
    tagList(
      make_status_box("coordinates", traps_results()$statuses, "coordinates"),
      uiOutput("traps_coord_plot_ui")
    )
  })

  output$traps_coord_plot_ui <- renderUI({
    req(traps_results())
    widget <- make_coord_widget(traps_results()$coord_summary)
    if (inherits(widget, "leaflet")) {
      leafletOutput("traps_leaflet", height = 320)
    } else {
      plotOutput("traps_coord_plot", height = 320)
    }
  })

  output$traps_leaflet <- renderLeaflet({
    req(traps_results())
    widget <- make_coord_widget(traps_results()$coord_summary)
    if (inherits(widget, "leaflet")) widget
  })

  output$traps_coord_plot <- renderPlot({
    req(traps_results())
    widget <- make_coord_widget(traps_results()$coord_summary)
    if (inherits(widget, "ggplot")) widget
  })

  output$traps_effort_ui <- renderUI({
    req(traps_results())
    tagList(
      make_status_box("effort", traps_results()$statuses, "effort"),
      plotOutput("traps_effort_plot", height = 260),
      DTOutput("traps_effort_table")
    )
  })

  output$traps_effort_plot <- renderPlot({
    req(traps_results())
    make_effort_plot(traps_results()$effort_summary)
  })

  output$traps_dates_ui <- renderUI({
    req(traps_results())
    tagList(
      make_status_box("start_date", traps_results()$statuses, "start_date"),
      make_status_box("end_date", traps_results()$statuses, "end_date"),
      plotOutput("timeline_plot", height = 420)
    )
  })

  output$traps_covars_ui <- renderUI({
    req(traps_results())
    tagList(
      if (nrow(traps_results()$covariate_summary) == 0) tags$p("No additional covariates found.") else DTOutput("traps_covars_table")
    )
  })

  output$det_session_ui <- renderUI({
    req(detections_results())
    res <- detections_results()
    tagList(
      make_status_box("session", res$statuses, "session"),
      tags$p(sprintf("Data type: %s", res$session_summary$type)),
      if (!is.null(res$session_summary$unique_values)) tags$p(paste("Unique values:", paste(res$session_summary$unique_values, collapse = ", "))),
      if (nrow(res$session_summary$missing_from_detections) > 0) tags$p(paste("Sessions present in traps but absent from detections:", paste(res$session_summary$missing_from_detections$session, collapse = ", ")))
    )
  })

  output$det_animal_ui <- renderUI({
    req(detections_results())
    tagList(
      make_status_box("animalID", detections_results()$statuses, "animalID"),
      DTOutput("det_animal_table")
    )
  })

  output$det_occasion_ui <- renderUI({
    req(detections_results())
    make_status_box("occasion", detections_results()$statuses, "occasion")
  })

  output$det_trapid_ui <- renderUI({
    req(detections_results())
    tagList(
      make_status_box("trapID", detections_results()$statuses, "trapID"),
      DTOutput("det_trapid_table")
    )
  })

  output$det_date_ui <- renderUI({
    req(detections_results())
    tagList(
      make_status_box("date", detections_results()$statuses, "date"),
      plotOutput("det_timeline_plot", height = 420)
    )
  })

  output$det_covars_ui <- renderUI({
    req(detections_results())
    tagList(
      make_status_box("sex", detections_results()$statuses, "sex"),
      if (nrow(detections_results()$sex_summary) > 0) DTOutput("det_sex_table"),
      if (nrow(detections_results()$covariate_summary) == 0) tags$p("No additional covariates found.") else DTOutput("det_covars_table")
    )
  })

  output$traps_session_count_table <- renderDT({
    req(traps_results())
    datatable(traps_results()$trap_summary$session_counts, options = list(pageLength = 5), rownames = FALSE)
  })

  output$traps_trapid_table <- renderDT({
    req(traps_results())
    flagged_dt(traps_results()$trap_summary$trap_table, page_length = 8)
  })

  output$traps_effort_table <- renderDT({
    req(traps_results())
    datatable(traps_results()$effort_summary$five_number, options = list(pageLength = 5), rownames = FALSE)
  })

  output$traps_covars_table <- renderDT({
    req(traps_results())
    datatable(traps_results()$covariate_summary, options = list(pageLength = 8), rownames = FALSE)
  })

  output$det_animal_table <- renderDT({
    req(detections_results())
    flagged_dt(detections_results()$animal_summary, page_length = 8)
  })

  output$det_trapid_table <- renderDT({
    req(detections_results())
    flagged_dt(detections_results()$trap_summary$trap_table, page_length = 8)
  })

  output$det_sex_table <- renderDT({
    req(detections_results())
    flagged_dt(detections_results()$sex_summary, page_length = 8)
  })

  output$det_covars_table <- renderDT({
    req(detections_results())
    datatable(detections_results()$covariate_summary, options = list(pageLength = 8), rownames = FALSE)
  })

  output$timeline_plot <- renderPlot({
    req(traps_results())
    make_timeline_plot(traps_results())
  })

  output$det_timeline_plot <- renderPlot({
    req(traps_results(), detections_results())
    make_timeline_plot(traps_results(), detections_results())
  })

  output$download_report <- downloadHandler(
    filename = function() {
      sprintf("scr_data_check_%s.html", Sys.Date())
    },
    content = function(file) {
      req(traps_results(), detections_results())
      tmp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("report_template.Rmd", tmp_rmd, overwrite = TRUE)
      rmarkdown::render(
        tmp_rmd,
        output_file = file,
        params = list(
          app_dir = normalizePath("."),
          traps_results = traps_results(),
          detections_results = detections_results()
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    }
  )
}

shinyApp(ui, server)
