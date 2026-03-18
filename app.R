library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)
library(SQMtools)
library(DT)
`%||%` <- function(a, b) if (!is.null(a)) a else b
# ─────────────────────────────────────────────
#  LIGHT THEME
# ─────────────────────────────────────────────
sqm_theme <- bs_theme(
  version      = 5,
  bg           = "#f7f9fc",
  fg           = "#1a2a3a",
  primary      = "#1a6eb5",
  secondary    = "#e8eef5",
  success      = "#1a9e6e",
  info         = "#3b9ede",
  font_scale   = 0.92,
  base_font    = font_google("IBM Plex Sans"),
  heading_font = font_google("IBM Plex Mono")
)
# ─────────────────────────────────────────────
#  CSS
# ─────────────────────────────────────────────
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;500&display=swap');
:root {
  --blue:   #1a6eb5;
  --teal:   #1a9e6e;
  --bg:     #f7f9fc;
  --panel:  #eef2f7;
  --card:   #ffffff;
  --border: #d0dae6;
  --muted:  #7a90a8;
  --text:   #1a2a3a;
}
body { background: var(--bg); color: var(--text); }
.navbar {
  background: linear-gradient(90deg, #0e4a82 0%, #1a6eb5 100%) !important;
  border-bottom: 3px solid #1a9e6e !important;
  padding: 0.5rem 1.5rem !important;
}
.navbar-brand {
  font-family: 'IBM Plex Mono', monospace !important;
  font-weight: 600;
  color: #ffffff !important;
  letter-spacing: 0.05em;
  font-size: 1.1rem;
}
.nav-link { color: rgba(255,255,255,0.65) !important; font-size: 0.85rem; }
.nav-link:hover { color: #ffffff !important; }
.nav-link.active { color: #ffffff !important; border-bottom: 2px solid #1a9e6e; }
.card {
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  border-radius: 8px !important;
  box-shadow: 0 1px 4px rgba(26,42,58,0.07) !important;
}
.card-header {
  background: var(--panel) !important;
  border-bottom: 1px solid var(--border) !important;
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.78rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--blue) !important;
  padding: 0.6rem 1rem !important;
}
.bslib-sidebar-layout > .sidebar {
  background: var(--panel) !important;
  border-right: 1px solid var(--border) !important;
}
.form-control, .form-select {
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  font-size: 0.85rem !important;
}
.form-control:focus, .form-select:focus {
  border-color: var(--blue) !important;
  box-shadow: 0 0 0 2px rgba(26,110,181,0.12) !important;
}
.form-label {
  font-size: 0.78rem;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin-bottom: 3px;
}
.btn-primary {
  background: var(--blue) !important;
  border-color: var(--blue) !important;
  color: #ffffff !important;
  font-weight: 600;
  font-size: 0.82rem;
  letter-spacing: 0.04em;
}
.btn-primary:hover { background: #1558a0 !important; }
.btn-outline-secondary {
  border-color: var(--border) !important;
  color: var(--muted) !important;
  font-size: 0.82rem;
  background: #ffffff !important;
}
.btn-outline-secondary:hover {
  border-color: var(--blue) !important;
  color: var(--blue) !important;
}
.project-badge {
  display: inline-block;
  background: rgba(26,110,181,0.08);
  border: 1px solid rgba(26,110,181,0.25);
  color: var(--blue);
  border-radius: 4px;
  padding: 2px 8px;
  font-size: 0.75rem;
  font-family: 'IBM Plex Mono', monospace;
  margin: 2px;
}
.section-divider {
  border: none;
  border-top: 1px solid var(--border);
  margin: 10px 0;
}
.path-info {
  font-size: 0.72rem;
  color: var(--muted);
  word-break: break-all;
  font-family: 'IBM Plex Mono', monospace;
  margin-bottom: 6px;
}
.dataTables_wrapper { color: var(--text) !important; }
table.dataTable { background: #ffffff !important; color: var(--text) !important; }
table.dataTable thead th {
  background: var(--panel) !important;
  color: var(--blue) !important;
  border-bottom: 1px solid var(--border) !important;
  font-family: 'IBM Plex Mono', monospace;
  font-size: 0.75rem;
  letter-spacing: 0.05em;
}
table.dataTable tbody tr:hover { background: #eef5fc !important; }
.dataTables_filter input, .dataTables_length select {
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
}
.dataTables_info, .dataTables_paginate { color: var(--muted) !important; font-size: 0.8rem; }
.paginate_button.current {
  background: var(--blue) !important;
  color: #ffffff !important;
  border-radius: 4px;
}
.btn-default {
  background: #ffffff !important;
  border: 1px solid var(--border) !important;
  color: var(--muted) !important;
  font-size: 0.82rem !important;
}
.btn-default:hover { border-color: var(--blue) !important; color: var(--blue) !important; }
.func-search-box { position: relative; }
.func-search-box .search-icon {
  position: absolute; left: 8px; top: 50%; transform: translateY(-50%);
  color: var(--muted); font-size: 0.8rem; pointer-events: none; z-index: 10;
}
.func-search-box .form-control { padding-left: 26px !important; }
.func-search-hint { font-size: 0.7rem; color: var(--muted); margin-top: 3px; line-height: 1.4; }
.func-match-badge {
  display: inline-block; background: rgba(26,158,110,0.1); border: 1px solid rgba(26,158,110,0.3);
  color: #1a9e6e; border-radius: 4px; padding: 1px 7px; font-size: 0.72rem;
  font-family: 'IBM Plex Mono', monospace; margin-top: 4px;
}
.func-nomatch-badge {
  display: inline-block; background: rgba(192,57,43,0.08); border: 1px solid rgba(192,57,43,0.25);
  color: #c0392b; border-radius: 4px; padding: 1px 7px; font-size: 0.72rem;
  font-family: 'IBM Plex Mono', monospace; margin-top: 4px;
}
.sqm-section { background: var(--card); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; margin-bottom: 12px; }
.sqm-section-header {
  background: var(--panel); border-bottom: 1px solid var(--border); padding: 7px 14px;
  font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; font-weight: 600;
  letter-spacing: 0.09em; text-transform: uppercase; color: var(--blue);
}
.sqm-section-body { padding: 10px 14px; }
.sqm-table { width: 100%; border-collapse: collapse; font-size: 0.8rem; }
.sqm-table thead th {
  background: var(--panel); color: var(--blue); font-family: 'IBM Plex Mono', monospace;
  font-size: 0.72rem; letter-spacing: 0.05em; padding: 5px 10px;
  border-bottom: 1px solid var(--border); text-align: right;
}
.sqm-table thead th:first-child { text-align: left; }
.sqm-table tbody td {
  padding: 5px 10px; border-bottom: 1px solid rgba(208,218,230,0.4);
  color: var(--text); text-align: right;
}
.sqm-table tbody td:first-child { text-align: left; color: var(--muted); font-size: 0.78rem; }
.sqm-table tbody tr:last-child td { border-bottom: none; }
.sqm-table tbody tr:hover td { background: #eef5fc; }
.sqm-subsection-label {
  font-size: 0.7rem; color: var(--muted); text-transform: uppercase;
  letter-spacing: 0.06em; margin: 10px 0 5px; padding-left: 2px;
}
.sidebar-box { border: 1px solid var(--border); border-radius: 5px; padding: 7px 9px 6px; margin-bottom: 5px; }
.sidebar-box .form-label { margin-top: 0 !important; font-size: 0.72rem !important; }
.sidebar-box .form-select, .sidebar-box select, .sidebar-box input[type=text] {
  font-size: 0.78rem !important; height: 27px !important; padding: 2px 6px !important;
}
.sidebar-box .func-search-hint { font-size: 0.68rem !important; margin-top: 2px !important; line-height: 1.3 !important; }
.sidebar-box .func-match-badge, .sidebar-box .func-nomatch-badge { font-size: 0.68rem !important; padding: 1px 6px !important; }
.sidebar-box .shiny-input-container { margin-bottom: 0 !important; }
.bslib-sidebar-layout > .sidebar .form-select, .bslib-sidebar-layout > .sidebar select {
  font-size: 0.75rem !important; padding-top: 2px !important; padding-bottom: 2px !important;
  height: 26px !important; font-family: 'IBM Plex Sans', sans-serif !important;
}
.bslib-sidebar-layout > .sidebar .form-label { margin-bottom: 0px !important; margin-top: 5px !important; line-height: 1.2 !important; }
.bslib-sidebar-layout > .sidebar .shiny-input-container:has(input[type=checkbox]) { min-height: unset !important; margin-bottom: 0 !important; padding: 0 !important; }
.bslib-sidebar-layout > .sidebar .shiny-input-container:has(input[type=checkbox]) .checkbox { margin: 0 !important; }
.bslib-sidebar-layout > .sidebar .shiny-input-container:has(input[type=checkbox]) label { font-size: 0.72rem !important; line-height: 1.6 !important; }
.bslib-sidebar-layout > .sidebar { padding: 0.5rem 0.6rem !important; }
.bslib-sidebar-layout > .sidebar .shiny-input-container { margin-bottom: 0 !important; padding-bottom: 0 !important; }
.bslib-sidebar-layout > .sidebar .form-group { margin-bottom: 0 !important; }
.bslib-sidebar-layout > .sidebar .form-label, .bslib-sidebar-layout > .sidebar label { margin-bottom: 1px !important; margin-top: 5px !important; display: block; }
.bslib-sidebar-layout > .sidebar select, .bslib-sidebar-layout > .sidebar input[type=number], .bslib-sidebar-layout > .sidebar input[type=text] {
  margin-bottom: 0 !important; padding-top: 2px !important; padding-bottom: 2px !important; height: 28px !important;
}
.bslib-sidebar-layout > .sidebar .shiny-input-container:has(input[type=checkbox]) { min-height: unset !important; margin-bottom: 0 !important; padding: 0 !important; line-height: 1.4 !important; }
.bslib-sidebar-layout > .sidebar .shiny-input-container:has(input[type=checkbox]) .checkbox { margin-top: 0 !important; margin-bottom: 0 !important; }
.bslib-sidebar-layout > .sidebar hr { margin: 4px 0 !important; }
.bslib-sidebar-layout > .sidebar .btn { margin-top: 4px !important; }
.bslib-sidebar-layout > .sidebar .func-search-hint { margin-top: 1px !important; margin-bottom: 0 !important; }
.bslib-sidebar-layout > .sidebar .func-match-badge, .bslib-sidebar-layout > .sidebar .func-nomatch-badge { margin-top: 2px !important; margin-bottom: 0 !important; }
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--blue); }
"
build_func_pattern <- function(search_text) {
  search_text <- trimws(search_text)
  if (nchar(search_text) == 0) return(NULL)
  terms <- trimws(unlist(strsplit(search_text, "[,;]+")))
  terms <- terms[nchar(terms) > 0]
  if (length(terms) == 0) return(NULL)
  escaped <- gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", terms)
  paste(escaped, collapse = "|")
}
available_func_counts <- function(proj, fun_level) {
  all_counts <- c(
    "Raw abundances (abund)"    = "abund",   "Percentages (percent)" = "percent",
    "Base counts (bases)"       = "bases",   "CPM (cpm)"             = "cpm",
    "TPM (tpm)"                 = "tpm",     "Copy number (copy_number)" = "copy_number"
  )
  Filter(function(ct) {
    tbl <- tryCatch(proj$functions[[fun_level]][[ct]], error = function(e) NULL)
    !is.null(tbl) && (is.matrix(tbl) || is.data.frame(tbl)) && nrow(tbl) > 0
  }, all_counts)
}
available_tax_counts <- function(proj) {
  all_counts <- c("Percentages (percent)" = "percent", "Raw abundances (abund)" = "abund")
  Filter(function(ct) {
    tbl <- tryCatch(proj$taxa$phylum[[ct]], error = function(e) NULL)
    !is.null(tbl) && (is.matrix(tbl) || is.data.frame(tbl)) && nrow(tbl) > 0
  }, all_counts)
}
has_data <- function(tbl) {
  !is.null(tbl) && (is.matrix(tbl) || is.data.frame(tbl)) && nrow(tbl) > 0
}
available_plot_types <- function(proj) {
  choices <- c()
  # Taxonomy: needs at least one rank with data
  has_tax <- any(sapply(c("phylum","class","order","family","genus","species"), function(r)
    tryCatch(has_data(proj$taxa[[r]]$percent), error = function(e) FALSE)
  ))
  if (has_tax) choices <- c(choices, "Taxonomy (barplot)" = "taxonomy_bar")
  # Functions: COG, KEGG, PFAM, plus any extra databases
  all_dbs <- tryCatch(names(proj$functions), error = function(e) character(0))
  for (db in all_dbs) {
    tbl <- tryCatch(proj$functions[[db]]$abund, error = function(e) NULL)
    if (has_data(tbl)) {
      val   <- paste0("func_", tolower(db))
      label <- paste(db, "functions")
      choices <- c(choices, setNames(val, label))
    }
  }
  # Bins
  has_bins <- tryCatch(has_data(proj$bins$table), error = function(e) FALSE)
  if (has_bins) choices <- c(choices, "Binning" = "bins")
  if (length(choices) == 0) choices <- c("(no data)" = "none")
  choices
}
available_tax_ranks <- function(proj) {
  all_ranks <- c("Phylum"="phylum","Class"="class","Order"="order",
                 "Family"="family","Genus"="genus","Species"="species")
  Filter(function(r)
    tryCatch(has_data(proj$taxa[[r]]$percent), error = function(e) FALSE),
    all_ranks)
}
avail_assembly <- function(proj) {
  ch <- c()
  if (tryCatch(has_data(proj$contigs$table), error = function(e) FALSE)) ch <- c(ch, "Contigs" = "contigs")
  if (tryCatch(has_data(proj$orfs$table),    error = function(e) FALSE)) ch <- c(ch, "ORFs"     = "orfs")
  ch
}
avail_taxonomy <- function(proj) {
  ranks <- c("superkingdom","phylum","class","order","family","genus","species")
  ch <- c()
  for (r in ranks) {
    tbl <- tryCatch(proj$taxa[[r]]$abund, error = function(e) NULL)
    if (has_data(tbl)) ch <- c(ch, setNames(paste0("tax_",r), tools::toTitleCase(r)))
  }
  ch
}
avail_functions <- function(proj) {
  dbs <- tryCatch(names(proj$functions), error = function(e) character(0))
  ch <- c()
  for (db in dbs) {
    tbl <- tryCatch(proj$functions[[db]]$abund, error = function(e) NULL)
    if (has_data(tbl)) ch <- c(ch, setNames(paste0("fun_",tolower(db)), db))
  }
  ch
}
avail_bins <- function(proj) {
  tbl <- tryCatch(proj$bins$table, error = function(e) NULL)
  if (has_data(tbl)) c("Bins" = "bins") else c()
}
ui <- page_navbar(
  title = "SQMxplore",
  theme = sqm_theme,
  navbar_options = navbar_options(theme = "dark", bg = "#0e4a82"),
  header = tagList(useShinyjs(), tags$head(tags$style(HTML(custom_css)))),
  nav_panel("Project",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300, open = TRUE,
        tags$div(class = "form-label mt-1", "Project directory"),
        shinyDirButton("dir_project", "Select directory", "Choose the project directory",
          multiple = FALSE, class = "btn-default w-100 mb-1"),
        tags$div(class = "path-info", textOutput("path_project", inline = TRUE)),
        uiOutput("project_info_ui"), uiOutput("manual_tables_ui"),
        actionButton("load_project", "Load", class = "btn-primary w-100 mb-2"),
        uiOutput("project_status_ui")
      ),
      tags$div(style = "padding: 1rem;", uiOutput("project_summary_ui"))
    )
  ),
  nav_panel("Plots",
    layout_sidebar(
      sidebar = sidebar(width = 250,
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Plot type"),
          uiOutput("plot_type_ui")
        ),
        uiOutput("plot_controls_ui"),
        tags$div(style = "margin-top:5px;",
          downloadButton("download_plot", "Download PNG", class = "btn-outline-secondary w-100"))
      ),
      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
          span("Visualization"), uiOutput("plot_status_badge"))),
        card_body(class = "p-2", uiOutput("sqm_plot_ui"))
      )
    )
  ),
  nav_panel("Tables",
    layout_sidebar(
      sidebar = sidebar(width = 270,
        uiOutput("tbl_assembly_ui"),
        uiOutput("tbl_taxonomy_ui"),
        uiOutput("tbl_functions_ui"),
        uiOutput("tbl_bins_ui"),

        uiOutput("table_sample_filter"),
        tags$div(style = "margin-top:5px;",
          downloadButton("download_table", "Download CSV", class = "btn-outline-secondary w-100"))
      ),
      card(card_header("Data"), card_body(class = "p-2", DTOutput("data_table")))
    )
  ),
  nav_panel("Krona",
    layout_sidebar(
      sidebar = sidebar(width = 250,
        uiOutput("krona_ktcheck_ui"),
        tags$hr(class = "section-divider"),
        tags$div(class = "form-label mt-1", "Filter samples"),
        uiOutput("krona_sample_filter_ui"),
        tags$hr(class = "section-divider"),
        actionButton("do_krona", "Generate Krona", class = "btn-primary w-100 mt-1"),
        tags$div(style = "margin-top:5px;", uiOutput("krona_download_ui")),
        tags$div(style = "margin-top:10px;", uiOutput("krona_status_ui"))
      ),
      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
          span("Krona Chart"), uiOutput("krona_badge_ui"))),
        card_body(class = "p-0", uiOutput("krona_view_ui"))
      )
    )
  )
)
server <- function(input, output, session) {
  roots        <- c(home = normalizePath("~"), root = "/")
  sqm_data     <- reactiveVal(NULL)
  status       <- reactiveVal("idle")
  tables_path  <- reactiveVal(NULL)
  need_manual  <- reactiveVal(FALSE)
  creator_name <- reactiveVal(NULL)
  is_sqm_full  <- reactiveVal(FALSE)

  # ── Dynamic plot type selector — only shows available options ──
  output$plot_type_ui <- renderUI({
    proj <- sqm_data()
    choices <- if (is.null(proj)) {
      c("Taxonomy (barplot)" = "taxonomy_bar")
    } else {
      available_plot_types(proj)
    }
    cur <- isolate(input$plot_type)
    selected <- if (!is.null(cur) && cur %in% choices) cur else choices[[1]]
    selectInput("plot_type", NULL, choices = choices, selected = selected)
  })

  # ── Dynamic table type selector — only shows available options ──
  # ── Build each category box (only shown when choices exist) ──
  make_table_box <- function(label, input_id, choices) {
    if (length(choices) == 0) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-bottom:6px;",
      tags$div(class = "form-label", label),
      selectInput(input_id, NULL, choices = choices)
    )
  }

  output$tbl_assembly_ui <- renderUI({
    req(sqm_data())
    ch <- avail_assembly(sqm_data())
    make_table_box("Assembly", "tbl_assembly", ch)
  })
  output$tbl_taxonomy_ui <- renderUI({
    req(sqm_data())
    ch <- avail_taxonomy(sqm_data())
    make_table_box("Taxonomy", "tbl_taxonomy", ch)
  })
  output$tbl_functions_ui <- renderUI({
    req(sqm_data())
    ch <- avail_functions(sqm_data())
    make_table_box("Functions", "tbl_functions", ch)
  })
  output$tbl_bins_ui <- renderUI({
    req(sqm_data())
    if (length(avail_bins(sqm_data())) == 0) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-bottom:6px;",
      tags$div(class = "form-label", "Bins"),
      actionButton("tbl_bins_btn", "Bins table",
        class = "btn-default w-100",
        style = "font-size:0.75rem; font-family:'IBM Plex Sans',sans-serif; font-weight:400; height:26px; padding:2px 8px; text-align:left; letter-spacing:0;")
    )
  })

  # ── active_table: a reactiveVal updated by each selector.
  #    assembly/taxonomy/functions use ignoreInit=TRUE (multi-option selectors).
  #    bins uses a dedicated actionButton to avoid the single-option problem.
  active_tbl_rv <- reactiveVal("none")

  observeEvent(input$tbl_assembly,  ignoreNULL=TRUE, ignoreInit=TRUE,
    { active_tbl_rv(input$tbl_assembly) })
  observeEvent(input$tbl_taxonomy,  ignoreNULL=TRUE, ignoreInit=TRUE,
    { active_tbl_rv(input$tbl_taxonomy) })
  observeEvent(input$tbl_functions, ignoreNULL=TRUE, ignoreInit=TRUE,
    { active_tbl_rv(input$tbl_functions) })
  observeEvent(input$tbl_bins_btn,  ignoreNULL=TRUE, ignoreInit=TRUE,
    { active_tbl_rv("bins") })

  # Initialise on project load
  observeEvent(sqm_data(), {
    proj <- sqm_data(); req(proj)
    first <- c(avail_assembly(proj), avail_taxonomy(proj),
               avail_functions(proj), avail_bins(proj))
    if (length(first) > 0) active_tbl_rv(first[[1]])
  })

  active_table <- reactive({ active_tbl_rv() })

  shinyDirChoose(input, "dir_project",       roots = roots)
  shinyDirChoose(input, "dir_manual_tables", roots = roots)
  path_project <- reactive({ req(input$dir_project); parseDirPath(roots, input$dir_project) })
  output$path_project <- renderText({ tryCatch(path_project(), error = function(e) "") })
  observeEvent(path_project(), {
    proj_dir <- path_project(); req(nchar(proj_dir) > 0)
    need_manual(FALSE); tables_path(NULL); creator_name(NULL)
    creator_file <- file.path(proj_dir, "creator.txt")
    if (file.exists(creator_file)) {
      creator <- trimws(readLines(creator_file, n = 1, warn = FALSE))
      creator_name(creator)
      if (grepl("SqueezeMeta", creator, ignore.case = TRUE)) {
        tables_path(proj_dir)
      } else {
        tp <- file.path(proj_dir, "tables")
        if (dir.exists(tp)) tables_path(tp) else need_manual(TRUE)
      }
    } else {
      need_manual(TRUE)
      showNotification("creator.txt not found. Please select the tables directory manually.",
        type = "warning", duration = 6)
    }
  })
  observeEvent(input$dir_manual_tables, {
    tp <- tryCatch(parseDirPath(roots, input$dir_manual_tables), error = function(e) NULL)
    req(tp); if (nchar(tp) > 0) { tables_path(tp); need_manual(FALSE) }
  })
  output$project_info_ui <- renderUI({
    req(path_project())
    proj_dir <- path_project()
    creator_file <- file.path(proj_dir, "creator.txt")
    creator_txt <- if (file.exists(creator_file)) trimws(readLines(creator_file, n=1, warn=FALSE)) else "unknown"
    tp <- tables_path()
    tagList(
      tags$div(class = "path-info",
        tags$span(style = "color:#7a90a8;", "Created by: "),
        tags$span(style = "color:#1a6eb5; font-weight:600;", creator_txt)),
      if (!is.null(tp)) tags$div(class = "path-info",
        tags$span(style = "color:#7a90a8;", "Tables: "), tp,
        if (dir.exists(tp)) tags$span(style = "color:#1a9e6e; margin-left:4px;", "\u2713")
        else tags$span(style = "color:#c0392b; margin-left:4px;", "\u2715 not found"))
    )
  })
  output$manual_tables_ui <- renderUI({
    req(need_manual())
    tagList(
      tags$div(class = "path-info", style = "color:#c0392b;", "Tables directory could not be found automatically."),
      tags$div(class = "form-label", "Select tables directory"),
      shinyDirButton("dir_manual_tables", "Select tables", "Choose the tables directory",
        multiple = FALSE, class = "btn-default w-100 mb-1")
    )
  })
  observeEvent(input$load_project, {
    tp <- tables_path()
    if (is.null(tp) || !dir.exists(tp)) {
      showNotification("Directory not available. Please select it manually.", type = "error", duration = 8); return()
    }
    status("loading")
    tryCatch({
      is_sqm <- grepl("SqueezeMeta", creator_name() %||% "", ignore.case = TRUE)
      proj <- if (is_sqm) loadSQM(tp) else loadSQMlite(tp)
      sqm_data(proj); is_sqm_full(is_sqm); status("ready")
    }, error = function(e) { status("error"); showNotification(paste("Error:", e$message), type = "error", duration = 8) })
  })
  output$project_status_ui <- renderUI({
    s <- status()
    col <- switch(s, idle="#7a90a8", loading="#3b9ede", ready="#1a9e6e", error="#c0392b")
    ico <- switch(s, idle="\u25cb", loading="\u25cc", ready="\u25cf", error="\u2715")
    tags$div(style="font-size:0.8rem;",
      tags$span(style=paste0("color:",col,"; margin-right:5px;"), ico),
      tags$span(style="color:#7a90a8;", "Status: "),
      tags$span(style=paste0("color:",col,"; font-weight:600;"), toupper(s)))
  })
  parse_tsv_block <- function(lines) {
    lines <- lines[nchar(trimws(lines)) > 0]; if (length(lines)==0) return(NULL)
    split_line <- function(l) trimws(unlist(strsplit(sub("^\t","",l),"\t")))
    rows <- lapply(lines, split_line); max_cols <- max(sapply(rows, length))
    rows <- lapply(rows, function(r){length(r)<-max_cols; r[is.na(r)]<-""; r})
    as.data.frame(do.call(rbind, rows), stringsAsFactors=FALSE)
  }
  make_html_table <- function(df) {
    if (is.null(df)||nrow(df)<2) return(NULL)
    header <- as.character(df[1,]); body <- df[-1,,drop=FALSE]
    th_cells <- paste0('<th>',ifelse(header=="","",header),'</th>',collapse="")
    tr_rows <- apply(body,1,function(row) paste0('<tr>',paste0('<td>',row,'</td>',collapse=""),'</tr>'))
    HTML(paste0('<table class="sqm-table"><thead><tr>',th_cells,'</tr></thead><tbody>',paste(tr_rows,collapse=""),'</tbody></table>'))
  }
  make_kv_table <- function(lines) {
    rows <- lapply(lines, function(l) {
      l <- sub("^\t+","",l); parts <- strsplit(l,"\t")[[1]]
      if(length(parts)>=2) tags$tr(tags$td(trimws(sub(":$","",parts[1]))), tags$td(trimws(parts[2])))
    })
    rows <- Filter(Negate(is.null), rows); if(length(rows)==0) return(NULL)
    tags$table(class="sqm-table", tags$thead(tags$tr(tags$th("Metric"),tags$th("Value"))), tags$tbody(tagList(rows)))
  }
  make_taxcov_table <- function(lines) {
    rows <- lapply(lines, function(l) {
      l <- sub("^\t+","",l); parts <- strsplit(l,"\t")[[1]]
      if(length(parts)>=2) tags$tr(tags$td(trimws(sub(":$","",parts[1]))), tags$td(trimws(parts[2])))
    })
    rows <- Filter(Negate(is.null), rows); if(length(rows)==0) return(NULL)
    tags$table(class="sqm-table", tags$thead(tags$tr(tags$th("Rank"),tags$th("Value"))), tags$tbody(tagList(rows)))
  }
  sqm_section <- function(title, ...) tags$div(class="sqm-section",
    tags$div(class="sqm-section-header",title), tags$div(class="sqm-section-body",...))
  output$project_summary_ui <- renderUI({
    proj <- sqm_data()
    if (is.null(proj)) return(tags$div(style="color:var(--muted);font-size:0.85rem;padding:1rem;","No project loaded yet."))

    panels <- list()

    # ── Project name badge (both SQM and SQMlite) ──
    project_name <- tryCatch(proj$misc$project_name %||% "", error=function(e) "")
    if (nchar(project_name)>0) panels[["name"]] <- tags$div(
      style="margin-bottom:12px;display:flex;align-items:center;gap:10px;",
      tags$span(style="font-family:'IBM Plex Mono',monospace;font-size:0.75rem;color:var(--muted);text-transform:uppercase;letter-spacing:0.06em;","Project"),
      tags$span(class="project-badge",style="font-size:0.85rem;padding:3px 10px;",project_name))

    if (is_sqm_full()) {
      # ── Full SQM: parse capture.output(summary()) ──
      raw <- tryCatch(capture.output(summary(proj)), error=function(e) NULL)
      if (!is.null(raw)) {
        sections <- list(); current <- NULL; buf <- c(); sep_pat <- "^\\s*-{5,}\\s*$"
        for (ln in raw) {
          if (grepl("BASE PROJECT NAME:",ln)||grepl(sep_pat,ln)) next
          sec_match <- regmatches(ln, regexpr("^\\t([A-Za-z][A-Za-z0-9 /]+):\\s*$",ln))
          if (length(sec_match)>0) {
            if(!is.null(current)) sections[[current]]<-buf
            current<-trimws(sub(":","",sub("^\t","",sec_match))); buf<-c()
          } else buf <- c(buf,ln)
        }
        if (!is.null(current)) sections[[current]] <- buf
        reads_key <- names(sections)[tolower(names(sections))=="reads"]
        if (length(reads_key)>0) {
          lines <- sections[[reads_key[1]]]; data_lines <- lines[nchar(trimws(lines))>0]
          if (length(data_lines)>=2) {
            df <- parse_tsv_block(c(sub("^\t\t","\tMetric\t",data_lines[1]), data_lines[-1]))
            df[,1] <- sub("^Mapping to ORFs$","Reads with ORFs",df[,1])
            df[,1] <- sub("^Percent$","Percent of reads with ORFs",df[,1])
            desired <- c("Input reads","Reads with ORFs","Percent of reads with ORFs")
            body <- df[-1,,drop=FALSE]; body <- body[match(desired,body[,1]),,drop=FALSE]; body <- body[!is.na(body[,1]),,drop=FALSE]
            panels[["READS"]] <- sqm_section("Reads", make_html_table(rbind(df[1,,drop=FALSE],body)))
          }
        }
        contigs_key <- names(sections)[tolower(names(sections))=="contigs"]
        if (length(contigs_key)>0) {
          lines <- sections[[contigs_key[1]]]; lines <- lines[nchar(trimws(lines))>0]
          kv_lines <- lines[grepl(":\t",lines)&!grepl("\t\t",lines)&sapply(strsplit(lines,"\t"),function(x) sum(nchar(trimws(x))>0))==2]
          abund_start <- which(grepl("Most abundant taxa",lines)); abund_lines <- c()
          if (length(abund_start)>0) { abund_lines <- lines[(abund_start+1):length(lines)]; abund_lines <- abund_lines[nchar(trimws(abund_lines))>0] }
          tax_ranks <- c("Superkingdom","Phylum","Class","Order","Family","Genus","Species")
          tax_rank_pat <- paste0("^\t(",paste(tax_ranks,collapse="|"),"):\t")
          is_tax_kv <- grepl(tax_rank_pat,kv_lines)
          body_parts <- list()
          if (length(kv_lines[!is_tax_kv])>0) body_parts[["kv"]] <- make_kv_table(kv_lines[!is_tax_kv])
          if (length(kv_lines[is_tax_kv])>0) {
            body_parts[["taxcovlabel"]]<-tags$div(class="sqm-subsection-label","Taxonomic classification")
            body_parts[["taxcov"]]<-make_taxcov_table(kv_lines[is_tax_kv])
          }
          if (length(abund_lines)>=2) {
            body_parts[["abundlabel"]] <- tags$div(class="sqm-subsection-label","Most abundant taxa")
            df_abund <- parse_tsv_block(c(sub("^\t\t","\tRank\t",abund_lines[1]),abund_lines[-1]))
            species_rows <- which(trimws(df_abund[-1,1])=="Species")+1
            if (length(species_rows)>0) for(ri in species_rows) df_abund[ri,-1]<-paste0("<em>",df_abund[ri,-1],"</em>")
            body_parts[["abund"]] <- make_html_table(df_abund)
          }
          panels[["CONTIGS"]] <- sqm_section("Contigs", tagList(body_parts))
        }
        orfs_key <- names(sections)[tolower(names(sections))=="orfs"]
        if (length(orfs_key)>0) {
          lines <- sections[[orfs_key[1]]]; data_lines <- lines[nchar(trimws(lines))>0]
          if (length(data_lines)>=2) panels[["ORFS"]] <- sqm_section("ORFs",
            make_html_table(parse_tsv_block(c(sub("^\t\t","\tMetric\t",data_lines[1]),data_lines[-1]))))
        }
      }
    } else {
      # ── SQMlite: capture.output(summary()) produces tab-delimited text
      # Format:
      #   BASE PROJECT NAME: ...
      #   \t\tS1\tS2\t...          <- sample header
      #   TOTAL READS\t...\t...
      #   TOTAL ORFs\t...\t...
      #   ---...---
      #   TAXONOMY:
      #   Classified reads:
      #   \t\tS1\tS2\t...
      #   Superkingdom\t...\t...
      #   ...
      #   Most abundant taxa (ignoring Unclassified):
      #   \t\tS1\tS2\t...
      #   Superkingdom\tval\t...
      #   ...
      #   ---...---
      #   FUNCTIONS:
      #   Classified ORFs:
      #   \t\tS1\tS2\t...
      #   KEGG\t...\t...
      #   COG\t...\t...

      raw <- tryCatch(capture.output(summary(proj)), error = function(e) NULL)
      if (!is.null(raw)) {
        sep_pat <- "^\\s*-{5,}\\s*$"
        raw <- raw[!grepl(sep_pat, raw)]  # strip separator lines

        # ── Helper: parse a block of tab lines into header + body df ──
        parse_lite_block <- function(lines) {
          lines <- lines[nchar(trimws(lines)) > 0]
          if (length(lines) < 2) return(NULL)
          # First line is the sample header: "\t\tS1\tS2\t..."
          hdr   <- trimws(unlist(strsplit(sub("^\t\t?", "", lines[1]), "\t")))
          body  <- do.call(rbind, lapply(lines[-1], function(l) {
            parts <- unlist(strsplit(l, "\t"))
            # first element may be empty if line starts with \t
            if (parts[1] == "") parts <- parts[-1]
            length(parts) <- length(hdr) + 1
            parts[is.na(parts)] <- ""
            parts
          }))
          df <- as.data.frame(body, stringsAsFactors = FALSE)
          colnames(df) <- c("Metric", hdr)
          df
        }

        # ── Extract project name ──
        name_line <- grep("BASE PROJECT NAME:", raw, value = TRUE)
        if (length(name_line) > 0 && nchar(project_name) == 0) {
          project_name <- trimws(sub(".*BASE PROJECT NAME:\\s*", "", name_line[1]))
          # update badge if not already set
          panels[["name"]] <- tags$div(
            style = "margin-bottom:12px;display:flex;align-items:center;gap:10px;",
            tags$span(style = "font-family:'IBM Plex Mono',monospace;font-size:0.75rem;color:var(--muted);text-transform:uppercase;letter-spacing:0.06em;", "Project"),
            tags$span(class = "project-badge", style = "font-size:0.85rem;padding:3px 10px;", project_name))
        }

        # ── Overview block: TOTAL READS + TOTAL ORFs ──
        # Lines before the first section header (TAXONOMY: / FUNCTIONS:)
        first_sec <- grep("^\\t?(TAXONOMY|FUNCTIONS):", raw)
        overview_lines <- if (length(first_sec) > 0) raw[seq_len(first_sec[1] - 1)] else raw
        # Keep only lines with numeric data (contain digits after a tab)
        data_lines <- overview_lines[grepl("^\t", overview_lines) & grepl("[0-9]", overview_lines)]
        # Find sample header line (\t\t...)
        hdr_idx <- which(grepl("^\t\t", overview_lines))
        if (length(hdr_idx) > 0 && length(data_lines) > 0) {
          hdr_line  <- overview_lines[hdr_idx[1]]
          hdr_parts <- trimws(unlist(strsplit(sub("^\t\t?", "", hdr_line), "\t")))
          tbl_rows <- lapply(data_lines, function(l) {
            parts <- unlist(strsplit(sub("^\t", "", l), "\t"))
            length(parts) <- length(hdr_parts) + 1
            parts[is.na(parts)] <- ""
            tags$tr(tagList(lapply(parts, tags$td)))
          })
          th_cells <- tagList(c(list(tags$th("Metric")), lapply(hdr_parts, tags$th)))
          panels[["OVERVIEW"]] <- sqm_section("Overview",
            tags$table(class = "sqm-table",
              tags$thead(tags$tr(th_cells)),
              tags$tbody(tagList(tbl_rows))))
        }

        # ── TAXONOMY section ──
        tax_start <- grep("^\\t?TAXONOMY:", raw)
        fun_start <- grep("^\\t?FUNCTIONS:", raw)
        if (length(tax_start) > 0) {
          tax_end  <- if (length(fun_start) > 0) fun_start[1] - 1 else length(raw)
          tax_body <- raw[(tax_start[1] + 1):tax_end]

          # Classified reads sub-block
          cr_start <- grep("Classified reads", tax_body)
          ma_start <- grep("Most abundant taxa", tax_body)

          tax_panels <- list()

          if (length(cr_start) > 0) {
            cr_end   <- if (length(ma_start) > 0) ma_start[1] - 1 else length(tax_body)
            cr_lines <- tax_body[(cr_start[1] + 1):cr_end]
            cr_lines <- cr_lines[nchar(trimws(cr_lines)) > 0]
            cr_df    <- parse_lite_block(c(cr_lines[grepl("^\t\t", cr_lines)][1],
                                           cr_lines[!grepl("^\t\t", cr_lines)]))
            if (!is.null(cr_df)) {
              cr_rows <- apply(cr_df, 1, function(r) tags$tr(tagList(lapply(r, tags$td))))
              th_cr   <- tagList(c(list(tags$th("Rank")), lapply(colnames(cr_df)[-1], tags$th)))
              tax_panels[["cr_lbl"]] <- tags$div(class = "sqm-subsection-label", "Classified reads")
              tax_panels[["cr"]] <- tags$table(class = "sqm-table",
                tags$thead(tags$tr(th_cr)), tags$tbody(tagList(cr_rows)))
            }
          }

          if (length(ma_start) > 0) {
            ma_lines <- tax_body[(ma_start[1] + 1):length(tax_body)]
            ma_lines <- ma_lines[nchar(trimws(ma_lines)) > 0]
            ma_df    <- parse_lite_block(c(ma_lines[grepl("^\t\t", ma_lines)][1],
                                           ma_lines[!grepl("^\t\t", ma_lines)]))
            if (!is.null(ma_df)) {
              # italicise species row
              sp_row <- which(trimws(ma_df[, 1]) == "Species")
              if (length(sp_row) > 0)
                ma_df[sp_row, -1] <- lapply(ma_df[sp_row, -1], function(v) paste0("<em>", v, "</em>"))
              ma_rows <- apply(ma_df, 1, function(r) tags$tr(tagList(lapply(r, tags$td))))
              th_ma   <- tagList(c(list(tags$th("Rank")), lapply(colnames(ma_df)[-1], tags$th)))
              tax_panels[["ma_lbl"]] <- tags$div(class = "sqm-subsection-label", "Most abundant taxa (ignoring Unclassified)")
              tax_panels[["ma"]] <- tags$table(class = "sqm-table",
                tags$thead(tags$tr(th_ma)), tags$tbody(tagList(ma_rows)))
            }
          }

          if (length(tax_panels) > 0)
            panels[["TAXONOMY"]] <- sqm_section("Taxonomy", tagList(tax_panels))
        }

        # ── FUNCTIONS section ──
        if (length(fun_start) > 0) {
          fun_body <- raw[(fun_start[1] + 1):length(raw)]
          fun_body <- fun_body[nchar(trimws(fun_body)) > 0]
          # Remove "Classified ORFs:" label line
          fun_body <- fun_body[!grepl("Classified ORFs", fun_body)]
          fun_df   <- parse_lite_block(c(fun_body[grepl("^\t\t", fun_body)][1],
                                         fun_body[!grepl("^\t\t", fun_body)]))
          if (!is.null(fun_df)) {
            fun_rows <- apply(fun_df, 1, function(r) tags$tr(tagList(lapply(r, tags$td))))
            th_fun   <- tagList(c(list(tags$th("Database")), lapply(colnames(fun_df)[-1], tags$th)))
            panels[["FUNCTIONS"]] <- sqm_section("Functions",
              tags$div(class = "sqm-subsection-label", "Classified ORFs"),
              tags$table(class = "sqm-table",
                tags$thead(tags$tr(th_fun)), tags$tbody(tagList(fun_rows))))
          }
        }

        panels[["note"]] <- tags$div(
          style = "font-size:0.75rem;color:var(--muted);margin-top:8px;font-family:'IBM Plex Mono',monospace;",
          "\u2139 Loaded as SQMlite \u2014 contig, ORF and bin details not available.")
      }
    }

    # ── Samples (both object types) ──
    samples <- tryCatch(proj$samples, error=function(e) NULL)
    if (!is.null(samples)) panels[["samples"]] <- sqm_section("Samples",
      tags$div(style="padding-top:2px;", tagList(lapply(samples,function(s) tags$span(class="project-badge",s)))))

    tagList(panels)
  })
  output$plot_controls_ui <- renderUI({
    pt <- input$plot_type; if (is.null(pt)) return(NULL)
    rank_choices <- c("Phylum"="phylum","Class"="class","Order"="order","Family"="family","Genus"="genus","Species"="species")
    if (pt == "taxonomy_bar") {
      tax_counts <- if (!is.null(sqm_data())) available_tax_counts(sqm_data()) else c("Percentage (percent)"="percent")
      avail_ranks <- if (!is.null(sqm_data())) available_tax_ranks(sqm_data()) else rank_choices
      tagList(
        tags$div(class="sidebar-box",
          tags$div(class="form-label","Taxonomic rank"), selectInput("tax_rank",NULL,choices=avail_ranks),
          if (is_sqm_full()) tagList(
            tags$div(class="form-label",style="margin-top:4px;","Search taxa"),
            tags$div(class="func-search-box", tags$span(class="search-icon","\U0001f50d"),
              textInput("tax_search",NULL,placeholder="")),
            tags$div(class="func-search-hint","Comma-separated. Empty \u2192 top N taxa."),
            uiOutput("tax_search_status")
          ) else tags$div(class="func-search-hint",style="color:#c0392b;","\u26a0 Taxonomy search requires a full SQM object.")
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(class="form-label","Count type"),
          selectInput("tax_count",NULL,choices=tax_counts,selected=if("percent"%in%tax_counts)"percent" else tax_counts[[1]]),
          tags$div(class="form-label",style="margin-top:4px;","No. of taxa"),
          numericInput("n_taxa",NULL,value=15,min=1,max=200,step=1)
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:0;",
            checkboxInput("tax_ignore_unmapped","Ignore unmapped",value=FALSE),
            checkboxInput("tax_ignore_unclassified","Ignore unclassified",value=FALSE),
            checkboxInput("tax_no_partial_classifications","No partial classif.",value=FALSE),
            checkboxInput("tax_rescale","Rescale",value=FALSE)
          )
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(style="font-family:'IBM Plex Mono',monospace;font-size:0.68rem;font-weight:600;letter-spacing:0.08em;text-transform:uppercase;color:var(--blue);margin-bottom:5px;","Format plot"),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:4px;",
            tags$div(tags$div(class="form-label","Width (px)"),  numericInput("tax_plot_width", NULL,value=800,min=200,max=3000,step=50)),
            tags$div(tags$div(class="form-label","Height (px)"), numericInput("tax_plot_height",NULL,value=560,min=200,max=3000,step=50))
          ),
          tags$div(class="form-label",style="margin-top:4px;","Max scale value"),
          numericInput("tax_max_scale_value",NULL,value=NA,min=0,step=1),
          tags$div(class="form-label",style="margin-top:4px;","Font size"),
          numericInput("tax_base_size",NULL,value=11,min=6,max=24,step=1)
        )
      )
    } else if (pt %in% c("func_cog","func_kegg","func_pfam")) {
      fun_label <- switch(pt,func_cog="COG",func_kegg="KEGG",func_pfam="PFAM")
      tagList(
        tags$div(class="sidebar-box",
          if (is_sqm_full()) tagList(
            tags$div(class="form-label",paste("Search",fun_label,"functions")),
            tags$div(class="func-search-box", tags$span(class="search-icon","\U0001f50d"),
              textInput("func_search",NULL,placeholder=paste0("e.g. ",switch(pt,func_cog="COG0001, transport",func_kegg="K00001, ribosome",func_pfam="PF00001, kinase")))),
            tags$div(class="func-search-hint","Comma-separated. Empty \u2192 top N functions."),
            uiOutput("func_search_status")
          ) else tags$div(class="func-search-hint",style="color:#c0392b;","\u26a0 Function search requires a full SQM object.")
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(class="form-label","Count type"), uiOutput("func_count_ui"),
          tags$div(class="form-label",style="margin-top:4px;","No. of functions"), uiOutput("n_funcs_ui")
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(style="font-family:'IBM Plex Mono',monospace;font-size:0.68rem;font-weight:600;letter-spacing:0.08em;text-transform:uppercase;color:var(--blue);margin-bottom:5px;","Format plot"),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:4px;",
            tags$div(tags$div(class="form-label","Width (px)"),  numericInput("func_plot_width", NULL,value=800,min=200,max=3000,step=50)),
            tags$div(tags$div(class="form-label","Height (px)"), numericInput("func_plot_height",NULL,value=560,min=200,max=3000,step=50))
          ),
          tags$div(class="form-label",style="margin-top:4px;","Font size"),
          numericInput("func_base_size",NULL,value=11,min=6,max=24,step=1)
        )
      )
    } else NULL
  })
  output$func_search_status <- renderUI({
    pt <- input$plot_type; req(pt %in% c("func_cog","func_kegg","func_pfam")); req(sqm_data())
    pattern <- build_func_pattern(input$func_search %||% ""); if (is.null(pattern)) return(NULL)
    fun_level <- switch(pt,func_cog="COG",func_kegg="KEGG",func_pfam="PFAM")
    n <- tryCatch({ ps <- subsetFun(sqm_data(),fun=pattern,ignore_case=TRUE,fixed=FALSE); nrow(ps$functions[[fun_level]]$abund) }, error=function(e) 0L)
    if (n==0) tags$div(class="func-nomatch-badge","\u2715 No matches")
    else tags$div(class="func-match-badge",paste0("\u2713 ",n," function",if(n!=1)"s" else ""))
  })
  output$tax_search_status <- renderUI({
    req(input$plot_type=="taxonomy_bar"); req(sqm_data())
    search_text <- trimws(input$tax_search %||% ""); if (nchar(search_text)==0) return(NULL)
    rank <- input$tax_rank %||% "phylum"
    all_taxa <- tryCatch(rownames(sqm_data()$taxa[[rank]]$abund), error=function(e) character(0))
    terms <- trimws(unlist(strsplit(search_text,"[,;]+")));  terms <- terms[nchar(terms)>0]
    matched <- unique(unlist(lapply(terms,function(t) all_taxa[grepl(t,all_taxa,ignore.case=TRUE)])))
    if (length(matched)==0) tags$div(class="func-nomatch-badge","\u2715 No matches")
    else tags$div(class="func-match-badge",paste0("\u2713 ",length(matched)," taxon",if(length(matched)!=1)"a" else ""))
  })
  output$n_funcs_ui   <- renderUI({ numericInput("n_funcs",NULL,value=20,min=1,max=200,step=1) })
  output$func_count_ui <- renderUI({
    pt <- input$plot_type; req(pt %in% c("func_cog","func_kegg","func_pfam"))
    fun_level <- switch(pt,func_cog="COG",func_kegg="KEGG",func_pfam="PFAM")
    counts <- if (!is.null(sqm_data())) available_func_counts(sqm_data(),fun_level) else c("Copy number (copy_number)"="copy_number")
    selectInput("func_count",NULL,choices=counts,selected=if("copy_number"%in%counts)"copy_number" else counts[[1]])
  })
  output$sqm_plot_ui <- renderUI({
    pt <- input$plot_type
    is_tax  <- !is.null(pt) && pt=="taxonomy_bar"
    is_func <- !is.null(pt) && pt %in% c("func_cog","func_kegg","func_pfam")
    h <- if(is_tax) input$tax_plot_height %||% 560 else if(is_func) input$func_plot_height %||% 560 else 560
    w <- if(is_tax) input$tax_plot_width  %||% 800 else if(is_func) input$func_plot_width  %||% 800 else NULL
    style <- if(!is.null(w)) paste0("width:",w,"px; overflow-x:auto;") else "width:100%;"
    tags$div(style=style,
      plotOutput("sqm_plot", width=if(!is.null(w)) paste0(w,"px") else "100%", height=paste0(h,"px")))
  })
  plot_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data(); pt <- input$plot_type
    req(!is.null(pt) && pt != "none")
    if (pt=="taxonomy_bar") {
      search_text <- if(is_sqm_full()) trimws(input$tax_search %||% "") else ""
      if (nchar(search_text)>0) {
        rank <- input$tax_rank %||% "phylum"
        all_taxa <- tryCatch(rownames(proj$taxa[[rank]]$abund),error=function(e) character(0))
        terms <- trimws(unlist(strsplit(search_text,"[,;]+")));  terms <- terms[nchar(terms)>0]
        matched <- unique(unlist(lapply(terms,function(t) all_taxa[grepl(t,all_taxa,ignore.case=TRUE)])))
        if (length(matched)==0) { showNotification(paste0("No taxa found matching: \"",search_text,"\""),type="warning",duration=5); return(NULL) }
        proj_sub <- tryCatch(subsetTax(proj,rank=rank,tax=matched),error=function(e) NULL)
        if (is.null(proj_sub)) { showNotification("subsetTax failed.",type="error",duration=5); return(NULL) }
        plotTaxonomy(proj_sub,rank=rank,count=input$tax_count,N=input$n_taxa,base_size=input$tax_base_size %||% 11,
          ignore_unmapped=isTRUE(input$tax_ignore_unmapped),ignore_unclassified=isTRUE(input$tax_ignore_unclassified),
          no_partial_classifications=isTRUE(input$tax_no_partial_classifications),rescale=isTRUE(input$tax_rescale),
          max_scale_value=if(is.na(input$tax_max_scale_value)) NULL else input$tax_max_scale_value)
      } else {
        plotTaxonomy(proj,rank=input$tax_rank,count=input$tax_count,N=input$n_taxa,base_size=input$tax_base_size %||% 11,
          ignore_unmapped=isTRUE(input$tax_ignore_unmapped),ignore_unclassified=isTRUE(input$tax_ignore_unclassified),
          no_partial_classifications=isTRUE(input$tax_no_partial_classifications),rescale=isTRUE(input$tax_rescale),
          max_scale_value=if(is.na(input$tax_max_scale_value)) NULL else input$tax_max_scale_value)
      }
    } else if (pt %in% c("func_cog","func_kegg","func_pfam")) {
      fun_level <- switch(pt,func_cog="COG",func_kegg="KEGG",func_pfam="PFAM")
      req(nchar(input$func_count %||% "") > 0)
      req(!is.null(input$n_funcs))
      search_text <- if(is_sqm_full()) trimws(input$func_search %||% "") else ""
      if (nchar(search_text)>0) {
        pattern <- build_func_pattern(search_text)
        proj_sub <- tryCatch(subsetFun(proj,fun=pattern,ignore_case=TRUE,fixed=FALSE),error=function(e) NULL)
        n_matches <- if(!is.null(proj_sub)) tryCatch(nrow(proj_sub$functions[[fun_level]]$abund),error=function(e) 0L) else 0L
        if (n_matches==0) { showNotification(paste0("No ",fun_level," functions found matching: \"",search_text,"\""),type="warning",duration=5); return(NULL) }
        plotFunctions(proj_sub,fun_level=fun_level,count=input$func_count,N=input$n_funcs,base_size=input$func_base_size %||% 11)
      } else {
        plotFunctions(proj,fun_level=fun_level,count=input$func_count,N=input$n_funcs,base_size=input$func_base_size %||% 11)
      }
    } else if (pt=="bins") { plotBins(proj) }
  })
  output$sqm_plot <- renderPlot({ plot_reactive() }, bg="#ffffff")
  output$plot_status_badge <- renderUI({
    if (is.null(sqm_data())) tags$span(class="badge",style="background:#eef2f7;color:#7a90a8;font-size:0.72rem;border:1px solid #d0dae6;","No project")
    else tags$span(class="badge",style="background:rgba(26,158,110,0.1);color:#1a9e6e;font-size:0.72rem;border:1px solid rgba(26,158,110,0.3);","\u25cf Ready")
  })
  output$download_plot <- downloadHandler(
    filename = function() paste0("sqm_plot_",Sys.Date(),".png"),
    content  = function(file) {
      pt <- isolate(input$plot_type)
      is_tax  <- !is.null(pt) && pt=="taxonomy_bar"
      is_func <- !is.null(pt) && pt %in% c("func_cog","func_kegg","func_pfam")
      w <- if(is_tax) isolate(input$tax_plot_width  %||% 800) else if(is_func) isolate(input$func_plot_width  %||% 800) else 1400
      h <- if(is_tax) isolate(input$tax_plot_height %||% 560) else if(is_func) isolate(input$func_plot_height %||% 560) else 900
      png(file,width=w,height=h,res=150,bg="#ffffff"); print(plot_reactive()); dev.off()
    }
  )
  output$table_sample_filter <- renderUI({
    req(sqm_data())
    tt <- active_table() %||% ""
    if (!startsWith(tt, "tax_") && !startsWith(tt, "fun_")) return(NULL)
    samples <- tryCatch(sqm_data()$samples, error = function(e) NULL)
    req(samples)
    tagList(
      tags$hr(class = "section-divider"),
      tags$div(class = "form-label", "Filter samples"),
      checkboxGroupInput("selected_samples", NULL, choices = samples, selected = samples)
    )
  })
  get_table_data <- reactive({
    req(sqm_data())
    proj <- sqm_data()
    tt   <- active_table()
    req(!is.null(tt) && tt != "none")
    smp  <- input$selected_samples
    tryCatch({
      if      (tt == "contigs") as.data.frame(proj$contigs$table)
      else if (tt == "orfs")    as.data.frame(proj$orfs$table)
      else if (tt == "bins")    as.data.frame(proj$bins$table)
      else if (startsWith(tt, "tax_")) {
        rank <- sub("^tax_", "", tt)
        d <- as.data.frame(proj$taxa[[rank]]$abund)
        if (!is.null(smp) && length(smp) > 0) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
      else if (startsWith(tt, "fun_")) {
        db <- toupper(sub("^fun_", "", tt))
        d  <- as.data.frame(proj$functions[[db]]$abund)
        if (!is.null(smp) && length(smp) > 0) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
    }, error = function(e) NULL)
  })
  output$data_table <- renderDT({
    df <- get_table_data(); req(df)
    tt <- active_table()
    row_label <- if      (tt == "contigs")          "Contig"
                 else if (tt == "orfs")             "ORF"
                 else if (tt == "bins")             "Bin"
                 else if (startsWith(tt, "tax_"))   "Taxon"
                 else if (startsWith(tt, "fun_"))   "Function"
                 else                               ""
    # Set row names as a proper column with the right header
    df <- cbind(setNames(data.frame(rownames(df), stringsAsFactors=FALSE), row_label), df)
    datatable(df, rownames=FALSE,
      options=list(pageLength=20, scrollX=TRUE, dom="lfrtip"),
      class="compact hover stripe")
  })
  output$download_table <- downloadHandler(
    filename = function() paste0("sqm_", isolate(active_table()) %||% "table", "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- get_table_data(); req(df)
      tt <- isolate(active_table())
      row_label <- if      (tt == "contigs")        "Contig"
                   else if (tt == "orfs")           "ORF"
                   else if (tt == "bins")           "Bin"
                   else if (startsWith(tt, "tax_")) "Taxon"
                   else if (startsWith(tt, "fun_")) "Function"
                   else                             ""
      df <- cbind(setNames(data.frame(rownames(df), stringsAsFactors=FALSE), row_label), df)
      write.csv(df, file, row.names=FALSE)
    }
  )
  # ─────────────────────────────────────────
  #  KRONA
  # ─────────────────────────────────────────
  krona_file   <- reactiveVal(NULL)
  krona_status <- reactiveVal("idle")
  kt_available <- reactive({
    system("ktImportText", ignore.stdout=TRUE, ignore.stderr=TRUE) == 0
  })
  output$krona_ktcheck_ui <- renderUI({
    if (kt_available()) {
      tags$div(style="font-size:0.8rem;",
        tags$span(style="color:#1a9e6e;margin-right:5px;","\u25cf"),
        tags$span(style="color:#7a90a8;","KronaTools: "),
        tags$span(style="color:#1a9e6e;font-weight:600;","AVAILABLE"))
    } else {
      tagList(
        tags$div(style="font-size:0.8rem;",
          tags$span(style="color:#c0392b;margin-right:5px;","\u2715"),
          tags$span(style="color:#7a90a8;","KronaTools: "),
          tags$span(style="color:#c0392b;font-weight:600;","NOT FOUND")),
        tags$div(class="path-info",style="margin-top:4px;color:#c0392b;",
          "ktImportText must be in PATH. ",
          tags$a(href="https://github.com/marbl/Krona",target="_blank",style="color:#1a6eb5;","Install KronaTools"))
      )
    }
  })
  output$krona_sample_filter_ui <- renderUI({
    req(sqm_data()); samples <- tryCatch(sqm_data()$samples,error=function(e) NULL); req(samples)
    checkboxGroupInput("krona_samples",NULL,choices=samples,selected=samples)
  })
  observeEvent(input$do_krona, {
    req(sqm_data())
    if (!kt_available()) { showNotification("ktImportText not found. Please install KronaTools.",type="error",duration=8); return() }
    krona_status("generating"); krona_file(NULL)
    tryCatch({
      proj <- sqm_data(); all_samples <- proj$samples; sel_samples <- input$krona_samples
      if (!is.null(sel_samples) && !setequal(sel_samples,all_samples)) proj <- subsetSamples(proj,sel_samples)
      out_file <- file.path(tempdir(),paste0("sqmxplore_krona_",format(Sys.time(),"%Y%m%d%H%M%S"),".html"))
      exportKrona(proj, output_name=out_file)
      if (file.exists(out_file)) { krona_file(out_file); krona_status("ready") }
      else { krona_status("error"); showNotification("Krona file was not generated.",type="error",duration=8) }
    }, error=function(e) { krona_status("error"); showNotification(paste("Krona error:",e$message),type="error",duration=10) })
  })
  output$krona_status_ui <- renderUI({
    s <- krona_status()
    col <- switch(s,idle="#7a90a8",generating="#3b9ede",ready="#1a9e6e",error="#c0392b")
    ico <- switch(s,idle="\u25cb",generating="\u25cc",ready="\u25cf",error="\u2715")
    lbl <- switch(s,idle="IDLE",generating="GENERATING\u2026",ready="READY",error="ERROR")
    tags$div(style="font-size:0.8rem;",
      tags$span(style=paste0("color:",col,";margin-right:5px;"),ico),
      tags$span(style="color:#7a90a8;","Status: "),
      tags$span(style=paste0("color:",col,";font-weight:600;"),lbl))
  })
  output$krona_badge_ui <- renderUI({
    s <- krona_status()
    if (s=="ready") tags$span(class="badge",style="background:rgba(26,158,110,0.1);color:#1a9e6e;font-size:0.72rem;border:1px solid rgba(26,158,110,0.3);","\u25cf Ready")
    else if (s=="generating") tags$span(class="badge",style="background:rgba(59,158,222,0.1);color:#3b9ede;font-size:0.72rem;border:1px solid rgba(59,158,222,0.3);","\u25cc Generating\u2026")
    else tags$span(class="badge",style="background:#eef2f7;color:#7a90a8;font-size:0.72rem;border:1px solid #d0dae6;","No chart")
  })
  output$krona_view_ui <- renderUI({
    kf <- krona_file()
    if (is.null(kf)||!file.exists(kf)) return(tags$div(
      style="color:var(--muted);font-size:0.85rem;padding:2rem;text-align:center;",
      tags$div(style="font-size:2rem;margin-bottom:8px;","\U0001f310"),
      tags$div("Select samples and click ",tags$strong("Generate Krona")," to build the chart.")))
    static_name <- paste0("krona_",basename(kf))
    addResourcePath(static_name, dirname(kf))
    # Read Krona HTML and patch it so the top bar is not clipped inside the iframe
    html_raw <- paste(readLines(kf, warn = FALSE), collapse = "
")
    # Krona uses position:fixed for #options — change to position:absolute so it
    # stays within the iframe document flow and is never clipped by the frame edge
    patch_css <- paste0(
      "<style>",
      "#options { position: absolute !important; top: 0 !important; }",
      "body { padding-top: 0 !important; margin-top: 0 !important; }",
      "canvas { margin-top: 0 !important; }",
      "</style>"
    )
    html_patched <- sub("</head>", paste0(patch_css, "</head>"), html_raw, fixed = TRUE)
    # Encode as data URI and serve via srcdoc to avoid cross-origin issues
    tags$iframe(
      srcdoc = html_patched,
      style  = "width:100%; height:760px; border:none; display:block;",
      id     = "krona_iframe"
    )
  })
  output$krona_download_ui <- renderUI({
    req(krona_status()=="ready")
    downloadButton("download_krona","Download HTML",class="btn-outline-secondary w-100")
  })
  output$download_krona <- downloadHandler(
    filename = function() paste0("krona_",Sys.Date(),".html"),
    content  = function(file) { kf<-krona_file(); req(kf,file.exists(kf)); file.copy(kf,file) }
  )
}
shinyApp(ui = ui, server = server)
