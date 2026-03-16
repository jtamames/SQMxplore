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
#  HELPERS UI
# ─────────────────────────────────────────────
stat_card <- function(id, label, icon_char = "◈") {
  card(
    class = "stat-card",
    card_body(
      class = "p-3",
      tags$div(class = "stat-icon", icon_char),
      tags$div(class = "stat-value", textOutput(id, inline = TRUE)),
      tags$div(class = "stat-label", label)
    )
  )
}

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

.stat-card { border-left: 3px solid var(--blue) !important; }
.stat-icon { font-size: 1.4rem; color: var(--blue); margin-bottom: 4px; }
.stat-value {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 1.6rem;
  font-weight: 600;
  line-height: 1;
  color: var(--text);
}
.stat-label {
  font-size: 0.72rem;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  margin-top: 4px;
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

/* ── Function search box ── */
.func-search-box {
  position: relative;
}
.func-search-box .search-icon {
  position: absolute;
  left: 8px;
  top: 50%;
  transform: translateY(-50%);
  color: var(--muted);
  font-size: 0.8rem;
  pointer-events: none;
  z-index: 10;
}
.func-search-box .form-control {
  padding-left: 26px !important;
}
.func-search-hint {
  font-size: 0.7rem;
  color: var(--muted);
  margin-top: 3px;
  line-height: 1.4;
}
.func-match-badge {
  display: inline-block;
  background: rgba(26,158,110,0.1);
  border: 1px solid rgba(26,158,110,0.3);
  color: #1a9e6e;
  border-radius: 4px;
  padding: 1px 7px;
  font-size: 0.72rem;
  font-family: 'IBM Plex Mono', monospace;
  margin-top: 4px;
}
.func-nomatch-badge {
  display: inline-block;
  background: rgba(192,57,43,0.08);
  border: 1px solid rgba(192,57,43,0.25);
  color: #c0392b;
  border-radius: 4px;
  padding: 1px 7px;
  font-size: 0.72rem;
  font-family: 'IBM Plex Mono', monospace;
  margin-top: 4px;
}

::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--blue); }
"

# ─────────────────────────────────────────────
#  HELPER: build regex pattern for subsetFun
#  from free text with comma/semicolon-separated terms
# ─────────────────────────────────────────────
build_func_pattern <- function(search_text) {
  # Returns NULL if empty, or a regex pattern "term1|term2|..."
  search_text <- trimws(search_text)
  if (nchar(search_text) == 0) return(NULL)

  terms <- trimws(unlist(strsplit(search_text, "[,;]+")))
  terms <- terms[nchar(terms) > 0]
  if (length(terms) == 0) return(NULL)

  # Escape special regex characters so each term is treated as literal
  escaped <- gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", terms)
  paste(escaped, collapse = "|")
}

# ─────────────────────────────────────────────
#  HELPER: detect which count types are available
#  in the object for a given functional level
# ─────────────────────────────────────────────
available_func_counts <- function(proj, fun_level) {
  all_counts <- c(
    "Raw abundances (abund)"     = "abund",
    "Percentages (percent)"      = "percent",
    "Base counts (bases)"        = "bases",
    "CPM (cpm)"                  = "cpm",
    "TPM (tpm)"                  = "tpm",
    "Copy number (copy_number)"  = "copy_number"
  )
  Filter(function(ct) {
    tbl <- tryCatch(proj$functions[[fun_level]][[ct]], error = function(e) NULL)
    !is.null(tbl) && (is.matrix(tbl) || is.data.frame(tbl)) && nrow(tbl) > 0
  }, all_counts)
}

available_tax_counts <- function(proj) {
  all_counts <- c(
    "Percentages (percent)"  = "percent",
    "Raw abundances (abund)" = "abund"
  )
  Filter(function(ct) {
    # Check at phylum level, which is always present
    tbl <- tryCatch(proj$taxa$phylum[[ct]], error = function(e) NULL)
    !is.null(tbl) && (is.matrix(tbl) || is.data.frame(tbl)) && nrow(tbl) > 0
  }, all_counts)
}

# ─────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────
ui <- page_navbar(
  title = "SQMxplore",
  theme = sqm_theme,

  navbar_options = navbar_options(theme = "dark", bg = "#0e4a82"),

  header = tagList(
    useShinyjs(),
    tags$head(tags$style(HTML(custom_css)))
  ),

  # ══════════════════════════════════════════
  #  TAB 1 — PROJECT
  # ══════════════════════════════════════════
  nav_panel(
    "Project",

    layout_sidebar(
      sidebar = sidebar(
        width = 300, open = TRUE,

        tags$div(class = "form-label mt-1", "Project directory"),
        shinyDirButton(
          "dir_project", "Select directory", "Choose the project directory",
          multiple = FALSE, class = "btn-default w-100 mb-1"
        ),
        tags$div(class = "path-info", textOutput("path_project", inline = TRUE)),

        uiOutput("project_info_ui"),
        uiOutput("manual_tables_ui"),

        hr(class = "section-divider"),

        actionButton("load_project", "Load", class = "btn-primary w-100 mb-2"),

        hr(class = "section-divider"),
        uiOutput("project_status_ui")
      ),

      fluidRow(
        column(3, stat_card("stat_contigs", "Contigs",  "⬡")),
        column(3, stat_card("stat_orfs",    "ORFs",     "◈")),
        column(3, stat_card("stat_samples", "Samples",  "◉")),
        column(3, stat_card("stat_taxa",    "Taxa",     "✦"))
      ),

      br(),

      fluidRow(
        column(12,
          card(
            card_header("Samples"),
            card_body(uiOutput("samples_badges"))
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════
  #  TAB 2 — PLOTS
  # ══════════════════════════════════════════
  nav_panel(
    "Plots",

    layout_sidebar(
      sidebar = sidebar(
        width = 270,

        tags$div(class = "form-label mt-1", "Plot type"),
        selectInput("plot_type", NULL,
          choices = c(
            "Taxonomy (barplot)"  = "taxonomy_bar",
            "COG functions"       = "func_cog",
            "KEGG functions"      = "func_kegg",
            "PFAM functions"      = "func_pfam",
            "Binning"             = "bins"
          )
        ),

        hr(class = "section-divider"),
        uiOutput("plot_controls_ui"),
        hr(class = "section-divider"),

        actionButton("do_plot", "Generate plot", class = "btn-primary w-100 mt-1"),
        br(), br(),
        downloadButton("download_plot", "Download PNG", class = "btn-outline-secondary w-100")
      ),

      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            span("Visualization"),
            uiOutput("plot_status_badge")
          )
        ),
        card_body(class = "p-2", plotOutput("sqm_plot", height = "560px"))
      )
    )
  ),

  # ══════════════════════════════════════════
  #  TAB 3 — TABLES
  # ══════════════════════════════════════════
  nav_panel(
    "Tables",

    layout_sidebar(
      sidebar = sidebar(
        width = 270,

        tags$div(class = "form-label mt-1", "Table"),
        selectInput("table_type", NULL,
          choices = c(
            "Contigs"             = "contigs",
            "ORFs"                = "orfs",
            "Taxonomy — phylum"   = "tax_phylum",
            "Taxonomy — genus"    = "tax_genus",
            "Functions — COG"     = "fun_cog",
            "Functions — KEGG"    = "fun_kegg"
          )
        ),

        hr(class = "section-divider"),
        tags$div(class = "form-label", "Filter samples"),
        uiOutput("table_sample_filter"),

        hr(class = "section-divider"),
        downloadButton("download_table", "Download CSV", class = "btn-outline-secondary w-100")
      ),

      card(
        card_header("Data"),
        card_body(class = "p-2", DTOutput("data_table"))
      )
    )
  )
)

# ─────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────
server <- function(input, output, session) {

  roots    <- c(home = normalizePath("~"), root = "/")
  sqm_data <- reactiveVal(NULL)
  status   <- reactiveVal("idle")

  tables_path  <- reactiveVal(NULL)
  need_manual  <- reactiveVal(FALSE)
  creator_name <- reactiveVal(NULL)
  is_sqm_full  <- reactiveVal(FALSE)   # TRUE only when loaded with loadSQM

  # ── Dir choosers ──────────────────────────
  shinyDirChoose(input, "dir_project",      roots = roots)
  shinyDirChoose(input, "dir_manual_tables", roots = roots)

  path_project <- reactive({
    req(input$dir_project)
    parseDirPath(roots, input$dir_project)
  })

  output$path_project <- renderText({
    tryCatch(path_project(), error = function(e) "")
  })

  # ── Read creator.txt and resolve tables path ──
  observeEvent(path_project(), {
    proj_dir <- path_project()
    req(nchar(proj_dir) > 0)

    need_manual(FALSE)
    tables_path(NULL)
    creator_name(NULL)

    creator_file <- file.path(proj_dir, "creator.txt")

    if (file.exists(creator_file)) {
      creator <- trimws(readLines(creator_file, n = 1, warn = FALSE))
      creator_name(creator)

      if (grepl("SqueezeMeta", creator, ignore.case = TRUE)) {
        tables_path(proj_dir)
      } else {
        tp <- file.path(proj_dir, "tables")
        if (dir.exists(tp)) {
          tables_path(tp)
        } else {
          need_manual(TRUE)
        }
      }
    } else {
      need_manual(TRUE)
      showNotification(
        "creator.txt not found. Please select the tables directory manually.",
        type = "warning", duration = 6
      )
    }
  })

  observeEvent(input$dir_manual_tables, {
    tp <- tryCatch(parseDirPath(roots, input$dir_manual_tables), error = function(e) NULL)
    req(tp)
    if (nchar(tp) > 0) {
      tables_path(tp)
      need_manual(FALSE)
    }
  })

  output$project_info_ui <- renderUI({
    req(path_project())
    proj_dir     <- path_project()
    creator_file <- file.path(proj_dir, "creator.txt")

    creator_txt <- if (file.exists(creator_file)) {
      trimws(readLines(creator_file, n = 1, warn = FALSE))
    } else {
      "unknown"
    }

    tp <- tables_path()

    tagList(
      tags$div(
        class = "path-info",
        tags$span(style = "color:#7a90a8;", "Created by: "),
        tags$span(style = "color:#1a6eb5; font-weight:600;", creator_txt)
      ),
      if (!is.null(tp)) {
        tags$div(
          class = "path-info",
          tags$span(style = "color:#7a90a8;", "Tables: "),
          tp,
          if (dir.exists(tp)) {
            tags$span(style = "color:#1a9e6e; margin-left:4px;", "✓")
          } else {
            tags$span(style = "color:#c0392b; margin-left:4px;", "✕ not found")
          }
        )
      }
    )
  })

  output$manual_tables_ui <- renderUI({
    req(need_manual())
    tagList(
      hr(class = "section-divider"),
      tags$div(
        class = "path-info",
        style = "color:#c0392b;",
        "Tables directory could not be found automatically."
      ),
      tags$div(class = "form-label", "Select tables directory"),
      shinyDirButton(
        "dir_manual_tables", "Select tables", "Choose the tables directory",
        multiple = FALSE, class = "btn-default w-100 mb-1"
      )
    )
  })

  # ── Load project ──────────────────────────
  observeEvent(input$load_project, {
    tp <- tables_path()

    if (is.null(tp) || !dir.exists(tp)) {
      showNotification(
        "Directory not available. Please select it manually.",
        type = "error", duration = 8
      )
      return()
    }

    status("loading")
    tryCatch({
      is_sqm <- grepl("SqueezeMeta", creator_name() %||% "", ignore.case = TRUE)
      proj <- if (is_sqm) loadSQM(tp) else loadSQMlite(tp)
      sqm_data(proj)
      is_sqm_full(is_sqm)
      status("ready")
    }, error = function(e) {
      status("error")
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })

  # ── Status ────────────────────────────────
  output$project_status_ui <- renderUI({
    s   <- status()
    col <- switch(s, idle = "#7a90a8", loading = "#3b9ede", ready = "#1a9e6e", error = "#c0392b")
    ico <- switch(s, idle = "○", loading = "◌", ready = "●", error = "✕")
    tags$div(
      style = "font-size:0.8rem;",
      tags$span(style = paste0("color:", col, "; margin-right:5px;"), ico),
      tags$span(style = "color:#7a90a8;", "Status: "),
      tags$span(style = paste0("color:", col, "; font-weight:600;"), toupper(s))
    )
  })

  # ── Stats ─────────────────────────────────
  output$stat_contigs <- renderText({
    req(sqm_data())
    tryCatch(format(nrow(sqm_data()$contigs$table), big.mark = ","), error = function(e) "—")
  })
  output$stat_orfs <- renderText({
    req(sqm_data())
    tryCatch(format(nrow(sqm_data()$orfs$table), big.mark = ","), error = function(e) "—")
  })
  output$stat_samples <- renderText({
    req(sqm_data())
    tryCatch(as.character(length(sqm_data()$samples)), error = function(e) "—")
  })
  output$stat_taxa <- renderText({
    req(sqm_data())
    tryCatch(format(nrow(sqm_data()$taxa$phylum$abund), big.mark = ","), error = function(e) "—")
  })

  output$samples_badges <- renderUI({
    req(sqm_data())
    samples <- tryCatch(sqm_data()$samples, error = function(e) NULL)
    req(samples)
    tagList(lapply(samples, function(s) tags$span(class = "project-badge", s)))
  })

  # ─────────────────────────────────────────
  #  Dynamic plot controls
  # ─────────────────────────────────────────
  output$plot_controls_ui <- renderUI({
    pt <- input$plot_type
    if (is.null(pt)) return(NULL)

    rank_choices <- c(
      "Phylum" = "phylum", "Class" = "class", "Order" = "order",
      "Family" = "family", "Genus" = "genus", "Species" = "species"
    )

    if (pt == "taxonomy_bar") {
      tax_counts <- if (!is.null(sqm_data())) available_tax_counts(sqm_data()) else c("Percentage (percent)" = "percent")
      tagList(
        tags$div(class = "form-label", "Taxonomic rank"),
        selectInput("tax_rank", NULL, choices = rank_choices),

        # ── Text search (full SQM object only) ──
        if (is_sqm_full()) tagList(
          tags$div(class = "form-label", "Search taxa"),
          tags$div(
            class = "func-search-box",
            tags$span(class = "search-icon", "🔍"),
            textInput("tax_search", NULL,
              placeholder = "e.g. Bacteroidota, Prevotella")
          ),
          tags$div(
            class = "func-search-hint",
            "Separate terms with commas. Partial, case-insensitive search.",
            br(),
            "Empty → show top N taxa."
          ),
          uiOutput("tax_search_status")
        ) else tags$div(
          class = "func-search-hint",
          style = "color:#c0392b;",
          "⚠ Taxonomy search requires a full SQM object (not SQMlite)."
        ),

        hr(class = "section-divider"),

        tags$div(class = "form-label", "Count type"),
        selectInput("tax_count", NULL, choices = tax_counts,
          selected = if ("percent" %in% tax_counts) "percent" else tax_counts[[1]]),
        tags$div(class = "form-label", "No. of taxa"),
        numericInput("n_taxa", NULL, value = 15, min = 1, max = 200, step = 1),
        tags$div(class = "form-label", "Font size"),
        numericInput("tax_base_size", NULL, value = 11, min = 6, max = 24, step = 1)
      )

    } else if (pt %in% c("func_cog", "func_kegg", "func_pfam")) {

      fun_label <- switch(pt,
        func_cog  = "COG",
        func_kegg = "KEGG",
        func_pfam = "PFAM"
      )

      tagList(
        # ── Text search (full SQM object only) ──
        if (is_sqm_full()) tagList(
          tags$div(class = "form-label", paste("Search", fun_label, "functions")),
          tags$div(
            class = "func-search-box",
            tags$span(class = "search-icon", "🔍"),
            textInput("func_search", NULL,
              placeholder = paste0("e.g. ", switch(pt,
                func_cog  = "COG0001, transport",
                func_kegg = "K00001, ribosome",
                func_pfam = "PF00001, kinase"
              ))
            )
          ),
          tags$div(
            class = "func-search-hint",
            "Separate terms with commas. Partial, case-insensitive search.",
            br(),
            "Empty → show top N functions."
          ),
          uiOutput("func_search_status")
        ) else tags$div(
          class = "func-search-hint",
          style = "color:#c0392b;",
          "⚠ Function search requires a full SQM object (not SQMlite)."
        ),

        hr(class = "section-divider"),

        # ── Count type ──────────────────────────
        tags$div(class = "form-label", "Count type"),
        uiOutput("func_count_ui"),

        hr(class = "section-divider"),

        uiOutput("n_funcs_ui"),

        tags$div(class = "form-label", "Font size"),
        numericInput("func_base_size", NULL, value = 11, min = 6, max = 24, step = 1)
      )

    } else {
      NULL
    }
  })

  # ── Search status badge ───────────────────
  output$func_search_status <- renderUI({
    pt <- input$plot_type
    req(pt %in% c("func_cog", "func_kegg", "func_pfam"))
    req(sqm_data())

    search_text <- input$func_search %||% ""
    pattern <- build_func_pattern(search_text)
    if (is.null(pattern)) return(NULL)

    fun_level <- switch(pt,
      func_cog  = "COG",
      func_kegg = "KEGG",
      func_pfam = "PFAM"
    )

    n_matches <- tryCatch({
      proj_sub <- subsetFun(sqm_data(), fun = pattern, ignore_case = TRUE, fixed = FALSE)
      nrow(proj_sub$functions[[fun_level]]$abund)
    }, error = function(e) 0L)

    if (n_matches == 0) {
      tags$div(class = "func-nomatch-badge", "✕ No matches")
    } else {
      tags$div(class = "func-match-badge",
        paste0("✓ ", n_matches, " function", if (n_matches != 1) "s" else "")
      )
    }
  })

  # ── Taxonomy search status badge ──────────
  output$tax_search_status <- renderUI({
    req(input$plot_type == "taxonomy_bar")
    req(sqm_data())

    search_text <- trimws(input$tax_search %||% "")
    if (nchar(search_text) == 0) return(NULL)

    rank <- input$tax_rank %||% "phylum"

    # Get all taxon names at the selected rank
    all_taxa <- tryCatch(
      rownames(sqm_data()$taxa[[rank]]$abund),
      error = function(e) character(0)
    )

    terms   <- trimws(unlist(strsplit(search_text, "[,;]+")))
    terms   <- terms[nchar(terms) > 0]
    matched <- unique(unlist(lapply(terms, function(t)
      all_taxa[grepl(t, all_taxa, ignore.case = TRUE)]
    )))

    if (length(matched) == 0) {
      tags$div(class = "func-nomatch-badge", "✕ No matches")
    } else {
      tags$div(class = "func-match-badge",
        paste0("✓ ", length(matched), " taxon", if (length(matched) != 1) "a" else "")
      )
    }
  })

  # ── Top N numeric control (always visible) ──
  output$n_funcs_ui <- renderUI({
    tagList(
      tags$div(class = "form-label", "No. of functions (top N)"),
      numericInput("n_funcs", NULL, value = 20, min = 1, max = 200, step = 1)
    )
  })

  # ── Count type selector for functions ────
  output$func_count_ui <- renderUI({
    pt <- input$plot_type
    req(pt %in% c("func_cog", "func_kegg", "func_pfam"))

    fun_level <- switch(pt, func_cog = "COG", func_kegg = "KEGG", func_pfam = "PFAM")

    counts <- if (!is.null(sqm_data())) {
      available_func_counts(sqm_data(), fun_level)
    } else {
      c("Copy number (copy_number)" = "copy_number")
    }

    selectInput("func_count", NULL, choices = counts,
      selected = if ("copy_number" %in% counts) "copy_number" else counts[[1]])
  })

  # ─────────────────────────────────────────
  #  Generate plot
  # ─────────────────────────────────────────
  plot_reactive <- eventReactive(input$do_plot, {
    req(sqm_data())
    proj <- sqm_data()
    pt   <- input$plot_type

    if (pt == "taxonomy_bar") {
      search_text <- if (is_sqm_full()) trimws(input$tax_search %||% "") else ""

      if (nchar(search_text) > 0) {
        # ── Search mode: subsetTax + plotTaxonomy ──
        rank <- input$tax_rank %||% "phylum"

        all_taxa <- tryCatch(
          rownames(proj$taxa[[rank]]$abund),
          error = function(e) character(0)
        )
        terms   <- trimws(unlist(strsplit(search_text, "[,;]+")))
        terms   <- terms[nchar(terms) > 0]
        matched <- unique(unlist(lapply(terms, function(t)
          all_taxa[grepl(t, all_taxa, ignore.case = TRUE)]
        )))

        if (length(matched) == 0) {
          showNotification(
            paste0("No taxa found matching: \"", search_text, "\""),
            type = "warning", duration = 5
          )
          return(NULL)
        }

        proj_sub <- tryCatch(
          subsetTax(proj, rank = rank, tax = matched),
          error = function(e) NULL
        )

        if (is.null(proj_sub)) {
          showNotification("subsetTax failed for the selected taxa.", type = "error", duration = 5)
          return(NULL)
        }

        plotTaxonomy(proj_sub, rank = rank, count = input$tax_count,
                     N = input$n_taxa, base_size = input$tax_base_size %||% 11)

      } else {
        # ── Top N mode ──
        plotTaxonomy(proj, rank = input$tax_rank, count = input$tax_count,
                     N = input$n_taxa, base_size = input$tax_base_size %||% 11)
      }

    } else if (pt %in% c("func_cog", "func_kegg", "func_pfam")) {

      fun_level <- switch(pt,
        func_cog  = "COG",
        func_kegg = "KEGG",
        func_pfam = "PFAM"
      )

      search_text <- if (is_sqm_full()) trimws(input$func_search %||% "") else ""

      if (nchar(search_text) > 0) {
        # ── Search mode: subsetFun + plotFunctions ──
        pattern <- build_func_pattern(search_text)

        proj_sub <- tryCatch(
          subsetFun(proj, fun = pattern, ignore_case = TRUE, fixed = FALSE),
          error = function(e) NULL
        )

        n_matches <- if (!is.null(proj_sub)) {
          tryCatch(nrow(proj_sub$functions[[fun_level]]$abund), error = function(e) 0L)
        } else 0L

        if (n_matches == 0) {
          showNotification(
            paste0("No ", fun_level, " functions found matching: \"", search_text, "\""),
            type = "warning", duration = 5
          )
          return(NULL)
        }

        plotFunctions(proj_sub, fun_level = fun_level, count = input$func_count, N = input$n_funcs, base_size = input$func_base_size %||% 11)

      } else {
        # ── Top N mode ──
        plotFunctions(proj, fun_level = fun_level, count = input$func_count, N = input$n_funcs, base_size = input$func_base_size %||% 11)
      }

    } else if (pt == "bins") {
      plotBins(proj)
    }
  })

  output$sqm_plot <- renderPlot({ plot_reactive() }, bg = "#ffffff")

  output$plot_status_badge <- renderUI({
    if (is.null(sqm_data())) {
      tags$span(class = "badge",
        style = "background:#eef2f7; color:#7a90a8; font-size:0.72rem; border:1px solid #d0dae6;",
        "No project")
    } else {
      tags$span(class = "badge",
        style = "background:rgba(26,158,110,0.1); color:#1a9e6e; font-size:0.72rem; border:1px solid rgba(26,158,110,0.3);",
        "● Ready")
    }
  })

  output$download_plot <- downloadHandler(
    filename = function() paste0("sqm_plot_", Sys.Date(), ".png"),
    content  = function(file) {
      png(file, width = 1400, height = 900, res = 150, bg = "#ffffff")
      print(plot_reactive())
      dev.off()
    }
  )

  # ── Sample filter ─────────────────────────
  output$table_sample_filter <- renderUI({
    req(sqm_data())
    samples <- tryCatch(sqm_data()$samples, error = function(e) NULL)
    req(samples)
    checkboxGroupInput("selected_samples", NULL, choices = samples, selected = samples)
  })

  # ── Table data ────────────────────────────
  get_table_data <- reactive({
    req(sqm_data())
    proj <- sqm_data()
    tt   <- input$table_type
    smp  <- input$selected_samples

    tryCatch({
      if      (tt == "contigs")    as.data.frame(proj$contigs$table)
      else if (tt == "orfs")       as.data.frame(proj$orfs$table)
      else if (tt == "tax_phylum") {
        d <- as.data.frame(proj$taxa$phylum$abund)
        if (!is.null(smp)) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
      else if (tt == "tax_genus") {
        d <- as.data.frame(proj$taxa$genus$abund)
        if (!is.null(smp)) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
      else if (tt == "fun_cog") {
        d <- as.data.frame(proj$functions$COG$abund)
        if (!is.null(smp)) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
      else if (tt == "fun_kegg") {
        d <- as.data.frame(proj$functions$KEGG$abund)
        if (!is.null(smp)) d[, colnames(d) %in% smp, drop = FALSE] else d
      }
    }, error = function(e) NULL)
  })

  output$data_table <- renderDT({
    df <- get_table_data()
    req(df)
    datatable(df,
      options = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip"),
      class = "compact hover stripe"
    )
  })

  output$download_table <- downloadHandler(
    filename = function() paste0("sqm_", input$table_type, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- get_table_data()
      req(df)
      write.csv(df, file, row.names = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
