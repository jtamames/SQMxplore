server <- function(input, output, session) {
  roots        <- c(home = normalizePath("~"), root = "/")
  sqm_data     <- reactiveVal(NULL)
  status       <- reactiveVal("idle")
  multi_dirs   <- reactiveVal(character(0))  # accumulates selected dirs for "Load multiple"
  tables_path  <- reactiveVal(NULL)
  need_manual  <- reactiveVal(FALSE)
  creator_name <- reactiveVal(NULL)
  is_sqm_full  <- reactiveVal(FALSE)

  ANALYSIS_TABS <- c("Plots", "Tables", "Pathways", "Multivariate", "Comparison", "MAG Map")


  # ── Dynamic plot type selector ──
  output$plot_category_ui <- renderUI({
    proj <- sqm_data()
    cats <- c()
    if (!is.null(proj)) {
      has_tax <- any(sapply(c("phylum","class","order","family","genus","species"), function(r)
        tryCatch(has_data(proj$taxa[[r]]$percent), error = function(e) FALSE)))
      if (has_tax) cats <- c(cats, "Taxonomy" = "taxonomy")
      dbs <- tryCatch(names(proj$functions), error = function(e) character(0))
      has_fun <- any(sapply(dbs, function(db)
        tryCatch(has_data(proj$functions[[db]]$abund), error = function(e) FALSE)))
      if (has_fun) cats <- c(cats, "Functions" = "functions")
      if (tryCatch(has_data(proj$bins$table), error = function(e) FALSE))
        cats <- c(cats, "MAGs" = "bins")
    } else {
      cats <- c("Taxonomy" = "taxonomy")
    }
    cur <- isolate(input$plot_category)
    sel <- if (!is.null(cur) && cur %in% cats) cur else cats[[1]]
    selectInput("plot_category", NULL, choices = cats, selected = sel)
  })

  output$plot_type_ui <- renderUI({
    proj <- sqm_data()
    cat  <- input$plot_category %||% "taxonomy"
    choices <- if (cat == "taxonomy") {
      c("Barplot" = "taxonomy_bar", "Heatmap" = "taxonomy_heatmap", "Krona" = "krona")
    } else if (cat == "functions") {
      ch <- c()
      if (!is.null(proj)) {
        dbs <- tryCatch(names(proj$functions), error = function(e) character(0))
        for (db in dbs) {
          tbl <- tryCatch(proj$functions[[db]]$abund, error = function(e) NULL)
          if (has_data(tbl)) {
            ch <- c(ch, setNames(paste0("func_", tolower(db)), db))
            if (toupper(db) == "COG"  && !is.null(COG_CATEGORIES)  && nrow(COG_CATEGORIES)  > 0)
              ch <- c(ch, "COG (functional classes)" = "cog_class")
            if (toupper(db) == "KEGG" && !is.null(KEGG_CATEGORIES) && nrow(KEGG_CATEGORIES) > 0)
              ch <- c(ch, "KEGG (functional classes)" = "kegg_class")
          }
        }
      }
      if (length(ch) == 0) ch <- c("(no data)" = "none")
      ch
    } else {
      c("MAGs" = "bins")
    }
    cur <- isolate(input$plot_type)
    sel <- if (!is.null(cur) && cur %in% choices) cur else choices[[1]]
    selectInput("plot_type", NULL, choices = choices, selected = sel)
  })

  # When category changes, reset plot_type to first valid option
  observeEvent(input$plot_category, ignoreInit = TRUE, {
    proj <- sqm_data()
    cat  <- input$plot_category %||% "taxonomy"
    new_pt <- if (cat == "taxonomy") "taxonomy_bar"
              else if (cat == "bins") "bins"
              else if (cat == "functions" && !is.null(proj)) {
                dbs <- tryCatch(names(proj$functions), error = function(e) character(0))
                first <- Filter(function(db)
                  tryCatch(has_data(proj$functions[[db]]$abund), error = function(e) FALSE), dbs)
                if (length(first) > 0) paste0("func_", tolower(first[[1]])) else "none"
              } else "none"
    updateSelectInput(session, "plot_type", selected = new_pt)
  })

  # Show/hide format bars based on plot type.
  # Deferred via onFlushed so shinyjs is ready before first reactive fire.
  session$onFlushed(function() {
    observeEvent(input$plot_type, {
      pt        <- input$plot_type %||% ""
      is_fn     <- startsWith(pt, "func_")
      is_tax_hm <- pt == "taxonomy_heatmap"
      if (pt == "taxonomy_bar") {
        shinyjs::show("fmt_tax"); shinyjs::hide("fmt_func"); shinyjs::hide("fmt_tax_hm")
      } else if (is_fn || pt == "cog_class" || pt == "kegg_class") {
        shinyjs::hide("fmt_tax"); shinyjs::show("fmt_func"); shinyjs::hide("fmt_tax_hm")
      } else if (is_tax_hm) {
        shinyjs::hide("fmt_tax"); shinyjs::hide("fmt_func"); shinyjs::show("fmt_tax_hm")
      } else {
        shinyjs::hide("fmt_tax"); shinyjs::hide("fmt_func"); shinyjs::hide("fmt_tax_hm")
      }
    }, ignoreNULL = FALSE)
  }, once = TRUE)


  # \u2500\u2500 Dynamic table type selector \u2014 only shows available options \u2500\u2500
  # \u2500\u2500 Build each category box (only shown when choices exist) \u2500\u2500
  make_table_box <- function(label, input_id, choices) {
    if (length(choices) == 0) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-bottom:6px;",
      tags$div(class = "form-label", label),
      selectInput(input_id, NULL, choices = choices)
    )
  }

  output$tbl_category_ui <- renderUI({
    req(sqm_data()); proj <- sqm_data()
    # Build available categories based on what data exists
    cats <- c()
    if (length(avail_assembly(proj)) > 0)  cats <- c(cats, "Assembly"  = "assembly")
    if (length(avail_taxonomy(proj))  > 0)  cats <- c(cats, "Taxa"      = "taxonomy")
    if (length(avail_functions(proj)) > 0)  cats <- c(cats, "Functions" = "functions")
    if (length(avail_bins(proj))      > 0)  cats <- c(cats, "Bins"      = "bins")
    if (length(cats) == 0) return(NULL)
    cur <- isolate(input$tbl_category)
    sel <- if (!is.null(cur) && cur %in% cats) cur else cats[[1]]
    tags$div(class = "sidebar-box",
      tags$div(class = "form-label", "Table type"),
      selectInput("tbl_category", NULL, choices = cats, selected = sel))
  })

  output$tbl_sub_controls_ui <- renderUI({
    req(sqm_data(), input$tbl_category)
    proj <- sqm_data()
    cat  <- input$tbl_category

    entries_selector <- tagList(
      tags$div(class = "form-label", style = "margin-top:4px;", "Rows per page"),
      selectInput("tbl_page_length", NULL,
        choices  = c("10" = 10, "20" = 20, "50" = 50, "100" = 100, "All" = -1),
        selected = isolate(input$tbl_page_length) %||% 20))

    if (cat == "assembly") {
      ch <- avail_assembly(proj)
      if (length(ch) == 0) return(NULL)
      tagList(
        make_table_box("Table", "tbl_assembly", ch),
        tags$div(class = "sidebar-box", entries_selector))

    } else if (cat == "taxonomy") {
      ch <- avail_taxonomy(proj)
      if (length(ch) == 0) return(NULL)
      rank0   <- if (length(ch) > 0) sub("^tax_", "", ch[[1]]) else ""
      metrics <- avail_tax_metrics(proj, rank0)
      tags$div(class = "sidebar-box",
        tags$div(class = "form-label", "Rank"),
        selectInput("tbl_taxonomy", NULL, choices = ch),
        help_label("Metric", paste0(
          "Raw abundances (abund): number of reads/features assigned. Not normalized.\n\n",
          "Percentages (percent): relative abundance as fraction of total. Removes sequencing depth bias.\n\n",
          "Base counts (bases): total bases assigned. Proportional to abundance and feature length.\n\n",
          "TPM: normalized by feature length and depth. Suitable for comparing expression levels.\n\n",
          "Copy number: estimated copies per cell equivalent. Useful for functional gene comparison."
        ), style = "margin-top:4px;"),
        selectInput("tbl_tax_metric", NULL, choices = metrics,
          selected = if (length(metrics) == 0) NULL else if ("percent" %in% metrics) "percent" else metrics[[1]]),
        entries_selector)

    } else if (cat == "functions") {
      ch <- avail_functions(proj)
      if (length(ch) == 0) return(NULL)
      db0     <- if (length(ch) > 0) resolve_db_name(proj, sub("^fun_", "", ch[[1]])) else ""
      metrics <- avail_fun_metrics(proj, db0)
      tags$div(class = "sidebar-box",
        tags$div(class = "form-label", "Database"),
        selectInput("tbl_functions", NULL, choices = ch),
        help_label("Metric", paste0(
          "Raw abundances (abund): number of reads/features assigned. Not normalized.\n\n",
          "Percentages (percent): relative abundance as fraction of total. Removes sequencing depth bias.\n\n",
          "Base counts (bases): total bases assigned. Proportional to abundance and feature length.\n\n",
          "TPM: normalized by feature length and depth. Suitable for comparing expression levels.\n\n",
          "Copy number: estimated copies per cell equivalent. Useful for functional gene comparison."
        ), style = "margin-top:4px;"),
        selectInput("tbl_fun_metric", NULL, choices = metrics,
          selected = if (length(metrics) == 0) NULL else if ("abund" %in% metrics) "abund" else metrics[[1]]),
        entries_selector)

    } else if (cat == "bins") {
      tags$div(class = "sidebar-box", entries_selector)
    }
  })

  # When category changes, auto-load the default table for that category
  observeEvent(input$tbl_category, ignoreInit = TRUE, {
    proj <- sqm_data(); req(proj)
    cat  <- input$tbl_category
    if (cat == "assembly") {
      ch <- avail_assembly(proj)
      if (length(ch) > 0) do_load_table(ch[[1]])
    } else if (cat == "taxonomy") {
      ch <- avail_taxonomy(proj)
      if (length(ch) > 0) do_load_table(ch[[1]])
    } else if (cat == "functions") {
      ch <- avail_functions(proj)
      if (length(ch) > 0) do_load_table(ch[[1]])
    } else if (cat == "bins") {
      do_load_table("bins")
    }
  })

  # \u2500\u2500 active_table: a reactiveVal updated by each selector.
  #    assembly/taxonomy/functions use ignoreInit=TRUE (multi-option selectors).
  #    bins uses a dedicated actionButton to avoid the single-option problem.
  active_tbl_rv  <- reactiveVal("none")
  tbl_status     <- reactiveVal("idle")   # idle | loading | ready
  tbl_data_rv    <- reactiveVal(NULL)     # holds the loaded data.frame

  observeEvent(input$tbl_assembly, ignoreNULL=TRUE, ignoreInit=TRUE, {
    do_load_table(input$tbl_assembly)
  })
  observeEvent(input$tbl_taxonomy, ignoreNULL=TRUE, ignoreInit=TRUE, {
    proj <- sqm_data(); req(proj)
    rank <- sub("^tax_", "", input$tbl_taxonomy)
    metrics <- avail_tax_metrics(proj, rank)
    cur <- isolate(input$tbl_tax_metric)
    sel <- if (!is.null(cur) && cur %in% metrics) cur else
           if (length(metrics) == 0) NULL else if ("percent" %in% metrics) "percent" else metrics[[1]]
    updateSelectInput(session, "tbl_tax_metric", choices = metrics, selected = sel)
    do_load_table(input$tbl_taxonomy)
  })
  observeEvent(input$tbl_functions, ignoreNULL=TRUE, ignoreInit=TRUE, {
    proj <- sqm_data(); req(proj)
    db <- resolve_db_name(proj, sub("^fun_", "", input$tbl_functions))
    metrics <- avail_fun_metrics(proj, db)
    cur <- isolate(input$tbl_fun_metric)
    sel <- if (!is.null(cur) && cur %in% metrics) cur else
           if (length(metrics) == 0) NULL else if ("abund" %in% metrics) "abund" else metrics[[1]]
    updateSelectInput(session, "tbl_fun_metric", choices = metrics, selected = sel)
    do_load_table(input$tbl_functions)
  })
  observeEvent(input$tbl_tax_metric, ignoreNULL=TRUE, ignoreInit=TRUE, {
    tt <- isolate(active_tbl_rv())
    if (!is.null(tt) && startsWith(tt, "tax_")) do_load_table(tt)
  })
  observeEvent(input$tbl_fun_metric, ignoreNULL=TRUE, ignoreInit=TRUE, {
    tt <- isolate(active_tbl_rv())
    if (!is.null(tt) && startsWith(tt, "fun_")) do_load_table(tt)
  })
  observeEvent(input$tbl_page_length, ignoreNULL=TRUE, ignoreInit=TRUE, {
    tt <- isolate(active_tbl_rv())
    if (!is.null(tt) && tt != "none") do_load_table(tt)
  })

  # Initialise on project load
  observeEvent(sqm_data(), {
    proj <- sqm_data(); req(proj)
    first <- c(avail_assembly(proj), avail_taxonomy(proj),
               avail_functions(proj), avail_bins(proj))
    if (length(first) > 0) do_load_table(first[[1]])
  })

  active_table <- reactive({ active_tbl_rv() })

  # Central loader: sets status loading, renders spinner, then loads in delay
  do_load_table <- function(tt) {
    tbl_status("loading")
    tbl_data_rv(NULL)
    shinyjs::delay(50, {
      active_tbl_rv(tt)
      proj <- sqm_data()
      smp  <- isolate(input$selected_samples)
      df <- tryCatch({
        if      (tt == "contigs") as.data.frame(proj$contigs$table)
        else if (tt == "orfs")    as.data.frame(proj$orfs$table)
        else if (tt == "bins")    as.data.frame(proj$bins$table)
        else if (startsWith(tt, "tax_")) {
          rank   <- sub("^tax_", "", tt)
          metric <- isolate(input$tbl_tax_metric) %||% "abund"
          d <- as.data.frame(proj$taxa[[rank]][[metric]])
          if (!is.null(smp) && length(smp) > 0) d[, colnames(d) %in% smp, drop=FALSE] else d
        }
        else if (startsWith(tt, "fun_")) {
          db     <- resolve_db_name(proj, sub("^fun_", "", tt))
          metric <- isolate(input$tbl_fun_metric) %||% "abund"
          d <- as.data.frame(proj$functions[[db]][[metric]])
          if (!is.null(smp) && length(smp) > 0) d <- d[, colnames(d) %in% smp, drop=FALSE]
          enrich_fun_table(proj, db, d)
        }
      }, error = function(e) NULL)
      tbl_data_rv(df)
      tbl_status("ready")
    })
  }

  shinyDirChoose(input, "dir_project",       roots = roots)
  shinyDirChoose(input, "dir_manual_tables", roots = roots)
  shinyDirChoose(input, "dir_multi_add",     roots = roots)
  path_project <- reactive({ req(input$dir_project); parseDirPath(roots, input$dir_project) })
  output$path_project <- renderText({ tryCatch(path_project(), error = function(e) "") })

  # ---- Load multiple UI ----
  output$multi_dirs_ui <- renderUI({
    if ((input$load_mode %||% "project") != "multiple") return(NULL)
    dirs <- multi_dirs()
    tagList(
      help_label("Project directories",
        "Select each SqueezeMeta project directory individually. They will be combined with combineSQM().",
        style = "margin-top:0.25rem;"),
      shinyDirButton("dir_multi_add", "Add directory", "Choose a project directory",
        multiple = FALSE, class = "btn-default w-100 mb-1"),
      if (length(dirs) > 0)
        tags$ul(style = "margin:4px 0 6px 0; padding-left:16px; font-size:0.8rem;",
          lapply(seq_along(dirs), function(i)
            tags$li(style = "display:flex; justify-content:space-between; align-items:center;",
              tags$span(style = "word-break:break-all;", basename(dirs[i])),
              actionLink(paste0("rm_dir_", i), "×",
                style = "color:#c0392b; margin-left:6px; flex-shrink:0;")
            )
          )
        ),
      if (length(dirs) == 0)
        tags$div(class = "path-info", style = "color:#7a90a8;", "No directories selected yet.")
    )
  })

  # Add a directory to the multi list
  observeEvent(input$dir_multi_add, {
    d <- tryCatch(parseDirPath(roots, input$dir_multi_add), error = function(e) NULL)
    req(d); req(nzchar(d))
    if (!d %in% multi_dirs()) multi_dirs(c(multi_dirs(), d))
  })

  # Remove buttons for each listed directory (dynamic observers)
  observe({
    dirs <- multi_dirs()
    lapply(seq_along(dirs), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("rm_dir_", idx)]], {
          current <- multi_dirs()
          if (idx <= length(current)) multi_dirs(current[-idx])
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
  })

  # Reset multi list when mode changes
  observeEvent(input$load_mode, { multi_dirs(character(0)) })

  output$project_dir_ui <- renderUI({
    mode <- input$load_mode %||% "project"
    if (mode == "multiple") return(NULL)
    if (mode == "project") {
      tagList(
        help_label("Project directory",
          "SqueezeMeta, SQM_reads or SQM_longreads project directory. It will look for a directory 'tables' in that directory, otherwise will ask for the appropriate location of the tables.",
          style = "margin-top:0.25rem;"),
        shinyDirButton("dir_project", "Select directory", "Choose the project directory",
          multiple = FALSE, class = "btn-default w-100 mb-1"),
        tags$div(class = "path-info", textOutput("path_project", inline = TRUE))
      )
    } else {
      tagList(
        help_label("Tables directory",
          "Directory containing the SQMlite tables (output of sqm2tables.py, sqmreads2tables.py or combine-sqm-tables.py).",
          style = "margin-top:0.25rem;"),
        shinyDirButton("dir_manual_tables", "Select tables directory", "Choose the tables directory",
          multiple = FALSE, class = "btn-default w-100 mb-1"),
        tags$div(class = "path-info", textOutput("path_manual_tables", inline = TRUE))
      )
    }
  })
  output$path_manual_tables <- renderText({
    tryCatch(parseDirPath(roots, input$dir_manual_tables), error = function(e) "")
  })
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
    req((input$load_mode %||% "project") == "project")
    req(need_manual())
    tagList(
      tags$div(class = "path-info", style = "color:#c0392b;", "Tables directory could not be found automatically."),
      tags$div(class = "form-label", "Select tables directory"),
      shinyDirButton("dir_manual_tables", "Select tables", "Choose the tables directory",
        multiple = FALSE, class = "btn-default w-100 mb-1")
    )
  })
  observeEvent(input$load_project, {
    mode <- input$load_mode %||% "project"

    # ---- Load multiple mode ----
    if (mode == "multiple") {
      dirs <- multi_dirs()
      if (length(dirs) < 2) {
        showNotification("Please select at least 2 project directories.", type = "error", duration = 6)
        return()
      }
      for (d in dirs) {
        if (!dir.exists(d)) {
          showNotification(paste("Directory not found:", d), type = "error", duration = 6)
          return()
        }
      }
      status("loading")
      shinyjs::delay(50, {
        tryCatch({
          # Load each project individually then combine
          projects <- lapply(dirs, function(d) {
            creator_file <- file.path(d, "creator.txt")
            is_sqm <- if (file.exists(creator_file))
              grepl("SqueezeMeta", trimws(readLines(creator_file, n = 1, warn = FALSE)),
                    ignore.case = TRUE)
            else FALSE
            if (is_sqm) loadSQM(d) else {
              tp <- file.path(d, "results", "tables")
              if (!dir.exists(tp)) tp <- d
              loadSQMlite(tp)
            }
          })
          proj <- do.call(combineSQM, c(projects, list(rescale_tpm = TRUE, rescale_copy_number = TRUE)))
          if (!inherits(proj, c("SQM", "SQMlite"))) class(proj) <- c("SQMlite", class(proj))
          sqm_data(proj); is_sqm_full(FALSE); status("ready")
          shinyjs::runjs("document.body.classList.remove('sqm-no-project');")
          for (tab in ANALYSIS_TABS) nav_show("main_navbar", tab)
          showNotification(paste(length(dirs), "projects combined successfully."), type = "message", duration = 5)
        }, error = function(e) {
          status("error")
          showNotification(paste("Error combining projects:", e$message), type = "error", duration = 10)
        })
      })
      return()
    }

    # ---- Single project / tables mode ----
    tp <- if (mode == "tables") {
      tryCatch(parseDirPath(roots, input$dir_manual_tables), error = function(e) NULL)
    } else {
      tables_path()
    }
    if (is.null(tp) || length(tp) == 0 || !nzchar(tp) || !dir.exists(tp)) {
      showNotification("Directory not available. Please select it.", type = "error", duration = 8); return()
    }
    status("loading")
    shinyjs::delay(50, {
      tryCatch({
        is_sqm <- if (mode == "tables") FALSE else
                  grepl("SqueezeMeta", creator_name() %||% "", ignore.case = TRUE)
        proj <- if (is_sqm) loadSQM(tp) else loadSQMlite(tp)
        sqm_data(proj); is_sqm_full(is_sqm); status("ready")
        shinyjs::runjs("document.body.classList.remove('sqm-no-project');")
        for (tab in ANALYSIS_TABS) nav_show("main_navbar", tab)
      }, error = function(e) {
        status("error")
        msg <- if (grepl("cannot open|No such file|not found|does not exist", e$message, ignore.case = TRUE))
          paste0("Cannot find results tables in: ", tp,
                 "\nMake sure the directory contains the expected table files.")
        else
          paste0("Could not load project: ", e$message)
        showNotification(msg, type = "error", duration = 12)
      })
    })
  })
  output$project_status_ui <- renderUI({
    s <- status()
    col <- switch(s, idle="#7a90a8", loading="#3b9ede", ready="#1a9e6e", error="#c0392b")
    ico <- switch(s, idle="○", loading="◌", ready="●", error="✕")
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
    s <- status()
    if (s == "loading") return(
      tags$div(
        style = paste0("display:flex; align-items:center; gap:12px;",
                       "padding:3rem 2rem; color:#3b9ede; font-size:0.95rem;"),
        tags$span(style="font-size:2rem;", "◌"),
        tags$div(
          tags$div(style="font-weight:600;", "Loading project, please wait…"),
          tags$div(style="font-size:0.8rem; color:var(--muted); margin-top:4px;",
                   "This may take a moment for large projects."))))
    proj <- sqm_data()
    if (is.null(proj)) return(tags$div(style="color:var(--muted);font-size:0.85rem;padding:1rem;","No project loaded yet."))

    panels <- list()

    # \u2500\u2500 Project name badge (both SQM and SQMlite) \u2500\u2500
    project_name <- tryCatch({
      pn <- proj$misc$project_name %||% ""
      if (length(pn) > 1) paste(pn, collapse = " + ") else pn
    }, error=function(e) "")
    if (nchar(project_name[1])>0) panels[["name"]] <- tags$div(
      style="margin-bottom:12px;display:flex;align-items:center;gap:10px;",
      tags$span(style="font-family:'IBM Plex Mono',monospace;font-size:0.75rem;color:var(--muted);text-transform:uppercase;letter-spacing:0.06em;","Project"),
      tags$span(class="project-badge",style="font-size:0.85rem;padding:3px 10px;",project_name))

    if (is_sqm_full()) {
      # \u2500\u2500 Full SQM: parse capture.output(summary()) \u2500\u2500
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
      # \u2500\u2500 SQMlite: capture.output(summary()) produces tab-delimited text
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

        # \u2500\u2500 Helper: parse a block of tab lines into header + body df \u2500\u2500
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

        # \u2500\u2500 Extract project name \u2500\u2500
        name_line <- grep("BASE PROJECT NAME:", raw, value = TRUE)
        if (length(name_line) > 0 && nchar(project_name[1]) == 0) {
          project_name <- trimws(sub(".*BASE PROJECT NAME:\\s*", "", name_line[1]))
          # update badge if not already set
          panels[["name"]] <- tags$div(
            style = "margin-bottom:12px;display:flex;align-items:center;gap:10px;",
            tags$span(style = "font-family:'IBM Plex Mono',monospace;font-size:0.75rem;color:var(--muted);text-transform:uppercase;letter-spacing:0.06em;", "Project"),
            tags$span(class = "project-badge", style = "font-size:0.85rem;padding:3px 10px;", project_name))
        }

        # \u2500\u2500 Overview block: TOTAL READS + TOTAL ORFs \u2500\u2500
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

        # \u2500\u2500 TAXONOMY section \u2500\u2500
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

        # \u2500\u2500 FUNCTIONS section \u2500\u2500
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

    # \u2500\u2500 Samples (both object types) \u2500\u2500
    samples <- tryCatch(proj$samples, error=function(e) NULL)
    if (!is.null(samples)) panels[["samples"]] <- sqm_section("Samples",
      tags$div(style="padding-top:2px;", tagList(lapply(samples,function(s) tags$span(class="project-badge",s)))))

    # ── Mapping stats: replace READS panel with 10.<project>.mappingstat ──
    mapping_panel <- tryCatch({
      proj_dir <- path_project()
      if (!is.null(proj_dir) && nchar(proj_dir) > 0) {
        search_dirs <- c(proj_dir, file.path(proj_dir, "results"))
        mstat_files <- unlist(lapply(search_dirs, function(d)
          list.files(d, pattern="^10\\..*\\.mappingstat$", full.names=TRUE)))
        if (length(mstat_files) > 0) {
          lines <- readLines(mstat_files[[1]], warn=FALSE)
          header_line <- lines[grepl("^#\\s*Sample", lines)]
          data_lines  <- lines[!grepl("^#", lines) & nchar(trimws(lines)) > 0]
          if (length(header_line) > 0 && length(data_lines) >= 1) {
            raw_header <- trimws(unlist(strsplit(sub("^#\\s*", "", header_line[[1]]), "\t")))
            col_rename <- c(
              "Sample"       = "Sample",
              "Total reads"  = "Total reads",
              "Mapped reads" = "Mapped reads",
              "Mapping perc" = "Mapping percentage",
              "Total bases"  = "Total bases"
            )
            header <- sapply(raw_header, function(h) {
              if (h %in% names(col_rename)) col_rename[[h]] else h
            }, USE.NAMES = FALSE)
            rows <- lapply(data_lines, function(l) trimws(unlist(strsplit(l, "\t"))))
            pct_col <- which(raw_header == "Mapping perc")
            if (length(pct_col) == 0) pct_col <- 4
            mapping_pcts <- suppressWarnings(as.numeric(sapply(rows,
              function(r) if (length(r) >= pct_col) r[[pct_col]] else NA)))
            low_mapping <- any(!is.na(mapping_pcts) & mapping_pcts < 50)
            th <- paste0("<th>", header, "</th>", collapse="")
            tr_rows <- sapply(rows, function(r) {
              cells <- sapply(seq_along(r), function(i) {
                v <- r[[i]]
                n <- suppressWarnings(as.numeric(v))
                style <- ""
                if (i == pct_col && !is.na(n) && n < 50)
                  style <- ' style="color:#c0392b; font-weight:600;"'
                if (!is.na(n) && i > 1 && !grepl("\\.", v))
                  v <- format(n, big.mark=",", scientific=FALSE)
                paste0("<td", style, ">", v, "</td>")
              })
              paste0("<tr>", paste(cells, collapse=""), "</tr>")
            })
            warning_tag <- if (low_mapping)
              tags$div(style="color:#c0392b; font-size:0.8rem; margin-top:6px; font-weight:600;",
                "\u26a0 Some samples have mapping percentage below 50%")
            else NULL
            tagList(
              HTML(paste0(
                '<table class="sqm-table">',
                '<thead><tr>', th, '</tr></thead>',
                '<tbody>', paste(tr_rows, collapse=""), '</tbody>',
                '</table>'
              )),
              warning_tag
            )
          }
        }
      }
    }, error=function(e) NULL)
    if (!is.null(mapping_panel))
      panels[["READS"]] <- sqm_section("Mapping statistics", mapping_panel)

    tagList(panels)
  })
  output$plot_controls_ui <- renderUI({
    pt <- input$plot_type; if (is.null(pt)) return(NULL)
    rank_choices <- c("Phylum"="phylum","Class"="class","Order"="order","Family"="family","Genus"="genus","Species"="species")
    if (pt == "krona") {
      return(tagList(
        tags$div(class="sidebar-box",
          tags$div(class="form-label", "Krona chart"),
          if (!kt_available())
            tags$div(style="font-size:0.8rem; color:#c0392b;",
              "ktImportText not found. Install KronaTools and add it to PATH.")
          else
            actionButton("do_krona_inline", "Generate Krona",
              class = "btn-primary w-100")
        )
      ))
    }
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
          help_label("Count type", .count_tip(tax_counts)),
          selectInput("tax_count",NULL,choices=tax_counts,selected=if(length(tax_counts)==0) NULL else if("percent"%in%tax_counts)"percent" else tax_counts[[1]]),
          tags$div(class="form-label",style="margin-top:4px;","No. of taxa"),
          numericInput("n_taxa",NULL,value=15,min=1,max=200,step=1)
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:0;",
            checkboxInput("tax_ignore_unmapped","Ignore unmapped",value=FALSE),
            checkboxInput("tax_ignore_unclassified","Ignore unclassified",value=FALSE),
            checkboxInput("tax_no_partial_classifications","Ignore ambiguous",value=FALSE),
            checkboxInput("tax_rescale","Rescale",value=FALSE)
          )
        ),
      )
    } else if (pt == "taxonomy_heatmap") {
      tax_counts  <- if (!is.null(sqm_data())) available_tax_counts(sqm_data()) else c("Percentages"="percent")
      avail_ranks <- if (!is.null(sqm_data())) available_tax_ranks(sqm_data()) else c("Phylum"="phylum")
      tagList(
        tags$div(class="sidebar-box",
          tags$div(class="form-label","Taxonomic rank"),
          selectInput("tax_hm_rank", NULL, choices=avail_ranks),
          help_label("Count type", .count_tip(tax_counts)),
          selectInput("tax_hm_count", NULL, choices=tax_counts,
            selected=if(length(tax_counts)==0) NULL else if("percent"%in%tax_counts)"percent" else tax_counts[[1]]),
          tags$div(class="form-label",style="margin-top:4px;","No. of taxa"),
          numericInput("tax_hm_n", NULL, value=30, min=1, max=500, step=1),
          help_label("Rescale", "Options for rescaling and normalizing data: None, Logarithmic (log₁₀(x+1)), Z-score (rows)"),
          selectInput("tax_hm_scale", NULL,
            choices=c("None"="none","Log₁₀(x+1)"="log","Z-score"="zscore"),
            selected="none")
        ),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:0;",
            checkboxInput("tax_hm_ignore_unmapped","Ignore unmapped",value=FALSE),
            checkboxInput("tax_hm_ignore_unclassified","Ignore unclassified",value=FALSE),
            checkboxInput("tax_hm_ignore_ambiguous","Ignore ambiguous",value=FALSE)
          )
        )
      )
    } else if (pt == "kegg_class") {
      tagList(
        uiOutput("func_category_ui"),
        tags$div(class="sidebar-box", style="margin-top:8px;",
          tags$div(class="form-label","Hierarchy level"),
          selectInput("kegg_class_level", NULL,
            choices  = c("L1 (broad categories)"="l1",
                         "L2 (subcategories)"="l2",
                         "L3 (pathways)"="l3"),
            selected = "l1"),
          help_label("Count type", .count_tip(c(
            "Raw abundances"="abund",
            "Percentage (selection)"="percent_sel",
            "Percentage (full dataset)"="percent_full",
            "TPM (selection)"="tpm_sel",
            "TPM (full dataset)"="tpm_full"))),
          selectInput("kegg_class_count", NULL,
            choices  = c("Raw abundances"="abund",
                         "Percentage (selection)"="percent_sel",
                         "Percentage (full dataset)"="percent_full",
                         "TPM (selection)"="tpm_sel",
                         "TPM (full dataset)"="tpm_full"),
            selected = "abund"),
          help_label("Rescale", "Options for rescaling and normalizing data: None, Logarithmic (log₁₀(x+1)), Z-score (rows)"),
          selectInput("plot_scale", NULL,
            choices=c("None"="none","Log₁₀(x+1)"="log","Z-score"="zscore"),
            selected="none"),
          tags$div(style="margin-top:6px;"),
          checkboxInput("kegg_class_show_other", "Show 'Other functions'", value=FALSE)
        )
      )
    } else if (pt == "cog_class") {
      fun_counts <- if (!is.null(sqm_data())) available_func_counts(sqm_data(), "COG") else c("Copy number"="copy_number")
      tagList(
        tags$div(class="sidebar-box",
          help_label("Count type", .count_tip(c("Raw abundances"="abund","Percentage"="percent_full","TPM"="tpm_full"))),
          selectInput("cog_class_count", NULL,
            choices  = c("Raw abundances"="abund",
                         "Percentage"="percent_full",
                         "TPM"="tpm_full"),
            selected = "abund"),
          tags$div(class="form-label",style="margin-top:4px;"),
          tags$div(style="display:flex; align-items:center; gap:4px; margin-top:6px;",
            checkboxInput("cog_class_excl_unknown", "Exclude 'Function unknown'", value=TRUE),
            tags$span(
              style="cursor:help; color:var(--muted); font-size:0.78rem; margin-top:2px;",
              title="Do not consider instances with other or no assigned function",
              "\u24d8"
            )
          ),
          help_label("Rescale", "Options for rescaling and normalizing data: None, Logarithmic (log₁₀(x+1)), Z-score (rows)"),
          selectInput("plot_scale", NULL,
            choices=c("None"="none","Log₁₀(x+1)"="log","Z-score"="zscore"),
            selected="none")
        )
      )
    } else if (startsWith(pt, "func_")) {
      fun_label <- resolve_db_name(sqm_data(), sub("^func_", "", pt))
      tagList(
        tags$div(class="sidebar-box",
          tagList(
            tags$div(class="form-label",paste("Search",fun_label,"functions")),
            tags$div(class="func-search-box", tags$span(class="search-icon","\U0001f50d"),
              textInput("func_search",NULL,placeholder=paste0("e.g. ", fun_label, "0001, keyword"))),
            tags$div(class="func-search-hint","Comma-separated. Empty \u2192 top N functions."),
            uiOutput("func_search_status")
          )
        ),
        uiOutput("func_category_ui"),
        tags$div(class="sidebar-box",style="margin-top:8px;",
          help_label("Count type", .count_tip(if(!is.null(sqm_data())) available_func_counts(sqm_data(), resolve_db_name(sqm_data(), sub("^func_","",input$plot_type))) else c("Copy number"="copy_number"))), uiOutput("func_count_ui"),
          tags$div(class="form-label",style="margin-top:4px;","No. of functions"), numericInput("n_funcs", NULL, value=20, min=1, max=500, step=1),
          help_label("Rescale", "Options for rescaling and normalizing data: None, Logarithmic (log₁₀(x+1)), Z-score (rows)"),
          selectInput("plot_scale", NULL,
            choices=c("None"="none","Log₁₀(x+1)"="log","Z-score"="zscore"),
            selected="none")
        ),
      )
    } else NULL
  })
  output$func_search_status <- renderUI({
    pt <- input$plot_type; req(startsWith(pt, "func_")); req(sqm_data())
    pattern <- build_func_pattern(input$func_search %||% ""); if (is.null(pattern)) return(NULL)
    fun_level <- resolve_db_name(sqm_data(), sub("^func_", "", pt))
    all_ids   <- tryCatch(rownames(sqm_data()$functions[[fun_level]]$abund), error=function(e) character(0))
    all_names <- tryCatch(sqm_data()$misc[[paste0(fun_level,"_names")]], error=function(e) character(0))
    terms <- trimws(unlist(strsplit(input$func_search %||% "", "[,;]+")))
    terms <- terms[nchar(terms) > 0]
    matched <- unique(unlist(lapply(terms, function(t) {
      by_id   <- all_ids[grepl(t, all_ids, ignore.case=TRUE)]
      by_name <- if (length(all_names)>0) names(all_names)[grepl(t, all_names, ignore.case=TRUE)] else character(0)
      union(by_id, by_name[by_name %in% all_ids])
    })))
    n <- length(matched)
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
  output$func_count_ui <- renderUI({
    pt <- input$plot_type; req(startsWith(pt, "func_"))
    fun_level <- resolve_db_name(sqm_data(), sub("^func_", "", pt))
    counts <- if (!is.null(sqm_data())) available_func_counts(sqm_data(),fun_level) else c("Copy number"="copy_number")
    selectInput("func_count",NULL,choices=counts,selected=if(length(counts)==0) NULL else if("copy_number"%in%counts)"copy_number" else counts[[1]])
  })
  output$sqm_plot_ui <- renderUI({
    pt <- input$plot_type
    is_tax     <- !is.null(pt) && pt == "taxonomy_bar"
    is_tax_hm   <- !is.null(pt) && pt == "taxonomy_heatmap"
    is_func     <- !is.null(pt) && startsWith(pt, "func_")
    is_cog_class  <- !is.null(pt) && pt == "cog_class"
    is_kegg_class <- !is.null(pt) && pt == "kegg_class"
    h <- if (is_tax) input$tax_plot_height %||% 560
         else if (is_func || is_cog_class || is_kegg_class) input$func_plot_height %||% 560
         else if (is_tax_hm) input$tax_hm_height %||% 560
         else 560
    w <- if (is_tax) input$tax_plot_width  %||% 800
         else if (is_func || is_cog_class || is_kegg_class) input$func_plot_width  %||% 1200
         else if (is_tax_hm) input$tax_hm_width %||% 1200
         else NULL
    style <- if (!is.null(w)) paste0("width:",w,"px; overflow-x:auto;") else "width:100%;"
    if (is_cog_class) {
      tags$div(style="width:100%; overflow-x:auto; overflow-y:auto; max-height:80vh;",
        plotlyOutput("sqm_cog_class_plot", width="100%", height="auto"))
    } else if (is_kegg_class) {
      tags$div(style="width:100%; overflow-x:auto; overflow-y:auto; max-height:80vh;",
        plotlyOutput("sqm_kegg_class_plot", width="100%", height="auto"))
    } else if (is_tax_hm) {
      tags$div(style="width:100%; overflow-x:auto; overflow-y:auto; max-height:80vh;",
        plotlyOutput("sqm_tax_hm_plot", width="100%", height="auto"))
    } else if (is_func) {
      tags$div(style="width:100%; overflow-x:auto; overflow-y:auto; max-height:80vh;",
        plotlyOutput("sqm_func_plot", width="100%", height="auto"))
    } else if (is_tax) {
      tags$div(style="width:100%; overflow-x:auto;",
        plotlyOutput("sqm_tax_plot", width="100%", height=paste0(h,"px")))
    } else if (!is.null(pt) && pt == "krona") {
      uiOutput("sqm_krona_inline_ui")
    } else {
      tags$div(style=style,
        plotOutput("sqm_plot", width=if(!is.null(w)) paste0(w,"px") else "100%", height=paste0(h,"px")))
    }
  })
  plot_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data(); pt <- input$plot_type
    req(!is.null(pt) && pt != "none")
    # Subset samples if selection is active
    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp)) {
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)
    }
    # Filter by functional category if selected
    if (pt == "func_cog" && !is.null(COG_CATEGORIES) &&
        nchar(input$cog_category %||% "") > 0) {
      keep <- COG_CATEGORIES$id[COG_CATEGORIES$category == input$cog_category]
      for (db in names(proj$functions$COG)) {
        m <- proj$functions$COG[[db]]
        if (is.matrix(m) || is.data.frame(m))
          proj$functions$COG[[db]] <- m[rownames(m) %in% keep, , drop=FALSE]
      }
    } else if ((pt == "func_kegg" || pt == "kegg_class") && !is.null(KEGG_CATEGORIES)) {
      sel_l1 <- input$kegg_cat_l1 %||% ""
      sel_l2 <- input$kegg_cat_l2 %||% ""
      if (nchar(sel_l1) > 0) {
        sub_cat <- KEGG_CATEGORIES[KEGG_CATEGORIES$l1 == sel_l1, ]
        if (nchar(sel_l2) > 0)
          sub_cat <- sub_cat[!is.na(sub_cat$l2) & sub_cat$l2 == sel_l2, ]
        keep <- unique(sub_cat$id)
        for (db in names(proj$functions$KEGG)) {
          m <- proj$functions$KEGG[[db]]
          if (is.matrix(m) || is.data.frame(m))
            proj$functions$KEGG[[db]] <- m[rownames(m) %in% keep, , drop=FALSE]
        }
      }
    }
    if (pt=="taxonomy_bar") {
      return(NULL)  # handled by tax_plot_reactive / sqm_tax_plot
    } else if (pt=="taxonomy_heatmap") {
      return(NULL)  # handled by tax_hm_reactive / sqm_tax_hm_plot
    } else if (pt == "cog_class") {
      return(NULL)  # handled by cog_class_reactive / sqm_cog_class_plot
    } else if (pt == "kegg_class") {
      return(NULL)  # handled by kegg_class_reactive / sqm_kegg_class_plot
    } else if (startsWith(pt, "func_")) {
      return(NULL)  # handled by func_plot_reactive / sqm_func_plot
    } else if (pt=="bins") { plotBins(proj) }
  })
  output$sqm_plot <- renderPlot({ plot_reactive() }, bg="#ffffff")

  # ── Plotly reactive for taxonomy plots ──────────────────────────────────────
  tax_plot_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data(); pt <- input$plot_type
    req(pt == "taxonomy_bar")
    rank <- input$tax_rank %||% "phylum"
    fs   <- input$tax_font_size %||% 11
    lw   <- input$tax_label_width %||% 30
    pal  <- input$tax_palette %||% "Blues"
    pw   <- input$tax_plot_width  %||% 1200
    ph   <- input$tax_plot_height %||% 560

    # Subset samples
    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp))
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)

    # Search filter
    search_text <- if(is_sqm_full()) trimws(input$tax_search %||% "") else ""
    if (nchar(search_text) > 0) {
      all_taxa <- tryCatch(rownames(proj$taxa[[rank]]$abund), error=function(e) character(0))
      terms    <- trimws(unlist(strsplit(search_text, "[,;]+")))
      terms    <- terms[nchar(terms) > 0]
      matched  <- unique(unlist(lapply(terms, function(t)
        all_taxa[grepl(t, all_taxa, ignore.case=TRUE)])))
      if (length(matched) == 0) {
        showNotification(paste0("No taxa found matching: \"", search_text, "\""),
                         type="warning", duration=5)
        return(NULL)
      }
      proj <- tryCatch(subsetTax(proj, rank=rank, tax=matched), error=function(e) proj)
    }

    # Get abundance matrix
    count  <- input$tax_count %||% "percent"
    mat    <- tryCatch(proj$taxa[[rank]][[count]], error=function(e) NULL)
    req(!is.null(mat) && (is.matrix(mat) || is.data.frame(mat)) && nrow(mat) > 0)
    mat    <- as.matrix(mat)

    # Filter options
    # Always remove "No CDS"
    mat <- mat[!grepl("^[Nn]o CDS$", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_ignore_unmapped))
      mat <- mat[!grepl("^[Uu]nmapped$|^[Nn]o [Hh]it", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_ignore_unclassified))
      mat <- mat[!grepl("^[Uu]nclassified$", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_no_partial_classifications))
      mat <- mat[!grepl("^[Uu]nclassified ", rownames(mat)), , drop=FALSE]
    req(nrow(mat) > 0)

    # Rescale each sample to 100% (only meaningful for percentages)
    if (isTRUE(input$tax_rescale) && count == "percent") {
      col_sums <- colSums(mat, na.rm=TRUE)
      col_sums[col_sums == 0] <- 1
      mat <- sweep(mat, 2, col_sums, "/") * 100
    }

    # Top N + Other
    n_taxa <- min(input$n_taxa %||% 15, nrow(mat))
    all_idx <- order(rowSums(mat, na.rm=TRUE), decreasing=TRUE)
    top_idx <- all_idx[seq_len(n_taxa)]
    rest_idx <- all_idx[seq(n_taxa + 1, nrow(mat))]
    if (length(rest_idx) > 0) {
      other_row <- matrix(colSums(mat[rest_idx, , drop=FALSE], na.rm=TRUE),
                          nrow=1, dimnames=list("Other", colnames(mat)))
      mat <- rbind(mat[top_idx, , drop=FALSE], other_row)
    } else {
      mat <- mat[top_idx, , drop=FALSE]
    }

    # Wrap labels
    wrap_label <- function(s) {
      if (nchar(s) <= lw) return(s)
      words <- strsplit(s, " ")[[1]]
      lines <- ""; cur <- ""
      for (w in words) {
        if (nchar(cur) == 0) { cur <- w
        } else if (nchar(cur) + 1 + nchar(w) <= lw) { cur <- paste(cur, w)
        } else { lines <- if(nchar(lines)==0) cur else paste0(lines,"<br>",cur); cur <- w }
      }
      if (nchar(cur)>0) lines <- if(nchar(lines)==0) cur else paste0(lines,"<br>",cur)
      lines
    }
    taxa_labels <- sapply(rownames(mat), wrap_label, USE.NAMES=FALSE)

    # Colour palette — all qualitative, high contrast between categories
    n_taxa_show <- nrow(mat)
    qual_base <- list(
      Paired       = c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c",
                       "#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928"),
      Set2         = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f",
                       "#e5c494","#b3b3b3"),
      Set3         = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462",
                       "#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f"),
      Dark2        = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02",
                       "#a6761d","#666666"),
      Tableau10    = c("#4e79a7","#f28e2b","#e15759","#76b7b2","#59a14f","#edc948",
                       "#b07aa1","#ff9da7","#9c755f","#bab0ac"),
      Alphabet     = c("#aa0dfe","#3283fe","#85660d","#782ab6","#565656","#1c8356",
                       "#16ff32","#f7e1a0","#e2e2e2","#1cbe4f","#c4451c","#dee5f2",
                       "#fa0087","#fc1cbf","#f0a0ff","#224808","#fbe426","#bdcdff",
                       "#b5ede5","#7ed7d1","#1d8f2c","#325a9b","#feaf16","#f8a19f",
                       "#90ad1c","#f6222e","#ffd6cc","#c075a6","#fc33c5","#683b79",
                       "#b4c687","#b0e0e6"),
      Polychrome36 = c("#5a5156","#e4e1e3","#f6222e","#fe6c00","#16ff32","#3283fe",
                       "#feaf16","#b00068","#1cbe4f","#c4451c","#dee5f2","#325a9b",
                       "#f8a19f","#90ad1c","#f6222e","#1d8f2c","#c075a6","#7ed7d1",
                       "#b5ede5","#782ab6","#aa0dfe","#fa0087","#fbe426","#bdcdff",
                       "#b4c687","#fc1cbf","#f0a0ff","#224808","#ffd6cc","#fc33c5",
                       "#feaf16","#f8a19f","#563d7c","#4cadb5","#a05e36","#e2e2e2")
    )
    base_cols <- qual_base[[pal]]
    if (is.null(base_cols)) base_cols <- qual_base[["Paired"]]
    colours <- if (n_taxa_show <= length(base_cols)) {
      base_cols[seq_len(n_taxa_show)]
    } else {
      colorRampPalette(base_cols)(n_taxa_show)
    }

    # Build stacked bar chart (one bar per sample, stacked by taxon)
    samples <- colnames(mat)
    p <- plot_ly(width=pw, height=ph)
    for (i in seq_len(nrow(mat))) {
      p <- add_trace(p,
        x    = samples,
        y    = mat[i, ],
        type = "bar",
        name = taxa_labels[i],
        marker = list(color = colours[i]),
        hovertemplate = paste0("<b>", taxa_labels[i], "</b><br>%{x}: %{y:.4f}<extra></extra>")
      )
    }
    p <- layout(p,
      barmode = "stack",
      xaxis   = list(title="", tickfont=list(size=fs), tickangle=-35),
      yaxis   = list(title=count, tickfont=list(size=fs), titlefont=list(size=fs)),
      legend  = list(font=list(size=max(fs-2,8)), traceorder="normal"),
      margin  = list(l=10, r=10, t=30, b=60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
    config(p, displayModeBar=FALSE)
  })

  output$sqm_tax_plot <- renderPlotly({ tax_plot_reactive() })

  # ── Plotly reactive for function plots ──────────────────────────────────────
  func_plot_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data(); pt <- input$plot_type
    req(startsWith(pt, "func_"))
    fun_level <- resolve_db_name(proj, sub("^func_", "", pt))
    req(nchar(input$func_count %||% "") > 0)
    req(!is.null(input$n_funcs))
    fs <- input$func_font_size %||% 11

    # Subset samples
    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp)) {
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)
    }

    # Category filter (COG / KEGG)
    if (pt == "func_cog" && !is.null(COG_CATEGORIES) &&
        nchar(input$cog_category %||% "") > 0) {
      keep <- COG_CATEGORIES$id[COG_CATEGORIES$category == input$cog_category]
      for (db in names(proj$functions$COG)) {
        m <- proj$functions$COG[[db]]
        if (is.matrix(m) || is.data.frame(m))
          proj$functions$COG[[db]] <- m[rownames(m) %in% keep, , drop=FALSE]
      }
    } else if ((pt == "func_kegg" || pt == "kegg_class") && !is.null(KEGG_CATEGORIES)) {
      sel_l1 <- input$kegg_cat_l1 %||% ""
      sel_l2 <- input$kegg_cat_l2 %||% ""
      if (nchar(sel_l1) > 0) {
        sub_cat <- KEGG_CATEGORIES[KEGG_CATEGORIES$l1 == sel_l1, ]
        if (nchar(sel_l2) > 0)
          sub_cat <- sub_cat[!is.na(sub_cat$l2) & sub_cat$l2 == sel_l2, ]
        keep <- unique(sub_cat$id)
        for (db in names(proj$functions$KEGG)) {
          m <- proj$functions$KEGG[[db]]
          if (is.matrix(m) || is.data.frame(m))
            proj$functions$KEGG[[db]] <- m[rownames(m) %in% keep, , drop=FALSE]
        }
      }
    }

    # Search filter
    search_text <- trimws(input$func_search %||% "")
    if (nchar(search_text) > 0) {
      all_ids   <- tryCatch(rownames(proj$functions[[fun_level]]$abund), error=function(e) character(0))
      all_names <- tryCatch(proj$misc[[paste0(fun_level,"_names")]], error=function(e) character(0))
      terms <- trimws(unlist(strsplit(search_text, "[,;]+")))
      terms <- terms[nchar(terms) > 0]
      matched <- unique(unlist(lapply(terms, function(t) {
        by_id   <- all_ids[grepl(t, all_ids, ignore.case=TRUE)]
        by_name <- if (length(all_names)>0) names(all_names)[grepl(t, all_names, ignore.case=TRUE)] else character(0)
        union(by_id, by_name[by_name %in% all_ids])
      })))
      if (length(matched)==0) {
        showNotification(paste0("No ",fun_level," functions found matching: \"",search_text,"\""),
                         type="warning", duration=5)
        return(NULL)
      }
      fun_sub <- lapply(proj$functions[[fun_level]], function(m) {
        if (is.matrix(m) || is.data.frame(m)) m[rownames(m) %in% matched, , drop=FALSE] else m
      })
      proj$functions[[fun_level]] <- fun_sub
    }

    # Get count matrix
    mat <- tryCatch(proj$functions[[fun_level]][[input$func_count]], error=function(e) NULL)
    req(!is.null(mat) && (is.matrix(mat) || is.data.frame(mat)) && nrow(mat) > 0)
    mat <- as.matrix(mat)

    # Remove Unclassified rows
    unclass_pat <- "^[Uu]nclassified$|^[Uu]nclassified |^No [Hh]it|^Unknown$"
    keep_rows <- !grepl(unclass_pat, rownames(mat))
    mat <- mat[keep_rows, , drop=FALSE]
    req(nrow(mat) > 0)


    # Compute order on raw values BEFORE scaling (stable order regardless of rescale)
    n_funcs <- min(input$n_funcs %||% 20, nrow(mat))
    row_totals <- rowSums(mat, na.rm=TRUE)
    top_idx <- order(row_totals, decreasing=TRUE)[seq_len(n_funcs)]
    mat <- mat[top_idx, , drop=FALSE]
    row_order <- order(rowSums(mat, na.rm=TRUE), decreasing=TRUE)

    # Apply rescaling after order is fixed
    scl <- input$plot_scale %||% "none"
    if (scl == "log") {
      mat <- log10(mat + 1)
    } else if (scl == "zscore") {
      mat <- t(apply(mat, 1, function(r) {
        s <- sd(r, na.rm=TRUE)
        if (is.na(s) || s == 0) r - mean(r, na.rm=TRUE) else (r - mean(r, na.rm=TRUE)) / s
      }))
    }
    mat <- mat[row_order, , drop=FALSE]

    # Enrich row names with function names if available
    fun_names <- tryCatch(proj$misc[[paste0(fun_level,"_names")]], error=function(e) NULL)
    if (!is.null(fun_names) && length(fun_names) > 0) {
      rn <- rownames(mat)
      nm <- fun_names[rn]
      nm[is.na(nm)] <- ""
      rownames(mat) <- ifelse(nchar(nm) > 0, paste0(rn, ": ", nm), rn)
    }

    # Wrap long row labels every N characters
    lw <- input$func_label_width %||% 40
    wrap_label <- function(s) {
      if (nchar(s) <= lw) return(s)
      words <- strsplit(s, " ")[[1]]
      lines <- ""; cur <- ""
      for (w in words) {
        if (nchar(cur) == 0) {
          cur <- w
        } else if (nchar(cur) + 1 + nchar(w) <= lw) {
          cur <- paste(cur, w)
        } else {
          lines <- if (nchar(lines) == 0) cur else paste0(lines, "<br>", cur)
          cur <- w
        }
      }
      if (nchar(cur) > 0) lines <- if (nchar(lines) == 0) cur else paste0(lines, "<br>", cur)
      lines
    }
    rownames(mat) <- sapply(rownames(mat), wrap_label, USE.NAMES=FALSE)

    pw <- input$func_plot_width  %||% 1200
    # Fix height so cell size is consistent (28px/row + overhead)
    ph <- nrow(mat) * 28 + 120

    # Build plotly heatmap with Blues colorscale
    p <- plot_ly(
      z         = mat,
      x         = colnames(mat),
      y         = rownames(mat),
      type      = "heatmap",
      colorscale = input$func_palette %||% "Blues",
      reversescale = FALSE,
      colorbar = list(lenmode="pixels", len=200, thickness=15),
      hovertemplate = "<b>%{y}</b><br>Sample: %{x}<br>Value: %{z}<extra></extra>",
      width     = pw,
      height    = ph
    )
    p <- layout(p,
      xaxis  = list(title="", tickfont=list(size=fs), tickangle=-45, automargin=TRUE),
      yaxis  = list(title="", tickfont=list(size=fs), automargin=TRUE, autorange="reversed"),
      margin = list(l=10, r=10, t=30, b=60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
    p <- config(p, displayModeBar=FALSE)
    p
  })

  output$sqm_func_plot <- renderPlotly({ func_plot_reactive() })

  # ── COG functional classes reactive ─────────────────────────────────────────
  cog_class_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data()
    req(input$plot_type == "cog_class")
    req(!is.null(COG_CATEGORIES))
    fs    <- input$func_font_size  %||% 11
    count <- input$cog_class_count %||% "abund"

    # Subset samples
    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp))
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)

    # Always aggregate from raw abundances to preserve additive semantics.
    # Then derive the requested metric from the aggregated raw counts.
    cog_db    <- names(proj$functions)[toupper(names(proj$functions)) == "COG"][1]
    req(!is.null(cog_db))
    abund_raw <- tryCatch(as.matrix(proj$functions[[cog_db]]$abund), error=function(e) NULL)
    req(!is.null(abund_raw) && nrow(abund_raw) > 0)

    # Map each COG id to its functional category (first category if multi-category)
    cog_ids    <- rownames(abund_raw)
    cat_lookup <- COG_CATEGORIES[!duplicated(COG_CATEGORIES$id), ]
    cat_vec    <- cat_lookup$category[match(cog_ids, cat_lookup$id)]
    # Keep only COGs with a known category
    has_cat    <- !is.na(cat_vec) & nchar(trimws(cat_vec)) > 0
    abund_raw  <- abund_raw[has_cat, , drop=FALSE]
    cat_vec    <- cat_vec[has_cat]
    req(nrow(abund_raw) > 0)

    # Aggregate raw counts by category (sum)
    cats       <- sort(unique(cat_vec))
    agg_raw    <- do.call(rbind, lapply(cats, function(cat) {
      rows <- which(cat_vec == cat)
      if (length(rows) == 1) abund_raw[rows, , drop=FALSE]
      else colSums(abund_raw[rows, , drop=FALSE], na.rm=TRUE)
    }))
    rownames(agg_raw) <- cats

    # Derive requested metric
    mat <- if (count == "percent_full") {
      # Percentage over full dataset: agg_raw / total_reads_per_sample * 100
      col_sums_full <- colSums(abund_raw, na.rm=TRUE)
      col_sums_full[col_sums_full == 0] <- 1
      sweep(agg_raw, 2, col_sums_full, "/") * 100
    } else if (count == "tpm_full") {
      # TPM: use precomputed per-gene TPM from SQMtools, sum by COG class
      tpm_gene <- tryCatch(as.matrix(proj$functions[[cog_db]]$tpm), error=function(e) NULL)
      if (!is.null(tpm_gene)) {
        tpm_gene <- tpm_gene[rownames(tpm_gene) %in% rownames(abund_raw), , drop=FALSE]
        cv_tpm   <- cat_vec[match(rownames(tpm_gene), rownames(abund_raw))]
        m <- do.call(rbind, lapply(cats, function(cat) {
          rows <- which(cv_tpm == cat)
          if (length(rows) == 0) return(rep(0, ncol(tpm_gene)))
          if (length(rows) == 1) as.numeric(tpm_gene[rows, , drop=TRUE])
          else colSums(tpm_gene[rows, , drop=FALSE], na.rm=TRUE)
        }))
        rownames(m) <- cats; m
      } else agg_raw
    } else {
      agg_raw   # raw abundances
    }

    mat <- as.matrix(mat)
    rownames(mat) <- cats

    # Exclude "Function unknown" and similar
    if (isTRUE(input$cog_class_excl_unknown)) {
      excl_pat <- "^[Ff]unction unknown$|^[Gg]eneral function prediction only$|^[Ff]unction Unknown$"
      mat <- mat[!grepl(excl_pat, rownames(mat)), , drop=FALSE]
    }
    req(nrow(mat) > 0)

    # Apply rescaling
    scl <- input$plot_scale %||% "none"
    row_order <- order(rowSums(mat, na.rm=TRUE), decreasing=TRUE)
    if (scl == "log") {
      mat <- log10(mat + 1)
    } else if (scl == "zscore") {
      mat <- t(apply(mat, 1, function(r) {
        s <- sd(r, na.rm=TRUE)
        if (is.na(s) || s == 0) r - mean(r, na.rm=TRUE) else (r - mean(r, na.rm=TRUE)) / s
      }))
    }
    mat <- mat[row_order, , drop=FALSE]

    pw <- input$func_plot_width  %||% 1200
    ph <- nrow(mat) * 28 + 120

    p <- plot_ly(
      z             = mat,
      x             = colnames(mat),
      y             = rownames(mat),
      type          = "heatmap",
      colorscale    = input$func_palette %||% "Blues",
      reversescale  = FALSE,
      colorbar      = list(lenmode="pixels", len=200, thickness=15),
      hovertemplate = "<b>%{y}</b><br>Sample: %{x}<br>Value: %{z:.4f}<extra></extra>",
      width = pw, height = ph
    )
    p <- layout(p,
      xaxis  = list(title="", tickfont=list(size=fs), tickangle=-45, automargin=TRUE),
      yaxis  = list(title="", tickfont=list(size=fs), automargin=TRUE, autorange="reversed"),
      margin = list(l=10, r=10, t=30, b=60),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
    )
    config(p, displayModeBar=FALSE)
  })
  output$sqm_cog_class_plot <- renderPlotly({ cog_class_reactive() })

  # ── KEGG functional classes reactive ───────────────────────────────────────────
  kegg_class_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data()
    req(input$plot_type == "kegg_class")
    req(!is.null(KEGG_CATEGORIES))
    fs    <- input$func_font_size    %||% 11
    count <- input$kegg_class_count  %||% "abund"
    level <- input$kegg_class_level  %||% "l1"

    # Subset samples
    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp))
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)

    kegg_db   <- names(proj$functions)[toupper(names(proj$functions)) == "KEGG"][1]
    req(!is.null(kegg_db))
    abund_raw <- tryCatch(as.matrix(proj$functions[[kegg_db]]$abund), error=function(e) NULL)
    req(!is.null(abund_raw) && nrow(abund_raw) > 0)

    # Build a weight table: for each (KO, category) pair, weight = 1 / n_categories_of_that_KO
    # This distributes reads proportionally across all categories a KO belongs to,
    # so totals remain additive and L1 = sum of its L2 = sum of its L3.
    # Exclude unwanted L1 categories and all their L2/L3 descendants
    kegg_excl_l1 <- tolower(c("Brite Hierarchies",
                               "Not included in pathway or brite",
                               "Not included in pathway",
                               "Human Diseases", "Organismal Systems"))
    kc_all <- KEGG_CATEGORIES[!is.na(KEGG_CATEGORIES$l1) &
                               !tolower(KEGG_CATEGORIES$l1) %in% kegg_excl_l1, ]
    # Also exclude specific L2 categories and their L3 descendants
    kegg_excl_l2 <- tolower(c("Cellular community - Eukaryotes"))
    kc_all <- kc_all[is.na(kc_all$l2) | !tolower(kc_all$l2) %in% kegg_excl_l2, ]

    kc <- kc_all[!is.na(kc_all[[level]]) & nchar(trimws(kc_all[[level]])) > 0, ]
    kc <- kc[kc$id %in% rownames(abund_raw), ]

    # Identify "other" KOs: in abund_raw but not in any kept category at this level
    # This includes: KOs from excluded L1/L2, KOs with no KEGG category at all
    other_ids <- setdiff(rownames(abund_raw), unique(kc$id))
    other_abund <- if (length(other_ids) > 0)
      colSums(abund_raw[other_ids, , drop=FALSE], na.rm=TRUE)
    else NULL

    # Keep kc_full reference for "full dataset" precomputed aggregation
    kc_full <- kc

    # Apply optional category filter (same kegg_cat_l1/l2 inputs as func_kegg)
    sel_l1 <- input$kegg_cat_l1 %||% ""
    sel_l2 <- input$kegg_cat_l2 %||% ""
    if (nchar(sel_l1) > 0) {
      kc <- kc[!is.na(kc$l1) & kc$l1 == sel_l1, ]
      if (nchar(sel_l2) > 0)
        kc <- kc[!is.na(kc$l2) & kc$l2 == sel_l2, ]
    }
    req(nrow(kc) > 0)

    # Count how many categories each KO maps to at this level
    ko_cat_counts <- table(kc$id)
    kc$weight     <- 1 / as.numeric(ko_cat_counts[kc$id])

    cats <- sort(unique(kc[[level]]))

    # Aggregate raw abund with proportional weights (selection only)
    agg_abund <- do.call(rbind, lapply(cats, function(cat) {
      rows <- kc[kc[[level]] == cat, ]
      if (nrow(rows) == 0) return(matrix(0, 1, ncol(abund_raw)))
      weighted <- sweep(abund_raw[rows$id, , drop=FALSE], 1, rows$weight, "*")
      colSums(weighted, na.rm=TRUE)
    }))
    rownames(agg_abund) <- cats

    # Append "Other functions" row if requested
    if (isTRUE(input$kegg_class_show_other) && !is.null(other_abund)) {
      other_row        <- matrix(other_abund, nrow=1, ncol=ncol(agg_abund),
                                 dimnames=list("Other functions", colnames(agg_abund)))
      agg_abund        <- rbind(agg_abund, other_row)
      cats             <- c(cats, "Other functions")
    }

    # Helper: compute TPM for a given agg matrix and kc subset
    compute_tpm <- function(agg, kc_sub, cats_sub) {
      orf_tbl <- tryCatch(proj$orfs$table, error=function(e) NULL)
      if (is.null(orf_tbl)) return(agg)
      ko_col  <- grep("KEGG", colnames(orf_tbl), value=TRUE)[1]
      len_col <- grep("[Ll]ength.*[Nn][Tt]|[Nn][Tt].*[Ll]ength|^Length$", colnames(orf_tbl), value=TRUE)[1]
      if (is.null(ko_col) || is.null(len_col)) return(agg)
      orf_ko  <- as.character(orf_tbl[[ko_col]])
      orf_len <- as.numeric(orf_tbl[[len_col]])
      mean_len_kb <- sapply(cats_sub, function(cat) {
        rows <- kc_sub[kc_sub[[level]] == cat, ]
        lens <- orf_len[orf_ko %in% rows$id]
        if (length(lens) == 0 || all(is.na(lens))) return(1)
        mean(lens, na.rm=TRUE) / 1000
      })
      mean_len_kb[mean_len_kb == 0] <- 1
      rpk <- agg / mean_len_kb
      rpk_sums <- colSums(rpk, na.rm=TRUE)
      rpk_sums[rpk_sums == 0] <- 1
      sweep(rpk, 2, rpk_sums, "/") * 1e6
    }

    # Derive requested metric
    mat <- if (count == "percent_sel") {
      col_sums <- colSums(agg_abund, na.rm=TRUE)
      col_sums[col_sums == 0] <- 1
      sweep(agg_abund, 2, col_sums, "/") * 100
    } else if (count == "percent_full") {
      # Denominator = total reads across ALL KOs in the full (unfiltered) abund matrix
      col_sums_full <- colSums(abund_raw, na.rm=TRUE)
      col_sums_full[col_sums_full == 0] <- 1
      sweep(agg_abund, 2, col_sums_full, "/") * 100
    } else if (count == "tpm_sel") {
      compute_tpm(agg_abund, kc, cats)
    } else if (count == "tpm_full") {
      tpm_gene <- tryCatch(as.matrix(proj$functions[[kegg_db]]$tpm), error=function(e) NULL)
      if (!is.null(tpm_gene)) {
        tpm_gene <- tpm_gene[rownames(tpm_gene) %in% kc_full$id, , drop=FALSE]
        m <- do.call(rbind, lapply(cats, function(cat) {
          ids_in_cat <- kc_full$id[kc_full[[level]] == cat]
          rows <- rownames(tpm_gene)[rownames(tpm_gene) %in% ids_in_cat]
          if (length(rows) == 0) return(matrix(0, 1, ncol(tpm_gene)))
          if (length(rows) == 1) as.numeric(tpm_gene[rows, , drop=TRUE])
          else colSums(tpm_gene[rows, , drop=FALSE], na.rm=TRUE)
        }))
        rownames(m) <- cats; m
      } else agg_abund
    } else {
      agg_abund
    }

    mat <- as.matrix(mat)
    rownames(mat) <- cats
    req(nrow(mat) > 0)

    # Rescale (order computed on raw values first)
    row_order <- order(rowSums(agg_abund, na.rm=TRUE), decreasing=TRUE)
    scl <- input$plot_scale %||% "none"
    if (scl == "log") {
      mat <- log10(mat + 1)
    } else if (scl == "zscore") {
      mat <- t(apply(mat, 1, function(r) {
        s <- sd(r, na.rm=TRUE)
        if (is.na(s) || s == 0) r - mean(r, na.rm=TRUE) else (r - mean(r, na.rm=TRUE)) / s
      }))
    }
    mat <- mat[row_order, , drop=FALSE]

    pw <- input$func_plot_width  %||% 1200
    ph <- nrow(mat) * 28 + 120

    p <- plot_ly(
      z=mat, x=colnames(mat), y=rownames(mat),
      type="heatmap", colorscale=input$func_palette %||% "Blues",
      reversescale=FALSE,
      colorbar=list(lenmode="pixels", len=200, thickness=15),
      hovertemplate="<b>%{y}</b><br>Sample: %{x}<br>Value: %{z:.4f}<extra></extra>",
      width=pw, height=ph
    )
    p <- layout(p,
      xaxis  = list(title="", tickfont=list(size=fs), tickangle=-45, automargin=TRUE),
      yaxis  = list(title="", tickfont=list(size=fs), automargin=TRUE, autorange="reversed"),
      margin = list(l=10, r=10, t=30, b=60),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"
    )
    config(p, displayModeBar=FALSE)
  })
  output$sqm_kegg_class_plot <- renderPlotly({ kegg_class_reactive() })

  # ── Taxonomy heatmap reactive ──
  tax_hm_reactive <- reactive({
    req(sqm_data()); proj <- sqm_data()
    req(input$plot_type == "taxonomy_heatmap")

    all_smp <- tryCatch(proj$misc$samples, error=function(e) NULL)
    sel_smp <- input$plot_samples
    if (!is.null(all_smp) && !is.null(sel_smp) && length(sel_smp) > 0 &&
        !setequal(sel_smp, all_smp))
      proj <- tryCatch(subsetSamples(proj, sel_smp), error=function(e) proj)

    rank  <- input$tax_hm_rank  %||% "phylum"
    count <- input$tax_hm_count %||% "percent"
    mat   <- tryCatch(as.matrix(proj$taxa[[rank]][[count]]), error=function(e) NULL)
    req(!is.null(mat) && nrow(mat) > 0)

    # Filters
    mat <- mat[rowSums(mat, na.rm=TRUE) > 0, , drop=FALSE]
    mat <- mat[!grepl("^[Nn]o CDS$", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_hm_ignore_unmapped))
      mat <- mat[!grepl("^[Uu]nmapped$|^[Nn]o [Hh]it", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_hm_ignore_unclassified))
      mat <- mat[!grepl("^[Uu]nclassified$", rownames(mat)), , drop=FALSE]
    if (isTRUE(input$tax_hm_ignore_ambiguous))
      mat <- mat[!grepl("^[Uu]nclassified ", rownames(mat)), , drop=FALSE]
    req(nrow(mat) > 0)

    # Compute order on raw values BEFORE scaling (stable order regardless of rescale)
    n <- min(input$tax_hm_n %||% 30, nrow(mat))
    top_idx <- order(rowSums(mat, na.rm=TRUE), decreasing=TRUE)[seq_len(n)]
    mat <- mat[top_idx, , drop=FALSE]
    row_order <- order(rowSums(mat, na.rm=TRUE), decreasing=TRUE)

    # Apply rescaling after order is fixed
    tax_hm_scl <- input$tax_hm_scale %||% "none"
    if (tax_hm_scl == "log") {
      mat <- log10(mat + 1)
    } else if (tax_hm_scl == "zscore") {
      mat <- t(apply(mat, 1, function(r) {
        s <- sd(r, na.rm=TRUE)
        if (is.na(s) || s == 0) r - mean(r, na.rm=TRUE) else (r - mean(r, na.rm=TRUE)) / s
      }))
    }
    mat <- mat[row_order, , drop=FALSE]

    pw <- input$tax_hm_width  %||% 1200
    fs <- input$tax_hm_font   %||% 11
    # Compute height so row size matches func heatmaps (~28px/row + margins)
    # Fix height so cell size matches func heatmaps (28px/row + overhead)
    ph <- nrow(mat) * 28 + 120

    p <- plot_ly(
      z=mat, x=colnames(mat), y=rownames(mat),
      type="heatmap", colorscale=input$tax_hm_palette %||% "Blues",
      reversescale=FALSE,
      colorbar=list(lenmode="pixels", len=200, thickness=15),
      hovertemplate="<b>%{y}</b><br>Sample: %{x}<br>Value: %{z:.4f}<extra></extra>",
      width=pw, height=ph
    )
    p <- layout(p,
      xaxis  = list(title="", tickfont=list(size=fs), tickangle=-45, automargin=TRUE),
      yaxis  = list(title="", tickfont=list(size=fs), automargin=TRUE, autorange="reversed"),
      margin = list(l=10, r=10, t=30, b=60),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"
    )
    config(p, displayModeBar=FALSE)
  })
  output$sqm_tax_hm_plot <- renderPlotly({ tax_hm_reactive() })

  # ── Helper: extract hclust dendrogram as segment coordinates ──────────────
  # Returns data.frame(x0,y0,x1,y1). Leaf x-positions are integers 1..n
  # matching the left-to-right order of hc$order.
  # Helper: hclust -> segment data.frame for ggplot2 dendrograms
  # Helper: hclust -> segment df for ggplot2 dendrograms

  output$plot_status_badge <- renderUI({
    if (is.null(sqm_data())) tags$span(class="badge",style="background:#eef2f7;color:#7a90a8;font-size:0.72rem;border:1px solid #d0dae6;","No project")
    else tags$span(class="badge",style="background:rgba(26,158,110,0.1);color:#1a9e6e;font-size:0.72rem;border:1px solid rgba(26,158,110,0.3);","\u25cf Ready")
  })
  output$plot_download_ui <- renderUI({
    req(sqm_data())
    pt <- input$plot_type %||% ""
    is_plotly <- pt == "taxonomy_bar" || pt == "taxonomy_heatmap" ||
                 startsWith(pt, "func_") || pt == "cog_class" || pt == "kegg_class"
    if (is_plotly) {
      plot_id <- switch(pt,
        taxonomy_bar     = "sqm_tax_plot",
        taxonomy_heatmap = "sqm_tax_hm_plot",
        cog_class        = "sqm_cog_class_plot",
        kegg_class       = "sqm_kegg_class_plot",
        "sqm_func_plot")
      tags$div(style = "margin-top:5px;",
        tags$button(
          class = "btn btn-outline-secondary w-100",
          style = "font-size:0.82rem;",
          onclick = sprintf(paste0(
            "var gd = document.querySelector('#%s');",
            "if (!gd || !gd._fullLayout) { alert('Plot not ready'); return; }",
            "Plotly.toImage(gd, {format:'png', width: gd._fullLayout.width, height: gd._fullLayout.height, scale:2}).then(function(url){",
            "  var a = document.createElement('a');",
            "  a.href = url; a.download = 'sqm_plot.png'; a.click();",
            "});"), plot_id),
          "Download PNG"))
    } else {
      tags$div(style = "margin-top:5px;",
        downloadButton("download_plot", "Download PNG", class = "btn-outline-secondary w-100"))
    }
  })

  output$download_plot <- downloadHandler(
    filename = function() paste0("sqm_plot_", Sys.Date(), ".png"),
    content  = function(file) {
      w <- isolate(input$tax_plot_width  %||% 800)
      h <- isolate(input$tax_plot_height %||% 560)
      png(file, width = w, height = h, res = 150, bg = "#ffffff")
      print(plot_reactive())
      dev.off()
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
  # \u2500\u2500 Helper: enrich function table with Name / Path columns \u2500\u2500
  # File format: header row is "\tName\tPath" (first col empty = row ID)
  #              data rows:    "K00001\talcohol dehydrogenase...\tMetabolism;..."
  enrich_fun_table <- function(proj, db, d) {
    ids <- rownames(d)

    # SQMtools stores names in proj$misc$<DB>_names and paths in proj$misc$<DB>_paths
    names_vec <- tryCatch(proj$misc[[paste0(db, "_names")]], error = function(e) NULL)
    paths_vec <- tryCatch(proj$misc[[paste0(db, "_paths")]], error = function(e) NULL)

    if (!is.null(names_vec) && length(names_vec) > 0) {
      name_col <- names_vec[ids]; name_col[is.na(name_col)] <- ""
      if (!is.null(paths_vec) && length(paths_vec) > 0) {
        path_col <- paths_vec[ids]; path_col[is.na(path_col)] <- ""
        return(cbind(Name = name_col, Path = path_col, d))
      }
      return(cbind(Name = name_col, d))
    }
    d
  }


  get_table_data <- reactive({ tbl_data_rv() })
  output$tbl_main_ui <- renderUI({
    s <- tbl_status()
    if (s == "idle") return(
      tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:2rem; margin-bottom:8px;", "📄"),
        tags$div("Select a table from the sidebar.")))
    if (s == "loading") return(
      tags$div(
        style = paste0("display:flex; align-items:center; gap:10px;",
                       "padding:1.5rem; color:#1a6eb5; font-size:0.88rem;"),
        tags$span(style="font-size:1.5rem;", "◌"),
        tags$span("Loading results, please wait…")))
    if (s == "ready") DTOutput("data_table") else NULL
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
    num_cols <- which(sapply(df, is.numeric)) - 1L  # 0-based for DT
    # Use rowCallback to format all numeric cells after render
    fmt_callback <- JS(paste0(
      "function(row, data, index) {",
      "  var ncols = ", length(which(sapply(df, is.numeric))), ";",
      "  var start = ", ncol(df) - length(which(sapply(df, is.numeric))), ";",
      "  for (var i = start; i < data.length; i++) {",
      "    var n = parseFloat(data[i]);",
      "    if (!isNaN(n)) {",
      "      var fmt = Math.abs(n) >= 10000 ? n.toExponential(3) : parseFloat(n.toFixed(3)).toString();",
      "      $('td:eq(' + i + ')', row).html(fmt);",
      "    }",
      "  }",
      "}"
    ))
    pl <- as.integer(isolate(input$tbl_page_length) %||% 20)
    datatable(df, rownames=FALSE,
      options=list(pageLength = if (pl == -1) nrow(df) else pl,
                   scrollX=TRUE, dom="frtip",
                   rowCallback = fmt_callback),
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
  # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  #  KRONA
  # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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
  # Krona inline in Plots tab
  output$sqm_krona_inline_ui <- renderUI({
    kf <- krona_file(); req(kf, file.exists(kf))
    tags$iframe(src = session$fileUrl("krona_inline", kf, contentType = "text/html"),
      width = "100%", height = "600px", frameborder = "0")
  })

  output$krona_inline <- downloadHandler(
    filename = "krona.html",
    content  = function(file) { kf <- krona_file(); file.copy(kf, file) }
  )

  observeEvent(input$do_krona_inline, {
    req(sqm_data()); req(kt_available())
    krona_status("generating"); krona_file(NULL)
    tryCatch({
      proj     <- sqm_data()
      sel_smp  <- input$plot_samples
      all_smp  <- proj$misc$samples
      if (!is.null(sel_smp) && length(sel_smp) > 0 && !setequal(sel_smp, all_smp))
        proj <- tryCatch(subsetSamples(proj, sel_smp), error = function(e) proj)
      out_file <- tempfile(fileext = ".html")
      exportKrona(proj, output_name = out_file)
      if (file.exists(out_file)) { krona_file(out_file); krona_status("ready") }
      else { krona_status("error"); showNotification("Krona file not generated.", type="error") }
    }, error = function(e) {
      krona_status("error")
      showNotification(paste("Krona error:", e$message), type="error", duration=10)
    })
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
    # Krona uses position:fixed for #options \u2014 change to position:absolute so it
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

  # \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
  # Pathways tab \u2014 exportPathway (SQMtools wrapper for pathview)
  # \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
  pw_status   <- reactiveVal("idle")   # idle | generating | ready | error
  pw_img_dir   <- reactiveVal(NULL)     # tempdir where PNGs were written
  pw_kegg_cache <- file.path(tempdir(), "sqmxplore_kegg_cache")  # shared XML/PNG cache

  # Helper: ensure a cached KEGG XML file is valid; delete and re-download if not
  .ensure_valid_xml <- function(pid, cache_dir) {
    xml_path <- file.path(cache_dir, paste0("ko", pid, ".xml"))
    if (file.exists(xml_path)) {
      first_bytes <- tryCatch(readLines(xml_path, n = 1, warn = FALSE), error = function(e) "")
      if (!grepl("^\\s*<", first_bytes)) {
        message("Cached XML for ", pid, " is invalid, deleting and re-downloading")
        file.remove(xml_path)
      }
    }
    if (!file.exists(xml_path))
      tryCatch(pathview::download.kegg(pathway.id=pid, species="ko", kegg.dir=cache_dir),
               error = function(e) message("KEGG XML download failed: ", e$message))
    xml_path
  }
  pw_img_files <- reactiveVal(NULL)    # character vector of PNG paths
  pw_legend    <- reactiveVal(NULL)    # list: colors, min, max, log_sc, cnt, fc
  pw_nodes     <- reactiveVal(NULL)    # data.frame: ko_ids, x, y, w, h, label, name
  pw_pathway_choices <- reactiveVal(NULL)  # named vector: "Name [id]" = "id"

  # \u2500\u2500 Pathway tree is static; just signal ready on project load \u2500\u2500
  observeEvent(sqm_data(), {
    req(sqm_data())
    pw_pathway_choices(TRUE)   # just a flag to trigger renderUI
  })

  # \u2500\u2500 Pathway selector: populated on project load \u2500\u2500
  output$pw_pathway_select_ui <- renderUI({
    # Show placeholder if project not loaded
    if (is.null(pw_pathway_choices()) || is.null(sqm_data())) {
      return(tags$div(style="font-size:0.78rem; color:var(--muted); padding:4px 0;",
        "Load a project to browse pathways."))
    }

    # Build collapsible tree from KEGG_HIERARCHY
    # Uses HTML details/summary \u2014 no JS needed
    search_box <- tags$input(
      id = "pw_search", type = "text",
      placeholder = "Search pathway\u2026",
      oninput = "filterPwTree(this.value)",
      style = paste0(
        "width:100%; box-sizing:border-box; padding:3px 6px;",
        "font-size:0.78rem; border:1px solid var(--border);",
        "border-radius:4px; margin-bottom:6px;",
        "background:var(--surface); color:var(--text);"))

    # Build tree HTML
    # Exclude "1.0 Global and overview maps" — these are composite maps that
    # cannot be rendered by pathview in the same way as individual pathway maps.
    KEGG_HIERARCHY_EXCL_L2 <- "1.0 Global and overview maps"
    tree_items <- lapply(names(KEGG_HIERARCHY), function(l1) {
      l2_items <- lapply(names(KEGG_HIERARCHY[[l1]]), function(l2) {
        if (l2 %in% KEGG_HIERARCHY_EXCL_L2) return(NULL)
        pathways <- KEGG_HIERARCHY[[l1]][[l2]]
        pw_links <- lapply(pathways, function(pw) {
          tags$div(
            class = "pw-item",
            "data-id" = pw$id,
            "data-name" = tolower(paste(pw$name, pw$id)),
            style = "padding:2px 4px 2px 8px; cursor:pointer; font-size:0.75rem; border-radius:3px;",
            onclick = sprintf(
              "event.stopPropagation(); Shiny.setInputValue('pw_pathway_id','%s',{priority:'event'}); document.querySelectorAll('.pw-item').forEach(function(el){el.style.background=''}); this.style.background='var(--accent-light)'; document.getElementById('pw_selected_label').textContent='%s [%s]';",
              pw$id, gsub("'", "\\\\'", pw$name), pw$id),
            tags$span(style="color:var(--muted); margin-right:4px; font-family:monospace;", pw$id),
            pw$name
          )
        })
        tags$details(
          style = "margin-left:8px;",
          tags$summary(
            style = paste0(
              "font-size:0.75rem; font-weight:600; color:var(--muted);",
              "cursor:pointer; padding:2px 2px; list-style:none;",
              "display:flex; align-items:center; gap:4px;"),
            tags$span(class="pw-chevron", style="font-size:0.6rem;", "\u25b6"),
            l2
          ),
          pw_links
        )
      })
      l2_items <- Filter(Negate(is.null), l2_items)
      tags$details(
        open = NA,  # start open
        style = "margin-bottom:2px;",
        tags$summary(
          style = paste0(
            "font-size:0.8rem; font-weight:700; color:var(--text);",
            "cursor:pointer; padding:3px 2px; list-style:none;",
            "display:flex; align-items:center; gap:4px;",
            "border-bottom:1px solid var(--border);"),
          tags$span(class="pw-chevron", style="font-size:0.65rem;", "\u25b6"),
          l1
        ),
        l2_items
      )
    })

    selected_label <- tags$div(
      id = "pw_selected_label",
      style = paste0(
        "font-size:0.75rem; color:var(--muted); font-style:italic;",
        "margin-bottom:4px; min-height:1.2em;"),
      if (!is.null(input$pw_pathway_id) && nchar(input$pw_pathway_id) > 0) {
        # Find name for current selection
        pid_cur <- input$pw_pathway_id
        pw_name <- tryCatch({
          found <- ""
          for (l1 in KEGG_HIERARCHY) for (l2 in l1) for (pw in l2)
            if (identical(pw$id, pid_cur)) { found <- pw$name; break }
          found
        }, error=function(e) "")
        if (nchar(pw_name) > 0) paste0(pw_name, " [", pid_cur, "]")
        else paste0("Selected: ", pid_cur)
      } else
        "None selected"
    )

    tree_css <- tags$style(HTML(
      "details[open] > summary .pw-chevron { transform: rotate(90deg); }
       .pw-item:hover { background: var(--accent-light) !important; }
       details > summary { outline: none; }
       details > summary::-webkit-details-marker { display: none; }"
    ))

    search_js <- tags$script(HTML(
      "function filterPwTree(q) {
        q = q.toLowerCase().trim();
        document.querySelectorAll('.pw-item').forEach(function(el) {
          var match = !q || el.getAttribute('data-name').includes(q);
          el.style.display = match ? '' : 'none';
        });
        document.querySelectorAll('details').forEach(function(d) {
          var vis = Array.from(d.querySelectorAll('.pw-item')).some(function(el) {
            return el.style.display !== 'none';
          });
          d.style.display = vis ? '' : 'none';
          if (q && vis) d.open = true;
          else if (!q) { d.style.display = ''; }
        });
      }"
    ))

    tagList(
      tree_css,
      search_box,
      selected_label,
      tags$div(
        style = paste0(
          "max-height:320px; overflow-y:auto; border:1px solid var(--border);",
          "border-radius:4px; padding:4px; background:var(--surface);"),
        tree_items
      ),
      search_js
    )
  })

  output$pw_sample_selector_ui <- renderUI({
    req(sqm_data())
    samples <- tryCatch(sqm_data()$misc$samples, error=function(e) NULL)
    req(samples)
    tags$div(class = "sidebar-box",
      tags$div(class = "form-label", "Samples"),
      tags$div(style = "display:flex; flex-wrap:wrap; gap:2px; margin-top:2px;",
        lapply(samples, function(s) {
          is_sel <- is.null(input$pw_samples) || s %in% input$pw_samples
          tags$label(
            style = paste0(
              "display:inline-flex; align-items:center; gap:3px;",
              "font-size:0.72rem; padding:2px 5px; border-radius:3px; cursor:pointer;",
              "border:1px solid ", if (is_sel) "#3b9ede" else "var(--border)", ";",
              "background:", if (is_sel) "rgba(59,158,222,0.08)" else "transparent", ";"),
            tags$input(
              type="checkbox", name="pw_samples", value=s,
              checked = if (is_sel) NA else NULL,
              style="margin:0; width:11px; height:11px;",
              onclick = paste0(
                "var cb=this; var vals=[];",
                "document.querySelectorAll('input[name=pw_samples]').forEach(function(el){",
                "if(el.checked) vals.push(el.value);});",
                "Shiny.setInputValue('pw_samples', vals, {priority:'event'});",
                "var lbl=cb.closest('label');",
                "lbl.style.borderColor=cb.checked?'#3b9ede':'var(--border)';",
                "lbl.style.background=cb.checked?'rgba(59,158,222,0.08)':'transparent';")),
            s)
        })
      )
    )
  })

  output$func_category_ui <- renderUI({
    pt <- input$plot_type
    req(startsWith(pt, "func_") || pt == "kegg_class")

    if (pt == "func_cog" && !is.null(COG_CATEGORIES)) {
      cats <- sort(setdiff(
        unique(COG_CATEGORIES$category),
        c("Function unknown", "General function prediction only")))
      tags$div(class="sidebar-box", style="margin-top:8px;",
        tags$div(class="form-label", "COG category"),
        selectInput("cog_category", NULL,
          choices  = c("All categories" = "", setNames(cats, cats)),
          selected = input$cog_category %||% ""))

    } else if ((pt == "func_kegg" || pt == "kegg_class") && !is.null(KEGG_CATEGORIES)) {
      kegg_level <- if (pt == "kegg_class") input$kegg_class_level %||% "l1" else "l3"
      if (pt == "kegg_class" && kegg_level == "l1") return(NULL)
      sel_l1 <- input$kegg_cat_l1 %||% ""
      sel_l2 <- if (pt == "kegg_class" && kegg_level == "l2") "" else input$kegg_cat_l2 %||% ""
      show_l2_in_tree <- !(pt == "kegg_class" && kegg_level == "l2")
      l1_vals <- sort(intersect(
        unique(KEGG_CATEGORIES$l1[!is.na(KEGG_CATEGORIES$l1)]),
        KEGG_L1_SHOW))

      tree_items <- lapply(l1_vals, function(l1) {
        if (!show_l2_in_tree) {
          is_sel_l1_flat <- sel_l1 == l1 && sel_l2 == ""
          return(tags$div(
            class = "pw-item",
            style = paste0(
              "padding:3px 4px; cursor:pointer; font-size:0.8rem; font-weight:700;",
              "border-radius:3px; color:var(--text);",
              if (is_sel_l1_flat) "background:var(--accent-light);" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('kegg_cat_l1','%s',{priority:'event'});
               Shiny.setInputValue('kegg_cat_l2','',{priority:'event'});
               document.querySelectorAll('.pw-item').forEach(function(el){el.style.background=''});
               this.style.background='var(--accent-light)';",
              gsub("'", "\\'", l1, fixed=TRUE)),
            l1))
        }
        l2_vals <- sort(unique(KEGG_CATEGORIES$l2[
          KEGG_CATEGORIES$l1 == l1 & !is.na(KEGG_CATEGORIES$l2)]))

        l2_items <- lapply(l2_vals, function(l2) {
          is_sel <- sel_l1 == l1 && sel_l2 == l2
          tags$div(
            class = "pw-item",
            style = paste0(
              "padding:2px 4px 2px 8px; cursor:pointer; font-size:0.75rem;",
              "border-radius:3px;",
              if (is_sel) "background:var(--accent-light);" else ""),
            onclick = sprintf(
              "event.stopPropagation();
               Shiny.setInputValue('kegg_cat_l1','%s',{priority:'event'});
               Shiny.setInputValue('kegg_cat_l2','%s',{priority:'event'});
               document.querySelectorAll('.pw-item').forEach(function(el){el.style.background=''});
               this.style.background='var(--accent-light)';",
              gsub("'","\\\\'",l1), gsub("'","\\\\'",l2)),
            l2)
        })

        # Add "All in <l1>" item at top of each l1 group
        is_sel_l1 <- sel_l1 == l1 && sel_l2 == ""
        all_item <- tags$div(
          class = "pw-item",
          style = paste0(
            "padding:2px 4px 2px 8px; cursor:pointer; font-size:0.75rem;",
            "border-radius:3px; font-style:italic; color:var(--muted);",
            if (is_sel_l1) "background:var(--accent-light);" else ""),
          onclick = sprintf(
            "event.stopPropagation();
             Shiny.setInputValue('kegg_cat_l1','%s',{priority:'event'});
             Shiny.setInputValue('kegg_cat_l2','',{priority:'event'});
             document.querySelectorAll('.pw-item').forEach(function(el){el.style.background=''});
             this.style.background='var(--accent-light)';",
            gsub("'","\\\\'",l1)),
          paste0("All ", l1))

        tags$details(
          if (sel_l1 == l1) list(open=NA) else list(),
          style = "margin-bottom:2px;",
          tags$summary(
            style = paste0(
              "font-size:0.8rem; font-weight:700; color:var(--text);",
              "cursor:pointer; padding:3px 2px; list-style:none;",
              "display:flex; align-items:center; gap:4px;"),
            tags$span(class="pw-chevron", style="font-size:0.6rem;", "\u25b6"),
            l1),
          all_item,
          l2_items
        )
      })

      # "All categories" item
      all_cats_item <- tags$div(
        style = paste0(
          "padding:3px 4px; cursor:pointer; font-size:0.78rem;",
          "border-radius:3px; font-style:italic; color:var(--muted); margin-bottom:4px;",
          if (sel_l1=="" && sel_l2=="") "background:var(--accent-light);" else ""),
        onclick = "Shiny.setInputValue('kegg_cat_l1','',{priority:'event'});
                   Shiny.setInputValue('kegg_cat_l2','',{priority:'event'});
                   document.querySelectorAll('.pw-item').forEach(function(el){el.style.background=''});
                   this.style.background='var(--accent-light)';",
        "All categories")

      # Selected label
      sel_label <- if (nchar(sel_l1) > 0)
        paste0(sel_l1, if (nchar(sel_l2)>0) paste0(" \u203a ", sel_l2) else " (all)")
      else "All categories"

      tags$div(class="sidebar-box", style="margin-top:8px;",
        tags$div(class="form-label", "KEGG category"),
        tags$div(
          style = paste0(
            "font-size:0.75rem; padding:3px 6px; margin-bottom:4px;",
            "background:var(--accent-light); border-radius:3px;",
            "border:1px solid var(--border); color:var(--text);"),
          id = "kegg_cat_label",
          sel_label),
        tags$div(
          style = paste0(
            "max-height:220px; overflow-y:auto; border:1px solid var(--border);",
            "border-radius:4px; padding:4px; background:var(--surface);"),
          all_cats_item,
          tree_items)
      )
    } else NULL
  })


  output$plot_sample_selector_ui <- renderUI({
    req(sqm_data())
    samples <- tryCatch(sqm_data()$misc$samples, error=function(e) NULL)
    req(samples)
    tags$div(class = "sidebar-box",
      tags$div(class = "form-label", "Samples"),
      tags$div(style = "display:flex; flex-wrap:wrap; gap:2px; margin-top:2px;",
        lapply(samples, function(s) {
          is_sel <- is.null(input$plot_samples) || s %in% input$plot_samples
          tags$label(
            style = paste0(
              "display:inline-flex; align-items:center; gap:3px;",
              "font-size:0.72rem; padding:2px 5px; border-radius:3px; cursor:pointer;",
              "border:1px solid ", if (is_sel) "#3b9ede" else "var(--border)", ";",
              "background:", if (is_sel) "rgba(59,158,222,0.08)" else "transparent", ";"),
            tags$input(
              type="checkbox", name="plot_samples", value=s,
              checked = if (is_sel) NA else NULL,
              style="margin:0; width:11px; height:11px;",
              onclick = paste0(
                "var cb=this; var vals=[];",
                "document.querySelectorAll('input[name=plot_samples]').forEach(function(el){",
                "if(el.checked) vals.push(el.value);});",
                "Shiny.setInputValue('plot_samples', vals, {priority:'event'});",
                "var lbl=cb.closest('label');",
                "lbl.style.borderColor=cb.checked?'#3b9ede':'var(--border)';",
                "lbl.style.background=cb.checked?'rgba(59,158,222,0.08)':'transparent';")),
            s)
        })
      )
    )
  })

  output$pw_count_ui <- renderUI({
    all_counts <- c("Copy number" = "copy_number", "TPM" = "tpm",
                    "Raw abundances" = "abund", "Percentages" = "percent",
                    "Base counts" = "bases")
    proj <- sqm_data()
    if (is.null(proj)) {
      # No project loaded yet \u2014 show all, exportPathway will validate
      avail <- all_counts
    } else {
      avail <- Filter(function(m) {
        if (m == "percent") return(TRUE)  # always computable
        tryCatch(!is.null(proj$functions$KEGG[[m]]) &&
                 nrow(proj$functions$KEGG[[m]]) > 0,
                 error = function(e) FALSE)
      }, all_counts)
      if (length(avail) == 0) avail <- c("Percentages" = "percent")
    }
    sel <- if (length(avail) == 0) NULL else if ("copy_number" %in% avail) "copy_number" else avail[[1]]
    selectInput("pw_count", NULL, choices = avail, selected = sel)
  })

  pathview_available <- reactive({
    requireNamespace("pathview", quietly = TRUE)
  })

  output$pw_pathview_check_ui <- renderUI({
    if (pathview_available()) {
      tags$div(style = "font-size:0.82rem; padding:6px 0;",
        tags$span(style = "color:#1a9e6e; margin-right:5px;", "\u2714"),
        tags$span(style = "color:#7a90a8;", "pathview: "),
        tags$span(style = "color:#1a9e6e; font-weight:600;", "available"))
    } else {
      tags$div(style = "font-size:0.82rem; padding:6px 0;",
        tags$span(style = "color:#c0392b; margin-right:5px;", "\u2715"),
        tags$span(style = "color:#7a90a8;", "pathview: "),
        tags$span(style = "color:#c0392b; font-weight:600;", "NOT FOUND"),
        tags$div(class = "path-info", style = "margin-top:4px; color:#c0392b;",
          "Install with: ",
          tags$code(style = "font-size:0.75rem;",
            'BiocManager::install("pathview")'))
      )
    }
  })

  # Show fold-change group pickers only in foldchange mode
  output$pw_foldchange_ui <- renderUI({
    req(input$pw_mode == "foldchange")
    req(sqm_data())
    samples <- tryCatch(sqm_data()$samples, error = function(e) NULL)
    req(samples)
    tagList(
      tags$div(class = "form-label", style = "margin-top:6px;", "Group A (reference)"),
      checkboxGroupInput("pw_fc_groupA", NULL, choices = samples,
                         selected = samples[1]),
      tags$div(class = "form-label", style = "margin-top:4px;", "Group B"),
      checkboxGroupInput("pw_fc_groupB", NULL, choices = samples,
                         selected = if (length(samples) > 1) samples[2] else samples[1])
    )
  })

  observe({
    # Auto-trigger on any pathway control change
    input$pw_pathway_id; input$pw_count; input$pw_mode
    input$pw_samples; input$pw_fc_groupA; input$pw_fc_groupB
    pid <- trimws(input$pw_pathway_id %||% "")
    isolate({
    req(sqm_data())
    req(nchar(pid) == 5)
    if (!pathview_available()) {
      showNotification(
        'pathview not installed. Run: BiocManager::install("pathview")',
        type = "error", duration = 10)
      return()
    }
    if (nchar(pid) == 0) {
      showNotification("Please select a KEGG Pathway from the dropdown.", type = "warning", duration=6)
      return()
    }
    pw_status("generating"); pw_img_files(NULL)

    shinyjs::delay(50, tryCatch({
      proj   <- sqm_data()
      mode    <- input$pw_mode
      log_sc  <- FALSE
      cnt     <- input$pw_count %||% "copy_number"
      fc_grps <- NULL
      if (mode == "foldchange") {
        grpA <- input$pw_fc_groupA; grpB <- input$pw_fc_groupB
        if (length(grpA) > 0 && length(grpB) > 0)
          fc_grps <- list(grpA, grpB)
      }
      # Normalise sample selection: NULL / empty / all-selected \u2192 same key
      all_smp_names <- tryCatch(sort(sqm_data()$misc$samples), error=function(e) character(0))
      sel_smp_raw   <- input$pw_samples
      sel_smp_norm  <- if (is.null(sel_smp_raw) || length(sel_smp_raw) == 0 ||
                           setequal(sel_smp_raw, all_smp_names))
                         all_smp_names
                       else sort(sel_smp_raw)
      # Shared KEGG cache dir (XML + orig PNG) \u2014 reused across runs
      dir.create(pw_kegg_cache, showWarnings = FALSE, recursive = TRUE)
      # Per-run output dir \u2014 unique per pathway+count+mode+log+samples+fc
      fc_key  <- if (!is.null(fc_grps))
                   paste0("fc_", paste(sort(fc_grps[[1]]),collapse=""),
                          "_vs_", paste(sort(fc_grps[[2]]),collapse=""))
                 else ""
      smp_key <- substr(gsub("[^a-zA-Z0-9]","", paste(sel_smp_norm, collapse="-")), 1, 30)
      run_key <- paste0(pid, "_", cnt, "_", mode,
                        if (log_sc) "_log" else "", "_", smp_key, fc_key)
      outdir  <- file.path(tempdir(), paste0("sqmxplore_pw_", run_key))
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      # Validate foldchange groups
      if (mode == "foldchange" && is.null(fc_grps)) {
        showNotification("Select at least one sample for each fold-change group.",
                         type = "warning", duration = 6)
        pw_status("idle"); return()
      }

      # Pre-compute sample colors so map and legend are consistent
      smp_for_colors <- tryCatch(proj$misc$samples, error=function(e) character(0))
      if (is.null(smp_for_colors) || length(smp_for_colors)==0)
        smp_for_colors <- tryCatch(colnames(proj$functions$KEGG$abund), error=function(e) character(0))
      n_smp_ep <- length(smp_for_colors)
      auto_cols <- if (n_smp_ep == 1) "#E41A1C" else
        hcl(h = seq(15, 375, length.out = n_smp_ep + 1)[seq_len(n_smp_ep)], c = 100, l = 55)

      # If output PNGs already exist for this exact config, skip re-rendering
      existing_pngs <- list.files(outdir, pattern=paste0("sqmxplore_",pid,".*[.]png$"),
                                  full.names=TRUE)
      existing_pngs <- existing_pngs[!grepl("[.]legend[.]", basename(existing_pngs))]
      skip_render <- length(existing_pngs) > 0
      message("DEBUG cache: run_key=", run_key,
              " skip=", skip_render,
              " smp_norm=", paste(sel_smp_norm, collapse=","))

      # Subset samples if selection is active
      if (skip_render) {
        # Cache hit: PNGs already exist — still need to update nodes and reactive vals
        xml_nodes_cached <- tryCatch({
          if (!requireNamespace("xml2", quietly=TRUE)) stop("xml2 not available")
          xml_path <- .ensure_valid_xml(pid, pw_kegg_cache)
          if (!file.exists(xml_path)) stop("XML not available")
          # Compute scale factors from cached PNG vs output PNG
          scale_x <- 1; scale_y <- 1
          png_orig <- file.path(pw_kegg_cache, paste0("ko", pid, ".png"))
          if (file.exists(png_orig) && requireNamespace("png", quietly=TRUE)) {
            orig_dim <- dim(png::readPNG(png_orig))
            orig_w <- orig_dim[2]; orig_h <- orig_dim[1]
            if (length(existing_pngs) > 0) {
              out_dim <- dim(png::readPNG(existing_pngs[1]))
              scale_x <- out_dim[2] / orig_w
              scale_y <- out_dim[1] / orig_h
            }
          }
          doc <- xml2::read_xml(xml_path)
          entries <- xml2::xml_find_all(doc, ".//entry[@type='ortholog']")
          map_entries <- xml2::xml_find_all(doc, ".//entry[@type='map']")
          rows <- lapply(entries, function(e) {
            ko_names <- trimws(xml2::xml_attr(e, "name"))
            g <- xml2::xml_find_first(e, "graphics")
            if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
            x <- as.numeric(xml2::xml_attr(g, "x")) * scale_x
            y <- as.numeric(xml2::xml_attr(g, "y")) * scale_y
            w <- as.numeric(xml2::xml_attr(g, "width")) * scale_x
            h <- as.numeric(xml2::xml_attr(g, "height")) * scale_y
            label <- trimws(xml2::xml_attr(g, "name"))
            list(ko_names=ko_names, x=x, y=y, w=w, h=h, label=label, link_pid="")
          })
          map_rows <- lapply(map_entries, function(e) {
            entry_name <- trimws(xml2::xml_attr(e, "name"))
            link_pid2  <- sub("^path:ko", "", entry_name)
            if (!grepl("^[0-9]{5}$", link_pid2)) return(NULL)
            g <- xml2::xml_find_first(e, "graphics")
            if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
            x <- as.numeric(xml2::xml_attr(g, "x")) * scale_x
            y <- as.numeric(xml2::xml_attr(g, "y")) * scale_y
            w <- as.numeric(xml2::xml_attr(g, "width")) * scale_x
            h <- as.numeric(xml2::xml_attr(g, "height")) * scale_y
            label <- trimws(xml2::xml_attr(g, "name"))
            list(ko_names="", x=x, y=y, w=w, h=h, label=label, link_pid=link_pid2)
          })
          all_rows <- Filter(Negate(is.null), c(rows, map_rows))
          if (length(all_rows) == 0) return(NULL)
          df <- data.frame(
            ko_names = sapply(all_rows, `[[`, "ko_names"),
            x = sapply(all_rows, `[[`, "x"),
            y = sapply(all_rows, `[[`, "y"),
            w = sapply(all_rows, `[[`, "w"),
            h = sapply(all_rows, `[[`, "h"),
            label = sapply(all_rows, `[[`, "label"),
            link_pid = sapply(all_rows, `[[`, "link_pid"),
            stringsAsFactors = FALSE
          )
          pos_key <- paste(round(df$x), round(df$y), sep=",")
          df[!duplicated(pos_key), ]
        }, error = function(e) { message("XML parse error (cache): ", e$message); NULL })
        pw_nodes(xml_nodes_cached)
        pngs_cached <- existing_pngs
        if (mode != "split" && length(pngs_cached) > 1) {
          multi <- pngs_cached[grepl("[.]multi[.]png$", basename(pngs_cached))]
          if (length(multi) > 0) pngs_cached <- multi
        }
        pw_img_dir(outdir)
        pw_img_files(pngs_cached)
        pw_status("ready")
      }

      if (!skip_render) {
        if (!setequal(sel_smp_norm, all_smp_names) && length(sel_smp_norm) > 0) {
          proj <- subsetSamples(proj, sel_smp_norm)
          auto_cols <- auto_cols[seq_along(sel_smp_norm)]
        }

        # If KEGG files are cached, copy them into outdir so exportPathway
        # finds them and skips the download — no monkey-patching needed
        xml_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".xml"))
        png_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".png"))
        if (file.exists(xml_cached))
          file.copy(xml_cached, file.path(outdir, paste0("ko", pid, ".xml")), overwrite = FALSE)
        if (file.exists(png_cached))
          file.copy(png_cached, file.path(outdir, paste0("ko", pid, ".png")), overwrite = FALSE)

        message("DEBUG before exportPathway: pid=", pid, " outdir=", outdir)
        exportPathway(
          proj,
          pathway_id         = pid,
          count              = cnt,
          split_samples      = (mode == "split"),
          log_scale          = log_sc,
          fold_change_groups = fc_grps,
          sample_colors      = if (mode != "foldchange") auto_cols else NULL,
          output_dir         = outdir,
          output_suffix      = paste0("sqmxplore_", pid)
        )
        message("DEBUG after exportPathway: files in outdir=",
                paste(list.files(outdir), collapse=", "))
      }

      # Save downloaded files back to cache if not already there
      for (ext in c(".xml", ".png")) {
        from_outdir <- file.path(outdir,        paste0("ko", pid, ext))
        to_cache    <- file.path(pw_kegg_cache, paste0("ko", pid, ext))
        if (file.exists(from_outdir) && !file.exists(to_cache))
          file.copy(from_outdir, to_cache)
      }

      # \u2500\u2500 Parse KGML to extract node positions for image map \u2500\u2500
      xml_nodes <- tryCatch({
        if (!requireNamespace("xml2", quietly=TRUE)) stop("xml2 not available")
        xml_path <- .ensure_valid_xml(pid, pw_kegg_cache)
        if (!file.exists(xml_path)) stop("XML not downloaded")

        # Also download the original KEGG PNG to measure its dimensions
        png_orig <- file.path(pw_kegg_cache, paste0("ko", pid, ".png"))
        if (!file.exists(png_orig)) {
          pathview::download.kegg(pathway.id=pid, species="ko", kegg.dir=pw_kegg_cache,
                                  file.type="png")
        }
        # Get scale factor: output PNG vs original KEGG PNG
        scale_x <- 1; scale_y <- 1
        if (file.exists(png_orig) && requireNamespace("png", quietly=TRUE)) {
          orig_dim <- dim(png::readPNG(png_orig))  # [height, width, channels]
          orig_w <- orig_dim[2]; orig_h <- orig_dim[1]
          # Find the output PNG (multi or single)
          out_pngs <- list.files(outdir, pattern=paste0("sqmxplore_",pid,".*[.]png$"),
                                 full.names=TRUE)
          out_pngs <- out_pngs[!grepl("[.]legend[.]", basename(out_pngs))]
          if (length(out_pngs) > 0) {
            out_dim <- dim(png::readPNG(out_pngs[1]))
            out_w <- out_dim[2]; out_h <- out_dim[1]
            scale_x <- out_w / orig_w
            scale_y <- out_h / orig_h

          }
        }

        doc <- xml2::read_xml(xml_path)

        # \u2500\u2500 Ortholog nodes (enzyme boxes) \u2500\u2500
        entries <- xml2::xml_find_all(doc, ".//entry[@type='ortholog']")
        rows <- lapply(entries, function(e) {
          ko_names <- trimws(xml2::xml_attr(e, "name"))
          g <- xml2::xml_find_first(e, "graphics")
          if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
          x <- as.numeric(xml2::xml_attr(g, "x")) * scale_x
          y <- as.numeric(xml2::xml_attr(g, "y")) * scale_y
          w <- as.numeric(xml2::xml_attr(g, "width"))  * scale_x
          h <- as.numeric(xml2::xml_attr(g, "height")) * scale_y
          if (anyNA(c(x,y,w,h))) return(NULL)
          label <- xml2::xml_attr(g, "name")
          list(ko_names=ko_names, x=x, y=y, w=w, h=h, label=label, link_pid="")
        })

        # \u2500\u2500 Map-link nodes (rounded rectangles linking to other pathways) \u2500\u2500
        map_entries <- xml2::xml_find_all(doc, ".//entry[@type='map']")
        map_rows <- lapply(map_entries, function(e) {
          entry_name <- trimws(xml2::xml_attr(e, "name"))  # e.g. "path:ko00020"
          link_pid   <- sub("^path:ko", "", entry_name)    # e.g. "00020"
          if (!grepl("^[0-9]{5}$", link_pid)) return(NULL)
          g <- xml2::xml_find_first(e, "graphics")
          if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
          x <- as.numeric(xml2::xml_attr(g, "x")) * scale_x
          y <- as.numeric(xml2::xml_attr(g, "y")) * scale_y
          w <- as.numeric(xml2::xml_attr(g, "width"))  * scale_x
          h <- as.numeric(xml2::xml_attr(g, "height")) * scale_y
          if (anyNA(c(x,y,w,h))) return(NULL)
          label <- xml2::xml_attr(g, "name")
          list(ko_names="", x=x, y=y, w=w, h=h, label=label, link_pid=link_pid)
        })

        all_rows <- Filter(Negate(is.null), c(rows, map_rows))
        if (length(all_rows) == 0) return(NULL)
        df <- data.frame(
          ko_names = sapply(all_rows, `[[`, "ko_names"),
          x        = sapply(all_rows, `[[`, "x"),
          y        = sapply(all_rows, `[[`, "y"),
          w        = sapply(all_rows, `[[`, "w"),
          h        = sapply(all_rows, `[[`, "h"),
          label    = sapply(all_rows, `[[`, "label"),
          link_pid = sapply(all_rows, `[[`, "link_pid"),
          stringsAsFactors = FALSE
        )
        # Deduplicate by position
        pos_key <- paste(round(df$x), round(df$y), sep=",")
        df[!duplicated(pos_key), ]
      }, error = function(e) { message("XML parse error: ", e$message); NULL })
      pw_nodes(xml_nodes)

      # Collect PNGs written by pathview (exclude legends and base map)
      pngs_all <- list.files(outdir, pattern = "[.]png$", full.names = TRUE)
      pngs_all <- pngs_all[!grepl("[.]legend[.]", basename(pngs_all))]
      # pathview writes the original base PNG (ko<pid>.png) alongside the output \u2014
      # exclude it: keep only files that contain the output_suffix in the name
      suffix_pat <- paste0("sqmxplore_", pid)
      pngs_all <- pngs_all[grepl(suffix_pat, basename(pngs_all), fixed=TRUE)]
      # In "together" mode with >1 sample, prefer the .multi.png over per-sample PNGs
      if (mode != "split" && length(pngs_all) > 1) {
        multi <- pngs_all[grepl("[.]multi[.]png$", basename(pngs_all))]
        if (length(multi) > 0) pngs_all <- multi
      }

      if (length(pngs_all) == 0) {
        pw_status("error")
        showNotification(
          paste0("No images were generated for pathway ", pid,
                 ". Check that the pathway ID is valid and has KEGG KO annotations."),
          type = "error", duration = 10)
      } else {
        # Compute legend info \u2014 use the actual selected samples
        samples_used <- sel_smp_norm
        if (length(samples_used) == 0)
          samples_used <- tryCatch(proj$misc$samples, error=function(e) character(0))
        n_smp  <- length(samples_used)
        s_cols <- auto_cols[seq_len(n_smp)]  # trim colors to selected samples
        # Compute min/max from KEGG data (same logic as exportPathway)
        mat <- tryCatch({
          m <- if (cnt == "percent") {
            100 * t(t(proj$functions$KEGG$abund) / proj$total_reads)
          } else {
            proj$functions$KEGG[[cnt]]
          }
          # Subset to selected samples
          if (!is.null(m) && length(samples_used) > 0 &&
              !setequal(samples_used, colnames(m)))
            m[, colnames(m) %in% samples_used, drop=FALSE]
          else m
        }, error = function(e) NULL)
        if (!is.null(mat) && mode == "foldchange" && !is.null(fc_grps)) {
          ps  <- 0.001
          mat <- mat + ps
          log2FC <- log(apply(mat[, fc_grps[[2]], drop=FALSE], 1, median) /
                        apply(mat[, fc_grps[[1]], drop=FALSE], 1, median), 2)
          mv <- max(abs(log2FC), na.rm=TRUE)
          leg_min <- -mv; leg_max <- mv; leg_log <- FALSE
        } else if (!is.null(mat)) {
          if (log_sc) mat <- log(mat + 0.001, 10)
          # Replicate pathview's node.map aggregation: sum KOs per node
          # so the scale matches what is actually painted on the map
          node_sums <- tryCatch({
            xml_path2 <- .ensure_valid_xml(pid, pw_kegg_cache)
            if (file.exists(xml_path2) && requireNamespace("xml2", quietly=TRUE)) {
              doc2    <- xml2::read_xml(xml_path2)
              entries2 <- xml2::xml_find_all(doc2, ".//entry[@type='ortholog']")
              sums <- lapply(entries2, function(e) {
                kos <- unique(sub("^ko:", "", trimws(unlist(strsplit(
                  trimws(xml2::xml_attr(e, "name")), "[[:space:]]+")))))
                kos_in <- kos[kos %in% rownames(mat)]
                if (length(kos_in) == 0) return(NULL)
                colSums(mat[kos_in, , drop=FALSE], na.rm=TRUE)
              })
              sums <- do.call(rbind, Filter(Negate(is.null), sums))
              if (!is.null(sums) && nrow(sums) > 0) sums else mat
            } else mat
          }, error=function(e) mat)
          leg_min <- min(node_sums, na.rm=TRUE)
          leg_max <- max(node_sums, na.rm=TRUE)
          leg_log <- log_sc
        } else {
          leg_min <- 0; leg_max <- 1; leg_log <- log_sc
        }
        pw_legend(list(
          colors  = s_cols,
          samples = samples_used,
          min     = leg_min,
          max     = leg_max,
          log_sc  = leg_log,
          cnt     = cnt,
          mode    = mode,
          fc_grps = fc_grps
        ))
        pw_img_dir(outdir)
        pw_img_files(pngs_all)
        pw_status("ready")
      }
    }, error = function(e) {
      message("DEBUG pathway error: ", e$message)
      pw_status("error")
      showNotification(paste("Pathway error:", e$message), type = "error", duration = 12)
    }))  # end tryCatch + delay
    }) # end isolate
  })

  output$pw_view_ui <- renderUI({
    s <- pw_status()
    if (s == "idle") return(
      tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:2rem; margin-bottom:8px;", "\U0001f5fa\ufe0f"),
        tags$div("Enter a KEGG Pathway ID and click ",
                 tags$strong("Generate map"), "."),
        tags$div(style = "margin-top:6px; font-size:0.78rem;",
          "Example IDs: 00910 (Nitrogen), 00630 (Glyoxylate), 01100 (Metabolic pathways)")))
    if (s == "generating") {
      pid_cur  <- isolate(input$pw_pathway_id) %||% ""
      pw_name  <- tryCatch({
        found <- ""
        for (l1 in KEGG_HIERARCHY) for (l2 in l1) for (pw in l2)
          if (identical(pw$id, pid_cur)) { found <- pw$name; break }
        found
      }, error=function(e) "")
      map_label <- if (nchar(pw_name) > 0) paste0(pw_name, " [", pid_cur, "]") else pid_cur
      return(tags$div(
        style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u25cc"),
        tags$div("Loading map for ", tags$strong(map_label), "\u2026"),
        tags$div(style = "margin-top:6px; font-size:0.78rem;", "Please wait")))
    }
    if (s == "error") return(
      tags$div(style = "color:#c0392b; font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u2715"),
        tags$div("Generation failed. See notification for details.")))
    # ready
    imgs    <- pw_img_files(); req(imgs)
    out_dir <- pw_img_dir()
    leg     <- pw_legend()
    # Serve the output dir as a static resource
    res_name <- paste0("pw_", basename(out_dir))
    addResourcePath(res_name, out_dir)
    nodes <- pw_nodes()
    kegg_names <- tryCatch(sqm_data()$misc$KEGG_names, error=function(e) NULL)

    # CSS tooltip that follows the cursor \u2014 fast, styleable, no delay
    tooltip_css <- tags$style(HTML("
      #pw-tooltip {
        position: fixed; pointer-events: none; z-index: 9999;
        background: rgba(20,30,50,0.92); color: #f0f4f8;
        padding: 5px 9px; border-radius: 5px; font-size: 0.75rem;
        max-width: 320px; line-height: 1.4; display: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.3);
        white-space: pre-wrap; word-break: break-word;
      }
    "))
    tooltip_div <- tags$div(id="pw-tooltip")
    tooltip_js  <- tags$script(HTML("
      (function() {
        var tip = document.getElementById('pw-tooltip');
        if (!tip) return;
        document.addEventListener('mousemove', function(e) {
          tip.style.left = (e.clientX + 14) + 'px';
          tip.style.top  = (e.clientY + 14) + 'px';
        });
      })();
      function pwShowTip(el) {
        var tip = document.getElementById('pw-tooltip');
        if (tip) { tip.textContent = el.getAttribute('data-tip'); tip.style.display = 'block'; }
      }
      function pwHideTip() {
        var tip = document.getElementById('pw-tooltip');
        if (tip) tip.style.display = 'none';
      }
    "))

    # Build data matrix for value lookup (same logic as exportPathway)
    pw_mat <- tryCatch({
      proj <- sqm_data(); req(proj)
      cnt_local <- leg$cnt %||% "copy_number"
      m <- if (cnt_local == "percent") {
        100 * t(t(proj$functions$KEGG$abund) / proj$total_reads)
      } else {
        proj$functions$KEGG[[cnt_local]]
      }
      if (!is.null(leg$log_sc) && leg$log_sc) log(m + 0.001, 10) else m
    }, error=function(e) NULL)

    # Serialize node tooltip data to JSON for JS overlay
    build_node_json <- function() {
      if (is.null(nodes) || nrow(nodes) == 0) return("[]")
      node_list <- apply(nodes, 1, function(r) {
        x <- as.numeric(r["x"]); y <- as.numeric(r["y"])
        w <- as.numeric(r["w"]); h <- as.numeric(r["h"])
        link_pid_val <- tryCatch(r["link_pid"], error=function(e) "")
        link_pid <- if (!is.null(link_pid_val) && !is.na(link_pid_val) &&
                        nchar(trimws(link_pid_val)) == 5) trimws(link_pid_val) else ""
        if (nchar(link_pid) == 5) {
          map_name <- tryCatch({
            found <- ""
            for (l1 in KEGG_HIERARCHY) for (l2 in l1) for (pw in l2)
              if (identical(pw$id, link_pid)) { found <- pw$name; break }
            found
          }, error=function(e) "")
          lbl <- if (nchar(map_name) > 0) map_name else as.character(r["label"])
          tip <- paste0(link_pid, "\n", lbl, "\n\u2192 Click to open")
        } else {
          ko_ids <- unique(sub("^ko:", "", trimws(unlist(strsplit(r["ko_names"], "[[:space:]]+")))))
          if (!is.null(kegg_names)) nms <- unique(na.omit(kegg_names[ko_ids]))
          else nms <- character(0)
          ko_str   <- paste(ko_ids, collapse=", ")
          name_str <- if (length(nms) > 0) paste(nms, collapse=" / ") else r["label"]
          tip <- if (nchar(trimws(name_str)) > 0) paste0(ko_str, "\n", name_str) else ko_str
          if (!is.null(pw_mat)) {
            val_rows <- pw_mat[rownames(pw_mat) %in% ko_ids, , drop=FALSE]
            if (nrow(val_rows) > 0) {
              col_sums <- colSums(val_rows, na.rm=TRUE)
              val_str  <- paste(sapply(names(col_sums), function(s) {
                v <- col_sums[[s]]
                fv <- if (abs(v) >= 10000) formatC(v, digits=3, format="e")
                      else formatC(v, digits=3, format="g")
                paste0(s, ": ", fv)
              }), collapse="\n")
              tip <- paste0(tip, "\n\u2014\n", val_str)
            } else {
              tip <- paste0(tip, "\n\u2014\n(not in data)")
            }
          }
        }
        list(x=x, y=y, w=w, h=h, tip=tip, pid=link_pid)
      })
      jsonlite::toJSON(unname(node_list), auto_unbox=TRUE)
    }

    make_img_map <- function(fname, map_id) {
      img_src <- paste0(res_name, "/", fname)
      if (is.null(nodes) || nrow(nodes) == 0) {
        return(tags$div(style="margin-bottom:12px;",
          tags$img(src=img_src, id=map_id,
            style="max-width:100%; border:1px solid var(--border); border-radius:6px;",
            alt=fname)))
      }
      node_json <- build_node_json()
      # Canvas overlay: positioned absolutely over the img, scaled via JS
      tagList(
        tags$div(style="margin-bottom:12px; position:relative; display:inline-block; width:100%;",
          tags$img(src=img_src, id=map_id,
            style="max-width:100%; display:block; border:1px solid var(--border); border-radius:6px; box-shadow:0 1px 4px rgba(0,0,0,.08);",
            alt=fname),
          tags$canvas(id=paste0(map_id,"_canvas"),
            style="position:absolute; top:0; left:0; width:100%; height:100%;")
        ),
        tags$script(HTML(sprintf('
          (function() {
            var nodes = %s;
            var img   = document.getElementById("%s");
            var canvas= document.getElementById("%s_canvas");
            function setup() {
              canvas.width  = img.offsetWidth;
              canvas.height = img.offsetHeight;
              var scaleX = img.offsetWidth  / img.naturalWidth;
              var scaleY = img.offsetHeight / img.naturalHeight;
              function hitTest(mx, my) {
                for (var i=0; i<nodes.length; i++) {
                  var n = nodes[i];
                  var x1 = (n.x - n.w/2) * scaleX;
                  var y1 = (n.y - n.h/2) * scaleY;
                  var x2 = (n.x + n.w/2) * scaleX;
                  var y2 = (n.y + n.h/2) * scaleY;
                  if (mx>=x1 && mx<=x2 && my>=y1 && my<=y2) return n;
                }
                return null;
              }
              canvas.addEventListener("mousemove", function(e) {
                var rect = canvas.getBoundingClientRect();
                var hit = hitTest(e.clientX - rect.left, e.clientY - rect.top);
                if (hit) {
                  canvas.style.cursor = hit.pid && hit.pid.length === 5 ? "pointer" : "crosshair";
                  var tip = document.getElementById("pw-tooltip");
                  if (tip) { tip.textContent = hit.tip; tip.style.display="block"; }
                } else {
                  canvas.style.cursor = "default";
                  pwHideTip();
                }
              });
              canvas.addEventListener("click", function(e) {
                var rect = canvas.getBoundingClientRect();
                var hit = hitTest(e.clientX - rect.left, e.clientY - rect.top);
                if (hit && hit.pid && hit.pid.length === 5) {
                  pwHideTip();
                  Shiny.setInputValue("pw_pathway_id", hit.pid, {priority:"event"});
                  var lbl = document.getElementById("pw_selected_label");
                  if (lbl) lbl.textContent = "Selected: " + hit.pid;
                  document.querySelectorAll(".pw-item").forEach(function(el) {
                    el.style.background = el.getAttribute("data-id") === hit.pid ? "var(--accent-light)" : "";
                  });
                }
              });
              canvas.addEventListener("mouseleave", pwHideTip);
            }
            if (img.complete) { setup(); }
            else { img.addEventListener("load", setup); }
            window.addEventListener("resize", function() {
              if (img.complete) setup();
            });
          })();
        ', node_json, map_id, map_id)))
      )
    }
    img_tags <- lapply(seq_along(imgs), function(i) {
      make_img_map(basename(imgs[[i]]), paste0("pwmap_", i))
    })
    # Prepend tooltip infrastructure once
    img_tags <- c(list(tooltip_css, tooltip_div, tooltip_js), img_tags)

    # \u2500\u2500 Inline legend \u2500\u2500
    cnt_labels <- c(abund="Raw abundance", percent="Percentage", bases="Bases",
                    tpm="TPM", copy_number="Copy number")
    cnt_lbl <- if (!is.null(leg) && leg$cnt %in% names(cnt_labels))
                 cnt_labels[leg$cnt] else leg$cnt

    legend_ui <- if (!is.null(leg)) {
      fmt_val <- function(v) {
        if (!is.null(leg$log_sc) && leg$log_sc) paste0("10^", round(v, 2))
        else formatC(v, digits=3, format="g")
      }
      if (leg$mode == "foldchange" && !is.null(leg$fc_grps)) {
        fc_colors <- c("red", "green")
        grad <- paste0("linear-gradient(to top, ", fc_colors[1], ", white, ", fc_colors[2], ")")
        tags$div(style="display:flex; align-items:flex-start; gap:12px;",
          tags$div(style="display:flex; align-items:stretch; gap:4px;",
            tags$div(style="display:flex; flex-direction:column; justify-content:space-between; font-size:0.65rem; color:var(--muted); text-align:right; height:120px;",
              tags$span(fmt_val(leg$max)), tags$span("0"), tags$span(fmt_val(leg$min))),
            tags$div(style=paste0("width:18px; height:120px; border-radius:3px; border:1px solid var(--border); background:", grad, ";"))
          ),
          tags$div(style="font-size:0.72rem; color:var(--muted); padding-top:4px;",
            tags$div(paste0("Log2FC ", cnt_lbl)),
            tags$div(style="margin-top:8px;",
              tags$span(style=paste0("display:inline-block;width:10px;height:10px;background:", fc_colors[2], ";border-radius:2px;margin-right:4px;")),
              "Group B > Group A"),
            tags$div(style="margin-top:4px;",
              tags$span(style=paste0("display:inline-block;width:10px;height:10px;background:", fc_colors[1], ";border-radius:2px;margin-right:4px;")),
              "Group A > Group B")
          )
        )
      } else {
        # Shared numeric scale, one color bar per sample
        n_ticks <- 5
        tick_vals <- seq(leg$max, leg$min, length.out = n_ticks)
        bar_tags <- lapply(seq_along(leg$colors), function(i) {
          col <- leg$colors[i]
          grad <- paste0("linear-gradient(to top, white, ", col, ")")
          tags$div(style="display:flex; flex-direction:column; align-items:center; gap:3px;",
            tags$div(style=paste0("width:14px; height:120px; border-radius:3px; border:1px solid var(--border); background:", grad, ";")),
            tags$div(style="font-size:0.65rem; color:var(--muted); max-width:50px; text-align:center; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
              leg$samples[i])
          )
        })
        tags$div(style="display:flex; align-items:flex-start; gap:6px;",
          tags$div(style="display:flex; flex-direction:column; justify-content:space-between; font-size:0.65rem; color:var(--muted); text-align:right; height:120px; padding-right:3px;",
            lapply(tick_vals, function(v) tags$span(fmt_val(v)))),
          tags$div(style="display:flex; gap:4px; align-items:flex-start;", bar_tags),
          tags$div(style="font-size:0.72rem; color:var(--muted); padding-top:4px; padding-left:4px;",
            cnt_lbl,
            if (!is.null(leg$log_sc) && leg$log_sc) " (log10)" else "")
        )
      }
    } else NULL

    tags$div(style="padding:8px;",
      img_tags,
      if (!is.null(legend_ui))
        tags$div(style="margin-top:8px; padding:10px; background:var(--surface); border:1px solid var(--border); border-radius:6px;",
          legend_ui)
    )
  })

  output$pw_status_ui <- renderUI({
    s <- pw_status()
    col <- switch(s, idle="#7a90a8", generating="#3b9ede", ready="#1a9e6e", error="#c0392b")
    ico <- switch(s, idle="\u25cb", generating="\u25cc", ready="\u25cf", error="\u2715")
    lbl <- switch(s, idle="IDLE", generating="GENERATING\u2026", ready="READY", error="ERROR")
    tags$div(style = "font-size:0.8rem;",
      tags$span(style = paste0("color:", col, "; margin-right:5px;"), ico),
      tags$span(style = "color:#7a90a8;", "Status: "),
      tags$span(style = paste0("color:", col, "; font-weight:600;"), lbl))
  })

  output$pw_badge_ui <- renderUI({
    s <- pw_status()
    if (s == "ready")
      tags$span(class="badge",
        style="background:rgba(26,158,110,0.1);color:#1a9e6e;font-size:0.72rem;border:1px solid rgba(26,158,110,0.3);",
        "\u25cf Ready")
    else if (s == "generating")
      tags$span(class="badge",
        style="background:rgba(59,158,222,0.1);color:#3b9ede;font-size:0.72rem;border:1px solid rgba(59,158,222,0.3);",
        "\u25cc Generating\u2026")
    else
      tags$span(class="badge",
        style="background:#eef2f7;color:#7a90a8;font-size:0.72rem;border:1px solid #d0dae6;",
        "No map")
  })

  output$pw_download_ui <- renderUI({
    req(pw_status() == "ready")
    downloadButton("download_pw_zip", "Download PNGs (.zip)",
                   class = "btn-outline-secondary w-100")
  })

  output$download_pw_zip <- downloadHandler(
    filename = function() {
      imgs <- pw_img_files()
      pid  <- trimws(input$pw_pathway_id)
      if (!is.null(imgs) && length(imgs) == 1)
        paste0("pathway_", pid, "_", Sys.Date(), ".png")
      else
        paste0("pathway_", pid, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      imgs <- pw_img_files(); req(imgs)
      if (length(imgs) == 1) {
        file.copy(imgs[1], file)
      } else {
        tmp_dir <- tempfile()
        dir.create(tmp_dir)
        file.copy(imgs, tmp_dir)
        old_wd <- setwd(tmp_dir)
        on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) })
        zip_cmd <- Sys.which("zip")
        if (nchar(zip_cmd) == 0) zip_cmd <- "/usr/bin/zip"
        utils::zip(zipfile = file, files = basename(imgs), zip = zip_cmd)
      }
    }
  )

  # \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
  # Multivariate tab \u2014 PCA via vegan::rda
  # \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
  mv_status  <- reactiveVal("idle")   # idle | ready | error
  mv_pca_res <- reactiveVal(NULL)     # list: rda object + metadata

  output$mv_feat_labels_ui <- renderUI({
    req(input$mv_method %||% "pca" %in% c("pca", "ca"))
    tags$div(style = "display:flex; align-items:center; gap:4px;",
      tags$input(id = "mv_show_feat_labels", type = "checkbox",
        style = "margin:0; width:13px; height:13px; cursor:pointer;",
        checked = if (isTRUE(input$mv_show_feat_labels)) NA else NULL,
        onclick = "Shiny.setInputValue('mv_show_feat_labels', this.checked, {priority:'event'});"),
      tags$label(`for` = "mv_show_feat_labels",
        style = "font-size:0.75rem; color:var(--muted); cursor:pointer; margin:0;",
        "Feature labels"))
  })

  output$mv_ext_labels_ui <- renderUI({
    res <- mv_pca_res()
    req(!is.null(res), !is.null(res$fun_names), input$mv_data_type == "functions",
        input$mv_method %||% "pca" %in% c("pca", "ca"))
    tags$div(style = "display:flex; align-items:center; gap:4px;",
      tags$input(id = "mv_show_ext_labels", type = "checkbox",
        style = "margin:0; width:13px; height:13px; cursor:pointer;",
        checked = if (isTRUE(input$mv_show_ext_labels)) NA else NULL,
        onclick = "Shiny.setInputValue('mv_show_ext_labels', this.checked, {priority:'event'});"),
      tags$label(`for` = "mv_show_ext_labels",
        style = "font-size:0.75rem; color:var(--muted); cursor:pointer; margin:0;",
        "Extended labels"))
  })

  output$mv_feat_style_ui <- renderUI({
    req(input$mv_method %||% "pca" %in% c("pca", "ca"))
    tags$div(style = "display:flex; align-items:center; gap:4px;",
      tags$span(style = "font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Features:"),
      tags$select(
        id = "mv_feat_style",
        style = paste0(
          "font-size:0.75rem; height:24px; padding:1px 4px;",
          "border:1px solid var(--border); border-radius:4px;",
          "background:#ffffff; color:var(--text); cursor:pointer;"),
        onchange = "Shiny.setInputValue('mv_feat_style', this.value, {priority:'event'});",
        tags$option(value = "arrows", selected = if ((input$mv_feat_style %||% "arrows") == "arrows") NA else NULL, "Arrows"),
        tags$option(value = "dots",   selected = if ((input$mv_feat_style %||% "arrows") == "dots")   NA else NULL, "Dots")
      )
    )
  })

  output$mv_card_title_ui <- renderUI({
    method <- input$mv_method %||% "pca"
    span(switch(method, pca = "PCA", ca = "CA", nmds = "NMDS"))
  })

  # \u2500\u2500 Unified sidebar controls (sidebar-box style, method-conditional) \u2500\u2500
  output$mv_sidebar_controls <- renderUI({
    req(sqm_data()); proj <- sqm_data()
    method    <- input$mv_method    %||% "pca"
    data_type <- input$mv_data_type %||% "taxonomy"
    is_pca    <- method == "pca"
    is_ca     <- method == "ca"
    is_eigen  <- is_pca || is_ca  # PCA and CA both show normalization (CA ignores it but shows Raw only)

    # Rank/DB choices
    if (data_type == "taxonomy") {
      rdb_choices <- available_tax_ranks(proj)
      rdb_label   <- "Rank"
    } else {
      rdb_choices <- avail_functions(proj)
      rdb_label   <- "Database"
    }

    # Metric choices
    rdb_val <- input$mv_rank_db
    if (!is.null(rdb_val) && nchar(rdb_val) > 0) {
      metrics <- if (data_type == "taxonomy") {
        rank <- sub("^tax_", "", rdb_val)
        avail_tax_metrics(proj, rank)
      } else {
        db <- resolve_db_name(proj, sub("^fun_", "", rdb_val))
        avail_fun_metrics(proj, db)
      }
    } else {
      metrics <- c()
    }

    # Samples
    samples <- tryCatch(proj$misc$samples, error = function(e) NULL)

    tagList(
      # ── Box 1: Analysis type ──
      tags$div(class = "sidebar-box",
        help_label("Analysis type",
          paste0(
            "PCA (Principal Component Analysis): linear method, assumes normally distributed data. ",
            "Best with CLR or log-normalized counts. Fast and interpretable, but sensitive to outliers and ",
            "does not handle zero-inflated data well.\n\n",
            "CA (Correspondence Analysis): like PCA but designed for count data. ",
            "Works directly on raw counts, preserves chi-square distances. ",
            "Recommended for compositional community data. May show arch effect with long gradients.\n\n",
            "NMDS (Non-metric Multidimensional Scaling): non-linear, rank-based method. ",
            "Most robust for ecological community data — handles zeros, non-normality and complex gradients. ",
            "Slower and requires choosing a distance metric. Use stress value to assess fit (< 0.2 acceptable, < 0.1 good)."
          )),
        selectInput("mv_method", NULL,
          choices = c("PCA" = "pca", "CA" = "ca", "NMDS" = "nmds"), selected = method)),

      # ── Box 2: Data type + Rank/DB ──
      tags$div(class = "sidebar-box",
        tags$div(class = "form-label", "Data type"),
        selectInput("mv_data_type", NULL,
          choices = c("Taxonomy" = "taxonomy", "Functions" = "functions"),
          selected = data_type),
        if (length(rdb_choices) > 0) tagList(
          tags$div(class = "form-label", style = "margin-top:4px;", rdb_label),
          selectInput("mv_rank_db", NULL, choices = rdb_choices,
            selected = input$mv_rank_db))
      ),

      # ── Box 3: Metric + Distance/Normalization + N features + Exclude unclassified ──
      tags$div(class = "sidebar-box",
        if (length(metrics) > 0) tagList(
          help_label("Metric",
            paste0(
              "Raw abundances: number of reads or features assigned. Not normalized — differences ",
              "may reflect sequencing depth rather than biology.\n\n",
              "Percentages: relative abundance as a fraction of the total. Removes sequencing depth bias ",
              "but introduces compositionality (values sum to 100%).\n\n",
              "TPM (Transcripts Per Million): normalized by feature length and sequencing depth. ",
              "Suitable for comparing expression levels across samples.\n\n",
              "Copy number: estimated number of copies of each feature per cell or genome equivalent. ",
              "Useful for comparing functional gene abundance across samples with different genome sizes.\n\n",
              "Base counts: total bases assigned to each feature. Proportional to both abundance and length."
            )),
          selectInput("mv_metric", NULL, choices = metrics,
            selected = if (!is.null(input$mv_metric) && input$mv_metric %in% metrics)
              input$mv_metric
            else if (length(metrics) == 0) NULL else if (length(metrics) == 0) NULL else if ("abund" %in% metrics) "abund" else metrics[[1]])),
        if (is_pca) tagList(
          help_label("Normalization",
            paste0(
              "CLR (Centered Log-Ratio): log-transforms relative abundances and centers them. ",
              "Recommended for compositional metagenomics data — removes the total-sum constraint ",
              "and makes data suitable for PCA.\n\n",
              "Log10 + pseudocount: log10(x+1) transformation. Compresses dynamic range and reduces ",
              "the influence of highly abundant features. Simpler than CLR but does not fully address compositionality.\n\n",
              "Z-score: each feature is centered and scaled across samples independently. ",
              "All features contribute equally regardless of abundance. Removes abundance information.\n\n",
              "Raw: no transformation. PC1 may reflect sequencing depth rather than community composition. ",
              "Only appropriate if data is already normalized (e.g. TPM, percentages)."
            ), style = "margin-top:4px;"),
          selectInput("mv_norm", NULL,
            choices = c("CLR" = "clr",
                        "Log10 + pseudocount" = "log",
                        "Z-score" = "zscore",
                        "Raw" = "raw"),
            selected = input$mv_norm %||% "clr")),
        if (is_ca) tagList(
          tags$div(style = "margin-top:4px; font-size:0.72rem; color:var(--muted);",
            "CA works on raw counts — no normalization needed.")),
        if (!is_pca && !is_ca) tagList(
          help_label("Distance",
            paste0(
              "Bray-Curtis: standard ecological distance based on relative abundances. ",
              "Recommended for most metagenomics datasets.\n\n",
              "Jaccard: presence/absence version of Bray-Curtis. Ignores abundance, only considers ",
              "which features are present or absent.\n\n",
              "Hellinger: square root of relative abundances followed by Euclidean distance. ",
              "Reduces the influence of dominant features.\n\n",
              "Euclidean: straight-line distance on raw counts. Sensitive to total abundance differences ",
              "— generally not recommended unless data is already normalized."
            ), style = "margin-top:4px;"),
          selectInput("mv_dist", NULL,
            choices = c("Bray-Curtis" = "bray", "Jaccard" = "jaccard",
                        "Hellinger" = "hellinger", "Euclidean" = "euclidean"),
            selected = input$mv_dist %||% "bray")),
        tags$div(class = "form-label", style = "margin-top:4px;", "Number of features"),
        numericInput("mv_n_features", NULL,
          value = input$mv_n_features %||% 100, min = 5, max = 5000, step = 10),
        tags$div(style = "margin-top:6px;",
          checkboxInput("mv_exclude_unclassified", "Exclude Unclassified",
            value = isTRUE(input$mv_exclude_unclassified %||% TRUE))),
        checkboxInput("mv_exclude_ambiguous", "Exclude ambiguous taxa",
          value = isTRUE(input$mv_exclude_ambiguous %||% FALSE))
      ),

      # (Feature labels checkbox moved to plot controls bar)

      # \u2500\u2500 Box 5: Samples \u2014 inline chips \u2500\u2500
      if (!is.null(samples))
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Samples"),
          tags$div(style = "display:flex; flex-wrap:wrap; gap:2px; margin-top:2px;",
            lapply(samples, function(s) {
              is_sel <- is.null(input$mv_samples) || s %in% input$mv_samples
              tags$label(
                style = paste0(
                  "display:inline-flex; align-items:center; gap:3px;",
                  "font-size:0.72rem; padding:2px 5px; border-radius:3px; cursor:pointer;",
                  "border:1px solid ", if (is_sel) "#3b9ede" else "var(--border)", ";",
                  "background:", if (is_sel) "rgba(59,158,222,0.08)" else "transparent", ";"),
                tags$input(
                  type = "checkbox", name = "mv_samples", value = s,
                  checked = if (is_sel) NA else NULL,
                  style = "margin:0; width:11px; height:11px;",
                  onclick = paste0(
                    "var cb=this; var vals=[];",
                    "document.querySelectorAll('input[name=mv_samples]').forEach(function(el){",
                    "if(el.checked) vals.push(el.value);});",
                    "Shiny.setInputValue('mv_samples', vals, {priority:'event'});",
                    "var lbl=cb.closest('label');",
                    "lbl.style.borderColor=cb.checked?'#3b9ede':'var(--border)';",
                    "lbl.style.background=cb.checked?'rgba(59,158,222,0.08)':'transparent';")),
                s)
            })
          )
        )
    )
  })

  # \u2500\u2500 Run PCA \u2500\u2500
  observeEvent(input$do_pca, {
    req(sqm_data(), input$mv_rank_db, input$mv_metric)
    method <- input$mv_method %||% "pca"
    if (!requireNamespace("vegan", quietly = TRUE)) {
      showNotification("Package 'vegan' is required. Install with: install.packages('vegan')",
                       type = "error", duration = 10)
      return()
    }
    mv_status("idle"); mv_pca_res(NULL)

    tryCatch({
      proj    <- sqm_data()
      rdb     <- input$mv_rank_db
      metric  <- input$mv_metric
      n_feat  <- max(5L, as.integer(input$mv_n_features %||% 100))
      excl_u  <- isTRUE(input$mv_exclude_unclassified)
      sel_smp <- input$mv_samples

      # ── Get matrix ──
      fun_names_vec <- NULL  # names lookup for functions data type
      mat <- if (input$mv_data_type == "taxonomy") {
        rank <- sub("^tax_", "", rdb)
        as.matrix(proj$taxa[[rank]][[metric]])
      } else {
        db <- resolve_db_name(proj, sub("^fun_", "", rdb))
        fun_names_vec <- tryCatch(proj$misc[[paste0(db, "_names")]], error = function(e) NULL)
        as.matrix(proj$functions[[db]][[metric]])
      }

      # Filter samples
      if (!is.null(sel_smp) && length(sel_smp) > 0)
        mat <- mat[, colnames(mat) %in% sel_smp, drop = FALSE]
      if (ncol(mat) < 2) stop("At least 2 samples are required.")

      # Remove unclassified rows
      excl_amb <- isTRUE(input$mv_exclude_ambiguous)
      if (excl_u) {
        excl_pat <- c("Unclassified", "Unmapped", "No database", "")
        mat <- mat[!rownames(mat) %in% excl_pat, , drop = FALSE]
      }
      if (excl_amb) {
        mat <- mat[!grepl("^unclassified", rownames(mat), ignore.case = TRUE), , drop = FALSE]
      }

      # Select N most abundant features (by row mean)
      row_means <- rowMeans(mat, na.rm = TRUE)
      top_idx   <- order(row_means, decreasing = TRUE)[seq_len(min(n_feat, nrow(mat)))]
      mat       <- mat[top_idx, , drop = FALSE]
      if (nrow(mat) < 2) stop("Not enough features after filtering.")

      # ── Normalization (PCA only — CA uses raw counts, NMDS uses distance metric) ──
      mat_t <- if (method == "pca") {
        norm <- input$mv_norm %||% "clr"
        t(switch(norm,
          clr    = { mat_ps <- mat + 1; apply(mat_ps, 2, function(x) log(x) - mean(log(x))) },
          log    = log10(mat + 1),
          zscore = apply(mat, 1, function(x) { s <- sd(x); if (s == 0) rep(0, length(x)) else (x - mean(x)) / s }) |> t(),
          raw    = mat
        ))
      } else {
        t(mat)  # CA and NMDS: raw counts, samples as rows
      }  # samples as rows

      if (method == "pca") {
        # ── PCA via vegan::rda ──
        ord <- vegan::rda(mat_t, scale = FALSE)
        eig    <- ord$CA$eig
        var_ex <- round(100 * eig / sum(eig), 1)
        # ── PCA quality warnings ──
        pca_warns <- c()
        pc1    <- var_ex[1]
        pc1pc2 <- var_ex[1] + var_ex[2]
        n_smp  <- nrow(mat_t)
        if (n_smp <= 2)
          pca_warns <- c(pca_warns,
            "Only 2 samples: PCA is trivial and results are not meaningful.")
        if (pc1 > 90)
          pca_warns <- c(pca_warns,
            paste0("PC1 explains ", pc1, "% of variance: data variation is nearly one-dimensional. The 2D biplot adds little information."))
        if (pc1pc2 < 30)
          pca_warns <- c(pca_warns,
            paste0("PC1+PC2 explain only ", pc1pc2, "% of variance: the biplot captures very little of the total variation and may be misleading."))
        else if (pc1pc2 < 50)
          pca_warns <- c(pca_warns,
            paste0("PC1+PC2 explain ", pc1pc2, "% of variance: less than half the total variation is represented in this plot."))
        norm_used <- input$mv_norm %||% "clr"
        if (norm_used == "raw")
          pca_warns <- c(pca_warns,
            "Raw data (no normalization): PC1 may reflect sequencing depth differences rather than community composition. Consider using CLR or log normalization.")
        if (norm_used == "zscore")
          pca_warns <- c(pca_warns,
            "Z-score normalization: each feature is scaled independently across samples. This removes abundance information and treats all features equally regardless of prevalence.")

        mv_pca_res(list(
          method     = "pca",
          ord        = ord,
          var_ex     = var_ex,
          mat_t      = mat_t,
          pca_warns  = if (length(pca_warns) > 0) pca_warns else NULL,
          fun_names  = fun_names_vec
        ))

      } else if (method == "ca") {
        # ── CA via vegan::cca (unconstrained = CA) ──
        # mat_t: samples as rows, features as columns; must be non-negative
        if (any(mat_t < 0)) stop("CA requires non-negative counts. Use raw abundances or percentages.")
        ord    <- vegan::cca(mat_t)
        eig    <- ord$CA$eig
        var_ex <- round(100 * eig / sum(eig), 1)
        ca_warns <- c()
        ca1ca2 <- var_ex[1] + var_ex[2]
        if (nrow(mat_t) <= 2)
          ca_warns <- c(ca_warns, "Only 2 samples: CA is trivial and results are not meaningful.")
        if (ca1ca2 < 30)
          ca_warns <- c(ca_warns,
            paste0("CA1+CA2 explain only ", ca1ca2, "% of inertia: the biplot captures very little of the total variation."))
        else if (ca1ca2 < 50)
          ca_warns <- c(ca_warns,
            paste0("CA1+CA2 explain ", ca1ca2, "% of inertia: less than half the total variation is represented."))

        mv_pca_res(list(
          method    = "ca",
          ord       = ord,
          var_ex    = var_ex,
          mat_t     = mat_t,
          pca_warns = if (length(ca_warns) > 0) ca_warns else NULL,
          fun_names = fun_names_vec
        ))

      } else {
        # \u2500\u2500 NMDS via vegan::metaMDS \u2500\u2500
        # Use Bray-Curtis on CLR-transformed data
        dist_sel <- input$mv_dist %||% "bray"
        if (ncol(mat) < 3) stop("NMDS requires at least 3 samples.")
        # mat_t = t(mat) for NMDS (raw counts, samples as rows)
        dist_mat <- if (dist_sel == "euclidean") {
          # Euclidean on raw counts
          vegan::vegdist(mat_t, method = "euclidean")
        } else if (dist_sel == "hellinger") {
          # Hellinger: sqrt of relative abundances
          hell <- vegan::decostand(mat_t, method = "hellinger")
          vegan::vegdist(hell, method = "euclidean")
        } else {
          # Bray-Curtis and Jaccard directly on raw counts
          vegan::vegdist(mat_t, method = dist_sel,
                         binary = (dist_sel == "jaccard"))
        }
        nmds_warnings <- character(0)
        ord <- withCallingHandlers(
          vegan::metaMDS(dist_mat, k = 2, trymax = 100,
                         trace = FALSE, autotransform = FALSE),
          warning = function(w) {
            nmds_warnings <<- c(nmds_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        stress_warn <- if (ord$stress < 0.01)
          "Warning: stress is near zero \u2014 you may have too few samples for a meaningful NMDS."
        else if (ord$stress > 0.2)
          "Warning: stress > 0.2 \u2014 ordination may not be reliable. Consider increasing sample size."
        else NULL
        mv_pca_res(list(
          method      = "nmds",
          ord         = ord,
          mat_t       = mat_t,
          stress      = round(ord$stress, 4),
          stress_warn = stress_warn,
          fun_names   = fun_names_vec
        ))
      }
      mv_status("ready")
    }, error = function(e) {
      mv_status("error")
      showNotification(paste("Analysis error:", e$message), type = "error", duration = 10)
    })
  })


  # \u2500\u2500 Plot \u2500\u2500
  mv_plot <- reactive({
    req(mv_pca_res(), mv_status() == "ready")
    res      <- mv_pca_res()
    method   <- res$method
    fs       <- input$mv_font_size %||% 11
    show_fl  <- isTRUE(input$mv_show_feat_labels)
    show_ext <- isTRUE(input$mv_show_ext_labels)
    feat_style <- input$mv_feat_style %||% "arrows"

    # Build extended label lookup if available and requested
    make_labels <- function(ids) {
      if (show_ext && !is.null(res$fun_names) && length(res$fun_names) > 0) {
        nms <- res$fun_names[ids]
        ifelse(is.na(nms) | nms == "", ids, paste0(ids, ": ", nms))
      } else {
        ids
      }
    }

    if (method == "pca") {
      pca     <- res$ord
      var_ex  <- res$var_ex
      sc_sites <- vegan::scores(pca, display = "sites",   choices = 1:2)
      sc_sp    <- vegan::scores(pca, display = "species", choices = 1:2)

      df_sites <- data.frame(PC1 = sc_sites[,1], PC2 = sc_sites[,2],
                              sample = rownames(sc_sites))
      df_sp    <- data.frame(PC1 = sc_sp[,1],    PC2 = sc_sp[,2],
                              feat = make_labels(rownames(sc_sp)))

      scale_f <- 0.7 * max(abs(df_sites[,1:2])) / max(abs(df_sp[,1:2]))
      df_sp$PC1 <- df_sp$PC1 * scale_f
      df_sp$PC2 <- df_sp$PC2 * scale_f

      xlab <- paste0("PC1 (", var_ex[1], "%)")
      ylab <- paste0("PC2 (", var_ex[2], "%)")
      title <- "PCA biplot"

      p <- ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        { if (feat_style == "arrows")
            ggplot2::geom_segment(data = df_sp,
              ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
              arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
              colour = "#3b9ede", alpha = 0.5, linewidth = 0.4)
          else
            ggplot2::geom_point(data = df_sp,
              ggplot2::aes(x = PC1, y = PC2),
              colour = "#3b9ede", alpha = 0.6, size = 1.5) } +
        ggplot2::geom_point(data = df_sites,
          ggplot2::aes(x = PC1, y = PC2), colour = "#e44c3a", size = 3) +
        ggplot2::geom_text(data = df_sites,
          ggplot2::aes(x = PC1, y = PC2, label = sample),
          vjust = -0.8, size = fs / 3.5, colour = "#333333") +
        { if (show_fl)
            ggplot2::geom_text(data = df_sp,
              ggplot2::aes(x = PC1, y = PC2, label = feat),
              size = fs / 4.5, colour = "#3b9ede", alpha = 0.8, hjust = 0.5, vjust = -0.5)
          else ggplot2::geom_blank() } +
        ggplot2::labs(x = xlab, y = ylab, title = title)

    } else if (method == "ca") {
      var_ex   <- res$var_ex
      sc_sites <- vegan::scores(res$ord, display = "sites",   choices = 1:2)
      sc_sp    <- vegan::scores(res$ord, display = "species", choices = 1:2)

      df_sites <- data.frame(CA1 = sc_sites[,1], CA2 = sc_sites[,2],
                              sample = rownames(sc_sites))
      df_sp    <- data.frame(CA1 = sc_sp[,1],    CA2 = sc_sp[,2],
                              feat = make_labels(rownames(sc_sp)))

      scale_f <- 0.7 * max(abs(df_sites[,1:2])) / max(abs(df_sp[,1:2]))
      df_sp$CA1 <- df_sp$CA1 * scale_f
      df_sp$CA2 <- df_sp$CA2 * scale_f

      xlab  <- paste0("CA1 (", var_ex[1], "%)")
      ylab  <- paste0("CA2 (", var_ex[2], "%)")
      title <- "CA biplot"

      p <- ggplot2::ggplot() +
        ggplot2::geom_hline(yintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        { if (feat_style == "arrows")
            ggplot2::geom_segment(data = df_sp,
              ggplot2::aes(x = 0, y = 0, xend = CA1, yend = CA2),
              arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
              colour = "#3b9ede", alpha = 0.5, linewidth = 0.4)
          else
            ggplot2::geom_point(data = df_sp,
              ggplot2::aes(x = CA1, y = CA2),
              colour = "#3b9ede", alpha = 0.6, size = 1.5) } +
        ggplot2::geom_point(data = df_sites,
          ggplot2::aes(x = CA1, y = CA2), colour = "#e44c3a", size = 3) +
        ggplot2::geom_text(data = df_sites,
          ggplot2::aes(x = CA1, y = CA2, label = sample),
          vjust = -0.8, size = fs / 3.5, colour = "#333333") +
        { if (show_fl)
            ggplot2::geom_text(data = df_sp,
              ggplot2::aes(x = CA1, y = CA2, label = feat),
              size = fs / 4.5, colour = "#3b9ede", alpha = 0.8, hjust = 0.5, vjust = -0.5)
          else ggplot2::geom_blank() } +
        ggplot2::labs(x = xlab, y = ylab, title = title)

    } else {
      # NMDS
      sc <- vegan::scores(res$ord, display = "sites")
      df_sites <- data.frame(MDS1 = sc[,1], MDS2 = sc[,2],
                              sample = rownames(sc))
      stress_lbl <- paste0("Stress = ", res$stress)

      p <- ggplot2::ggplot(df_sites, ggplot2::aes(x = MDS1, y = MDS2)) +
        ggplot2::geom_hline(yintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        ggplot2::geom_vline(xintercept = 0, colour = "#cccccc", linewidth = 0.4) +
        ggplot2::geom_point(colour = "#e44c3a", size = 3) +
        ggplot2::geom_text(ggplot2::aes(label = sample),
          vjust = -0.8, size = fs / 3.5, colour = "#333333") +
        ggplot2::annotate("text", x = Inf, y = -Inf, label = stress_lbl,
          hjust = 1.1, vjust = -0.5, size = fs / 3.5,
          colour = if (!is.null(res$stress_warn)) "#c0392b" else "#7a90a8") +
        ggplot2::labs(x = "MDS1", y = "MDS2", title = "NMDS ordination")
    }

    p +
      ggplot2::theme_bw(base_size = fs) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = fs + 1, face = "bold"),
        panel.grid   = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "#cccccc"))
  })


  output$mv_plot_out <- renderPlot({ mv_plot() },
    width  = function() input$mv_plot_width  %||% 700,
    height = function() input$mv_plot_height %||% 500)

  output$mv_plot_ui <- renderUI({
    s <- mv_status()
    if (s == "idle") return(
      tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:2rem; margin-bottom:8px;", "\U0001f9ee"),
        tags$div("Select options and click ", tags$strong("Run analysis"), ".")))
    if (s == "error") return(
      tags$div(style = "color:#c0392b; font-size:0.85rem; padding:2rem; text-align:center;",
        tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u2715"),
        tags$div("Analysis failed. See notification for details.")))
    uiOutput("mv_plot_sized")
  })
  output$mv_plot_sized <- renderUI({
    res <- mv_pca_res()
    warn_banner <- if (!is.null(res)) {
      msgs <- if (res$method == "nmds") {
        if (!is.null(res$stress_warn)) res$stress_warn else character(0)
      } else {
        if (!is.null(res$pca_warns)) res$pca_warns else character(0)
      }
      if (length(msgs) > 0)
        tags$div(
          style = paste0(
            "margin-top:6px; padding:6px 10px; font-size:0.78rem;",
            "background:rgba(196,57,43,0.08); border:1px solid rgba(196,57,43,0.3);",
            "border-radius:4px; color:#c0392b;"),
          lapply(msgs, function(m) tags$div(
            tags$span(style="margin-right:5px;", "\u26a0"), m))
        )
      else NULL
    } else NULL
    tagList(
      tags$div(
        style = paste0("width:", input$mv_plot_width %||% 700, "px; max-width:100%;"),
        plotOutput("mv_plot_out",
          width  = "100%",
          height = paste0(input$mv_plot_height %||% 500, "px"))),
      warn_banner
    )
  })


  output$mv_status_ui <- renderUI({
    s <- mv_status()
    col <- switch(s, idle="#7a90a8", ready="#1a9e6e", error="#c0392b")
    ico <- switch(s, idle="\u25cb", ready="\u25cf", error="\u2715")
    lbl <- switch(s, idle="IDLE", ready="READY", error="ERROR")
    tags$div(style = "font-size:0.8rem;",
      tags$span(style = paste0("color:", col, "; margin-right:5px;"), ico),
      tags$span(style = "color:#7a90a8;", "Status: "),
      tags$span(style = paste0("color:", col, "; font-weight:600;"), lbl))
  })

  output$mv_badge_ui <- renderUI({
    s <- mv_status()
    if (s == "ready")
      tags$span(class="badge",
        style="background:rgba(26,158,110,0.1);color:#1a9e6e;font-size:0.72rem;border:1px solid rgba(26,158,110,0.3);",
        "\u25cf Ready")
    else
      tags$span(class="badge",
        style="background:#eef2f7;color:#7a90a8;font-size:0.72rem;border:1px solid #d0dae6;",
        "No plot")
  })

  output$mv_download_ui <- renderUI({
    req(mv_status() == "ready")
    downloadButton("download_mv_png", "Download PNG", class = "btn-outline-secondary w-100")
  })

  output$download_mv_png <- downloadHandler(
    filename = function() paste0("pca_", Sys.Date(), ".png"),
    content  = function(file) {
      p <- mv_plot()
      w <- (input$mv_plot_width  %||% 700) / 100
      h <- (input$mv_plot_height %||% 500) / 100
      ggplot2::ggsave(file, plot = p, width = w, height = h, dpi = 150)
    }
  )

  # ===========================================================================
  # COMPARISON TAB — differential abundance analysis
  # ===========================================================================

  cmp_status  <- reactiveVal("idle")   # idle | running | ready | error
  cmp_results <- reactiveVal(NULL)     # list of pairwise result data.frames
  cmp_groups  <- reactiveVal(NULL)     # named vector: sample -> group

  # ---- Category selector ---------------------------------------------------
  output$cmp_category_ui <- renderUI({
    proj <- sqm_data(); req(proj)
    cats <- c("Taxonomy" = "taxonomy")
    funcs <- tryCatch(names(proj$functions), error = function(e) character(0))
    if (length(funcs) > 0) cats <- c(cats, "Functions" = "functions")
    selectInput("cmp_category", NULL, choices = cats, selected = "taxonomy")
  })

  # ---- Sub-selector (rank or DB) -------------------------------------------
  output$cmp_sub_ui <- renderUI({
    proj <- sqm_data(); req(proj)
    cat  <- input$cmp_category %||% "taxonomy"
    if (cat == "taxonomy") {
      avail <- c("superkingdom","phylum","class","order","family","genus","species")
      avail <- avail[sapply(avail, function(r) !is.null(proj$taxa[[r]]))]
      choices <- setNames(avail, paste0(toupper(substring(avail,1,1)), substring(avail,2)))
      tagList(
        tags$div(class = "form-label", "Taxonomic rank"),
        selectInput("cmp_tax_rank", NULL, choices = choices, selected = "phylum")
      )
    } else {
      funcs <- tryCatch(names(proj$functions), error = function(e) character(0))
      tagList(
        tags$div(class = "form-label", "Database"),
        selectInput("cmp_func_db", NULL, choices = funcs, selected = funcs[1])
      )
    }
  })

  # ---- Metric selector (Wilcoxon only) ------------------------------------
  output$cmp_metric_ui <- renderUI({
    method <- input$cmp_method %||% "wilcoxon"
    if (method != "wilcoxon") {
      tags$div(style = "font-size:0.78rem; color:var(--muted); margin-top:6px;",
        tags$em("DESeq2 and edgeR always use raw counts (abund). ",
                "Normalisation is performed internally by each method."))
    } else {
      cat <- input$cmp_category %||% "taxonomy"
      choices <- if (cat == "taxonomy")
        c("Raw counts" = "abund", "Percentages" = "percent", "Base counts" = "bases")
      else
        c("Raw counts" = "abund", "TPM" = "tpm", "Copy number" = "copy_number", "Base counts" = "bases")
      tagList(
        help_label("Metric",
          "Metric used for Wilcoxon test.\nRaw counts: not normalised for sequencing depth.\nPercentages: relative abundance, removes depth bias but compositional.\nTPM / Copy number: normalised, suitable for cross-sample comparison.",
          style = "margin-top:6px;"),
        selectInput("cmp_metric", NULL, choices = choices, selected = "percent", width = "100%")
      )
    }
  })

  # ---- Group assignment UI -------------------------------------------------
  output$cmp_group_ui <- renderUI({
    proj <- sqm_data(); req(proj)
    samples <- proj$misc$samples
    n <- length(samples)
    half <- ceiling(n / 2)
    def1 <- samples[seq_len(half)]
    def2 <- samples[(half + 1):n]
    tagList(
      tags$div(style = "display:flex; gap:10px;",
        tags$div(style = "flex:1;",
          tags$div(class = "form-label", "Group 1"),
          checkboxGroupInput("cmp_group1", NULL, choices = samples, selected = def1)
        ),
        tags$div(style = "flex:1;",
          tags$div(class = "form-label", "Group 2"),
          checkboxGroupInput("cmp_group2", NULL, choices = samples, selected = def2)
        )
      )
    )
  })

  # Mutual exclusion: if a sample is in group1, remove it from group2 and vice versa
  observeEvent(input$cmp_group1, {
    g2 <- input$cmp_group2
    if (!is.null(g2)) {
      overlap <- intersect(input$cmp_group1, g2)
      if (length(overlap) > 0)
        updateCheckboxGroupInput(session, "cmp_group2", selected = setdiff(g2, overlap))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$cmp_group2, {
    g1 <- input$cmp_group1
    if (!is.null(g1)) {
      overlap <- intersect(input$cmp_group2, g1)
      if (length(overlap) > 0)
        updateCheckboxGroupInput(session, "cmp_group1", selected = setdiff(g1, overlap))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ---- Card title & badge --------------------------------------------------
  output$cmp_card_title_ui <- renderUI({
    if (cmp_status() == "idle") tags$span("Comparison")
    else uiOutput("cmp_pair_selector_ui")
  })

  output$cmp_badge_ui <- renderUI({
    s <- cmp_status()
    if (s == "idle") return(NULL)
    cls <- switch(s, running = "launcher-status-running", ready = "launcher-status-finished",
                     error = "launcher-status-error", "launcher-status-idle")
    spinner <- if (s == "running")
      tags$span(class = "spinner-border spinner-border-sm me-1", role = "status")
    tags$span(class = paste("badge rounded-pill", cls), spinner, toupper(s))
  })

  output$cmp_status_ui <- renderUI({
    s <- cmp_status()
    if (s %in% c("idle", "ready")) return(NULL)
    if (s == "running") tags$div(style = "font-size:0.8rem; color:var(--muted);", "Running analysis...")
    else if (s == "error") tags$div(style = "font-size:0.8rem; color:#c0392b;",
      "Analysis failed. Check inputs.")
  })

  # ---- Pair selector (shown when results ready) ----------------------------
  output$cmp_pair_selector_ui <- renderUI({
    res <- cmp_results(); req(res)
    pairs <- names(res)
    if (length(pairs) <= 1) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-bottom:8px;",
      tags$div(class = "form-label", "Comparison"),
      selectInput("cmp_pair", NULL, choices = pairs, selected = pairs[1], width = "100%")
    )
  })

  # ---- Run comparison ------------------------------------------------------
  observeEvent(input$do_cmp, {
    proj <- sqm_data(); req(proj)
    samples <- proj$misc$samples; n <- length(samples)

    # Collect group assignments from two checkbox groups
    g1_samples <- input$cmp_group1 %||% character(0)
    g2_samples <- input$cmp_group2 %||% character(0)

    if (length(g1_samples) == 0 || length(g2_samples) == 0) {
      showNotification("Each group must have at least one sample.", type = "error", duration = 6)
      return()
    }
    overlap <- intersect(g1_samples, g2_samples)
    if (length(overlap) > 0) {
      showNotification(paste("Samples in both groups:", paste(overlap, collapse = ", ")),
                       type = "error", duration = 6)
      return()
    }

    # Robustness warnings
    method  <- input$cmp_method %||% "wilcoxon"
    n1 <- length(g1_samples); n2 <- length(g2_samples)
    if (n1 == 1 || n2 == 1) {
      if (method %in% c("deseq2", "edger")) {
        showNotification(
          paste0(method, " requires biological replicates in each group. ",
                 "With only 1 sample per group the results are not statistically valid. ",
                 "Use Wilcoxon instead, or add more samples."),
          type = "error", duration = 10)
        return()
      }
      showNotification(
        "Warning: one group has only 1 sample. Wilcoxon cannot compute a p-value with n=1. ",
        type = "warning", duration = 8)
    } else if (n1 < 3 || n2 < 3) {
      showNotification(
        paste0("Low statistical power: groups have ", n1, " and ", n2,
               " samples. Results should be interpreted with caution. ",
               "At least 3 replicates per group are recommended."),
        type = "warning", duration = 8)
    }

    all_assigned <- c(g1_samples, g2_samples)
    grps <- c(setNames(rep("Group1", length(g1_samples)), g1_samples),
              setNames(rep("Group2", length(g2_samples)), g2_samples))
    group_levels <- c("Group1", "Group2")

    cmp_status("running"); cmp_results(NULL)

    # Extract the count matrix
    # For DESeq2/edgeR always use raw counts; for Wilcoxon use selected metric
    method_sel <- input$cmp_method %||% "wilcoxon"
    metric_sel <- if (method_sel == "wilcoxon") (input$cmp_metric %||% "percent") else "abund"

    mat <- tryCatch({
      cat <- input$cmp_category %||% "taxonomy"
      if (cat == "taxonomy") {
        rank    <- input$cmp_tax_rank %||% "phylum"
        tax_obj <- proj$taxa[[rank]]
        # Try selected metric first, then fallback
        m <- NULL
        for (metric in c(metric_sel, "abund", "percent", "bases")) {
          candidate <- if (is.list(tax_obj) && !is.data.frame(tax_obj)) tax_obj[[metric]]
                       else if (metric == "abund") tax_obj else NULL
          if (!is.null(candidate) && (is.data.frame(candidate) || is.matrix(candidate))) {
            m <- as.matrix(candidate); break
          }
        }
        m
      } else {
        db      <- input$cmp_func_db %||% names(proj$functions)[1]
        fun_obj <- proj$functions[[db]]
        m <- NULL
        for (metric in c(metric_sel, "abund", "copy_number", "tpm", "bases")) {
          candidate <- if (is.list(fun_obj) && !is.data.frame(fun_obj)) fun_obj[[metric]]
                       else if (metric == "abund") fun_obj else NULL
          if (!is.null(candidate) && (is.data.frame(candidate) || is.matrix(candidate))) {
            m <- as.matrix(candidate); break
          }
        }
        m
      }
    }, error = function(e) NULL)

    # Apply exclusion filters (same as Multivariate tab)
    # Always exclude "No CDS"
    mat <- mat[!grepl("^No CDS$", rownames(mat), ignore.case = TRUE), , drop = FALSE]
    if (isTRUE(input$cmp_excl_unclassified)) {
      excl_pat <- c("Unclassified", "Unmapped", "No database", "")
      mat <- mat[!rownames(mat) %in% excl_pat, , drop = FALSE]
    }
    if (isTRUE(input$cmp_excl_ambiguous)) {
      mat <- mat[!grepl("^unclassified", rownames(mat), ignore.case = TRUE), , drop = FALSE]
    }

    if (is.null(mat) || nrow(mat) == 0) {
      cmp_status("error")
      # Diagnostic: show actual structure
      dt <- input$cmp_data_type %||% "tax_phylum"
      diag_msg <- tryCatch({
        proj2 <- sqm_data()
        cat2  <- input$cmp_category %||% "taxonomy"
        if (cat2 == "taxonomy") {
          rank2 <- input$cmp_tax_rank %||% "phylum"
          obj   <- proj2$taxa[[rank2]]
          paste0("taxa[[", rank2, "]] class=", paste(class(obj), collapse=","),
                 " dim=", paste(dim(obj), collapse="x"),
                 " names=", paste(head(names(obj),5), collapse=","))
        } else {
          db2 <- input$cmp_func_db %||% names(proj2$functions)[1]
          obj <- proj2$functions[[db2]]
          paste0("functions[[", db2, "]] class=", paste(class(obj), collapse=","),
                 " dim=", paste(dim(obj), collapse="x"),
                 " names=", paste(head(names(obj),5), collapse=","))
        }
      }, error = function(e) paste("diag error:", e$message))
      showNotification(paste("Matrix error:", diag_msg), type = "error", duration = 20)
      return()
    }

    # Keep only assigned samples
    mat <- mat[, colnames(mat) %in% names(grps), drop = FALSE]
    grps <- grps[colnames(mat)]
    method <- input$cmp_method %||% "wilcoxon"

    # Run Group1 vs Group2
    pairs <- list(c("Group1", "Group2"))
    results <- tryCatch({
      lapply(pairs, function(pair) {
        g1 <- pair[1]; g2 <- pair[2]
        s1 <- names(grps)[grps == g1]; s2 <- names(grps)[grps == g2]
        sub_mat <- mat[, c(s1, s2), drop = FALSE]
        sub_grp <- factor(grps[c(s1, s2)])

        if (method == "wilcoxon") {
          run_wilcoxon(sub_mat, s1, s2, g1, g2)
        } else if (method == "deseq2") {
          run_deseq2(sub_mat, sub_grp, g1, g2)
        } else {
          run_edger(sub_mat, sub_grp, g1, g2)
        }
      })
    }, error = function(e) {
      cmp_status("error")
      showNotification(paste("Analysis error:", e$message), type = "error", duration = 10)
      NULL
    })

    if (!is.null(results)) {
      pair_names <- sapply(pairs, function(p) paste(p, collapse = " vs "))
      names(results) <- pair_names
      # Attach robustness note to results
      if (n1 < 3 || n2 < 3)
        attr(results, "warning") <- paste0(
          "Groups have only ", n1, " and ", n2, " samples. ",
          "Statistical power is low and results may not be reproducible. ",
          "At least 3 biological replicates per group are recommended.")
      cmp_results(results)
      cmp_status("ready")
    }
  })

  # ---- Helper: Wilcoxon ----------------------------------------------------
  run_wilcoxon <- function(mat, s1, s2, g1, g2) {
    # Proportions
    tot1 <- colSums(mat[, s1, drop=FALSE]) + 1
    tot2 <- colSums(mat[, s2, drop=FALSE]) + 1
    prop1 <- sweep(mat[, s1, drop=FALSE], 2, tot1, "/")
    prop2 <- sweep(mat[, s2, drop=FALSE], 2, tot2, "/")
    mean1 <- rowMeans(prop1); mean2 <- rowMeans(prop2)
    lfc   <- log2((mean2 + 1e-9) / (mean1 + 1e-9))
    pvals <- apply(mat, 1, function(r) {
      tryCatch(wilcox.test(r[s1], r[s2])$p.value, error = function(e) NA)
    })
    padj <- p.adjust(pvals, method = "BH")
    data.frame(feature = rownames(mat), log2FC = lfc, pvalue = pvals,
               padj = padj, mean_g1 = mean1, mean_g2 = mean2,
               stringsAsFactors = FALSE)
  }

  # ---- Helper: DESeq2 ------------------------------------------------------
  run_deseq2 <- function(mat, grp, g1, g2) {
    if (!requireNamespace("DESeq2", quietly = TRUE))
      stop("DESeq2 is not installed. Install it with BiocManager::install('DESeq2').")
    mat_int <- round(mat)
    mode(mat_int) <- "integer"
    col_data <- data.frame(group = grp, row.names = colnames(mat_int))
    dds <- DESeq2::DESeqDataSetFromMatrix(mat_int, col_data, ~ group)
    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    res <- DESeq2::results(dds, contrast = c("group", g2, g1), independentFiltering = FALSE)
    res <- as.data.frame(res)
    sn <- colnames(mat)
    s1 <- sn[as.character(grp) == g1]; s2 <- sn[as.character(grp) == g2]
    mean1 <- rowMeans(mat[, s1, drop=FALSE])
    mean2 <- rowMeans(mat[, s2, drop=FALSE])
    data.frame(feature = rownames(res), log2FC = res$log2FoldChange,
               pvalue = res$pvalue, padj = res$padj,
               mean_g1 = mean1[rownames(res)], mean_g2 = mean2[rownames(res)],
               stringsAsFactors = FALSE)
  }

  # ---- Helper: edgeR -------------------------------------------------------
  run_edger <- function(mat, grp, g1, g2) {
    if (!requireNamespace("edgeR", quietly = TRUE))
      stop("edgeR is not installed. Install it with BiocManager::install('edgeR').")
    mat_int <- round(mat); mode(mat_int) <- "integer"
    dge <- edgeR::DGEList(counts = mat_int, group = grp)
    dge <- edgeR::calcNormFactors(dge)
    design <- model.matrix(~ grp)
    dge <- edgeR::estimateDisp(dge, design)
    fit <- edgeR::glmQLFit(dge, design)
    qlf <- edgeR::glmQLFTest(fit)
    tt   <- edgeR::topTags(qlf, n = Inf, sort.by = "none")$table
    sn <- colnames(mat)
    s1 <- sn[as.character(grp) == g1]; s2 <- sn[as.character(grp) == g2]
    mean1 <- rowMeans(mat[, s1, drop=FALSE])
    mean2 <- rowMeans(mat[, s2, drop=FALSE])
    data.frame(feature = rownames(tt), log2FC = tt$logFC,
               pvalue = tt$PValue, padj = tt$FDR,
               mean_g1 = mean1[rownames(tt)], mean_g2 = mean2[rownames(tt)],
               stringsAsFactors = FALSE)
  }

  # ---- Main display --------------------------------------------------------
  output$cmp_main_ui <- renderUI({
    res <- cmp_results(); req(res)
    pair <- input$cmp_pair %||% names(res)[1]
    df   <- res[[pair]];  req(df)
    fdr  <- input$cmp_fdr %||% 0.05
    lfc  <- input$cmp_lfc %||% 1

    tagList(
      # Robustness notice stored in results
      if (!is.null(attr(res, "warning")))
        tags$div(class = "alert alert-warning", style = "font-size:0.82rem; padding:6px 10px; margin-bottom:8px;",
          tags$strong("Statistical caution: "), attr(res, "warning")),

      # Pair selector (only when >1 pair)
      if (length(res) > 1)
        tags$div(class = "sidebar-box", style = "margin-bottom:8px;",
          tags$div(class = "form-label", "Comparison"),
          selectInput("cmp_pair", NULL, choices = names(res),
                      selected = pair, width = "100%")),

      # Volcano plot
      tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
        tags$div(class = "form-label", style = "margin:0;", "Volcano plot"),
        tags$button(
          class = "btn btn-outline-secondary btn-sm",
          style = "font-size:0.78rem;",
          onclick = paste0(
            "var gd = document.querySelector('#cmp_main_ui .js-plotly-plot');",
            "if (!gd || !gd._fullLayout) { alert('Plot not ready'); return; }",
            "Plotly.toImage(gd, {format:'png', width:gd._fullLayout.width,",
            " height:gd._fullLayout.height, scale:2}).then(function(url){",
            "  var a = document.createElement('a');",
            "  a.href = url; a.download = 'volcano_plot.png'; a.click();",
            "});"
          ),
          "💾 Download PNG"
        )
      ),
      plotly::renderPlotly({
        df2 <- df[!is.na(df$padj), ]
        pair_name <- input$cmp_pair %||% names(res)[1]
        grp_labels <- strsplit(pair_name, " vs ")[[1]]
        g1_label <- if (length(grp_labels) >= 1) grp_labels[1] else "Group1"
        g2_label <- if (length(grp_labels) >= 2) grp_labels[2] else "Group2"
        df2$col <- ifelse(df2$padj <= fdr & df2$log2FC >= lfc,  g2_label,
                   ifelse(df2$padj <= fdr & df2$log2FC <= -lfc, g1_label, "NS"))
        df2$col <- factor(df2$col, levels = c(g1_label, g2_label, "NS"))
        cols <- setNames(c("#2980b9", "#e74c3c", "#bdc3c7"), c(g1_label, g2_label, "NS"))
        # Build hover text: "feature\nGroup1: val1  Group2: val2"
        # Look up feature description for hover
        cat_hover <- isolate(input$cmp_category) %||% "taxonomy"
        feat_names <- if (cat_hover == "functions") {
          db_hover  <- isolate(input$cmp_func_db) %||% ""
          nv <- tryCatch(sqm_data()$misc[[paste0(db_hover, "_names")]], error = function(e) NULL)
          if (!is.null(nv)) nv[df2$feature] else rep("", nrow(df2))
        } else {
          rep("", nrow(df2))
        }
        feat_names[is.na(feat_names)] <- ""
        feat_label <- ifelse(nzchar(feat_names),
          paste0(df2$feature, ": ", feat_names),
          df2$feature)
        df2$hover_txt <- paste0(
          feat_label, "<br>",
          g1_label, ": ", round(df2$mean_g1, 4), "<br>",
          g2_label, ": ", round(df2$mean_g2, 4)
        )
        p <- plotly::plot_ly(df2, x = ~log2FC, y = ~-log10(padj + 1e-300),
          color = ~col, colors = cols,
          type = "scatter", mode = "markers",
          text = ~hover_txt,
          hovertemplate = "%{text}<br>log2FC: %{x:.3f}<br>-log10(FDR): %{y:.2f}<extra></extra>",
          marker = list(size = 6, opacity = 0.75)) |>
        plotly::layout(
          xaxis = list(title = "log2 Fold Change", zeroline = TRUE),
          yaxis = list(title = "-log10(FDR)"),
          legend = list(title = list(text = "")),
          shapes = list(
            list(type="line", x0=lfc,  x1=lfc,  y0=0, y1=1, yref="paper",
                 line=list(dash="dot", color="#aaa")),
            list(type="line", x0=-lfc, x1=-lfc, y0=0, y1=1, yref="paper",
                 line=list(dash="dot", color="#aaa")),
            list(type="line",
                 x0=min(df2$log2FC, na.rm=TRUE) - 0.5,
                 x1=max(df2$log2FC, na.rm=TRUE) + 0.5,
                 y0=-log10(fdr), y1=-log10(fdr),
                 line=list(dash="dot", color="#aaa"))
          )
        )
        plotly::config(p, displayModeBar = FALSE)
      }),

      # Results table (filtered)
      local({
        df_sig <- df[!is.na(df$padj) & df$padj <= fdr & abs(df$log2FC) >= lfc, ]
        n_total <- nrow(df_sig)
        n_up    <- sum(df_sig$log2FC > 0, na.rm = TRUE)
        n_down  <- sum(df_sig$log2FC < 0, na.rm = TRUE)
        pair_nm  <- input$cmp_pair %||% "Group1 vs Group2"
        grp_lbls <- strsplit(pair_nm, " vs ")[[1]]
        g1_lbl   <- if (length(grp_lbls) >= 1) grp_lbls[1] else "Group1"
        g2_lbl   <- if (length(grp_lbls) >= 2) grp_lbls[2] else "Group2"
        badge_style <- function(bg)
          paste0("cursor:pointer; border:none; border-radius:4px; padding:3px 10px;",
                 "font-size:0.8rem; font-weight:600; color:#fff; background:", bg, ";")
        tagList(
        tags$div(class = "form-label", style = "margin-bottom:4px;", "Significant features"),
        tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
          tags$div(style = "display:flex; align-items:center; gap:6px;",
            tags$button(style = badge_style("#555"),
              onclick = "Shiny.setInputValue('cmp_direction', 'all', {priority:'event'})",
              paste0("All: ", n_total)),
            tags$button(style = badge_style("#2980b9"),
              onclick = "Shiny.setInputValue('cmp_direction', 'down', {priority:'event'})",
              paste0(g1_lbl, ": ", n_down)),
            tags$button(style = badge_style("#e74c3c"),
              onclick = "Shiny.setInputValue('cmp_direction', 'up', {priority:'event'})",
              paste0(g2_lbl, ": ", n_up))
          ),
          tags$div(style = "display:flex; align-items:center; gap:6px;",
            tags$span(style = "font-size:0.8rem; color:var(--muted);", "Show"),
            selectInput("cmp_page_len", NULL, choices = c(10, 20, 50, 100), selected = 20, width = "75px"),
            tags$span(style = "font-size:0.8rem; color:var(--muted);", "per page")
        )
        )
        )
      }),
      DT::renderDT({
        n_rows <- as.integer(input$cmp_page_len %||% 20)
        direction <- input$cmp_direction %||% "all"
        df2 <- df[!is.na(df$padj) & df$padj <= fdr & abs(df$log2FC) >= lfc, ]
        if (direction == "up")   df2 <- df2[df2$log2FC > 0, , drop = FALSE]
        if (direction == "down") df2 <- df2[df2$log2FC < 0, , drop = FALSE]
        df2 <- df2[order(df2$padj), ]
        df2$log2FC  <- round(df2$log2FC, 3)
        df2$pvalue  <- signif(df2$pvalue, 3)
        df2$padj    <- signif(df2$padj, 3)
        df2$mean_g1 <- round(df2$mean_g1, 4)
        df2$mean_g2 <- round(df2$mean_g2, 4)
        cat <- isolate(input$cmp_category) %||% "taxonomy"
        if (cat == "functions") {
          db        <- isolate(input$cmp_func_db) %||% ""
          names_vec <- tryCatch(sqm_data()$misc[[paste0(db, "_names")]], error = function(e) NULL)
          if (!is.null(names_vec) && length(names_vec) > 0) {
            df2$name <- names_vec[df2$feature]
            df2$name[is.na(df2$name)] <- ""
            df2 <- df2[, c("feature","name","log2FC","pvalue","padj","mean_g1","mean_g2"), drop = FALSE]
          } else {
            df2 <- df2[, c("feature","log2FC","pvalue","padj","mean_g1","mean_g2"), drop = FALSE]
          }
        } else {
          df2 <- df2[, c("feature","log2FC","pvalue","padj","mean_g1","mean_g2"), drop = FALSE]
        }
        DT::datatable(df2, rownames = FALSE, filter = "none",
          options = list(pageLength = n_rows, scrollX = TRUE, dom = "tip"))
      })
    )
  })

  output$cmp_download_ui <- renderUI({
    req(cmp_results())
    downloadButton("cmp_download", "Download results", class = "btn-outline-secondary w-100")
  })

  output$cmp_download <- downloadHandler(
    filename = function() paste0("comparison_", input$cmp_method, "_", Sys.Date(), ".tsv"),
    content  = function(file) {
      res <- cmp_results(); req(res)
      dfs <- lapply(names(res), function(nm) {
        df <- res[[nm]]; df$comparison <- nm; df
      })
      write.table(do.call(rbind, dfs), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )


  # ===========================================================================
  # MAG MAP TAB — metabolic completeness diagram with interactive overlays
  # ===========================================================================

  # ---- Helper: extract KOs for a bin --------------------------------------
  get_bin_kos <- function(proj, bin_name) {
    # Extract KO identifiers for a given bin.
    #
    # SQM stores KOs in proj$orfs$table[["KEGG ID"]] (one KO per ORF row).
    # Values may carry a trailing asterisk (e.g. "K21573*") which must be
    # stripped before validation.  Bins are not in the ORF table directly;
    # the link is:  ORF -> "Contig ID"  and  contig -> "Bin ID" in contigs$table.

    orf_tbl <- tryCatch(proj$orfs$table,     error = function(e) NULL)
    ctg_tbl <- tryCatch(proj$contigs$table,  error = function(e) NULL)
    if (is.null(orf_tbl) || is.null(ctg_tbl)) return(character(0))

    # -- 1. Find which contigs belong to this bin ------------------------------
    # "Bin ID" column in contigs$table (exact name from SQM)
    bin_col <- grep("^Bin ID$", colnames(ctg_tbl), value = TRUE)[1]
    if (is.na(bin_col)) bin_col <- grep("\\bbin\\b", colnames(ctg_tbl),
                                        ignore.case = TRUE, value = TRUE)[1]
    if (is.na(bin_col) || is.null(bin_col)) return(character(0))

    bin_ids  <- as.character(ctg_tbl[[bin_col]])
    mask_ctg <- bin_ids == bin_name
    # also try stripping common suffixes (.fa.contigs, .fa.sub.contigs)
    if (!any(mask_ctg, na.rm = TRUE)) {
      stripped <- sub("\\.fa(\\.sub)?\\.contigs$", "", bin_name)
      mask_ctg <- bin_ids == stripped
    }
    bin_contigs <- rownames(ctg_tbl)[mask_ctg & !is.na(mask_ctg)]
    if (length(bin_contigs) == 0) return(character(0))

    # -- 2. Find ORFs on those contigs -----------------------------------------
    ctg_col <- grep("^Contig ID$", colnames(orf_tbl), value = TRUE)[1]
    if (is.na(ctg_col)) ctg_col <- grep("\\bcontig\\b", colnames(orf_tbl),
                                         ignore.case = TRUE, value = TRUE)[1]
    if (is.na(ctg_col) || is.null(ctg_col)) return(character(0))

    orf_rows <- orf_tbl[as.character(orf_tbl[[ctg_col]]) %in% bin_contigs, ,
                        drop = FALSE]
    if (nrow(orf_rows) == 0) return(character(0))

    # -- 3. Extract KO identifiers ---------------------------------------------
    # Column is "KEGG ID"; values may be "K00001", "K00001*", NA, "-", etc.
    ko_col <- grep("^KEGG ID$", colnames(orf_tbl), value = TRUE)[1]
    if (is.na(ko_col)) ko_col <- grep("^KEGG$", colnames(orf_tbl),
                                       ignore.case = TRUE, value = TRUE)[1]
    if (is.na(ko_col) || is.null(ko_col)) return(character(0))

    kos_raw <- as.character(orf_rows[[ko_col]])
    kos <- unlist(strsplit(kos_raw, "[,;[:space:]]+"))
    kos <- sub("\\*+$", "", kos)        # strip trailing asterisk(s)
    kos <- unique(kos[grepl("^K\\d{5}$", kos)])
    kos
  }

  # ---- Category definitions: explicit curated KO lists + rect coords --------
  # rect = c(x0, y0, x1, y1) as fractions [0,1] of image (1536x1024 px).
  # kos  = explicit curated list of KEGG KOs for that functional category.
  # Completeness = (KOs present in MAG) / (total KOs in list) * 100.
  MAG_MAP_CATEGORIES <- list(

    # Central Carbon Metabolism
    "Glycolysis" = list(
      rect = c(0.1146, 0.1494, 0.2995, 0.2246),
      kos  = c("K00844","K12407","K00845","K25026",
               "K01810","K06859","K13810","K15916",
               "K00850","K16370","K21071","K00918",
               "K01623","K01624","K11645","K16305","K16306",
               "K00134","K00150","K00927",
               "K01834","K15633","K15634","K15635",
               "K01689","K00873","K12406",
               "K01596","K01610","K00016")
    ),
    "Pentose Phosphate\nPathway" = list(
      rect = c(0.1146, 0.2500, 0.3034, 0.3252),
      kos  = c("K00036","K01057","K07404","K00033",
               "K01807","K01808","K01783",
               "K00615","K00616","K00622")
    ),
    "Entner-Doudoroff\nPathway" = list(
      rect = c(0.1139, 0.3574, 0.2949, 0.4316),
      kos  = c("K00036","K01057","K00033","K01690","K01625")
    ),
    "TCA Cycle" = list(
      rect = c(0.2480, 0.2246, 0.4167, 0.4316),
      kos  = c("K00163","K00161","K00162","K00627","K00382",
               "K01647","K01648","K01681","K01682",
               "K00031","K00030","K00164","K00658",
               "K01902","K01903",
               "K00239","K00240","K00241","K00242",
               "K01676","K01677","K01678",
               "K00116","K00025","K00026","K00024",
               "K01637","K01638")
    ),
    "CO2 Fixation" = list(
      rect = c(0.3971, 0.1494, 0.5195, 0.2246),
      kos  = c("K01601","K01602","K00855",
               "K00197","K00194","K15022",
               "K01938","K01491","K00297",
               "K14138","K14139","K14140","K14141",
               "K00169","K00170","K00171","K00172","K01006",
               "K00174","K00175","K00177","K00176",
               "K01647","K15230","K15231")
    ),
    "Fermentation" = list(
      rect = c(0.4167, 0.3574, 0.5195, 0.4316),
      kos  = c("K01568","K13979","K04072","K00001","K00121",
               "K00656","K00625","K00925",
               "K00016","K03777","K01613",
               "K01659","K00004",
               "K00929","K00634","K01034","K01035")
    ),

    # N, S and CH4 Metabolism
    "Nitrogen\nFixation" = list(
      rect = c(0.5560, 0.1494, 0.7129, 0.2246),
      kos  = c("K02588","K02586","K02591",
               "K02585","K02587","K02592","K02589",
               "K02593","K02594","K02595","K02596",
               "K02597","K02600","K02601","K02584",
               "K00531","K22896","K22897")
    ),
    "Assimilatory N" = list(
      rect = c(0.7526, 0.1494, 0.8913, 0.2246),
      kos  = c("K01915","K01914",
               "K00264","K00265","K00266",
               "K00261","K00262","K00263",
               "K00367","K10534",
               "K00362","K00363","K00366",
               "K02575","K02576","K02577","K02578")
    ),
    "Denitrification" = list(
      rect = c(0.5560, 0.2500, 0.7122, 0.3252),
      kos  = c("K00370","K00371","K00374",
               "K02567","K02568","K00376",
               "K02305","K04561","K00368","K15864")
    ),
    "Sulfur Cycle" = list(
      rect = c(0.7526, 0.2500, 0.8913, 0.3252),
      kos  = c("K00958","K00394","K00395",
               "K11180","K11181",
               "K17224","K17225","K17226","K17227","K17228","K17229",
               "K16950","K16951","K17252",
               "K00956","K00957","K00390",
               "K00380","K00381","K01738")
    ),
    "Nitrification" = list(
      rect = c(0.5560, 0.3574, 0.7129, 0.4316),
      kos  = c("K10944","K10945","K10946","K10535",
               "K00370","K00371","K20932","K20935")
    ),
    "Methane\nMetabolism" = list(
      rect = c(0.7526, 0.3574, 0.8926, 0.4316),
      kos  = c("K00399","K00401","K00402",
               "K03421","K03422","K03423","K03424","K03388","K03390","K03391",
               "K00205","K00319","K04480",
               "K00320","K00577","K00578","K00579","K00580","K00581","K00582","K00583","K00584",
               "K00200","K00201","K00202","K00203","K00204",
               "K14083","K14084",
               "K16157","K16158","K16159","K16160","K16161","K16162",
               "K00440","K00441","K00442","K00443",
               "K01499","K13942","K14126")
    ),

    # Biosynthesis / Anabolism
    "Amino Acids" = list(
      rect = c(0.1120, 0.5127, 0.2337, 0.5859),
      kos  = c("K01915","K00265","K00266","K00261","K00262","K01940","K00811","K00812","K00813",
               "K00928","K00133","K01714","K01778","K00003","K00872","K01733","K01697",
               "K00600","K00831","K01079","K00604","K00605",
               "K00930","K00145","K01438","K00620","K00618","K01755",
               "K00765","K01513","K02502","K01501","K02501","K00817","K01814","K00013",
               "K01695","K01696","K00820","K00766","K01817","K00609","K00601","K01823",
               "K14170","K14172","K04518",
               "K01652","K01653","K00053","K01687","K00826",
               "K00548","K01760","K00549","K01738","K01739")
    ),
    "Nucleotides" = list(
      rect = c(0.2578, 0.5127, 0.3809, 0.5859),
      kos  = c("K00764","K01945","K01944","K01923","K01924",
               "K00602","K01492","K01756",
               "K01951","K00088","K00942",
               "K01465","K00526","K02844","K00611",
               "K01537","K00226","K00940","K13800",
               "K00525","K00549","K01495")
    ),
    "Vitamins /\nCofactors" = list(
      rect = c(0.4062, 0.5127, 0.4876, 0.7070),
      kos  = c("K00941","K00788","K00946","K00949","K03148","K03149","K03151",
               "K00793","K01497","K00794","K02794","K02793",
               "K06215","K06214","K00763","K01556","K00815","K01580",
               "K01071","K00859","K00939",
               "K00652","K01935","K00833","K01012",
               "K01737","K00796","K00297","K01479",
               "K02232","K02229","K02230","K02228",
               "K00798","K01772","K02224","K02225","K02226","K02227",
               "K01890","K01698","K00552","K00599","K01773","K01744","K01770","K00606",
               "K00568","K00667","K03183","K03187","K02548","K02549","K01661","K02551","K02550")
    ),
    "Fatty Acids" = list(
      rect = c(0.1120, 0.5859, 0.2585, 0.7070),
      kos  = c("K00648","K11533","K00647",
               "K00059","K01703","K01704",
               "K00208","K09458","K00645",
               "K01962","K01963","K01961","K01964",
               "K00232","K01692","K00022","K07508",
               "K00249","K01715","K00019","K00632","K00626",
               "K00981","K02517","K00965","K01448")
    ),
    "Cell Wall" = list(
      rect = c(0.2578, 0.5859, 0.3809, 0.7070),
      kos  = c("K01921","K00075","K01914","K00286","K01919","K01920","K01498",
               "K03469","K01778","K06078","K06079",
               "K05363","K03587","K03588","K03589",
               "K01093","K01067",
               "K02536","K02527","K02526","K02528",
               "K02517","K02519","K02535","K03280",
               "K00748","K02850","K02849","K02848",
               "K00754","K02843","K02842",
               "K00699","K00974","K14327","K14328")
    ),

    # Respiration / Energy
    "ETC" = list(
      rect = c(0.5306, 0.5127, 0.7441, 0.5859),
      kos  = c("K00330","K00331","K00332","K00333","K00334","K00335","K00336",
               "K00337","K00338","K00339","K00340","K00341","K00342","K00343",
               "K00239","K00240","K00241","K00242",
               "K00411","K00412","K00413","K00414","K00415",
               "K02256","K02262","K02264","K02265",
               "K02277","K02276","K00425","K00426",
               "K02548","K03186","K03187")
    ),
    "ATP Synthase" = list(
      rect = c(0.7441, 0.5127, 0.8861, 0.5859),
      kos  = c("K02111","K02112","K02115","K02113","K02114",
               "K02108","K02109","K02110","K02116",
               "K02132","K02133","K02134")
    ),
    "Oxidative\nPhosphorylation" = list(
      rect = c(0.5306, 0.5859, 0.7441, 0.6816),
      kos  = c("K00330","K00331","K00332","K00333","K00334","K00335","K00336",
               "K00337","K00338","K00339","K00340","K00341","K00342","K00343",
               "K00239","K00240","K00241","K00242",
               "K00411","K00412","K00413","K00414","K00415",
               "K02256","K02262","K02264","K02265",
               "K00425","K00426",
               "K02111","K02112","K02115","K02113","K02114",
               "K02108","K02109","K02110","K02116")
    ),
    "Anaerobic\nRespiration" = list(
      rect = c(0.7441, 0.5859, 0.8854, 0.6816),
      kos  = c("K00370","K00371","K00374",
               "K02567","K02568","K00368","K15864",
               "K02305","K04561","K00376",
               "K00958","K00394","K00395","K11180","K11181",
               "K00244","K00245","K00246","K00247",
               "K07306","K07305","K03603","K04755","K02639")
    ),
    "Photosynthesis" = list(
      rect = c(0.5306, 0.6816, 0.8854, 0.7598),
      kos  = c("K02703","K02704","K02705","K02706","K02707","K02708",
               "K08905","K02689","K02690","K02691","K02692","K02693","K02694",
               "K02696","K02697","K02698","K02699","K02700","K02701","K02702",
               "K02711","K02712","K02713","K02714","K02715","K02716",
               "K01601","K01602","K00855","K02641","K02642")
    ),

    # Transporters / Systems
    "ABC\nTransporters" = list(
      rect = c(0.1276, 0.8223, 0.2441, 0.8906),
      kos  = c("K06147","K06148","K06149",
               "K01998","K01999","K02000","K02001","K02002",
               "K10542","K10543","K10544","K10545","K10546",
               "K10118","K10119","K10120",
               "K02031","K02032","K02033","K02034","K02035",
               "K02003","K02004","K02005","K02006",
               "K09687","K09688","K09689",
               "K10551","K10552","K10553",
               "K01990","K01991","K01992",
               "K10440","K10441","K10442",
               "K09969","K10009","K10013",
               "K02028","K02027","K02026",
               "K02036","K02037","K02038","K02039","K02040",
               "K10820","K10821","K10822","K12340","K12341")
    ),
    "Sec / Tat\nSystems" = list(
      rect = c(0.2669, 0.8223, 0.3737, 0.8906),
      kos  = c("K03070","K03071","K03072","K03073","K03074","K03075","K03076","K03077",
               "K03110","K03106","K03286",
               "K03522","K03523","K13771",
               "K03116","K03117","K03118","K03425",
               "K02453","K02454","K02455","K02456","K02457","K02458",
               "K02459","K02460","K02461","K02462","K02463","K02464")
    ),
    "Efflux\nPumps" = list(
      rect = c(0.3958, 0.8223, 0.4980, 0.8906),
      kos  = c("K18138","K18139","K03585","K06147",
               "K18143","K18144","K08139","K03543","K08140",
               "K03380","K03327","K09687","K09688",
               "K02470","K13655","K07672","K19415")
    ),
    "Motility" = list(
      rect = c(0.5195, 0.8223, 0.6159, 0.8906),
      kos  = c("K02406","K02394",
               "K02387","K02388","K02389","K02390","K02391","K02392","K02393",
               "K02395","K02396","K02397","K02398","K02399","K02400","K02401",
               "K02407","K02408","K02409","K02410","K02411","K02412","K02413",
               "K02414","K02415","K02416","K02417","K02418","K02419","K02420",
               "K02556","K02557","K02558",
               "K02403","K02404","K02405",
               "K02428","K02429","K02430","K02431","K02432",
               "K03406","K03407","K03408","K03409","K03410",
               "K03413","K03412","K02659","K02658")
    ),
    "CRISPR" = list(
      rect = c(0.6400, 0.8223, 0.7344, 0.8906),
      kos  = c("K19078","K19079",
               "K19080","K19081","K19082","K19083","K19084","K19085","K19086",
               "K19611","K19473","K21319",
               "K19088","K19089","K19090","K19091","K21572")
    ),
    "Stress\nResponse" = list(
      rect = c(0.7546, 0.8223, 0.8737, 0.8906),
      kos  = c("K03782","K03781","K00428","K00432",
               "K04564","K04565","K00562",
               "K00386","K03671","K04077","K04078",
               "K03686","K04083","K03687","K05056",
               "K03799","K04080","K04081","K04082",
               "K03553","K03217","K02313","K02314","K03111",
               "K02083","K11931","K03610",
               "K03704","K03705","K03706","K03707","K03708","K03709",
               "K06203","K06204")
    )
  )
  magmap_selected_bin <- reactiveVal(NULL)

  # Completeness: present KOs / curated KO list size * 100
  magmap_completeness <- reactive({
    proj    <- sqm_data(); req(proj)
    bin     <- magmap_selected_bin(); req(bin)
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))
    lapply(MAG_MAP_CATEGORIES, function(cat_info) {
      cat_kos <- unique(cat_info$kos)
      if (length(cat_kos) == 0) return(list(pct = NA_real_, present = 0L, total = 0L))
      present <- sum(cat_kos %in% mag_kos)
      list(pct     = round(100 * present / length(cat_kos), 1),
           present = present,
           total   = length(cat_kos))
    })
  })

  # MAG selector UI
  output$magmap_bin_select_ui <- renderUI({
    proj <- sqm_data()
    if (is.null(proj))
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "Load a SQM project with binning first."))
    bins <- tryCatch(rownames(proj$bins$table), error = function(e) NULL)
    if (is.null(bins) || length(bins) == 0)
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "No MAGs found in this project."))
    selectInput("magmap_bin", NULL, choices = c("— select a MAG —" = "", bins))
  })

  observeEvent(input$magmap_bin, {
    v <- input$magmap_bin
    magmap_selected_bin(if (nzchar(v)) v else NULL)
  })

  # Sidebar info panel
  output$magmap_selected_ui <- renderUI({
    bin <- magmap_selected_bin()
    if (is.null(bin))
      return(tags$div(style = "font-size:0.8rem; color:var(--muted);",
        "Select a MAG above to see completeness overlay."))
    proj    <- sqm_data()
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))
    tags$div(
      tags$div(class = "form-label", "Selected MAG"),
      tags$div(style = "font-size:0.82rem; word-break:break-all; font-weight:600;", bin),
      tags$div(style = "font-size:0.78rem; color:var(--muted); margin-top:4px;",
        paste0(length(mag_kos), " unique KEGG KOs"))
    )
  })

  # Main view: BacMet image + SVG overlay (coloured rects + % labels)
  output$magmap_view_ui <- renderUI({
    proj <- sqm_data()

    # Resolve image (www/ folder preferred; fall back to base64 or copy)
    img_src   <- "BacMet.png"
    www_path  <- file.path(app_dir, "www", "BacMet.png")
    root_path <- file.path(app_dir, "BacMet.png")
    if (!file.exists(www_path) && !file.exists(root_path))
      return(tags$div(style = "padding:2rem; color:var(--muted);",
        "BacMet.png not found. Place it in the app's www/ folder or the app directory."))
    if (!file.exists(www_path) && file.exists(root_path)) {
      if (requireNamespace("base64enc", quietly = TRUE)) {
        img_src <- paste0("data:image/png;base64,",
                          base64enc::base64encode(root_path))
      } else {
        dir.create(file.path(app_dir, "www"), showWarnings = FALSE)
        file.copy(root_path, www_path)
      }
    }

    bin  <- magmap_selected_bin()
    comp <- if (!is.null(bin) && !is.null(proj))
              tryCatch(magmap_completeness(), error = function(e) NULL)
            else NULL

    # SVG defs: drop-shadow for text legibility
    svg_defs <- '<defs></defs>'

    # Build SVG elements: progress-bar fill + black bold % text at top
    svg_elements <- ""
    if (!is.null(comp)) {
      svg_elements <- paste(mapply(function(cat_name, cat_info, cat_comp) {
        r    <- cat_info$rect
        pct  <- cat_comp$pct
        pres <- cat_comp$present
        tot  <- cat_comp$total

        fill_col <- "70,130,220"   # single colour for all bars

        label    <- if (is.na(pct)) "N/A" else paste0(round(pct), "%")
        tip      <- htmltools::htmlEscape(
                      paste0(gsub("\n", " ", cat_name), "\n",
                             label, "  (", pres, "/", tot, " KOs)"),
                      attribute = TRUE)

        # Coordinates in viewBox [0,100] units
        x0 <- r[1] * 100; y0 <- r[2] * 100
        w  <- (r[3] - r[1]) * 100; h <- (r[4] - r[2]) * 100
        cx <- (r[1] + r[3]) / 2 * 100

        # Progress bar width proportional to pct
        fill_w <- if (is.na(pct)) 0 else w * (pct / 100)

        # clip-path id (alphanumeric only)
        cp_id <- paste0("cp_", gsub("[^A-Za-z0-9]", "_", cat_name))

        # Label flush with top border
        ty <- y0 + 0.15

        paste0(
          # Clip path so progress bar respects rounded corners
          if (fill_w > 0) sprintf(
            '<clipPath id="%s"><rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" rx="0.8" ry="0.8"/></clipPath>\n',
            cp_id, x0, y0, w, h) else "",

          # Progress bar fill
          if (fill_w > 0) sprintf(
            '<rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" fill="rgba(%s,0.45)" clip-path="url(#%s)" data-cat="%s" data-tip="%s"/>\n',
            x0, y0, fill_w, h, fill_col, cp_id, htmltools::htmlEscape(cat_name, attribute=TRUE), tip) else "",

          # Box outline (no fill)
          sprintf('<rect class="magmap-cat" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f" width="%.3f" height="%.3f"', x0, y0, w, h),
          ' fill="transparent"'  ,
          sprintf(' stroke="rgba(%s,0.85)" stroke-width="0.35"', fill_col),
          ' rx="0.8" ry="0.8"',
          sprintf(' data-tip="%s"/>\n', tip),

          # % label: black, bold, small, flush top
          sprintf('<text class="magmap-label" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f"', cx, ty),
          ' dominant-baseline="hanging" text-anchor="middle"',
          ' font-family="Arial,sans-serif" font-weight="bold"',
          ' font-size="1.7"',
          ' fill="black"',
          sprintf(' data-tip="%s">', tip),
          label,
          '</text>\n'
        )
      }, names(MAG_MAP_CATEGORIES), MAG_MAP_CATEGORIES, comp, SIMPLIFY = FALSE),
      collapse = "")
    }

    # Tooltip + click JS
    tooltip_js <- "
(function() {
  var tip = document.getElementById('magmap-tooltip');
  if (!tip) {
    tip = document.createElement('div');
    tip.id = 'magmap-tooltip';
    tip.style.cssText =
      'position:fixed;pointer-events:none;display:none;' +
      'background:rgba(15,15,15,0.93);color:#fff;font-size:0.8rem;' +
      'padding:7px 13px;border-radius:7px;z-index:9999;white-space:pre;' +
      'box-shadow:0 3px 12px rgba(0,0,0,0.5);line-height:1.55;';
    document.body.appendChild(tip);
  }
  var svg = document.getElementById('magmap-svg-overlay');
  if (!svg) return;
  svg.querySelectorAll('[data-cat]').forEach(function(el) {
    el.style.cursor = 'pointer';
    el.addEventListener('mouseenter', function() {
      tip.textContent = el.getAttribute('data-tip'); tip.style.display = 'block';
    });
    el.addEventListener('mousemove', function(e) {
      tip.style.left = (e.clientX + 16) + 'px';
      tip.style.top  = (e.clientY - 36) + 'px';
    });
    el.addEventListener('mouseleave', function() { tip.style.display = 'none'; });
    el.addEventListener('click', function() {
      var cat = el.getAttribute('data-cat');
      if (cat && typeof Shiny !== 'undefined')
        Shiny.setInputValue('magmap_clicked_cat', cat, {priority: 'event'});
    });
  });
})();
"

    tagList(
      tags$div(
        style = "position:relative; display:inline-block; width:100%; max-width:100%;",
        tags$img(src = img_src, style = "width:100%; height:auto; display:block;",
                 alt = "Metabolic diagram"),
        tags$svg(
          id                  = "magmap-svg-overlay",
          xmlns               = "http://www.w3.org/2000/svg",
          viewBox             = "0 0 100 100",
          preserveAspectRatio = "none",
          style = paste0("position:absolute; top:0; left:0;",
                         " width:100%; height:100%;",
                         " pointer-events:auto; overflow:visible;"),
          HTML(paste0(svg_defs, svg_elements))
        )
      ),
      tags$script(HTML(tooltip_js))
    )
  })

  # ── MAG Map: clicked category → KO detail table ─────────────────────────────

  magmap_clicked_cat <- reactiveVal(NULL)

  observeEvent(input$magmap_clicked_cat, {
    v <- input$magmap_clicked_cat
    magmap_clicked_cat(if (!is.null(v) && nzchar(v)) v else NULL)
  })

  output$magmap_detail_ui <- renderUI({
    cat_name <- magmap_clicked_cat()
    if (is.null(cat_name)) return(NULL)

    bin <- magmap_selected_bin()
    if (is.null(bin)) return(NULL)

    proj <- sqm_data()
    if (is.null(proj)) return(NULL)

    # KOs present in this MAG
    mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))

    # Find the matching category key (cat_name from JS has \n replaced by space)
    cat_key <- names(MAG_MAP_CATEGORIES)[
      vapply(names(MAG_MAP_CATEGORIES), function(n) {
        identical(n, cat_name) ||
        identical(gsub("\n", " ", n), cat_name) ||
        identical(gsub("\n", "\\n", n), cat_name)
      }, logical(1))
    ]
    if (length(cat_key) == 0) return(NULL)
    cat_kos <- unique(MAG_MAP_CATEGORIES[[cat_key[1]]]$kos)

    # KO name + path lookup
    # Source 1: proj$functions$KEGG$names — data.frame, rownames=KO, cols: Name, Path
    # Source 2: proj$misc$KEGG_names — named vector KO->name (fallback for names only)
    # Source 3: parse KEGGPATH from orf table (fallback for paths)
    ko_names_df <- tryCatch({
      df <- proj$functions$KEGG$names
      if (is.data.frame(df) && nrow(df) > 0) df else NULL
    }, error = function(e) NULL)

    misc_names <- tryCatch(proj$misc$KEGG_names, error = function(e) NULL)

    # Helper: get name for a KO
    ko_name_fn <- function(ko) {
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df))
        return(as.character(ko_names_df[ko, "Name"]))
      if (!is.null(misc_names) && ko %in% names(misc_names))
        return(as.character(misc_names[[ko]]))
      ""
    }

    # Helper: get L3 pathways for a KO
    # Path column format: "L1; L2; L3 | L1; L2; L3 | ..."
    ko_l3_fn <- function(ko) {
      path_raw <- NA_character_
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df))
        path_raw <- as.character(ko_names_df[ko, "Path"])
      if (is.na(path_raw) || !nzchar(path_raw)) return(character(0))
      blocks <- strsplit(path_raw, " | ", fixed = TRUE)[[1]]
      l3 <- unique(vapply(blocks, function(b) {
        parts <- strsplit(trimws(b), "; ", fixed = TRUE)[[1]]
        if (length(parts) >= 3) trimws(parts[[3]]) else NA_character_
      }, character(1)))
      l3[!is.na(l3) & nzchar(l3)]
    }

    # Build per-KO rows
    rows <- lapply(cat_kos, function(ko) {
      list(
        ko      = ko,
        name    = ko_name_fn(ko),
        l3      = ko_l3_fn(ko),
        present = ko %in% mag_kos
      )
    })

    # Collect all L3s, sort (real pathways first)
    all_l3 <- sort(unique(unlist(lapply(rows, `[[`, "l3"))))
    no_path <- vapply(rows, function(r) length(r$l3) == 0, logical(1))
    has_unassigned <- any(no_path)

    cat_display <- gsub("\n", " ", cat_key[1])
    n_present   <- sum(vapply(rows, `[[`, logical(1), "present"))
    n_total     <- length(rows)

    # Build grouped HTML
    make_ko_row <- function(r) {
      icon  <- if (r$present) "✓" else "✗"
      color <- if (r$present) "#1a7a3a" else "#c0392b"
      bg    <- if (r$present) "#f0fff4" else "#fff5f5"
      sprintf(
        '<tr style="background:%s;">
          <td style="padding:3px 8px 3px 22px;font-family:monospace;font-size:0.8rem;white-space:nowrap;width:80px;">%s</td>
          <td style="padding:3px 8px;font-size:0.79rem;">%s</td>
          <td style="padding:3px 12px;text-align:center;font-size:1rem;color:%s;width:40px;">%s</td>
        </tr>',
        bg,
        htmltools::htmlEscape(r$ko),
        htmltools::htmlEscape(r$name),
        color, icon)
    }

    make_l3_header <- function(label) sprintf(
      '<tr style="background:#e8edf5;"><td colspan="3" style="padding:5px 8px;font-weight:600;font-size:0.81rem;color:#1a3a6b;letter-spacing:0.01em;">%s</td></tr>',
      htmltools::htmlEscape(label))

    tbl_html <- ""
    for (l3 in all_l3) {
      grp <- Filter(function(r) l3 %in% r$l3, rows)
      if (length(grp) == 0) next
      tbl_html <- paste0(tbl_html, make_l3_header(l3),
                         paste(vapply(grp, make_ko_row, character(1)), collapse = ""))
    }
    if (has_unassigned) {
      grp <- rows[no_path]
      tbl_html <- paste0(tbl_html, make_l3_header("(no pathway assigned)"),
                         paste(vapply(grp, make_ko_row, character(1)), collapse = ""))
    }

    card(
      style = "margin-top:12px;",
      card_header(
        tags$span(style = "font-weight:600;", cat_display),
        tags$span(style = "margin-left:12px; font-size:0.82rem; color:var(--muted);",
          sprintf("%d / %d KOs present (%.0f%%)", n_present, n_total,
                  100 * n_present / max(n_total, 1)))
      ),
      card_body(class = "p-0",
        tags$div(
          style = "max-height:380px; overflow-y:auto;",
          tags$table(
            style = "width:100%; border-collapse:collapse;",
            tags$thead(
              tags$tr(style = "background:#d0d8ea; position:sticky; top:0; z-index:1;",
                tags$th(style = "padding:5px 8px 5px 22px; text-align:left; font-size:0.81rem;", "KO"),
                tags$th(style = "padding:5px 8px; text-align:left; font-size:0.81rem;", "Name"),
                tags$th(style = "padding:5px 12px; text-align:center; font-size:0.81rem; width:40px;", "Present")
              )
            ),
            tags$tbody(HTML(tbl_html))
          )
        )
      )
    )
  })


  # get_bin_kos() is defined earlier in this server scope — no duplication needed.



  # ===========================================================================
  # LAUNCHER — Run tab
  # All input IDs prefixed "lnch_" to avoid conflicts with Watermelon inputs.
  # ===========================================================================

  lnch_roots <- c(home = normalizePath("~"))

  shinyFiles::shinyFileChoose(input, "lnch_samples_file",   roots = lnch_roots)
  shinyFiles::shinyDirChoose(input,  "lnch_input_dir",     roots = lnch_roots)
  shinyFiles::shinyDirChoose(input,  "lnch_workdir",       roots = lnch_roots)
  shinyFiles::shinyFileChoose(input, "lnch_extdb",         roots = lnch_roots)
  shinyFiles::shinyDirChoose(input,  "lnch_sf_dir",        roots = lnch_roots)
  shinyFiles::shinyDirChoose(input,  "lnch_sf_savedir",    roots = lnch_roots)

  lnch_samples_path <- reactive({
    # Use path from the samples file creator if set
    rv_path <- lnch_samples_file_path_rv()
    if (!is.null(rv_path) && nzchar(rv_path)) return(rv_path)
    req(input$lnch_samples_file)
    shinyFiles::parseFilePaths(lnch_roots, input$lnch_samples_file)$datapath
  })

  # Reset rv when user picks a file manually
  observeEvent(input$lnch_samples_file, { lnch_samples_file_path_rv(NULL) })
  lnch_input_path <- reactive({
    rv <- lnch_input_dir_rv()
    if (!is.null(rv) && nzchar(rv)) return(rv)
    req(input$lnch_input_dir)
    shinyFiles::parseDirPath(lnch_roots, input$lnch_input_dir)
  })

  # Reset rv when user picks manually
  observeEvent(input$lnch_input_dir, { lnch_input_dir_rv(NULL) })
  lnch_workdir_path <- reactive({
    req(input$lnch_workdir)
    shinyFiles::parseDirPath(lnch_roots, input$lnch_workdir)
  })
  lnch_extdb_path <- reactive({
    req(input$lnch_extdb)
    shinyFiles::parseFilePaths(lnch_roots, input$lnch_extdb)$datapath
  })

  output$lnch_samples_path <- renderText({ lnch_samples_path() })
  output$lnch_input_path   <- renderText({ lnch_input_path() })
  output$lnch_workdir_path <- renderText({ lnch_workdir_path() })
  output$lnch_extdb_path   <- renderText({ lnch_extdb_path() })

  lnch_proc              <- reactiveVal(NULL)
  lnch_log_buffer        <- reactiveVal("")
  lnch_status            <- reactiveVal("Idle")
  lnch_current_consensus <- reactiveVal(50)
  lnch_current_step      <- reactiveVal(NULL)

  session$onFlushed(function() {
    observe({
      shinyjs::toggleState("lnch_run",  condition = lnch_status() != "Running")
      shinyjs::toggleState("lnch_stop", condition = lnch_status() == "Running")
    })
  }, once = TRUE)

  output$lnch_status_badge <- renderUI({
    s   <- lnch_status()
    cls <- switch(s,
      "Idle"     = "launcher-status-idle",
      "Running"  = "launcher-status-running",
      "Finished" = "launcher-status-finished",
      "Error"    = "launcher-status-error",
      "Aborted"  = "launcher-status-aborted",
      "launcher-status-idle"
    )
    spinner <- if (s == "Running")
      tags$span(class = "spinner-border spinner-border-sm me-1",
                role = "status", `aria-hidden` = "true")
    tags$span(class = paste("badge rounded-pill", cls), spinner, s)
  })

  output$lnch_step_display <- renderUI({
    step <- lnch_current_step()
    if (is.null(step) || lnch_status() %in% c("Idle", "Finished", "Error", "Aborted"))
      return(NULL)
    tags$div(id = "lnch-step-display", step)
  })

  observeEvent(input$lnch_program, {
    if (input$lnch_program != "SqueezeMeta.pl")
      updateSelectInput(session, "lnch_profile", selected = "custom")
  })

  observeEvent(input$lnch_profile, {
    req(input$lnch_profile)
    if (input$lnch_program != "SqueezeMeta.pl") return()
    profile <- get_profile_by_name(input$lnch_profile)
    if (is.null(profile)) return()
    p <- profile$parameters
    updateNumericInput(session,  "lnch_threads",       value    = p$threads)
    updateSelectInput(session,   "lnch_mode",          selected = p$mode)
    updateSelectInput(session,   "lnch_assembler",     selected = p$assembler)
    updateSelectInput(session,   "lnch_mapper",        selected = p$mapper)
    updateTextInput(session,     "lnch_assembly_opts", value    = p$assembly_options)
    updateCheckboxInput(session, "lnch_no_bins",       value    = p$skip_binning)
    lnch_current_consensus(if (!is.null(p$consensus)) p$consensus else 50)
    showNotification(paste("Profile applied:", profile$name), type = "message")
  })




  observeEvent(input$lnch_run, {
    req(lnch_samples_path(), lnch_input_path(), lnch_workdir_path(), input$lnch_project_name)
    extdb_val <- NULL
    df_file   <- tryCatch(
      shinyFiles::parseFilePaths(lnch_roots, input$lnch_extdb),
      error = function(e) data.frame()
    )
    if (nrow(df_file) > 0) extdb_val <- df_file$datapath
    lnch_log_buffer("")
    lnch_current_step(NULL)
    res <- tryCatch({
      run_squeezemeta(
        program             = input$lnch_program,
        samples_file        = lnch_samples_path(),
        input_dir           = lnch_input_path(),
        project_name        = input$lnch_project_name,
        workdir             = lnch_workdir_path(),
        mode                = input$lnch_mode,
        threads             = input$lnch_threads,
        run_trimmomatic     = input$lnch_trimmomatic,
        cleaning_parameters = input$lnch_cleaning_params,
        assembler           = input$lnch_assembler,
        assembly_options    = input$lnch_assembly_opts,
        min_contig_length   = input$lnch_min_contig,
        use_singletons      = input$lnch_singletons,
        no_cog              = input$lnch_no_cog,
        no_kegg             = input$lnch_no_kegg,
        no_pfam             = input$lnch_no_pfam,
        eukaryotes          = input$lnch_euk,
        doublepass          = input$lnch_dbl,
        extdb               = extdb_val,
        consensus           = lnch_current_consensus(),
        mapper              = input$lnch_mapper,
        mapping_options     = input$lnch_mapping_opts,
        no_bins             = input$lnch_no_bins,
        only_bins           = input$lnch_only_bins,
        binners             = input$lnch_binners
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      NULL
    })
    if (!is.null(res)) {
      lnch_status("Running")
      lnch_proc(res$process)
      showNotification("Pipeline started", type = "message")
    }
  })

  # ===========================================================================
  # SAMPLES FILE CREATOR
  # ===========================================================================

  sf_dir        <- reactiveVal(NULL)   # selected FASTQ directory
  sf_files      <- reactiveVal(list()) # list of file entries: name, sample, pair
  sf_savedir    <- reactiveVal(NULL)   # where to save the result
  lnch_input_dir_rv <- reactiveVal(NULL)  # override input dir from samples creator

  # Open the creator modal when button clicked
  observeEvent(input$lnch_create_samples, {
    showModal(modalDialog(
      title = "Create samples file",
      size  = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton("sf_save",   "Save samples file", class = "btn-primary"),
        actionButton("sf_cancel", "Cancel",            class = "btn-default")
      ),
      tags$div(
        # Step 1: pick FASTQ directory
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "1. Select FASTQ directory"),
          shinyDirButton("lnch_sf_dir", "Choose directory", "Select directory with FASTQ files",
            class = "btn-default w-100 mb-1"),
          tags$div(class = "launcher-file-path", textOutput("sf_dir_path", inline = TRUE))
        ),
        # Step 2: file list
        uiOutput("sf_file_list_ui"),
        # Step 3: save location
        uiOutput("sf_save_ui")
      )
    ))
  })

  observeEvent(input$sf_cancel, {
    removeModal(); sf_files(list()); sf_dir(NULL); sf_savedir(NULL)
  })

  # When FASTQ directory selected, list files and auto-set input dir
  observeEvent(input$lnch_sf_dir, {
    d <- tryCatch(shinyFiles::parseDirPath(lnch_roots, input$lnch_sf_dir), error = function(e) NULL)
    req(d); req(nzchar(d))
    sf_dir(d)
    sf_savedir(d)  # default save dir = FASTQ dir
    fq_files <- list.files(d, pattern = "\\.(fastq|fq|fasta|fa)(\\.gz)?$",
                           ignore.case = TRUE, full.names = FALSE)
    if (length(fq_files) == 0) {
      showNotification("No FASTQ/FASTA files found in this directory.", type = "warning")
      sf_files(list())
    } else {
      entries <- lapply(fq_files, function(f) {
        base  <- sub("\\.(fastq|fq|fasta|fa)(\\.gz)?$", "", f, ignore.case = TRUE)
        pair  <- if (grepl("_R2|_2$|\\.2$", base)) "pair2" else "pair1"
        sname <- sub("_R[12]$|_[12]$|\\.[12]$", "", base)
        list(file = f, sample = sname, pair = pair)
      })
      sf_files(entries)
    }
  })

  # Save dir picker
  observeEvent(input$lnch_sf_savedir, {
    d <- tryCatch(shinyFiles::parseDirPath(lnch_roots, input$lnch_sf_savedir), error = function(e) NULL)
    req(d); req(nzchar(d)); sf_savedir(d)
  })

  output$sf_dir_path <- renderText({ sf_dir() %||% "" })

  output$sf_file_list_ui <- renderUI({
    entries <- sf_files()
    if (length(entries) == 0) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-top:8px;",
      tags$div(class = "form-label", paste0("2. Set sample names and pair (", length(entries), " files found)")),
      tags$div(style = "max-height:300px; overflow-y:auto;",
        tags$table(style = "width:100%; border-collapse:collapse; font-size:0.82rem;",
          tags$thead(
            tags$tr(
              tags$th(style = "padding:4px 6px; text-align:left; border-bottom:1px solid var(--border);", "File"),
              tags$th(style = "padding:4px 6px; text-align:left; border-bottom:1px solid var(--border);", "Sample name"),
              tags$th(style = "padding:4px 6px; text-align:left; border-bottom:1px solid var(--border);", "Pair")
            )
          ),
          tags$tbody(
            lapply(seq_along(entries), function(i) {
              e <- entries[[i]]
              tags$tr(style = if (i %% 2 == 0) "background:var(--panel);" else "",
                tags$td(style = "padding:3px 6px; word-break:break-all;", e$file),
                tags$td(style = "padding:3px 6px;",
                  textInput(paste0("sf_sname_", i), NULL, value = e$sample,
                    placeholder = "sample name", width = "100%")
                ),
                tags$td(style = "padding:3px 6px;",
                  selectInput(paste0("sf_pair_", i), NULL,
                    choices  = c("pair1", "pair2"),
                    selected = e$pair,
                    width = "90px")
                )
              )
            })
          )
        )
      )
    )
  })

  output$sf_save_ui <- renderUI({
    if (length(sf_files()) == 0) return(NULL)
    tags$div(class = "sidebar-box", style = "margin-top:8px;",
      tags$div(class = "form-label", "3. Save location"),
      shinyDirButton("lnch_sf_savedir", "Choose save directory", "Select output directory",
        class = "btn-default w-100 mb-1"),
      tags$div(class = "launcher-file-path", textOutput("sf_savedir_path", inline = TRUE)),
      tags$div(class = "form-label", style = "margin-top:6px;", "File name"),
      textInput("sf_filename", NULL, value = "samples.tsv",
        placeholder = "samples.tsv", width = "100%")
    )
  })

  output$sf_savedir_path <- renderText({ sf_savedir() %||% "" })

  # Save the samples file
  observeEvent(input$sf_save, {
    entries <- sf_files()
    req(length(entries) > 0)
    sdir <- sf_savedir()
    req(!is.null(sdir) && nzchar(sdir))

    n <- length(entries)
    rows <- lapply(seq_len(n), function(i) {
      sname <- trimws(input[[paste0("sf_sname_", i)]] %||% entries[[i]]$sample)
      pair  <- input[[paste0("sf_pair_",  i)]] %||% entries[[i]]$pair
      fname <- entries[[i]]$file   # just filename, not full path
      c(sname, fname, pair)
    })

    # Validate: no empty sample names
    empty <- which(sapply(rows, function(r) !nzchar(r[1])))
    if (length(empty) > 0) {
      showNotification(paste("Sample name missing for row(s):", paste(empty, collapse = ", ")),
                       type = "error", duration = 6)
      return()
    }

    # Validate: each sample must have only pair1, or both pair1 AND pair2
    by_sample <- split(rows, sapply(rows, `[`, 1))
    invalid <- Filter(function(srows) {
      pairs <- sapply(srows, `[`, 3)
      # Must have pair1; if pair2 present must also have pair1
      !("pair1" %in% pairs)
    }, by_sample)
    if (length(invalid) > 0) {
      showNotification(
        paste("These samples have pair2 but no pair1:", paste(names(invalid), collapse = ", ")),
        type = "error", duration = 8)
      return()
    }

    # Validate: all samples must follow the same pairing scheme
    # (all single-ended OR all paired-ended)
    is_paired <- sapply(by_sample, function(srows) {
      pairs <- sapply(srows, `[`, 3)
      "pair2" %in% pairs
    })
    if (length(unique(is_paired)) > 1) {
      showNotification(
        "Mixed pairing: some samples are paired-end and others are single-end. All must be the same.",
        type = "error", duration = 8)
      return()
    }

    fname_out <- trimws(input$sf_filename %||% "samples.tsv")
    if (!nzchar(fname_out)) fname_out <- "samples.tsv"
    outpath <- file.path(sdir, fname_out)

    lines <- sapply(rows, function(r) paste(r, collapse = "\t"))
    writeLines(lines, outpath)

    # Set input dir to the FASTQ directory
    lnch_input_dir_rv(sf_dir())

    removeModal()
    sf_files(list()); sf_dir(NULL); sf_savedir(NULL)
    showNotification(paste("Saved:", outpath), type = "message", duration = 6)
    lnch_samples_file_path_rv(outpath)
  })

  # reactiveVal to override the samples file path when created via the wizard
  lnch_samples_file_path_rv <- reactiveVal(NULL)

  # Helper: load a finished SqueezeMeta project into Watermelon
  do_load_sqm <- function(proj_dir, tables_dir) {
    status("loading")
    shinyjs::delay(200, {
      tryCatch({
        creator_file <- file.path(proj_dir, "creator.txt")
        is_sqm <- if (file.exists(creator_file))
          grepl("SqueezeMeta", trimws(readLines(creator_file, n = 1, warn = FALSE)),
                ignore.case = TRUE)
        else TRUE
        tp   <- if (is_sqm) proj_dir else tables_dir
        proj <- if (is_sqm) loadSQM(tp) else loadSQMlite(tp)
        sqm_data(proj)
        is_sqm_full(is_sqm)
        status("ready")
        creator_name(if (file.exists(creator_file))
          trimws(readLines(creator_file, n = 1, warn = FALSE)) else "SqueezeMeta")
        tables_path(tp)
        shinyjs::runjs("document.body.classList.remove('sqm-no-project');")
        for (tab in ANALYSIS_TABS) nav_show("main_navbar", tab)
        nav_select("main_navbar", "Load")
        showNotification("Project loaded successfully.", type = "message", duration = 5)
      }, error = function(e) {
        status("error")
        showNotification(paste("Could not load project:", e$message),
                         type = "warning", duration = 10)
      })
    })
  }

  observe({
    p <- lnch_proc()
    req(p)
    invalidateLater(1000, session)
    strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)
    out       <- strip_ansi(p$read_output_lines())
    err       <- strip_ansi(p$read_error_lines())
    new_lines <- c(out[!grepl("Broken pipe", out)], err)
    if (length(new_lines) > 0) {
      lnch_log_buffer(paste(lnch_log_buffer(), paste(new_lines, collapse = "\n"), sep = "\n"))
      session$sendCustomMessage("lnch_scroll_log", list())
      # Detect STEP messages: e.g. "[3 seconds]: STEP2 -> RNA PREDICTION: ..."
      step_lines <- grep("STEP[0-9]+\\s*->", new_lines, value = TRUE)
      if (length(step_lines) > 0) {
        last <- step_lines[length(step_lines)]
        # Extract just "STEP<n> -> description" dropping the "[N seconds]: " prefix
        step_txt <- sub(".*?(STEP[0-9]+\\s*->\\s*.*)", "\\1", last)
        # Remove script filename: e.g. "01.run_all_assemblies.pl" or "02.rnas.pl"
        step_txt <- gsub("\\s*[0-9]+\\.[a-zA-Z0-9_]+\\.pl\\b", "", step_txt)
        step_txt <- gsub("\\s{2,}", " ", trimws(step_txt))
        step_txt <- gsub(":\\s*$", "", step_txt)  # remove trailing colon
        lnch_current_step(step_txt)
      }
    }
    if (!p$is_alive()) {
      exit_code <- p$get_exit_status()
      lnch_status(if (exit_code == 0) "Finished" else "Error")
      lnch_proc(NULL)

      # Auto-load the project into Watermelon when the run completes successfully
      if (exit_code == 0) {
        proj_dir <- file.path(lnch_workdir_path(), input$lnch_project_name)
        if (dir.exists(proj_dir)) {

          # Determine which table-generation script to use
          tables_script <- switch(input$lnch_program,
            "SqueezeMeta.pl"   = "sqm2tables.py",
            "sqm_reads.pl"     = "sqmreads2tables.py",
            "sqm_longreads.pl" = "sqmreads2tables.py",
            "sqm2tables.py"
          )
          tables_dir <- file.path(proj_dir, "results", "tables")

          # Run table-generation script if tables don't exist yet
          if (!dir.exists(tables_dir) || length(list.files(tables_dir)) == 0) {
            showNotification(
              paste("Run finished. Generating tables with", tables_script, "..."),
              type = "message", duration = 6
            )
            lnch_log_buffer(paste0(lnch_log_buffer(),
              "\n--- Generating tables with ", tables_script, " ---\n"))

            tbl_script_path <- Sys.which(tables_script)
            if (!nzchar(tbl_script_path)) {
              showNotification(paste(tables_script, "not found in PATH."),
                               type = "error", duration = 10)
            } else {
              tables_proc <- tryCatch(
                processx::process$new(
                  command   = tbl_script_path,
                  args      = c(proj_dir, tables_dir),
                  stdout    = "|", stderr = "|",
                  supervise = TRUE, wd = proj_dir
                ),
                error = function(e) {
                  showNotification(paste("Could not run", tables_script, ":", e$message),
                                   type = "warning", duration = 10)
                  NULL
                }
              )

              if (!is.null(tables_proc)) {
                # Poll until the script finishes, streaming output to the log
                poll_tables <- function() {
                  out <- tables_proc$read_output_lines()
                  err <- tables_proc$read_error_lines()
                  new_lines <- c(out, err)
                  if (length(new_lines) > 0)
                    lnch_log_buffer(paste(lnch_log_buffer(),
                      paste(new_lines, collapse = "\n"), sep = "\n"))
                  if (tables_proc$is_alive()) {
                    shinyjs::delay(1000, poll_tables())
                  } else {
                    tbl_exit <- tables_proc$get_exit_status()
                    if (tbl_exit != 0) {
                      showNotification(
                        paste(tables_script, "failed (exit", tbl_exit, "). Check the log."),
                        type = "error", duration = 10)
                      status("error")
                    } else {
                      lnch_log_buffer(paste0(lnch_log_buffer(),
                        "\n--- Tables generated successfully ---\n"))
                      do_load_sqm(proj_dir, tables_dir)
                    }
                  }
                }
                poll_tables()
              }
            }

          } else {
            # Tables already exist — load directly
            showNotification("Run finished. Loading project...", type = "message", duration = 4)
            do_load_sqm(proj_dir, tables_dir)
          }
        }
      }
    }
  })

  observeEvent(input$lnch_stop, {
    showModal(modalDialog(
      title  = "Confirm abort",
      "Are you sure you want to abort the running pipeline?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("lnch_confirm_abort", "Yes, abort", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$lnch_confirm_abort, {
    removeModal()
    p <- lnch_proc()
    if (!is.null(p) && p$is_alive()) {
      p$kill_tree()
      lnch_log_buffer(paste(lnch_log_buffer(), "\n--- ABORTED BY USER ---\n", sep = "\n"))
      lnch_status("Aborted")
      showNotification("Pipeline aborted", type = "warning")
      lnch_proc(NULL)
    }
  })

  observeEvent(input$lnch_no_bins, {
    if (input$lnch_no_bins) {
      updateCheckboxInput(session, "lnch_only_bins", value = FALSE)
      updateCheckboxGroupInput(session, "lnch_binners", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "lnch_binners", selected = c("concoct", "metabat2"))
    }
  })

  observeEvent(input$lnch_only_bins, {
    if (input$lnch_only_bins) updateCheckboxInput(session, "lnch_no_bins", value = FALSE)
  })

  output$lnch_log <- renderUI({
    tags$pre(style = "margin:0; white-space:pre-wrap;", lnch_log_buffer())
  })

}
