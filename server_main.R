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

