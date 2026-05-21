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
      updateSelectInput(session, "lnch_profile", selected = "Custom")
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

  output$lnch_card_title <- renderUI({
    buf <- lnch_log_buffer()
    if (is.null(buf) || !nzchar(trimws(buf))) {
      tags$span("Metagenomics pipeline")
    } else {
      tags$span("Execution log")
    }
  })

  output$lnch_log <- renderUI({
    buf <- lnch_log_buffer()
    if (is.null(buf) || !nzchar(trimws(buf))) {
      # Idle: show the metagenomics workflow schema as a friendly placeholder.
      return(tags$div(class = "launcher-log-placeholder",
        style = "padding: 12px 4px; text-align:center; width:100%; box-sizing:border-box;",
        tags$div(
          style = "color: var(--muted); font-size: 0.82rem; margin-bottom: 12px; line-height: 1.55; text-align:center;",
          "SqueezeMeta starts in step 1 (works on reads). ",
          "SQM_longreads starts in step 2 (treats (long)reads as contigs). ",
          "SQM_reads starts in step 3. ",
          "Only SqueezeMeta performs step 5."
        ),
        tags$img(
          src   = "schema.png",
          alt   = "Metagenomic analysis workflow",
          style = "display:block; max-width:100%; width:100%; height:auto; border-radius:4px; margin:0 auto;"
        )
      ))
    }
    tags$pre(class = "launcher-log-text", style = "margin:0; white-space:pre-wrap;", buf)
  })
