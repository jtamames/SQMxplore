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
