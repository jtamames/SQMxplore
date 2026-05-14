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


