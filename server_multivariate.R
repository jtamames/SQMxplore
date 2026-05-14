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
