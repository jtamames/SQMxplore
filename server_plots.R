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
