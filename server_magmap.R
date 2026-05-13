  # ===========================================================================
  # MAG MAP TAB — metabolic completeness diagram with interactive overlays
  # ===========================================================================

  # ReactiveVals for the per-pathway pathview view inside MAG Map
  magmap_pw_status  <- reactiveVal("idle")   # idle | generating | ready | error
  magmap_pw_pid     <- reactiveVal(NULL)     # pathway ID being shown
  magmap_pw_name    <- reactiveVal(NULL)     # pathway display name
  magmap_pw_img     <- reactiveVal(NULL)     # path to rendered PNG
  magmap_pw_nodes   <- reactiveVal(NULL)     # node df for hover/overlay
  magmap_view_mode  <- reactiveVal("map")    # "map" | "pathway"

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
      rect  = c(0.1190, 0.1478, 0.2584, 0.2259),
      paths = c("Glycolysis / Gluconeogenesis")
    ),
    "Pentose Phosphate\nPathway" = list(
      rect  = c(0.1190, 0.2440, 0.2584, 0.3290),
      paths = c("Pentose phosphate pathway",
                "Pentose and glucuronate interconversions")
    ),
    "Entner-Doudoroff\nPathway" = list(
      rect  = c(0.1190, 0.3457, 0.2584, 0.4310),
      paths = c("Glycolysis / Gluconeogenesis",
                "Pentose phosphate pathway")
    ),
    "TCA Cycle" = list(
      rect  = c(0.2950, 0.2189, 0.3829, 0.3639),
      paths = c("Citrate cycle (TCA cycle)",
                "Glyoxylate and dicarboxylate metabolism")
    ),
    "CO2 Fixation" = list(
      rect  = c(0.3968, 0.1506, 0.5139, 0.2286),
      paths = c("Carbon fixation by Calvin cycle",
                "Other carbon fixation pathways")
    ),
    "Fermentation" = list(
      rect  = c(0.3968, 0.3471, 0.5149, 0.4294),
      paths = c("Pyruvate metabolism",
                "Propanoate metabolism",
                "Butanoate metabolism",
                "C5-Branched dibasic acid metabolism")
    ),

    # N, S and CH4 Metabolism
    "Nitrogen\nFixation" = list(
      rect  = c(0.5511, 0.1520, 0.6980, 0.2300),
      paths = c("Nitrogen metabolism")
    ),
    "Assimilatory N" = list(
      rect  = c(0.7333, 0.1520, 0.8625, 0.2300),
      paths = c("Nitrogen metabolism")
    ),
    "Denitrification" = list(
      rect  = c(0.5500, 0.2496, 0.6980, 0.3262),
      paths = c("Nitrogen metabolism")
    ),
    "Sulfur Cycle" = list(
      rect  = c(0.7342, 0.2496, 0.8625, 0.3262),
      paths = c("Sulfur metabolism")
    ),
    "Nitrification" = list(
      rect  = c(0.5493, 0.3457, 0.6980, 0.4255),
      paths = c("Nitrogen metabolism")
    ),
    "Methane\nMetabolism" = list(
      rect  = c(0.7342, 0.3471, 0.8699, 0.4255),
      paths = c("Methane metabolism")
    ),

    # Biosynthesis / Anabolism
    "Amino Acids" = list(
      rect  = c(0.1301, 0.5115, 0.2444, 0.5897),
      paths = c("Alanine, aspartate and glutamate metabolism",
                "Glycine, serine and threonine metabolism",
                "Cysteine and methionine metabolism",
                "Valine, leucine and isoleucine biosynthesis",
                "Lysine biosynthesis",
                "Arginine biosynthesis",
                "Histidine metabolism",
                "Phenylalanine, tyrosine and tryptophan biosynthesis")
    ),
    "Nucleotides" = list(
      rect  = c(0.2704, 0.5115, 0.3950, 0.5897),
      paths = c("Purine metabolism",
                "Pyrimidine metabolism")
    ),
    "Vitamins /\nCofactors" = list(
      rect  = c(0.4165, 0.5563, 0.4926, 0.6483),
      paths = c("Thiamine metabolism",
                "Riboflavin metabolism",
                "Vitamin B6 metabolism",
                "Nicotinate and nicotinamide metabolism",
                "Pantothenate and CoA biosynthesis",
                "Biotin metabolism",
                "Lipoic acid metabolism",
                "Folate biosynthesis",
                "Porphyrin metabolism",
                "Ubiquinone and other terpenoid-quinone biosynthesis")
    ),
    "Fatty Acids" = list(
      rect  = c(0.1301, 0.6120, 0.2444, 0.6901),
      paths = c("Fatty acid biosynthesis",
                "Fatty acid degradation",
                "Glycerophospholipid metabolism")
    ),
    "Cell Wall" = list(
      rect  = c(0.2704, 0.6120, 0.3950, 0.6901),
      paths = c("Peptidoglycan biosynthesis",
                "Lipopolysaccharide biosynthesis",
                "Teichoic acid biosynthesis",
                "Amino sugar and nucleotide sugar metabolism")
    ),

    # Respiration / Energy
    # ETC, ATP Synthase and Oxidative Phosphorylation all map to KEGG 00190.
    "ETC" = list(
      rect  = c(0.5288, 0.5019, 0.6952, 0.5780),
      paths = c("Oxidative phosphorylation")
    ),
    "ATP Synthase" = list(
      rect  = c(0.7296, 0.5019, 0.8625, 0.5780),
      paths = c("Oxidative phosphorylation")
    ),
    "Oxidative\nPhosphorylation" = list(
      rect  = c(0.5288, 0.5995, 0.6952, 0.6761),
      paths = c("Oxidative phosphorylation")
    ),
    "Anaerobic\nRespiration" = list(
      rect  = c(0.7296, 0.5981, 0.8625, 0.6761),
      paths = c("Nitrogen metabolism",
                "Sulfur metabolism",
                "Oxidative phosphorylation")
    ),
    "Photosynthesis" = list(
      rect  = c(0.5288, 0.6943, 0.8625, 0.7473),
      paths = c("Photosynthesis",
                "Photosynthesis - antenna proteins")
    ),

    # Transporters / Systems
    "ABC\nTransporters" = list(
      rect  = c(0.1425, 0.8058, 0.2565, 0.8705),
      paths = c("ABC transporters")
    ),
    "Sec / Tat\nSystems" = list(
      rect  = c(0.2723, 0.8072, 0.3783, 0.8705),
      paths = c("Protein export",
                "Bacterial secretion system")
    ),
    "Efflux\nPumps" = list(
      rect  = c(0.3935, 0.8072, 0.4963, 0.8705),
      paths = c("ABC transporters",
                "Phosphotransferase system (PTS)",
                "Two-component system")
    ),
    "Motility" = list(
      rect  = c(0.5112, 0.8058, 0.6013, 0.8705),
      paths = c("Flagellar assembly",
                "Bacterial chemotaxis")
    ),
    "CRISPR" = list(
      rect  = c(0.6171, 0.8072, 0.7156, 0.8705),
      paths = c("DNA replication",
                "Homologous recombination")
    ),
    "Stress\nResponse" = list(
      rect  = c(0.7296, 0.8072, 0.8476, 0.8705),
      paths = c("Mismatch repair",
                "Two-component system")
    )
  )

  # Build KO lists for each category from KEGG_CATEGORIES (the master KEGG
  # pathway database loaded in global.R). This guarantees every KO listed in
  # a category actually belongs to one of the L3 pathways assigned to it.
  if (exists("KEGG_CATEGORIES") && !is.null(KEGG_CATEGORIES)) {
    for (cn in names(MAG_MAP_CATEGORIES)) {
      paths <- MAG_MAP_CATEGORIES[[cn]]$paths
      kos <- unique(KEGG_CATEGORIES$id[
        !is.na(KEGG_CATEGORIES$l3) &
        KEGG_CATEGORIES$l3 %in% paths
      ])
      MAG_MAP_CATEGORIES[[cn]]$kos <- kos
    }
  } else {
    # Fallback: if KEGG_CATEGORIES is unavailable, leave kos empty
    for (cn in names(MAG_MAP_CATEGORIES))
      MAG_MAP_CATEGORIES[[cn]]$kos <- character(0)
  }
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

    # ── Pull MAG stats from proj$bins$table (loaded by SqueezeMeta from bintable) ──
    bt  <- tryCatch(proj$bins$table, error = function(e) NULL)
    row <- NULL
    if (!is.null(bt) && bin %in% rownames(bt)) row <- bt[bin, , drop = FALSE]

    stat_row <- function(label, value, unit = "") {
      if (is.null(value) || length(value) == 0 ||
          (is.numeric(value) && is.na(value)) ||
          (is.character(value) && !nzchar(trimws(value))))
        return(NULL)
      val_txt <- if (is.numeric(value)) {
        if (value >= 1e6)      sprintf("%.2f Mb", value / 1e6)
        else if (value >= 1e3) sprintf("%.1f kb", value / 1e3)
        else                   format(value, big.mark = ",", scientific = FALSE)
      } else as.character(value)
      tags$div(style = "display:flex; justify-content:space-between; gap:8px; font-size:0.78rem; padding:2px 0;",
        tags$span(style = "color:var(--muted);", label),
        tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                  paste0(val_txt, if (nzchar(unit)) paste0(" ", unit) else ""))
      )
    }

    get_val <- function(row, col_pattern, exact = FALSE) {
      if (is.null(row)) return(NULL)
      cn  <- colnames(row)
      idx <- if (exact) match(col_pattern, cn) else grep(col_pattern, cn, ignore.case = TRUE)[1]
      if (is.na(idx) || length(idx) == 0) return(NULL)
      v <- row[[idx]]
      if (is.factor(v)) as.character(v) else v
    }

    completeness  <- get_val(row, "^Completeness$")
    contamination <- get_val(row, "^Contamination$")
    size_bp       <- get_val(row, "^(Length|Size)$|^Length \\(bp\\)$")
    num_contigs   <- get_val(row, "^Num.*contigs?$|^Number of contigs$|^Contigs$")
    taxonomy_raw  <- get_val(row, "^Tax$")

    # ── Coverage: columnas "Coverage <sample>" de proj$bins$table ────────────
    cov_block <- NULL
    if (!is.null(row)) {
      cov_cols <- grep("^Coverage ", colnames(row), value = TRUE)
      if (length(cov_cols) > 0) {
        cov_rows <- lapply(cov_cols, function(cc) {
          sample_name <- sub("^Coverage ", "", cc)
          val <- suppressWarnings(as.numeric(row[[cc]]))
          if (is.na(val)) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); word-break:break-all;", sample_name),
            tags$span(style = "font-weight:600;", sprintf("%.2f\u00d7", val))
          )
        })
        cov_rows <- Filter(Negate(is.null), cov_rows)
        if (length(cov_rows) > 0)
          cov_block <- tags$div(
            style = "margin-top:8px;",
            tags$div(
              style = "font-size:0.72rem; color:var(--muted); text-transform:uppercase; letter-spacing:0.04em; margin-bottom:2px;",
              "Coverage per sample"
            ),
            do.call(tags$div, cov_rows)
          )
      }
    }

    # ── Taxonomy: parsear string separado por ";" en niveles independientes ───
    # Soporta "k__Bacteria;p__Proteobacteria;..." o "Bacteria;Proteobacteria;..."
    # Soporta prefijos con _ simple (k_) o doble (k__), y "rank:value" (Tax 16S)
    RANK_LABELS <- c(
      k = "Kingdom", d = "Domain", p = "Phylum", c = "Class",
      o = "Order",   f = "Family", g = "Genus",  s = "Species"
    )
    PLAIN_RANKS <- c("Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

    taxonomy_block <- NULL
    if (!is.null(taxonomy_raw) && nzchar(trimws(as.character(taxonomy_raw)))) {
      tax_str    <- trimws(as.character(taxonomy_raw))
      tax_levels <- trimws(unlist(strsplit(tax_str, ";")))
      tax_levels <- tax_levels[nzchar(tax_levels)]

      # Detectar formato: "k_X" / "k__X",  "rank:value", o sin prefijo
      has_underscore <- any(grepl("^[a-z]_+", tax_levels))
      has_colon      <- any(grepl("^[a-z]+:", tax_levels))

      tax_rows <- if (has_underscore) {
        lapply(tax_levels, function(lvl) {
          # Captura la letra de rango y el valor: "k_Bacteria" o "k__Bacteria"
          m <- regmatches(lvl, regexec("^([a-z])_+(.+)$", lvl))[[1]]
          if (length(m) < 3) return(NULL)
          letter <- m[2]; value <- trimws(m[3])
          label  <- RANK_LABELS[[letter]]
          if (is.null(label)) label <- toupper(letter)
          if (!nzchar(value) || value %in% c("NA", "Unknown", "unclassified")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                      if (letter == "s") tags$em(value) else value)
          )
        })
      } else if (has_colon) {
        # Formato "rank:value" del Tax 16S
        lapply(tax_levels, function(lvl) {
          m <- regmatches(lvl, regexec("^([^:]+):(.+)$", lvl))[[1]]
          if (length(m) < 3) return(NULL)
          label <- trimws(m[2]); value <- trimws(m[3])
          # Capitalizar label
          label <- paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
          if (!nzchar(value) || value %in% c("NA", "Unknown", "unclassified")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;", value)
          )
        })
      } else {
        # Sin prefijos: asignar rangos en orden
        mapply(function(value, label) {
          if (!nzchar(trimws(value)) || value %in% c("NA", "Unknown")) return(NULL)
          tags$div(
            style = "display:flex; justify-content:space-between; gap:8px; font-size:0.76rem; padding:1px 0;",
            tags$span(style = "color:var(--muted); flex-shrink:0;", label),
            tags$span(style = "font-weight:600; text-align:right; word-break:break-word;",
                      if (label == "Species") tags$em(value) else value)
          )
        }, tax_levels, PLAIN_RANKS[seq_along(tax_levels)], SIMPLIFY = FALSE)
      }
      tax_rows <- Filter(Negate(is.null), tax_rows)

      if (length(tax_rows) > 0)
        taxonomy_block <- tags$div(
          style = "margin-top:8px;",
          tags$div(
            style = "font-size:0.72rem; color:var(--muted); text-transform:uppercase; letter-spacing:0.04em; margin-bottom:2px;",
            "Taxonomy"
          ),
          do.call(tags$div, tax_rows)
        )
    }

    tags$div(
      tags$div(class = "form-label", "Selected MAG"),
      tags$div(style = "font-size:0.82rem; word-break:break-all; font-weight:600;", bin),
      tags$div(style = "font-size:0.74rem; color:var(--muted); margin-top:2px; margin-bottom:8px;",
        paste0(length(mag_kos), " unique KEGG KOs")),

      stat_row("Completeness",  if (!is.null(completeness))  sprintf("%.1f%%", as.numeric(completeness))),
      stat_row("Contamination", if (!is.null(contamination)) sprintf("%.1f%%", as.numeric(contamination))),
      stat_row("Size",          if (!is.null(size_bp))       as.numeric(size_bp)),
      stat_row("Contigs",       if (!is.null(num_contigs))   as.numeric(num_contigs)),

      taxonomy_block,
      cov_block
    )
  })

  # Main view: BacMet image + SVG overlay (coloured rects + % labels)
  # OR pathview pathway diagram when magmap_view_mode() == "pathway"
  output$magmap_view_ui <- renderUI({

    # ── PATHWAY MODE ────────────────────────────────────────────────────────────
    if (magmap_view_mode() == "pathway") {
      s <- magmap_pw_status()
      back_btn <- actionButton("magmap_back", "\u2190 Back",
                               class = "btn-default",
                               style = "margin-bottom:10px; font-size:0.82rem;")
      if (s == "generating") {
        return(tagList(back_btn,
          tags$div(style = "color:var(--muted); font-size:0.85rem; padding:2rem; text-align:center;",
            tags$div(style = "font-size:1.5rem; margin-bottom:8px;", "\u25cc"),
            tags$div("Generating pathway map for ",
                     tags$strong(magmap_pw_name() %||% magmap_pw_pid()), "\u2026"),
            tags$div(style = "margin-top:6px; font-size:0.78rem;",
              "Green = present in MAG, grey = absent"))))
      }
      if (s == "error") {
        return(tagList(back_btn,
          tags$div(style = "color:#c0392b; font-size:0.85rem; padding:2rem; text-align:center;",
            tags$div(style = "font-size:1.5rem;", "\u2715"),
            tags$div("Pathway generation failed."))))
      }
      if (s == "ready") {
        img_path  <- magmap_pw_img();  req(img_path, file.exists(img_path))
        nodes     <- magmap_pw_nodes()
        res_name  <- paste0("magmap_pw_", magmap_pw_pid())
        addResourcePath(res_name, dirname(img_path))
        img_src   <- paste0(res_name, "/", basename(img_path))
        bin       <- magmap_selected_bin() %||% ""
        mag_kos   <- tryCatch(get_bin_kos(sqm_data(), bin), error = function(e) character(0))
        kegg_names <- tryCatch(sqm_data()$misc$KEGG_names, error = function(e) NULL)

        # Tooltip CSS + div (reuse pw- id so same JS works)
        tooltip_css <- tags$style(HTML("
          #pw-tooltip {
            position:fixed; pointer-events:none; z-index:9999;
            background:rgba(20,30,50,0.92); color:#f0f4f8;
            padding:5px 9px; border-radius:5px; font-size:0.75rem;
            max-width:320px; line-height:1.4; display:none;
            box-shadow:0 2px 8px rgba(0,0,0,0.3); white-space:pre-wrap; word-break:break-word;
          }
        "))
        tooltip_div <- tags$div(id = "pw-tooltip")
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
            if (tip) { tip.textContent = el.getAttribute('data-tip'); tip.style.display='block'; }
          }
          function pwHideTip() {
            var tip = document.getElementById('pw-tooltip');
            if (tip) tip.style.display='none';
          }
        "))

        # Build node JSON for hover overlay + coloring
        kgml_w <- attr(nodes, "kgml_w") %||% 1200
        kgml_h <- attr(nodes, "kgml_h") %||% 900
        node_json <- if (!is.null(nodes) && nrow(nodes) > 0) {
          node_list <- lapply(seq_len(nrow(nodes)), function(i) {
            r      <- nodes[i, ]
            ko_ids <- unique(sub("^ko:", "", trimws(unlist(strsplit(r$ko_names, "[[:space:]]+")))))
            ko_ids <- ko_ids[grepl("^K[0-9]{5}$", ko_ids)]
            nms    <- if (!is.null(kegg_names)) unique(na.omit(kegg_names[ko_ids])) else character(0)
            ko_str   <- paste(ko_ids, collapse = ", ")
            name_str <- if (length(nms) > 0) paste(nms, collapse = " / ") else r$label
            present  <- as.integer(any(ko_ids %in% mag_kos))
            tip <- paste0(ko_str, "\n", name_str, "\n\u2014 ", if (present == 1L) "PRESENT" else "absent")
            list(x = r$x, y = r$y, w = r$w, h = r$h, tip = tip, present = present)
          })
          jsonlite::toJSON(node_list, auto_unbox = TRUE)
        } else "[]"

        map_id <- "magmap_pwmap"
        img_tag <- tags$div(
          style = "position:relative; display:inline-block; width:100%;",
          tags$img(src = img_src, id = map_id,
            style = "max-width:100%; display:block; border:1px solid var(--border); border-radius:6px;",
            alt = "KEGG pathway"),
          tags$canvas(id = paste0(map_id, "_canvas"),
            style = "position:absolute; top:0; left:0; width:100%; height:100%;")
        )
        overlay_js <- tags$script(HTML(sprintf('
          (function() {
            var nodes  = %s;
            var KGML_W = %s;
            var KGML_H = %s;
            var img    = document.getElementById("%s");
            var canvas = document.getElementById("%s_canvas");
            var ctx    = canvas.getContext("2d");
            function setup() {
              canvas.width  = img.offsetWidth;
              canvas.height = img.offsetHeight;
              var scaleX = img.offsetWidth  / KGML_W;
              var scaleY = img.offsetHeight / KGML_H;
              ctx.clearRect(0, 0, canvas.width, canvas.height);
              for (var i = 0; i < nodes.length; i++) {
                var n = nodes[i];
                var x = (n.x - n.w / 2) * scaleX;
                var y = (n.y - n.h / 2) * scaleY;
                var w = n.w * scaleX;
                var h = n.h * scaleY;
                ctx.fillStyle   = n.present === 1 ? "rgba(26,122,58,0.72)" : "rgba(180,180,180,0.65)";
                ctx.fillRect(x, y, w, h);
                ctx.strokeStyle = n.present === 1 ? "#0f5c2a" : "#999999";
                ctx.lineWidth   = 1;
                ctx.strokeRect(x, y, w, h);
              }
              canvas.addEventListener("mousemove", function(e) {
                var rect = canvas.getBoundingClientRect();
                var mx   = e.clientX - rect.left;
                var my   = e.clientY - rect.top;
                var scX  = img.offsetWidth  / KGML_W;
                var scY  = img.offsetHeight / KGML_H;
                var hit  = null;
                for (var i = 0; i < nodes.length; i++) {
                  var n = nodes[i];
                  if (mx >= (n.x - n.w/2)*scX && mx <= (n.x + n.w/2)*scX &&
                      my >= (n.y - n.h/2)*scY && my <= (n.y + n.h/2)*scY) { hit = n; break; }
                }
                canvas.style.cursor = hit ? "crosshair" : "default";
                var tip = document.getElementById("pw-tooltip");
                if (hit) { tip.textContent = hit.tip; tip.style.display = "block"; }
                else      { tip.style.display = "none"; }
              });
              canvas.addEventListener("mouseleave", function() {
                var tip = document.getElementById("pw-tooltip");
                if (tip) tip.style.display = "none";
              });
            }
            if (img.complete && img.naturalWidth > 0) {
              requestAnimationFrame(function() { requestAnimationFrame(setup); });
            } else {
              img.addEventListener("load", function() {
                requestAnimationFrame(function() { requestAnimationFrame(setup); });
              });
            }
            window.addEventListener("resize", setup);
            // ResizeObserver catches Shiny panel layout settling after load
            if (window.ResizeObserver) {
              var ro = new ResizeObserver(function() { setup(); });
              ro.observe(img);
            }
          })();
        ', node_json, kgml_w, kgml_h, map_id, map_id)))

        return(tagList(
          back_btn,
          tags$div(style = "font-size:0.78rem; color:var(--muted); margin-bottom:6px;",
            tags$span(style = "display:inline-block; width:12px; height:12px; background:#1a7a3a; border-radius:2px; margin-right:4px; vertical-align:middle;"),
            "Present in MAG  ",
            tags$span(style = "display:inline-block; width:12px; height:12px; background:#e8e8e8; border:1px solid #ccc; border-radius:2px; margin-right:4px; margin-left:10px; vertical-align:middle;"),
            "Absent"
          ),
          tooltip_css, tooltip_div, tooltip_js,
          img_tag, overlay_js
        ))
      }
      return(tagList(back_btn))
    }

    # ── MAP MODE (default) ──────────────────────────────────────────────────────
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

        fill_col <- "220,40,40"   # red overlay for all bars

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

        # Label position: default = above top-left corner of the box
        # Exceptions:
        #   Motility, ATP Synthase  → top-right (avoid overlap with other legends)
        #   TCA Cycle               → top-center
        label_pos <- "TL"
        if (cat_name %in% c("Motility", "ATP Synthase")) label_pos <- "TR"
        if (cat_name == "TCA Cycle")                    label_pos <- "TC"

        if (label_pos == "TR") {
          lx <- x0 + w
          ty <- y0 - 0.3
          anchor <- "end"
        } else if (label_pos == "TC") {
          lx <- cx
          ty <- y0 - 0.3
          anchor <- "middle"
        } else {
          lx <- x0
          ty <- y0 - 0.3
          anchor <- "start"
        }

        paste0(
          # Clip path so progress bar respects rounded corners
          if (fill_w > 0) sprintf(
            '<clipPath id="%s"><rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" rx="0.8" ry="0.8"/></clipPath>\n',
            cp_id, x0, y0, w, h) else "",

          # Progress bar fill
          if (fill_w > 0) sprintf(
            '<rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" fill="rgba(%s,0.45)" clip-path="url(#%s)" data-cat="%s" data-tip="%s"/>\n',
            x0, y0, fill_w, h, fill_col, cp_id, htmltools::htmlEscape(cat_name, attribute=TRUE), tip) else "",

          # Box outline removed (no stroke)
          sprintf('<rect class="magmap-cat" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f" width="%.3f" height="%.3f"', x0, y0, w, h),
          ' fill="transparent"'  ,
          ' stroke="none"',
          ' rx="0.8" ry="0.8"',
          sprintf(' data-tip="%s"/>\n', tip),

          # % label: black, bold, positioned outside the top edge of the box
          sprintf('<text class="magmap-label" data-cat="%s"', htmltools::htmlEscape(cat_name, attribute=TRUE)),
          sprintf(' x="%.3f" y="%.3f"', lx, ty),
          sprintf(' dominant-baseline="auto" text-anchor="%s"', anchor),
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
  // Clean up any leftover calibration UI from earlier sessions
  var oldCoord = document.getElementById('magmap-coord-tip');
  if (oldCoord) oldCoord.remove();
  var oldPanel = document.getElementById('magmap-calib-panel');
  if (oldPanel) oldPanel.remove();

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
    # Source 0: KEGG_NAMES global (master KEGG database, loaded once in global.R)
    # Source 1: proj$functions$KEGG$names
    # Source 2: proj$misc$KEGG_names
    # Source 3: orfs table KEGGFUN

    ko_names_df <- tryCatch({
      df <- proj$functions$KEGG$names
      if (is.data.frame(df) && nrow(df) > 0) df else NULL
    }, error = function(e) NULL)

    misc_names <- tryCatch(proj$misc$KEGG_names, error = function(e) NULL)

    # Build KO -> name dictionary from ORF table as a fallback
    orf_ko_names <- tryCatch({
      ot <- proj$orfs$table
      if (!is.null(ot) && all(c("KEGG ID", "KEGGFUN") %in% colnames(ot))) {
        kos_raw  <- as.character(ot[, "KEGG ID"])
        funs_raw <- as.character(ot[, "KEGGFUN"])
        kos_clean <- sub("\\*+$", "", kos_raw)
        keep <- !is.na(kos_clean) & nzchar(kos_clean) &
                !is.na(funs_raw)  & nzchar(funs_raw)
        if (any(keep)) {
          tapply(funs_raw[keep], kos_clean[keep],
                 function(v) v[which(nzchar(v))[1]])
        } else NULL
      } else NULL
    }, error = function(e) NULL)

    # Helper: get name for a KO
    ko_name_fn <- function(ko) {
      if (exists("KEGG_NAMES") && !is.null(KEGG_NAMES) && ko %in% names(KEGG_NAMES)) {
        nm <- as.character(KEGG_NAMES[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df)) {
        nm <- as.character(ko_names_df[ko, "Name"])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(misc_names) && ko %in% names(misc_names)) {
        nm <- as.character(misc_names[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      if (!is.null(orf_ko_names) && ko %in% names(orf_ko_names)) {
        nm <- as.character(orf_ko_names[[ko]])
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }
      "(no annotation available)"
    }

    # Build KO -> KEGGPATH dictionary from ORF table as a fallback for L3
    orf_ko_paths <- tryCatch({
      ot <- proj$orfs$table
      if (!is.null(ot) && all(c("KEGG ID", "KEGGPATH") %in% colnames(ot))) {
        kos_raw   <- as.character(ot[, "KEGG ID"])
        paths_raw <- as.character(ot[, "KEGGPATH"])
        kos_clean <- sub("\\*+$", "", kos_raw)
        keep <- !is.na(kos_clean)   & nzchar(kos_clean) &
                !is.na(paths_raw)   & nzchar(paths_raw)
        if (any(keep)) {
          tapply(paths_raw[keep], kos_clean[keep],
                 function(v) v[which(nzchar(v))[1]])
        } else NULL
      } else NULL
    }, error = function(e) NULL)

    # Helper: get L3 pathways for a KO
    # Path column format: "L1; L2; L3 | L1; L2; L3 | ..."
    # Sources (in priority order):
    #   1. KEGG_CATEGORIES global (complete KEGG database, covers all KOs)
    #   2. proj$functions$KEGG$names (only KOs in project)
    #   3. proj$orfs$table KEGGPATH (only KOs in project)
    ko_l3_fn <- function(ko) {
      # Source 1: global KEGG_CATEGORIES (complete database)
      if (exists("KEGG_CATEGORIES") && !is.null(KEGG_CATEGORIES)) {
        kc_rows <- KEGG_CATEGORIES[KEGG_CATEGORIES$id == ko & !is.na(KEGG_CATEGORIES$l3), , drop = FALSE]
        if (nrow(kc_rows) > 0) {
          l3 <- unique(as.character(kc_rows$l3))
          l3 <- l3[!is.na(l3) & nzchar(l3)]
          if (length(l3) > 0) return(l3)
        }
      }
      # Source 2: project KEGG names table
      path_raw <- NA_character_
      if (!is.null(ko_names_df) && ko %in% rownames(ko_names_df))
        path_raw <- as.character(ko_names_df[ko, "Path"])
      # Source 3: ORF table KEGGPATH
      if ((is.na(path_raw) || !nzchar(path_raw)) &&
          !is.null(orf_ko_paths) && ko %in% names(orf_ko_paths))
        path_raw <- as.character(orf_ko_paths[[ko]])
      if (is.na(path_raw) || !nzchar(path_raw)) return(character(0))
      blocks <- strsplit(path_raw, " | ", fixed = TRUE)[[1]]
      l3 <- unique(vapply(blocks, function(b) {
        parts <- strsplit(trimws(b), "; ", fixed = TRUE)[[1]]
        if (length(parts) >= 3) trimws(parts[[3]]) else NA_character_
      }, character(1)))
      l3[!is.na(l3) & nzchar(l3)]
    }

    # Build per-KO rows — l3 filtered to only the paths of this category
    cat_paths <- MAG_MAP_CATEGORIES[[cat_key[1]]]$paths
    rows <- lapply(cat_kos, function(ko) {
      all_l3 <- ko_l3_fn(ko)
      # Only show pathways that belong to this category
      l3 <- all_l3[all_l3 %in% cat_paths]
      if (length(l3) == 0) l3 <- all_l3  # fallback: show all if none match (shouldn't happen)
      list(
        ko      = ko,
        name    = ko_name_fn(ko),
        l3      = l3,
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

    # Lookup pathway ID from name via KEGG_HIERARCHY
    find_pid <- function(path_name) {
      tryCatch({
        for (l1 in KEGG_HIERARCHY) for (l2 in l1) for (pw in l2)
          if (identical(pw$name, path_name)) return(pw$id)
        NULL
      }, error = function(e) NULL)
    }

    make_l3_header <- function(label) {
      pid_val <- find_pid(label)
      if (!is.null(pid_val)) {
        # Clickable: sends pid to Shiny via magmap_pw_clicked
        sprintf(
          '<tr style="background:#e8edf5; cursor:pointer;" onclick="Shiny.setInputValue(\'magmap_pw_clicked\',{pid:\'%s\',name:\'%s\',ts:Date.now()},{priority:\'event\'})">
            <td colspan="3" style="padding:5px 8px;font-weight:600;font-size:0.81rem;color:#1a5598;letter-spacing:0.01em;text-decoration:underline dotted;">%s &#x2197;</td>
          </tr>',
          htmltools::htmlEscape(pid_val, attribute = TRUE),
          htmltools::htmlEscape(label, attribute = TRUE),
          htmltools::htmlEscape(label))
      } else {
        sprintf(
          '<tr style="background:#e8edf5;"><td colspan="3" style="padding:5px 8px;font-weight:600;font-size:0.81rem;color:#1a3a6b;letter-spacing:0.01em;">%s</td></tr>',
          htmltools::htmlEscape(label))
      }
    }

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

  # ── Observer: pathway header clicked → render pathview for that MAG ──────────
  observeEvent(input$magmap_pw_clicked, {
    v <- input$magmap_pw_clicked
    req(!is.null(v), nzchar(v$pid))
    pid  <- v$pid
    name <- v$name
    bin  <- magmap_selected_bin(); req(bin)
    proj <- sqm_data(); req(proj)

    if (!requireNamespace("pathview", quietly = TRUE)) {
      showNotification("pathview not installed. Run: BiocManager::install(\"pathview\")",
                       type = "error", duration = 10)
      return()
    }

    magmap_pw_status("generating")
    magmap_pw_pid(pid)
    magmap_pw_name(name)
    magmap_pw_img(NULL)
    magmap_pw_nodes(NULL)
    magmap_view_mode("pathway")

    shinyjs::delay(50, tryCatch({
      mag_kos <- tryCatch(get_bin_kos(proj, bin), error = function(e) character(0))

      dir.create(pw_kegg_cache, showWarnings = FALSE, recursive = TRUE)

      # Download KEGG PNG and XML if not cached
      png_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".png"))
      xml_cached <- file.path(pw_kegg_cache, paste0("ko", pid, ".xml"))
      if (!file.exists(png_cached))
        tryCatch(pathview::download.kegg(pathway.id = pid, species = "ko",
                                         kegg.dir = pw_kegg_cache, file.type = "png"),
                 error = function(e) message("PNG download failed: ", e$message))
      if (!file.exists(xml_cached))
        .ensure_valid_xml(pid, pw_kegg_cache)

      if (!file.exists(png_cached)) {
        showNotification(paste("Could not download KEGG image for", pid),
                         type = "error", duration = 8)
        magmap_pw_status("error"); return()
      }

      # Parse KGML for node positions
      xml_nodes <- tryCatch({
        req(file.exists(xml_cached))
        doc <- xml2::read_xml(xml_cached)
        # KGML coordinates match the PNG pixel dimensions exactly
        kgml_w <- NA_real_; kgml_h <- NA_real_
        if (file.exists(png_cached) && requireNamespace("png", quietly = TRUE)) {
          d <- dim(png::readPNG(png_cached))  # [height, width, channels]
          kgml_w <- d[2]; kgml_h <- d[1]
        }
        if (is.na(kgml_w)) { kgml_w <- 1200; kgml_h <- 900 }
        entries <- xml2::xml_find_all(doc, ".//entry[@type='ortholog']")
        all_rows <- Filter(Negate(is.null), lapply(entries, function(e) {
          ko_names <- trimws(xml2::xml_attr(e, "name"))
          g <- xml2::xml_find_first(e, "graphics")
          if (is.na(xml2::xml_attr(g, "x"))) return(NULL)
          x <- as.numeric(xml2::xml_attr(g, "x"))
          y <- as.numeric(xml2::xml_attr(g, "y"))
          w <- as.numeric(xml2::xml_attr(g, "width"))
          h <- as.numeric(xml2::xml_attr(g, "height"))
          if (anyNA(c(x, y, w, h))) return(NULL)
          label <- xml2::xml_attr(g, "name")
          list(ko_names = ko_names, x = x, y = y, w = w, h = h, label = label)
        }))
        if (length(all_rows) == 0) return(NULL)
        df <- data.frame(
          ko_names = sapply(all_rows, `[[`, "ko_names"),
          x = sapply(all_rows, `[[`, "x"), y = sapply(all_rows, `[[`, "y"),
          w = sapply(all_rows, `[[`, "w"), h = sapply(all_rows, `[[`, "h"),
          label = sapply(all_rows, `[[`, "label"),
          stringsAsFactors = FALSE
        )
        df <- df[!duplicated(paste(round(df$x), round(df$y), sep = ",")), ]
        attr(df, "kgml_w") <- kgml_w
        attr(df, "kgml_h") <- kgml_h
        df
      }, error = function(e) { message("MAG map XML parse error: ", e$message); NULL })

      magmap_pw_nodes(xml_nodes)
      magmap_pw_img(png_cached)
      magmap_pw_status("ready")

    }, error = function(e) {
      message("MAG map pathview error: ", e$message)
      magmap_pw_status("error")
      showNotification(paste("Pathway error:", e$message), type = "error", duration = 10)
    }))
  })

  # ── Back button: return to map view ─────────────────────────────────────────
  observeEvent(input$magmap_back, {
    magmap_view_mode("map")
    magmap_pw_status("idle")
    magmap_pw_img(NULL)
    magmap_pw_nodes(NULL)
  })




