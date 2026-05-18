ui <- page_navbar(
  id = "main_navbar",
  title = tags$img(
    src = paste0("data:image/png;base64,", WATERMELON_LOGO_B64),
    height = "62px",
    style = "margin: 2px 0;"
  ),
  window_title = "Watermelon",
  theme = sqm_theme,
  navbar_options = navbar_options(theme = "light", bg = "#ffffff"),
  header = tagList(
    useShinyjs(),
    tags$head(tags$style(HTML(custom_css))),
    tags$head(tags$style(HTML("
      .navbar .nav-link { font-size: 1.05rem !important; }
    "))),
    tags$script(HTML("document.body.classList.add('sqm-no-project');")),
    tags$script(tooltip_init_js)
  ),
  # ── Run ─────────────────────────────────────────────────────────────────
  nav_panel(
    "Run",
    layout_sidebar(
      sidebar = sidebar(
        width = 310,

        # ---- Project Setup ----
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Project Setup"),
          textInput("lnch_project_name", "Project name", placeholder = "my_project"),
          selectInput("lnch_program", "Program",
            choices = c("SqueezeMeta" = "SqueezeMeta.pl",
                        "sqm_reads"   = "sqm_reads.pl",
                        "sqm_longreads" = "sqm_longreads.pl")),
          conditionalPanel(
            condition = "input.lnch_program == 'SqueezeMeta.pl'",
            selectInput("lnch_mode", "Execution mode",
              choices = c("coassembly","sequential","merged","seqmerge"))
          )
        ),

        # ---- Input Files ----
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Input Files"),
          tags$div(style = "display:flex; gap:4px;",
            shinyFilesButton("lnch_samples_file", "Samples file (-s)", "Choose file", multiple = FALSE),
            actionButton("lnch_create_samples", "Create", class = "btn-default btn-sm",
              title = "Create a new samples file by selecting FASTQ files from a directory")
          ),
          tags$div(class = "launcher-file-path", textOutput("lnch_samples_path")),
          shinyDirButton("lnch_input_dir", "Input directory (-f)", "Choose directory", multiple = FALSE),
          tags$div(class = "launcher-file-path", textOutput("lnch_input_path")),
          shinyDirButton("lnch_workdir", "Working directory", "Choose directory", multiple = FALSE),
          tags$div(class = "launcher-file-path", textOutput("lnch_workdir_path"))
        ),

        # ---- Profile (SqueezeMeta only) ----
        conditionalPanel(
          condition = "input.lnch_program == 'SqueezeMeta.pl'",
          tags$div(class = "sidebar-box",
            tags$div(class = "form-label", "Profile"),
            selectInput("lnch_profile", NULL,
              choices  = sapply(get_builtin_profiles(), function(x) x$name),
              selected = "Standard Metagenome")
          )
        ),

        # ---- Advanced ----
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Advanced"),
          accordion(open = FALSE, multiple = TRUE,

            accordion_panel("Filtering",
              checkboxInput("lnch_trimmomatic", "Run Trimmomatic", FALSE),
              conditionalPanel(condition = "input.lnch_trimmomatic == true",
                textInput("lnch_cleaning_params", "Parameters",
                  value = "LEADING:8 TRAILING:8 SLIDINGWINDOW:10:15 MINLEN:30"))
            ),

            conditionalPanel(condition = "input.lnch_program == 'SqueezeMeta.pl'",
              accordion_panel("Assembly",
                selectInput("lnch_assembler", "Assembler",
                  choices = c("megahit","spades","rnaspades","canu","flye")),
                textInput("lnch_assembly_opts", "Options", placeholder = ""),
                numericInput("lnch_min_contig", "Min contig length", 200, min = 0),
                checkboxInput("lnch_singletons", "Use singletons", FALSE)
              )
            ),

            accordion_panel("Annotation",
              checkboxInput("lnch_no_cog",  "No COG",  FALSE),
              checkboxInput("lnch_no_kegg", "No KEGG", FALSE),
              checkboxInput("lnch_no_pfam", "No PFAM", TRUE),
              checkboxInput("lnch_euk",     "Eukaryotes", FALSE),
              checkboxInput("lnch_dbl",     "Doublepass", FALSE),
              shinyFilesButton("lnch_extdb", "External DBs", "Select file", multiple = FALSE),
              tags$div(class = "launcher-file-path", textOutput("lnch_extdb_path"))
            ),

            conditionalPanel(condition = "input.lnch_program == 'SqueezeMeta.pl'",
              accordion_panel("Mapping",
                selectInput("lnch_mapper", "Mapper",
                  choices = c("bowtie","bwa","minimap2-ont","minimap2-pb","minimap2-sr")),
                textInput("lnch_mapping_opts", "Options", placeholder = "")
              )
            ),

            conditionalPanel(condition = "input.lnch_program == 'SqueezeMeta.pl'",
              accordion_panel("Binning",
                checkboxInput("lnch_no_bins", "No bins", FALSE),
                conditionalPanel(condition = "input.lnch_no_bins == false",
                  checkboxInput("lnch_only_bins", "Only bins", FALSE),
                  checkboxGroupInput("lnch_binners", "Binners",
                    choices  = c("Concoct"="concoct","Metabat2"="metabat2","MaxBin"="maxbin"),
                    selected = c("concoct","metabat2"))
                )
              )
            ),

            accordion_panel("Performance",
              numericInput("lnch_threads", "Threads", 8, min = 1)
            )
          )
        ),

        # ---- Run / Abort ----
        actionButton("lnch_run",  "\u25b6 Run",   class = "btn-primary w-100 mb-1"),
        actionButton("lnch_stop", "\u25a0 Abort",  class = "btn-danger  w-100")
      ),

      # ── Log panel ──────────────────────────────────────────────────────
      card(
        card_header(
          tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
            "Execution log",
            uiOutput("lnch_status_badge"))
        ),
        card_body(
          uiOutput("lnch_step_display"),
          tags$div(id = "launcher-log-container", uiOutput("lnch_log")),
          tags$script(HTML("
            // Auto-scroll log
            Shiny.addCustomMessageHandler('lnch_scroll_log', function(msg) {
              var el = document.getElementById('launcher-log-container');
              if (el) el.scrollTop = el.scrollHeight;
            });

            // Close the shinyFiles directory picker window after Select is clicked.
            // The button id is sF-selectButton, backdrop class is sF-modalBackdrop.
            $(document).on('click', '#sF-selectButton', function() {
              setTimeout(function() {
                $('.sF-modalContainer').hide().remove();
                $('.sF-modalBackdrop').remove();
              }, 100);
            });
          "))
        )
      )
    )
  ),

nav_panel("Load",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300, open = TRUE,
        tags$div(class = "sidebar-box",
          help_label("Load mode",
            "Load project: loads data from a SqueezeMeta project directory (expects tables in results/tables).\nLoad tables: loads data from an already-created tables directory.\nLoad multiple: loads and combines data from multiple project or tables directories (e.g. sequential mode runs)."),
          radioButtons("load_mode", NULL,
            choices  = c("Load project" = "project", "Load tables" = "tables",
                         "Load multiple" = "multiple"),
            selected = "project", inline = TRUE)),
        uiOutput("project_dir_ui"),
        uiOutput("project_info_ui"), uiOutput("manual_tables_ui"),
        uiOutput("multi_dirs_ui"),
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
          tags$div(class = "form-label", "Category"),
          uiOutput("plot_category_ui")
        ),
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Plot type"),
          uiOutput("plot_type_ui")
        ),
        uiOutput("plot_controls_ui"),
        tags$div(style = "margin-top:5px;",
        uiOutput("plot_sample_selector_ui")),
        uiOutput("plot_download_ui")
      ),
      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
          span("Visualization"), uiOutput("plot_status_badge"))),
        card_body(class = "p-2",
          # ── Format bar: taxonomy controls (hidden by default) ──
          tags$div(id = "fmt_tax",
            style = "display:none;",
            tags$div(
              style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; flex-wrap:wrap; padding:2px;",
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "W:"),
              numericInput("tax_plot_width",  NULL, value=1200, min=400, max=4000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "H:"),
              numericInput("tax_plot_height", NULL, value=560,  min=200, max=3000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Font:"),
              numericInput("tax_font_size",   NULL, value=11,   min=6,   max=24,   step=1,  width="65px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Palette:"),
              selectInput("tax_palette", NULL,
                choices  = c("Paired","Set2","Set3","Dark2","Tableau10","Alphabet","Polychrome36"),
                selected = "Paired", width="110px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Label width:"),
              numericInput("tax_label_width", NULL, value=30, min=5, max=100, step=5, width="65px")
            )
          ),
          # ── Format bar: function controls (hidden by default) ──
          tags$div(id = "fmt_func",
            style = "display:none;",
            tags$div(
              style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; flex-wrap:wrap; padding:2px;",
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "W:"),
              numericInput("func_plot_width",  NULL, value=1200, min=400, max=4000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "H:"),
              numericInput("func_plot_height", NULL, value=560,  min=200, max=3000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Font:"),
              numericInput("func_font_size",   NULL, value=11,   min=6,   max=24,   step=1,  width="65px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Palette:"),
              selectInput("func_palette", NULL,
                choices  = c("Blues","Viridis","YlOrRd","RdBu","Greens","Hot","Portland","Jet"),
                selected = "Blues", width="100px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Label width:"),
              numericInput("func_label_width", NULL, value=40, min=10, max=200, step=5, width="65px")
            )
          ),
          # ── Format bar: taxonomy heatmap controls (hidden by default) ──
          tags$div(id = "fmt_tax_hm",
            style = "display:none;",
            tags$div(
              style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; flex-wrap:wrap; padding:2px;",
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "W:"),
              numericInput("tax_hm_width",  NULL, value=1200, min=400, max=4000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "H:"),
              numericInput("tax_hm_height", NULL, value=560,  min=200, max=3000, step=50, width="80px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Font:"),
              numericInput("tax_hm_font",   NULL, value=11,   min=6,   max=24,   step=1,  width="65px"),
              tags$div(style="font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Palette:"),
              selectInput("tax_hm_palette", NULL,
                choices  = c("Blues","Viridis","YlOrRd","RdBu","Greens","Hot","Portland","Jet"),
                selected = "Blues", width="100px")
            )
          ),
          uiOutput("sqm_plot_ui")
        )
      )
    )
  ),
  nav_panel("Tables",
    layout_sidebar(
      sidebar = sidebar(width = 270,
        uiOutput("tbl_category_ui"),
        tags$hr(class = "section-divider"),
        uiOutput("tbl_sub_controls_ui"),
        uiOutput("table_sample_filter"),
        tags$div(style = "margin-top:5px;",
          downloadButton("download_table", "Download CSV", class = "btn-outline-secondary w-100"))
      ),
      card(card_header("Data"), card_body(class = "p-2",
        uiOutput("tbl_main_ui")))
    )
  ),
  nav_panel("Pathways",
    layout_sidebar(
      sidebar = sidebar(width = 270,
        uiOutput("pw_pathview_check_ui"),
        tags$hr(class = "section-divider"),
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "KEGG Pathway"),
          uiOutput("pw_pathway_select_ui"),
          help_label("Count type", "Type of abundance measurement used for the ordination analysis"),
          uiOutput("pw_count_ui")
        ),
        tags$div(class = "sidebar-box",
          tags$div(class = "form-label", "Mode"),
          radioButtons("pw_mode", NULL,
            choices = c("All samples together" = "together",
                        "One per sample"       = "split",
                        "Fold-change"          = "foldchange"),
            selected = "together"),
          uiOutput("pw_foldchange_ui")
        ),
        tags$hr(class = "section-divider"),
        uiOutput("pw_sample_selector_ui"),

        tags$div(style = "margin-top:5px;", uiOutput("pw_download_ui")),
        tags$div(style = "margin-top:10px;", uiOutput("pw_status_ui"))
      ),
      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
          span("KEGG Pathway Map"), uiOutput("pw_badge_ui"))),
        card_body(class = "p-2", uiOutput("pw_view_ui"))
      )
    )
  ),
  nav_panel("Multivariate",
    layout_sidebar(
      sidebar = sidebar(width = 270,
        uiOutput("mv_sidebar_controls"),
        tags$hr(class = "section-divider"),
        actionButton("do_pca", "Run analysis", class = "btn-primary w-100"),
        tags$script(HTML("
          (function() {
            // Inputs that, when changed, mark the analysis as stale
            var watchIds = [
              'mv_method','mv_data_type','mv_rank_db','mv_metric',
              'mv_norm','mv_dist','mv_n_features',
              'mv_exclude_unclassified','mv_exclude_ambiguous','mv_samples'
            ];
            var btn = document.getElementById('do_pca');
            function markStale() {
              if (!btn) btn = document.getElementById('do_pca');
              if (!btn) return;
              btn.classList.add('mv-stale');
            }
            function markFresh() {
              if (!btn) btn = document.getElementById('do_pca');
              if (!btn) return;
              btn.classList.remove('mv-stale');
            }
            // Watch Shiny input changes
            $(document).on('shiny:inputchanged', function(e) {
              if (watchIds.indexOf(e.name) !== -1) markStale();
              if (e.name === 'do_pca') markFresh();
            });
          })();
        ")),
        tags$div(style = "margin-top:5px;", uiOutput("mv_download_ui")),
        tags$div(style = "margin-top:10px;", uiOutput("mv_status_ui"))
      ),
      card(
        card_header(div(style = "display:flex; justify-content:space-between; align-items:center;",
          uiOutput("mv_card_title_ui"), uiOutput("mv_badge_ui"))),
        card_body(class = "p-2",
          tags$div(
            style = "display:flex; gap:8px; align-items:center; margin-bottom:6px; flex-wrap:wrap; padding:2px;",
            tags$div(style = "font-size:0.75rem; color:var(--muted); white-space:nowrap;", "W:"),
            numericInput("mv_plot_width",  NULL, value = 700, min = 300, max = 1600, step = 50, width = "75px"),
            tags$div(style = "font-size:0.75rem; color:var(--muted); white-space:nowrap;", "H:"),
            numericInput("mv_plot_height", NULL, value = 500, min = 200, max = 1200, step = 50, width = "75px"),
            tags$div(style = "font-size:0.75rem; color:var(--muted); white-space:nowrap;", "Font:"),
            numericInput("mv_font_size",   NULL, value = 11,  min = 6,   max = 24,   step = 1,  width = "65px"),
            uiOutput("mv_feat_labels_ui"),
            uiOutput("mv_ext_labels_ui"),
            uiOutput("mv_feat_style_ui")
          ),
          uiOutput("mv_plot_ui")
        )
      )
    )
  ),
  nav_panel("Comparison",
    layout_sidebar(
      sidebar = sidebar(width = 290,

        tags$div(class = "sidebar-box",
          help_label("Category", "Choose between taxonomy or functional annotation."),
          uiOutput("cmp_category_ui")
        ),

        tags$div(class = "sidebar-box",
          uiOutput("cmp_sub_ui")
        ),

        tags$div(class = "sidebar-box",
          help_label("Method",
            "Wilcoxon: non-parametric rank test, no distributional assumptions. Works with any metric.\nDESeq2: negative binomial GLM for count data. Always uses raw counts internally.\nedgeR: negative binomial GLM for count data. Always uses raw counts internally."),
          selectInput("cmp_method", NULL,
            choices  = c("Wilcoxon" = "wilcoxon", "DESeq2" = "deseq2", "edgeR" = "edger"),
            selected = "wilcoxon"),
          uiOutput("cmp_metric_ui")
        ),

        tags$div(class = "sidebar-box",
          help_label("Groups",
            "Select samples for each group. Checking a sample in one group automatically removes it from the other."),
          uiOutput("cmp_group_ui")
        ),

        tags$div(class = "sidebar-box",
          help_label("Filters", "FDR threshold and minimum absolute fold-change."),
          tags$div(style = "display:flex; gap:8px;",
            tags$div(style = "flex:1;",
              tags$div(class = "form-label", "FDR ≤"),
              numericInput("cmp_fdr", NULL, value = 0.05, min = 0, max = 1, step = 0.01, width = "100%")),
            tags$div(style = "flex:1;",
              tags$div(class = "form-label", "Min |log2FC|"),
              numericInput("cmp_lfc", NULL, value = 1, min = 0, step = 0.1, width = "100%"))
          )
        ,
          tags$hr(style = "margin:8px 0;"),
          checkboxInput("cmp_excl_unclassified", "Exclude Unmapped / Unclassified", value = TRUE),
          checkboxInput("cmp_excl_ambiguous",    "Exclude ambiguous taxa", value = FALSE)
        ),

        tags$hr(class = "section-divider"),
        actionButton("do_cmp", "Run comparison", class = "btn-primary w-100"),
        tags$div(style = "margin-top:8px;", uiOutput("cmp_status_ui")),
        tags$div(style = "margin-top:8px;", uiOutput("cmp_download_ui"))
      ),

      card(
        card_header(
          div(style = "display:flex; justify-content:space-between; align-items:center;",
            uiOutput("cmp_card_title_ui"),
            uiOutput("cmp_badge_ui"))
        ),
        card_body(class = "p-2",
          uiOutput("cmp_main_ui")
        )
      )
    )
  ),
  nav_panel("MAGs", value = "MAG Map",
    layout_sidebar(
      sidebar = sidebar(width = 290,
        tags$div(class = "sidebar-box",
          help_label("View",
            "Single MAG: explore one MAG's metabolic completeness on the diagram. Comparative table: presence/absence of category genes across all MAGs."),
          radioButtons("magmap_view_select", NULL,
            choices = c("Single MAG" = "single", "Comparative table" = "comparative"),
            selected = "single")
        ),
        conditionalPanel(
          condition = "input.magmap_view_select != 'comparative'",
          tags$div(class = "sidebar-box",
            help_label("MAG", "Select a MAG to visualise its metabolic completeness on the diagram. Requires a full SQM project with KEGG annotation and binning."),
            uiOutput("magmap_bin_select_ui")
          ),
          tags$div(class = "sidebar-box",
            help_label("KO list",
              "Extended: all KOs in the KEGG pathways assigned to each category. Central: curated set of diagnostic / canonical KOs for each pathway."),
            radioButtons("magmap_ko_mode", NULL,
              choices = c("Extended" = "extended", "Central" = "central"),
              selected = "extended", inline = TRUE)
          ),
          tags$hr(class = "section-divider"),
          uiOutput("magmap_selected_ui")
        ),
        conditionalPanel(
          condition = "input.magmap_view_select == 'comparative'",
          tags$div(class = "sidebar-box",
            help_label("Category",
              "Choose which metabolic category to compare across all MAGs."),
            uiOutput("magmap_comp_category_ui")
          ),
          tags$div(class = "sidebar-box",
            tags$div(style = "font-size:0.8rem; color:var(--muted);",
              "Presence/absence of the central (curated) KO set for the selected category, across all MAGs in the project.")
          )
        )
      ),
      tags$div(
        conditionalPanel(
          condition = "input.magmap_view_select != 'comparative'",
          card(
            card_header(
              help_label("Metabolic Map",
                "The diagram shows major metabolic categories. Blue bar overlay indicates the completeness of each category based on KEGG KOs present in the selected MAG. Hover for details, click a category to see KO detail below.")
            ),
            card_body(class = "p-2",
              uiOutput("magmap_view_ui")
            )
          ),
          uiOutput("magmap_detail_ui")
        ),
        conditionalPanel(
          condition = "input.magmap_view_select == 'comparative'",
          uiOutput("magmap_comparative_ui")
        )
      )
    )
  ),
  nav_panel("About",
    tags$div(style = "max-width:760px; margin: 2rem auto; padding: 0 1rem;",

      tags$div(class = "sqm-section",
        tags$div(class = "sqm-section-header", "Watermelon"),
        tags$div(class = "sqm-section-body",
          tags$p(
            tags$strong("Watermelon"),
            " is an interactive Shiny dashboard for running, visualising and exploring ",
            tags$a("SqueezeMeta", href = "https://github.com/jtamames/SqueezeMeta", target = "_blank"),
            " metagenomics results."
          ),
          tags$p(
            tags$span(style = "color:var(--muted);", "©"),
            " Javier Tamames, CNB-CSIC (Madrid, Spain) 2026."
          )
        )
      ),

      tags$div(class = "sqm-section",
        tags$div(class = "sqm-section-header", "Source code & support"),
        tags$div(class = "sqm-section-body",
          tags$p(
            "Source code is available on GitHub: ",
            tags$a("github.com/jtamames/Watermelon",
              href = "https://github.com/jtamames/Watermelon", target = "_blank")
          ),
          tags$p(
            "For bug reports and questions please open an issue at ",
            tags$a("github.com/jtamames/Watermelon/issues",
              href = "https://github.com/jtamames/Watermelon/issues", target = "_blank"),
            "."
          )
        )
      ),

      tags$div(class = "sqm-section",
        tags$div(class = "sqm-section-header", "Citation"),
        tags$div(class = "sqm-section-body",
          tags$p("If you use SqueezeMeta or SQMtools in your research, please cite:"),
          tags$ul(
            tags$li(
              "Tamames & Puente-Sánchez (2019). SqueezeMeta, a highly portable metagenomics pipeline based on simultaneous coassembly of multiple samples. ",
              tags$em("Frontiers in Microbiology."),
              " doi: ",
              tags$a("10.3389/fmicb.2018.03349",
                href = "https://doi.org/10.3389/fmicb.2018.03349", target = "_blank")
            ),
            tags$li(
              "Puente-Sánchez et al. (2020). SQMtools: automated processing and visual analysis of ‘omics data with R and anvi’o. ",
              tags$em("BMC Bioinformatics."),
              " doi: ",
              tags$a("10.1186/s12859-020-03703-2",
                href = "https://doi.org/10.1186/s12859-020-03703-2", target = "_blank")
            )
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(
    tags$img(
      src    = paste0("data:image/png;base64,", CSIC_LOGO_B64),
      height = "30px",
      style  = "margin: 2px 12px 2px 0; opacity:0.9;"
    )
  )
)
