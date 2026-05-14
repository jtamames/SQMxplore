server <- function(input, output, session) {

  # ── Core: init, project loading, tables, plot/table selectors ──────────────
  source("server_main.R",          local = TRUE)

  # ── Plots tab: taxonomy, functions, COG, KEGG, Krona ───────────────────────
  source("server_plots.R",         local = TRUE)

  # ── Pathways tab: pathview / exportPathway ──────────────────────────────────
  source("server_pathways.R",      local = TRUE)

  # ── Multivariate tab: PCA ───────────────────────────────────────────────────
  source("server_multivariate.R",  local = TRUE)

  # ── Comparison tab: differential abundance ──────────────────────────────────
  source("server_comparison.R",    local = TRUE)

  # ── MAG Map tab ─────────────────────────────────────────────────────────────
  source("server_magmap.R",        local = TRUE)

  # ── Launcher tab: run SqueezeMeta ───────────────────────────────────────────
  source("server_launcher.R",      local = TRUE)

}
