#!/bin/bash
# Watermelon installer
# Installs all R dependencies inside the active conda environment
# Usage:
#   conda activate SqueezeMeta
#   bash install.sh

set -e

echo "============================================"
echo " Watermelon dependency installer"
echo "============================================"
echo ""

# ── Check we are inside a conda environment ────
if [ -z "$CONDA_PREFIX" ]; then
  echo "ERROR: No conda environment is active."
  echo "Please activate your SqueezeMeta environment first:"
  echo "  conda activate SqueezeMeta"
  exit 1
fi

echo "Active conda environment: $CONDA_PREFIX"
echo ""

# ── Check R is available ───────────────────────
if ! command -v Rscript &> /dev/null; then
  echo "ERROR: Rscript not found in the active conda environment."
  echo "Install R with: conda install -c conda-forge r-base"
  exit 1
fi

R_VERSION=$(Rscript -e 'cat(as.character(getRversion()))')
echo "R version: $R_VERSION"
echo ""

# ── 1. System libraries via conda/mamba (no sudo required) ────────────────
echo "[1/3] Installing system libraries..."
# Use mamba if available (much faster than conda), fall back to conda
if command -v mamba &> /dev/null; then
  CONDA_CMD="mamba"
  echo "  Using mamba for fast installation"
else
  CONDA_CMD="conda"
  echo "  mamba not found, using conda (this may take a while)"
fi
# All required libraries are available in conda-forge — no sudo needed.
# R will find them automatically inside the active conda environment.
$CONDA_CMD install -y -c conda-forge \
  cmake zip \
  libcurl openssl libxml2 \
  fontconfig harfbuzz fribidi \
  freetype libpng libtiff libjpeg-turbo \
  xz-tools 2>/dev/null \
  || echo "  (some packages may already be installed — continuing)"
echo "  ✓ System libraries done"
echo ""

# ── 2. R packages (CRAN) ───────────────────────
echo "[2/3] Installing R packages from CRAN..."
Rscript -e "
  repos <- 'https://cran.rstudio.com/'
  pkgs <- c('shiny', 'shinyjs', 'shinyFiles', 'bslib', 'DT', 'plotly',
            'SQMtools', 'vegan', 'ggplot2', 'htmlwidgets', 'xml2', 'processx', 'httr', 'jsonlite',
            'base64enc', 'png')
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat('  Installing:', paste(missing, collapse=', '), '\n')
    install.packages(missing, repos=repos)
    still_missing <- missing[!sapply(missing, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0)
      stop('Failed to install: ', paste(still_missing, collapse=', '))
  } else {
    cat('  All CRAN packages already installed\n')
  }
"
echo "  ✓ CRAN packages installed"
echo ""

# ── 3. R packages (Bioconductor) ──────────────
echo "[3/3] Installing Bioconductor packages (pathview, Biostrings, DESeq2, edgeR)..."
Rscript -e "
  if (!requireNamespace('BiocManager', quietly=TRUE))
    install.packages('BiocManager', repos='https://cran.rstudio.com/')
  BiocManager::install(ask = FALSE, update = FALSE)
  pkgs <- c('pathview', 'Biostrings', 'DESeq2', 'edgeR')
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat('  Installing:', paste(missing, collapse=', '), '\n')
    BiocManager::install(missing, ask = FALSE)
    still_missing <- missing[!sapply(missing, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0)
      warning('Failed to install: ', paste(still_missing, collapse=', '),
              ' — Pathways tab will not be available.')
  } else {
    cat('  Bioconductor packages already installed\n')
  }
"
echo "  ✓ Bioconductor packages installed"
echo ""

echo "============================================"
echo " Installation complete!"
echo ""
echo " To run Watermelon:"
echo "   conda activate SqueezeMeta"
echo "   cd /path/to/watermelon"
echo "   Rscript -e 'shiny::runApp(\"app.R\", launch.browser=FALSE)'"
echo "============================================"
