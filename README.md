# SQMxplore

Interactive Shiny dashboard for visualizing and exploring [SqueezeMeta](https://github.com/jtamames/SqueezeMeta) metagenomics results.

## Features

- **Project** — loads SQM (full) and SQMlite projects, displays a structured summary including reads, contigs, ORFs, taxonomy coverage and most abundant taxa
- **Plots** — interactive barplots for taxonomy (all ranks) and functions (COG, KEGG, PFAM, external DBs), with search, count type selection, and adjustable size/font; bins barplot
- **Tables** — browsable, downloadable tables for assembly (contigs, ORFs), taxonomy (all ranks), functions (COG, KEGG, PFAM, external DBs) and bins; multiple metrics (abund, percent, bases, cpm, tpm, copy_number); COG and KEGG tables include Name and Path annotation columns
- **Krona** — generates and displays interactive Krona taxonomy charts inline, with per-sample filtering and HTML download

---

## Requirements

### R (≥ 4.1)

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "shinyFiles",
  "bslib",
  "DT"
))
```

Install **SQMtools** from CRAN:

```r
install.packages("SQMtools")
```

Or install the latest development version from the SqueezeMeta repository:

```r
install.packages(
  "/path/to/SqueezeMeta/lib/SQMtools",
  repos = NULL,
  type  = "source"
)
```

### KronaTools (optional, required for Krona tab)

KronaTools provides the `ktImportText` binary used to generate Krona charts.

**conda (recommended):**
```bash
conda install -c bioconda krona
ktUpdateTaxonomy.sh   # update taxonomy database
```

**Manual install:**
```bash
git clone https://github.com/marbl/Krona.git
cd Krona/KronaTools
./install.pl
```

After installation, verify that `ktImportText` is available in your `PATH`:
```bash
ktImportText --help
```

The Krona tab in SQMxplore will display a status indicator showing whether KronaTools is available.

---

## Installation

Clone this repository:

```bash
git clone https://github.com/jtamames/SQMxplore.git
cd SQMxplore
```

---

## Usage

Launch the app from R:

```r
shiny::runApp("app.R")
```

Or from the command line:

```bash
Rscript -e 'shiny::runApp("app.R")'
```

### Loading a project

1. Go to the **Project** tab
2. Click **Select directory** and choose your SqueezeMeta project directory
3. The app will auto-detect the project type from `creator.txt`:
   - **SqueezeMeta projects** (created with `SqueezeMeta.pl`): point to the project root directory
   - **SQMlite projects** (created with `sqm2tables.py`, `sqmreads2tables.py` or `combine-sqm-tables.py`): point to the tables directory
4. Click **Load**

If the tables directory cannot be detected automatically, a manual directory selector will appear.

---

## Project types

| Type | Load function | Features |
|------|--------------|----------|
| SQM (full) | `loadSQM` | All tabs, all plots, taxonomy/function search, bins |
| SQMlite | `loadSQMlite` | Plots, Tables, Krona; no contig/ORF/bin detail; no subset functions |

---

## Tabs

### Project
Displays a structured summary of the loaded project: sample list, read counts, contig and ORF statistics, taxonomic classification coverage, most abundant taxa per rank.

### Plots
Select a plot type from the sidebar:
- **Taxonomy (barplot)** — choose rank, count type, number of taxa; optionally search for specific taxa (SQM full only)
- **COG / KEGG / PFAM functions** — heatmap of most abundant functions; optionally search by ID or keyword
- **Binning** — barplot of most abundant bins

All plots support adjustable width, height and font size. Changes apply immediately. Use **Download PNG** to export.

### Tables
Four independent category selectors:
- **Assembly** — Contigs table, ORFs table (SQM full only)
- **Taxonomy** — select rank and metric (percent, abund)
- **Functions** — select database and metric (abund, percent, bases, cpm, tpm, copy_number); COG and KEGG tables include Name and Path annotation columns
- **Bins** — bins summary table (SQM full only)

Taxonomy and function tables include a **Filter samples** checkbox to show a subset of samples. Use **Download CSV** to export the current view.

### Krona
Generates an interactive Krona taxonomy chart using `exportKrona` from SQMtools and `ktImportText` from KronaTools. Optionally filter by sample before generating. The chart is displayed inline and can be downloaded as a self-contained HTML file.

---

## Dependencies summary

| Package | Source | Required |
|---------|--------|----------|
| shiny | CRAN | Yes |
| shinyjs | CRAN | Yes |
| shinyFiles | CRAN | Yes |
| bslib | CRAN | Yes |
| DT | CRAN | Yes |
| SQMtools | CRAN / SqueezeMeta repo | Yes |
| KronaTools (`ktImportText`) | conda / GitHub | Krona tab only |

---

## Citation

If you use SqueezeMeta or SQMtools in your research, please cite:

- Tamames & Puente-Sánchez (2019). SqueezeMeta, a highly portable metagenomics pipeline based on simultaneous coassembly of multiple samples. *Frontiers in Microbiology*. doi:[10.3389/fmicb.2018.03349](https://doi.org/10.3389/fmicb.2018.03349)
- Puente-Sánchez et al. (2020). SQMtools: automated processing and visual analysis of 'omics data with R and anvi'o. *BMC Bioinformatics*. doi:[10.1186/s12859-020-03703-2](https://doi.org/10.1186/s12859-020-03703-2)

---

## License

MIT
