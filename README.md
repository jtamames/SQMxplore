# Watermelon

Interactive Shiny dashboard for running, visualising and exploring [SqueezeMeta](https://github.com/jtamames/SqueezeMeta) metagenomics results.


## Features

- **Run** — launch SqueezeMeta, sqm_reads or sqm_longreads pipelines directly from the browser, monitor progress in real time, and automatically load results when the run finishes
- **Load** — loads SQM (full) and SQMlite projects (single or multiple combined with `combineSQM`), displays a structured summary
- **Plots** — interactive barplots and heatmaps for taxonomy (all ranks) and functions (COG, KEGG, PFAM, external DBs), with search, count type selection, rescaling options, and adjustable size/font; bins barplot
- **Tables** — browsable, downloadable tables for assembly (contigs, ORFs), taxonomy (all ranks), functions (COG, KEGG, PFAM, external DBs) and bins
- **Krona** — generates and displays interactive Krona taxonomy charts inline
- **Pathways** — overlays functional abundance data onto KEGG pathway maps
- **Multivariate** — ordination analysis (PCA and NMDS)
- **Comparison** — differential abundance analysis between sample groups using Wilcoxon, DESeq2 or edgeR, with volcano plots and results tables
<img width="1367" height="1134" alt="Figure1orig" src="https://github.com/user-attachments/assets/622d9969-cd7a-44f1-8a07-f4503a29f628" />

- **MAGs** — explore the metabolic potential of individual MAGs (bins): a cell-diagram view with KEGG completeness overlays per metabolic category, a KEGG pathway browser that colours the genes present in a MAG on any KEGG map, and a comparative presence/absence table of category genes across all MAGs
<img width="1461" height="1266" alt="Figure2orig" src="https://github.com/user-attachments/assets/36d915e7-5778-4eec-8bce-306736173e41" />

- **About** — citation information and links

---

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/jtamames/Watermelon.git
cd Watermelon
```

### 2. Install R dependencies

Make sure your SqueezeMeta conda environment is active, then run the installer:

```bash
conda activate SqueezeMeta
bash install.sh
```

The installer handles all R packages (CRAN and Bioconductor) and any required system libraries automatically.

### 3. Run the app

```bash
conda activate SqueezeMeta
./watermelon.sh
```

`watermelon.sh` starts the app and, once the server is ready, opens your default
browser automatically. If the browser cannot be opened (for example on a headless
server), the URL is printed so you can open it manually:

```
============================================================
 Watermelon
------------------------------------------------------------
 Please open your browser and enter the following URL:
   http://127.0.0.1:3838
============================================================
```

To use a different port, or to skip opening the browser:

```bash
./watermelon.sh 8080            # custom port
./watermelon.sh 3838 nobrowser  # do not auto-open the browser
```

> **Important:** the SqueezeMeta conda environment must be active before launching the app, both for the visualisation features and for the Run tab to find `SqueezeMeta.pl` in the PATH.

---

## KronaTools (optional, required for Krona tab)

```bash
which ktImportText  # if prints a path, already available
conda install -c bioconda krona  # if not available
ktUpdateTaxonomy.sh
```

---

## Usage

### Run tab — launching a pipeline

**Project Setup** — project name, program (SqueezeMeta / sqm_reads / sqm_longreads), execution mode.

**Input Files**

- **Samples file (-s)** — click **Create** to open a wizard: select a FASTQ directory, assign sample names and pair1/pair2 per file (auto-guessed from `_R1`/`_R2` suffixes), choose a save location, and click Save. The file path and input directory are auto-populated in the sidebar.
- **Input directory (-f)** — auto-populated when using the samples file creator.
- **Working directory** — where the project folder will be created.

**Profile** — a preset that fills in the advanced parameters: *Standard Metagenome*, *Large Metagenome (merged assembly)*, *Sequential (per-sample)*, *Metatranscriptome*, *Fast (no binning)* or *Custom*. Selecting a profile updates threads, mode, assembler, mapper and binning options; these can still be edited afterwards.

**Advanced** — Filtering, Assembly, Annotation, Mapping, Binning, Performance.

**Monitoring** — the current pipeline step (e.g. `STEP4 -> HOMOLOGY SEARCHES`) is shown above the log. Click **■ Abort** to stop.

**Auto-loading** — when the pipeline finishes, Watermelon automatically runs `sqm2tables.py` (or `sqmreads2tables.py`), loads the project, and switches to the Load tab.

---

### Load tab — loading an existing project

Three modes (hover the ⓘ icon for details):

- **Load project** — SqueezeMeta project directory.
- **Load tables** — directory of pre-generated SQMlite tables.
- **Load multiple** — add directories one by one; combined with `combineSQM()`. Requires at least 2 directories.

---

### Comparison tab — differential abundance analysis

**Data** — taxonomy (by rank: superkingdom → species) or functional databases (COG, KEGG, PFAM...).

**Method:**

| Method | Notes | Min replicates |
|--------|-------|----------------|
| Wilcoxon | Non-parametric, no distributional assumptions | 2 per group |
| DESeq2 | Negative binomial GLM, recommended for count data | 2 per group |
| edgeR | Negative binomial GLM, good for small n | 2 per group |

DESeq2 and edgeR are blocked when any group has only 1 sample. A warning is shown when any group has fewer than 3 samples.

**Groups** — two checkbox columns (Group 1 / Group 2). Checking a sample in one group removes it from the other automatically.

**Filters** — FDR threshold and minimum |log2FC|.

**Results** — interactive volcano plot + results table (feature, name for functional data, log2FC, p-value, FDR, mean group 1, mean group 2). Downloadable as TSV.

---

### MAGs tab — metabolic exploration of bins

Requires a full SQM project with KEGG annotation and binning.

**Single MAG** — explore one MAG at a time, in two modes:

- **Metabolism diagram** — a cell diagram showing major metabolic categories
  (central carbon metabolism, N/S/CH4 metabolism, biosynthesis, respiration,
  transporters). Each category is shaded according to how complete its gene set
  is in the selected MAG. Hover for details; click a category to see a KO detail
  table grouped by KEGG pathway. Pathway headers in the table are clickable and
  render the corresponding KEGG map with the MAG's genes coloured.
- **Metabolic maps** — browse or search the full KEGG pathway hierarchy and
  render any pathway, with the genes present in the selected MAG coloured red.

The **KO list** toggle switches between *Extended* (every KO in the KEGG pathways
assigned to a category) and *Central* (a curated set of diagnostic / canonical
KOs for each pathway).

**Comparative table** — presence/absence of a gene set across all MAGs in the
project, as a coloured matrix (genes in rows, MAGs in columns). The gene set can
be chosen either **by category** (curated central KO set) or **by KEGG pathway**
(all KOs of any KEGG pathway). MAG columns are labelled with their completeness.

---

## Project types

| Type | Load function | Features |
|------|--------------|----------|
| SQM (full) | `loadSQM` | All tabs, all plots, taxonomy/function search, bins, MAGs |
| SQMlite | `loadSQMlite` | Plots, Tables, Krona, Pathways, Multivariate, Comparison; no contig/ORF/bin detail; no MAGs tab |

---

## Dependencies summary

| Package | Source | Required for |
|---------|--------|--------------|
| shiny | CRAN | Core |
| shinyjs | CRAN | Core |
| shinyFiles | CRAN | Core |
| bslib | CRAN | Core |
| DT | CRAN | Core |
| SQMtools | CRAN / SqueezeMeta repo | Core |
| processx | CRAN | Run tab |
| plotly | CRAN | Plots, Comparison tabs |
| ggplot2 | CRAN | Plots tab |
| vegan | CRAN | Multivariate tab |
| xml2 | CRAN | Pathways, MAGs tabs |
| jsonlite | CRAN | Pathways, MAGs tabs |
| base64enc | CRAN | MAGs tab (cell diagram) |
| png | CRAN | MAGs tab (KEGG maps) |
| pathview | Bioconductor | Pathways, MAGs tabs |
| DESeq2 | Bioconductor | Comparison tab (optional) |
| edgeR | Bioconductor | Comparison tab (optional) |
| KronaTools (`ktImportText`) | conda / GitHub | Krona tab |

---

## Server deployment (remote access)

### 1. Install R packages on the server

```bash
sudo apt-get install -y cmake liblzma-dev
sudo R -e "install.packages(c('shiny','shinyjs','shinyFiles','bslib','DT','plotly',
  'ggplot2','SQMtools','vegan','xml2','jsonlite','processx','base64enc','png'), repos='https://cran.rstudio.com/')"
sudo R -e 'install.packages("BiocManager", repos="https://cran.rstudio.com/")'
sudo R -e 'BiocManager::install(c("pathview","Biostrings","DESeq2","edgeR"))'
```

> Run outside any conda environment (`conda deactivate` first, verify with `which R`).

### 2. Install Shiny Server

```bash
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.21.1012-amd64.deb
sudo dpkg -i shiny-server-1.5.21.1012-amd64.deb
```

### 3. Deploy and configure

```bash
sudo mkdir -p /srv/shiny-server/watermelon
sudo cp -r /path/to/Watermelon/* /srv/shiny-server/watermelon/
```

Edit `/etc/shiny-server/shiny-server.conf`:

```
run_as shiny;
server {
  listen 3838;
  location /watermelon {
    app_dir /srv/shiny-server/watermelon;
    log_dir /var/log/shiny-server;
  }
}
```

### 4. Data access and startup

```bash
sudo chmod -R o+rX /path/to/your/sqm/projects
chmod o+x /home/your_username
sudo systemctl start shiny-server
sudo systemctl enable shiny-server
sudo ufw allow 3838/tcp
```

Access at: `http://<server-ip>:3838/watermelon`

> **Run tab in server mode:** the `shiny` user needs `SqueezeMeta.pl` in its PATH. Add the conda environment `bin` directory to `/etc/environment`.

> **Directory navigation:** use **root** (maps to `/`) in the file browser to reach your data.

---

## Remote access from outside the local network

**SSH tunnel:**
```bash
ssh -L 3838:localhost:3838 user@server-ip
# open http://localhost:3838/watermelon
```

**nginx reverse proxy** — see full example in the project wiki.

---

## Troubleshooting

**`SqueezeMeta.pl` not found (Run tab)**
```bash
conda activate SqueezeMeta
./watermelon.sh
```

**`processx` error**
```bash
conda activate SqueezeMeta
R -e "install.packages('processx', repos='https://cran.rstudio.com/')"
```

**DESeq2 / edgeR not available**
```bash
conda activate SqueezeMeta
R -e "BiocManager::install(c('DESeq2','edgeR'))"
```

**Cannot find results tables** — the error message shows the path that was tried. For SQMlite projects, point to the directory containing the `.tsv` files, not the parent project directory.

**App does not load**
```bash
tail -f /var/log/shiny-server/*.log
```

---

## Citation

If you use SqueezeMeta or SQMtools in your research, please cite:

- Tamames & Puente-Sánchez (2019). SqueezeMeta, a highly portable metagenomics pipeline based on simultaneous coassembly of multiple samples. *Frontiers in Microbiology*. doi:[10.3389/fmicb.2018.03349](https://doi.org/10.3389/fmicb.2018.03349)
- Puente-Sánchez et al. (2020). SQMtools: automated processing and visual analysis of 'omics data with R and anvi'o. *BMC Bioinformatics*. doi:[10.1186/s12859-020-03703-2](https://doi.org/10.1186/s12859-020-03703-2)

---

## License

MIT

© Javier Tamames, CNB-CSIC (Madrid, Spain) 2026
