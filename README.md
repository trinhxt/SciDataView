<p align="center">
  <img src="app/icon/app_icon.svg" width="96" height="96" alt="SciDataView Logo" />
</p>

<h1 align="center">SciDataView</h1>

<p align="center">
  <strong>Universal Scientific Tabular Data Profiler & Inspector</strong><br>
  <em>Zero-Dependency • Standalone Windows Portable • WebAssembly Serverless • Headless CLI</em>
</p>

<p align="center">
  <a href="https://github.com/trinhxt/SciDataView"><img src="https://img.shields.io/badge/Language-R-276DC3.svg?logo=R&logoColor=white" alt="Language R"></a>
  <a href="https://github.com/trinhxt/SciDataView"><img src="https://img.shields.io/badge/Framework-Shiny-1B9AAA.svg?logo=rstudio&logoColor=white" alt="Shiny"></a>
  <a href="https://github.com/trinhxt/SciDataView"><img src="https://img.shields.io/badge/WebAssembly-Shinylive-654FF0.svg?logo=webassembly&logoColor=white" alt="WebAssembly"></a>
  <a href="https://github.com/trinhxt/SciDataView"><img src="https://img.shields.io/badge/Platform-Windows%20Portable-0078D6.svg?logo=windows&logoColor=white" alt="Windows Portable"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-MIT-green.svg" alt="License MIT"></a>
</p>

---

## 🌟 Overview

**SciDataView** is an enterprise-grade scientific tabular data exploration, hygiene screening, and profiling tool. It bridges the gap between heavyweight statistical suites and lightweight data viewers, offering immediate, publication-ready data summaries with zero software installation required.

SciDataView supports three distinct deployment paradigms:
1. **Windows Standalone Portable Edition**: Self-contained R-Portable engine. Double-click to run anywhere (USB, Desktop, VM) without R, RStudio, or administrator permissions.
2. **WebAssembly / Shinylive (Serverless Web)**: Runs 100% inside modern web browsers (Chrome, Edge, Firefox, Safari) using client-side `webR`. Zero server infrastructure required.
3. **Headless Batch CLI Execution**: Non-interactive command-line profiling for bioinformatics pipelines, HPC clusters, and automated cron jobs.

---

## ✨ Key Features

- **Universal Format Ingestion**:
  - Delimited text: `.csv`, `.tsv`, `.txt`, `.tab`, `.dat`, `.semicolon`, `.pipe`.
  - Serialized analytics formats: Apache Parquet (`.parquet`), Arrow/Feather (`.feather`, `.arrow`), FST (`.fst`), QS/QS2 (`.qs`, `.qs2`), R objects (`.rds`).
  - Spreadsheets & Statistical suites: Microsoft Excel (`.xlsx`, `.xls`), Stata (`.dta`), SPSS (`.sav`, `.zsav`), SAS (`.sas7bdat`).
- **Autonomous Structure Detection**:
  - Automatic detection of metadata/title rows with dynamic skip row selectors.
  - Automatic detection of delimiters, headers, and UTF-8 / latin1 encodings.
- **Data Quality & Hygiene Screening**:
  - Instant alerts for zero-variance features, severe missingness (>50%), near-empty columns (>90%), and duplicate records.
- **Interactive Column Inventory**:
  - Column classification into Continuous Numeric, Categorical String, Discrete, Date/Time, and Unique Identifiers.
  - Real-time missingness bars, distinct counts, sample values.
  - Bi-directional interactive column sorting (▲/▼).
  - Dynamic on-the-fly Data Type overriding with instant recalculation.
- **Numeric Distributions & Moments**:
  - Native SVG Sparkline mini-histograms (subsampled $\le 3,000$ points for 60fps rendering).
  - Parametric & non-parametric moments: Mean, SD, Median, IQR, Min, Max.
  - Fisher-Pearson Skewness coefficient and Tukey IQR Outlier detection.
- **Pearson Correlation Matrix & Bivariate Visualizer**:
  - High-performance collinearity matrix with color heatmapping.
  - **Dimensionality Guard**: Automatically limits computation to top 35 features by variance when $N > 35$ to guarantee smooth browser rendering.
  - Click-to-inspect bivariate scatter plot with regression trendline and $R^2$ variance metric.
- **Fast Data Inspector**:
  - Vectorized multi-column text search with debounced filtering.
  - Zero-lag server-side pagination with clean navigation controls.
- **Lazy Tab Evaluation & Skeleton Shimmer**:
  - Initial upload profiling finishes in $<0.05$ seconds.
  - Heavy calculations (moments, frequencies, correlation) evaluate on-demand upon tab activation and are cached reactively.
  - Shimmer placeholder animations indicate progress during tab transitions.
- **Standalone Offline Export**:
  - One-click export of self-contained, CSS-inlined HTML reports (`_data_profile.html`).
  - Terminal-formatted ASCII plain-text summary reports (`_data_summary.txt`).

---

## 🚀 Getting Started

### Method 1: Portable Windows Edition (No R Required)
1. Download `SciDataView-Windows-Portable.zip` from [Releases](https://github.com/trinhxt/SciDataView/releases).
2. Extract the archive to any folder or USB drive.
3. Double-click `SciDataView.lnk` (or `app/SciDataView.bat`).
4. The application opens automatically in your default web browser.

### Method 2: Standard R Developer Workflow
If you already have R installed:
```bash
# Clone the repository
git clone https://github.com/trinhxt/SciDataView.git
cd SciDataView

# Install required dependencies
Rscript install_deps.R

# Launch desktop app
Rscript app/run_app.R
```

### Method 3: Headless CLI Batch Profiling
Run non-interactive profiling from terminal or bash scripts:
```bash
Rscript app/app.R "path/to/dataset.csv"
```
Generated reports will be output automatically as:
- `dataset_data_summary.txt`
- `dataset_data_profile.html`

### Method 4: Compile to WebAssembly (Shinylive)
To export the application as a static website powered by WebAssembly:
```bash
Rscript app/export_shinylive.R
```
The compiled static website will be created in `dist_web/`, ready to be hosted on GitHub Pages, Netlify, or Vercel.

---

## 📁 Repository Structure

```
SciDataView/
├── .gitignore                   # Excludes binaries (R-Portable, library, dist_web)
├── LICENSE                      # MIT License
├── README.md                    # Project documentation
├── install_deps.R               # One-click dependency installer for R developers
├── SciDataView.lnk              # Desktop launcher shortcut
│
└── app/
    ├── app.R                    # Core Shiny application & profiling engine
    ├── export_shinylive.R       # WebAssembly export script
    ├── run_app.R                # Process manager & port allocator
    ├── SciDataView.bat          # Windows batch runner (with R-Portable/PATH fallback)
    ├── SciDataView.vbs          # Silent background launcher
    └── icon/                    # Application icons (.ico, .svg, .png)
```

> **Note for Contributors:** The binary R-Portable distribution and local compiled package libraries are strictly excluded from git tracking via `.gitignore` to keep the repository lightweight (< 500 KB).

---

## 📄 License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.
