# SciDataView

Fast tabular data profiler for scientific and analytical datasets.

Available as:
- **Windows Portable**: Self-contained R environment. Runs without installing R or requiring administrator privileges.
- **WebAssembly (Shinylive)**: Runs entirely client-side in the browser via webR.
- **CLI Batch Mode**: Non-interactive command-line tool for automated pipelines.

---

## Supported Formats

- **Delimited text**: CSV, TSV, TXT (auto-detects delimiter, header, encoding, and title rows to skip).
- **Serial formats**: Parquet, Arrow, Feather, FST, QS/QS2, RDS.
- **Statistical software & spreadsheets**: Excel (`.xlsx`, `.xls`), Stata (`.dta`), SPSS (`.sav`), SAS (`.sas7bdat`).

---

## Capabilities

- **Quality Screening**: Flags zero-variance columns, high missingness (>50%), near-empty columns (>90%), and duplicate rows.
- **Column Inventory**: Inferred types, missing rates, distinct counts, and interactive type casting.
- **Numeric Profiles**: Distribution sparklines, mean, SD, median, IQR, min, max, skewness, and Tukey outlier counts.
- **Categorical Breakdowns**: Unique level counts and top-level frequencies.
- **Correlation Matrix**: Pearson correlation with click-to-view bivariate scatter plots. Automatically limits to the top 35 variables by variance when column count exceeds 35 to maintain UI responsiveness.
- **Data Preview**: Multi-column text search with pagination.
- **Export**: Self-contained HTML reports (single file, offline CSS) and plain-text summaries.

---

## Quick Start

### 1. Direct 1-Click Launch (Recommended)
After cloning or downloading the repository:
- Double-click **`SciDataView.lnk`** in the root folder (or run **`app/SciDataView.bat`**).
- **Smart Runtime Auto-Detection**:
  - Automatically detects local `R-Portable` or system-installed R (via PATH, Registry, or Program Files).
  - Automatically verifies and installs missing R packages from CRAN on first run.
  - If R is not installed on the system, offers an automated setup wizard to download and configure R-Portable seamlessly.

### 2. Manual CLI Launch
```bash
git clone https://github.com/trinhxt/SciDataView.git
cd SciDataView

# Optional manual dependency install (auto-installed on launch if omitted):
Rscript install_deps.R

# Launch app directly:
Rscript app/run_app.R
```

### 3. CLI Batch Mode
```bash
Rscript app/app.R "path/to/data.csv"
```
Outputs `<filename>_data_summary.txt` and `<filename>_data_profile.html`.

### 4. Build WebAssembly Static Site
```bash
Rscript app/export_shinylive.R
```
Compiles static site assets into `dist_web/`.

---

## Repository Structure

```
SciDataView/
├── .github/workflows/deploy_web.yml   # Automatic GitHub Pages deployment
├── .gitignore                         # Ignores R-Portable, local packages, and dist_web
├── app/
│   ├── app.R                          # Main application logic
│   ├── export_shinylive.R             # Shinylive export script
│   ├── run_app.R                      # Process manager and browser launcher
│   ├── SciDataView.bat                # Windows launcher (with smart auto-detection & setup)
│   ├── setup_r.ps1                    # Automated R setup script for clean machines
│   └── icon/                          # Application icons
├── install_deps.R                     # Dependency installation script
├── LICENSE                            # MIT License
├── README.md
└── SciDataView.lnk
```

---

## License

MIT License. See [LICENSE](LICENSE) for details.
