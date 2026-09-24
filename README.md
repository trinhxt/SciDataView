# SciDataView

Fast tabular data profiler for scientific and analytical datasets.

[![Launch Web App](https://img.shields.io/badge/Launch-WebAssembly%20App-0078D4?style=for-the-badge&logo=googlechrome&logoColor=white)](https://trinhxt.github.io/SciDataView/)

👉 **Use Online**: **[https://trinhxt.github.io/SciDataView/](https://trinhxt.github.io/SciDataView/)**
*(Runs 100% client-side in your browser via WebAssembly/webR. Zero installation required, completely private — no data uploaded to any server).*

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

## Usage

Open **[https://trinhxt.github.io/SciDataView/](https://trinhxt.github.io/SciDataView/)** in any modern browser (Windows, macOS, Linux, iPadOS, ChromeOS). No installation or R required. Upload your data file and the profiler runs entirely inside your browser.

---

## Repository Structure

```
SciDataView/
├── .github/
│   └── workflows/
│       └── deploy_web.yml   # Automatic GitHub Pages deployment
├── app/
│   ├── app.R                # Main application logic
│   ├── export_shinylive.R   # Shinylive WebAssembly exporter
│   ├── install_deps.R       # Dependency installation script
│   ├── run_app.R            # Process manager and browser launcher
│   ├── SciDataView.bat      # Windows launcher
│   └── icon/                # Application icons (.ico, .png, .svg)
├── .gitignore
├── LICENSE                  # MIT License
├── README.md
└── SciDataView.lnk          # Windows 1-click launcher
```

---

## License

MIT License. See [LICENSE](LICENSE) for details.
