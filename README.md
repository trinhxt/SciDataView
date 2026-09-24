# SciDataView

An R-based app for reading and summarizing many data table formats.

[![Launch Web App](https://img.shields.io/badge/Launch_App-0078D4?style=for-the-badge&logo=googlechrome&logoColor=white)](https://trinhxt.github.io/SciDataView/)

## Usage

Open **[https://trinhxt.github.io/SciDataView/](https://trinhxt.github.io/SciDataView/)** in any modern browser. No installation required. All data processing runs locally in your browser via WebAssembly - nothing is uploaded.

## Capabilities

### Supported Formats

| Category | Formats |
|----------|---------|
| Delimited text | CSV, TSV, TXT (auto-detects delimiter, header, encoding, title rows) |
| Spreadsheets | Excel `.xlsx`, `.xls` |
| Statistical software | Stata `.dta`, SPSS `.sav`, SAS `.sas7bdat` |
| Serialized | Parquet, Arrow, Feather, FST, QS/QS2, RDS |

### Analysis Features

- **Quality Screening** - Flags zero-variance columns, high missingness (>50%), near-empty columns (>90%), duplicate rows.
- **Column Inventory** - Inferred types, missing rates, distinct counts, interactive type casting.
- **Numeric Profiles** - Sparkline distributions, mean, SD, median, IQR, skewness, Tukey outlier counts.
- **Categorical Breakdown** - Unique level counts and top-level frequencies.
- **Correlation Matrix** - Pearson correlations with click-to-view scatter plots.
- **Data Inspector** - Multi-column text search with pagination.
- **Export** - Self-contained HTML report and plain-text summary.

## License

MIT - see [LICENSE](LICENSE).
