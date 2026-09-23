# ==============================================================================
# SciDataView - Development Dependencies Installer
# ==============================================================================
# Run this script in an active R session to install all required packages:
#   Rscript install_deps.R
# ==============================================================================

required_packages <- c(
  "shiny",
  "bslib",
  "readr",
  "readxl",
  "haven",
  "vroom",
  "tibble",
  "dplyr",
  "purrr",
  "shinylive"
)

# Optional packages for high-performance serialized formats (Parquet, Feather, FST, QS)
optional_packages <- c(
  "arrow",
  "fst",
  "qs",
  "qs2"
)

message("------------------------------------------------------------------------------")
message("Checking and installing core packages for SciDataView...")
message("------------------------------------------------------------------------------")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing package: %s ...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(sprintf("  [OK] %s is already installed.", pkg))
  }
}

message("\nChecking optional serialized format packages (Arrow, FST, QS)...")
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch({
      install.packages(pkg, repos = "https://cloud.r-project.org")
      message(sprintf("  [OK] Installed optional: %s", pkg))
    }, error = function(e) {
      message(sprintf("  [SKIP] Optional package '%s' skipped (%s)", pkg, e$message))
    })
  } else {
    message(sprintf("  [OK] %s is already installed.", pkg))
  }
}

message("------------------------------------------------------------------------------")
message("Setup complete! You can run SciDataView with: Rscript app/run_app.R")
message("------------------------------------------------------------------------------")
