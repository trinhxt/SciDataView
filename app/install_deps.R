# ==============================================================================
# SciDataView - Development Dependencies Installer
# ==============================================================================
# Run this script in an active R session to install all required packages:
#   Rscript install_deps.R
# ==============================================================================

required_packages <- c(
  "shiny",
  "httpuv",
  "bslib",
  "data.table",
  "readxl",
  "later",
  "shinylive"
)

# Optional packages for high-performance serialized & statistical formats
optional_packages <- c(
  "haven",
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
    install.packages(pkg)
  } else {
    message(sprintf("  [OK] %s is already installed.", pkg))
  }
}

# Only check optional serial packages if not running in CI (avoids slow C++ builds on Linux runners)
is_ci <- nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))
if (!is_ci) {
  message("\nChecking optional serialized format packages (Arrow, FST, QS)...")
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg)
        message(sprintf("  [OK] Installed optional: %s", pkg))
      }, error = function(e) {
        message(sprintf("  [SKIP] Optional package '%s' skipped (%s)", pkg, e$message))
      })
    } else {
      message(sprintf("  [OK] %s is already installed.", pkg))
    }
  }
}

message("------------------------------------------------------------------------------")
message("Setup complete! You can run SciDataView with: Rscript app/run_app.R")
message("------------------------------------------------------------------------------")
