# ==============================================================================
# SCIDATAVIEW - WEBASSEMBLY / SHINYLIVE EXPORT SCRIPT (GIAI ĐOẠN 2)
# ==============================================================================
# This script compiles SciDataView into a standalone static website powered by
# WebAssembly (webR). It can be hosted on GitHub Pages, Netlify, Vercel, or any
# static web host without requiring an R server or backend runtime.
# ==============================================================================

options(warn = 1)

# Setup library paths for isolated R-Portable environment
root_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (basename(root_dir) == "app") root_dir <- dirname(root_dir)
app_dir  <- file.path(root_dir, "app")
dest_dir <- file.path(root_dir, "dist_web")
lib_dir  <- file.path(app_dir, "library")
r_lib    <- file.path(app_dir, "R-Portable", "library")

if (dir.exists(lib_dir)) {
  .libPaths(unique(c(lib_dir, r_lib, .libPaths())))
}

# 1. Check or install shinylive package
if (!requireNamespace("shinylive", quietly = TRUE)) {
  message("Package 'shinylive' is not installed. Installing from CRAN...")
  if (dir.exists(lib_dir)) {
    install.packages("shinylive", lib = lib_dir)
  } else {
    install.packages("shinylive")
  }
}

library(shinylive)

message("------------------------------------------------------------------------------")
message("SciDataView WebAssembly (Shinylive) Exporter")
message("------------------------------------------------------------------------------")
message("Source app directory : ", app_dir)
message("Destination web dir  : ", dest_dir)

# 2. Stage clean directory (excluding R-Portable and local binaries)
staging_dir <- file.path(tempdir(), "scidataview_wasm_stage")
if (dir.exists(staging_dir)) unlink(staging_dir, recursive = TRUE)
dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

file.copy(file.path(app_dir, "app.R"), file.path(staging_dir, "app.R"))
if (dir.exists(file.path(app_dir, "icon"))) {
  dir.create(file.path(staging_dir, "icon"), showWarnings = FALSE)
  file.copy(dir(file.path(app_dir, "icon"), full.names = TRUE), file.path(staging_dir, "icon"))
}

# 3. Export via shinylive
message("Packaging app with WebAssembly assets...")
shinylive::export(
  appdir  = staging_dir,
  destdir = dest_dir,
  template_params = list(
    title = "SciDataView",
    include_in_head = paste(
      '<link rel="icon" type="image/x-icon" href="icon/app.ico">',
      '<link rel="icon" type="image/png" sizes="192x192" href="icon/app_icon.png">',
      '<link rel="icon" type="image/svg+xml" href="icon/app_icon.svg">',
      sep = "\n"
    )
  ),
  quiet   = FALSE
)

# 4. Copy static icons directly to destination and create root favicon.ico
if (dir.exists(file.path(app_dir, "icon"))) {
  dir.create(file.path(dest_dir, "icon"), showWarnings = FALSE, recursive = TRUE)
  file.copy(dir(file.path(app_dir, "icon"), full.names = TRUE), file.path(dest_dir, "icon"), overwrite = TRUE)
  if (file.exists(file.path(app_dir, "icon", "app.ico"))) {
    file.copy(file.path(app_dir, "icon", "app.ico"), file.path(dest_dir, "favicon.ico"), overwrite = TRUE)
  }
}

# Clean up staging
unlink(staging_dir, recursive = TRUE)

message("------------------------------------------------------------------------------")
message("Export successfully completed!")
message("Output location: ", dest_dir)
message("To test locally:")
message("  python -m http.server --directory \"", dest_dir, "\" 8080")
message("  or run in R: httpuv::runStaticServer(\"", dest_dir, "\")")
message("------------------------------------------------------------------------------")
