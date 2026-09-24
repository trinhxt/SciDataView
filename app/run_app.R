# ==============================================================================
# SciDataView - Desktop Application Runner & Process Manager
# ==============================================================================

# Ensure R does not prompt for workspace save on exit
options(save.defaults = list(ask = "no"))

# 1. Setup isolated library paths
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)

if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "\\", mustWork = FALSE)
  app_dir  <- dirname(script_path)
  root_dir <- normalizePath(file.path(app_dir, ".."), winslash = "\\", mustWork = FALSE)
} else {
  root_dir <- normalizePath(getwd(), winslash = "\\", mustWork = FALSE)
  app_dir  <- file.path(root_dir, "app")
}

# If standalone portable libraries exist, prioritize them while keeping system libs as fallback
bundled_libs <- c(
  if (dir.exists(file.path(app_dir, "library"))) file.path(app_dir, "library"),
  if (dir.exists(file.path(root_dir, "library"))) file.path(root_dir, "library"),
  if (dir.exists(file.path(app_dir, "R-Portable", "library"))) file.path(app_dir, "R-Portable", "library"),
  if (dir.exists(file.path(root_dir, "R-Portable", "library"))) file.path(root_dir, "R-Portable", "library")
)

if (length(bundled_libs) > 0) {
  .libPaths(unique(c(bundled_libs, .libPaths())))
}

# 2. Ensure a writable user library is available and CRAN mirror configured
if (identical(getOption("repos"), c(CRAN = "@CRAN@")) || is.null(getOption("repos")["CRAN"])) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

user_lib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(user_lib)) {
  user_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library",
                        paste0(R.version$major, ".", sub("\\..*", "", R.version$minor)))
}
if (!dir.exists(user_lib)) {
  tryCatch(dir.create(user_lib, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
}
if (dir.exists(user_lib) && !(user_lib %in% .libPaths())) {
  .libPaths(c(user_lib, .libPaths()))
}

# 3. Pre-flight package check & automatic dependency installation
core_packages <- c(
  "shiny", "httpuv", "bslib", "dplyr", "purrr", "tibble",
  "data.table", "readr", "readxl", "haven", "later", "vroom"
)
missing_packages <- core_packages[!vapply(core_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("================================================================================")
  message("[SciDataView] Missing required package(s): ", paste(missing_packages, collapse = ", "))
  message("[SciDataView] Installing dependencies from CRAN. Please wait a moment...")
  message("================================================================================")
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(shiny)
  library(httpuv)
})

# 2. Allocate an ephemeral random port to prevent conflicts
port <- httpuv::randomPort(min = 10000, max = 65000)
host <- "127.0.0.1"
app_url <- sprintf("http://%s:%d", host, port)

message("================================================================================")
message(sprintf("SciDataView Desktop Server started at: %s", app_url))
message("Press Ctrl+C or simply close the browser tab to exit.")
message("================================================================================")

# 3. Launch the Shiny application directly in the user's default browser
# Standard browseURL eliminates all Chromium GPU crashes and black screen bugs
runApp(
  appDir = app_dir,
  port = port,
  host = host,
  launch.browser = utils::browseURL
)

# 4. Guarantee clean and immediate process termination when Shiny stops
q(save = "no", status = 0)
