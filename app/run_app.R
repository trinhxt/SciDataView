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

lib_dir <- if (dir.exists(file.path(app_dir, "library"))) file.path(app_dir, "library") else file.path(root_dir, "library")
r_lib   <- if (dir.exists(file.path(app_dir, "R-Portable", "library"))) file.path(app_dir, "R-Portable", "library") else file.path(root_dir, "R-Portable", "library")

# Restrict .libPaths strictly to bundled packages
.libPaths(c(lib_dir, r_lib))

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
