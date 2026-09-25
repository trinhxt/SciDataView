# ==============================================================================
# SCRIPT: app.R
# ==============================================================================
# Purpose:
#   Universal Tabular Dataset Summarizer & Profiler.
#   Features:
#     - Clean card layout with standard typography
#     - Metric overview cards and column inventory
#     - Missingness progress indicators and data quality screening flags
#     - Numeric distributions with SVG sparklines, skewness, and Tukey outlier detection
#     - Categorical level distributions
#     - Pearson correlation matrix
#     - Universal file reader (.csv, .tsv, .xlsx, .rds, .dta, .sav, .sas7bdat, .parquet, .feather, .arrow, .fst, .qs, .linear, etc.)
#     - Automatic title/metadata row detection and skip with manual UI override
#     - Interactive search in Data Inspector tab
#     - Text report (.txt) and standalone offline HTML report (.html) export
#     - Command-line batch execution: Rscript app.R [file]
#
# Date: 2026-09-23
# ==============================================================================

# ==============================================================================
# SCIDATAVIEW - DESKTOP EDITION (OPTIMIZED LEAN RUNTIME)
# ==============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(data.table)
  library(readxl)
  library(later)
})
# Set max upload size to 1 GB
options(shiny.maxRequestSize = 2000 * 1024^2)

# Shinylive / Chromium WebAssembly download fix:
# Standard shiny::downloadButton includes an empty HTML5 `download` attribute.
# In Shinylive (WebAssembly), Chromium browsers (Chrome, Edge) use the DOM element ID
# instead of the Content-Disposition filename, resulting in `btn_download_*.htm`.
# Removing `download` allows the server-defined filename and extension to take effect.
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Serve icon assets statically from app/icon
icon_dir <- if (dir.exists("app/icon")) "app/icon" else if (dir.exists("icon")) "icon" else "."
try(shiny::addResourcePath("icon", normalizePath(icon_dir, mustWork = FALSE)), silent = TRUE)

get_app_icon_svg <- function() {
  candidates <- c("app/icon/app_icon.svg", "icon/app_icon.svg")
  for (cand in candidates) {
    if (file.exists(cand)) {
      raw <- paste(readLines(cand, warn = FALSE), collapse = "\n")
      raw <- sub('^<\\?xml[^>]*\\?>\\s*', '', raw)
      return(raw)
    }
  }
  ""
}

# ==============================================================================
# 1. CORE UTILITIES & UNIVERSAL FILE READER
# ==============================================================================

# Automatic header row / title skip detector
detect_title_skip <- function(file_path, ext = NULL, sheet = 1, max_scan = 20) {
  if (is.null(ext)) ext <- tolower(tools::file_ext(file_path))
  
  # Binary & sequence formats have strict schemas and no freeform title rows
  if (ext %in% c("parquet", "feather", "arrow", "fst", "qs", "qs2", "rds", "dta", "sav", "sas7bdat",
                 "fasta", "fa", "faa", "fna", "fas")) {
    return(0)
  }
  
  preview <- tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      suppressMessages(as.data.frame(readxl::read_excel(file_path, sheet = sheet, col_names = FALSE, n_max = max_scan)))
    } else {
      suppressWarnings(data.table::fread(file_path, header = FALSE, nrows = max_scan, data.table = FALSE))
    }
  }, error = function(e) NULL)
  
  if (is.null(preview) || nrow(preview) <= 1) return(0)
  
  scan_n <- min(nrow(preview), max_scan)
  scores <- sapply(seq_len(scan_n), function(i) {
    row <- as.character(unlist(preview[i, ]))
    non_empty <- row[!is.na(row) & trimws(row) != ""]
    n_col <- length(row)
    if (length(non_empty) == 0) return(-5.0)
    
    fill_rate <- length(non_empty) / max(1, n_col)
    uniq_rate <- length(unique(non_empty)) / max(1, length(non_empty))
    num_vals  <- suppressWarnings(as.numeric(non_empty))
    char_rate <- sum(is.na(num_vals)) / max(1, length(non_empty))
    
    score <- fill_rate * 2.5 + uniq_rate * 1.5 + char_rate * 1.0
    if (fill_rate < 0.4 && n_col >= 3) score <- score - 3.0
    score
  })
  
  best_idx <- which.max(scores)
  if (best_idx > 1 && scores[best_idx] > 2.0 && scores[best_idx] > (scores[1] + 0.5)) {
    return(best_idx - 1)
  }
  
  0
}

# Fast, zero-dependency FASTA sequence parser with UniProt & general database classification
parse_fasta_file <- function(file_path, max_lines = -1) {
  lines <- readLines(file_path, n = max_lines, warn = FALSE)
  if (length(lines) == 0) {
    stop("The FASTA file is empty.")
  }
  
  # Strip whitespace and eliminate all empty / blank lines anywhere in file
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  
  if (length(lines) == 0) {
    stop("The FASTA file contains no data (only blank lines).")
  }
  
  # Drop any malformed / empty header lines (lone '>' or '> ' with no identifier)
  empty_gt <- startsWith(lines, ">") & !nzchar(trimws(substring(lines, 2)))
  if (any(empty_gt)) {
    lines <- lines[!empty_gt]
  }
  
  # A valid FASTA header MUST start with '>' and contain an identifier
  is_header <- startsWith(lines, ">")
  if (!any(is_header)) {
    stop("No valid FASTA headers (lines starting with '>' and containing an identifier) found in file.")
  }
  
  header_idx <- which(is_header)
  
  # If there are leading non-header lines before first '>', ignore them
  if (header_idx[1] > 1) {
    lines <- lines[header_idx[1]:length(lines)]
    is_header <- startsWith(lines, ">")
    header_idx <- which(is_header)
  }
  
  n_seqs <- length(header_idx)
  headers <- lines[header_idx]
  
  # Group sequence lines by sequence index using cumsum
  seq_ids <- cumsum(is_header)
  non_h <- lines[!is_header]
  non_h_id <- seq_ids[!is_header]
  
  seqs <- character(n_seqs)
  if (length(non_h) > 0) {
    non_h_clean <- gsub("[[:space:]]+", "", non_h)
    agg <- tapply(non_h_clean, non_h_id, paste, collapse = "")
    seqs[as.integer(names(agg))] <- unname(agg)
  }
  
  clean_h <- trimws(substring(headers, 2))
  
  # Check for pipe-delimited format (UniProt / NCBI / PDB: >db|accession|entry_name description)
  has_pipe <- grepl("^[a-zA-Z0-9_-]+\\|", clean_h)
  
  db_code <- rep("other", n_seqs)
  acc <- character(n_seqs)
  entry_name <- character(n_seqs)
  desc <- character(n_seqs)
  organism <- rep(NA_character_, n_seqs)
  gene <- rep(NA_character_, n_seqs)
  
  if (any(has_pipe)) {
    p_h <- clean_h[has_pipe]
    raw_db <- sub("^([a-zA-Z0-9_-]+)\\|.*", "\\1", p_h)
    db_clean <- tolower(trimws(raw_db))
    
    # Map common DB codes
    db_label <- ifelse(db_clean == "sp", "sp (Swiss-Prot)",
                ifelse(db_clean == "tr", "tr (TrEMBL)",
                ifelse(db_clean == "pdb", "pdb (PDB)",
                ifelse(db_clean == "ref", "ref (RefSeq)",
                ifelse(db_clean == "iso", "iso (Isoform)",
                ifelse(nzchar(raw_db), paste0(raw_db, " (custom)"), "other"))))))
    db_code[has_pipe] <- db_label
    
    p_rest <- sub("^[a-zA-Z0-9_-]+\\|", "", p_h)
    p_acc <- sub("\\|.*", "", p_rest)
    acc[has_pipe] <- trimws(p_acc)
    
    p_after_acc <- sub("^[^\\|]*\\|", "", p_rest)
    entry_part <- sub("[[:space:]]+.*", "", p_after_acc)
    entry_name[has_pipe] <- trimws(entry_part)
    
    desc_part <- sub("^[^[:space:]]+[[:space:]]*", "", p_after_acc)
    desc[has_pipe] <- trimws(desc_part)
  }
  
  if (any(!has_pipe)) {
    np_h <- clean_h[!has_pipe]
    first_tok <- sub("[[:space:]]+.*", "", np_h)
    rest_tok <- sub("^[^[:space:]]+[[:space:]]*", "", np_h)
    acc[!has_pipe] <- trimws(first_tok)
    entry_name[!has_pipe] <- trimws(first_tok)
    desc[!has_pipe] <- trimws(rest_tok)
  }
  
  acc <- ifelse(nzchar(acc), acc, paste0("seq_", seq_len(n_seqs)))
  db_code <- ifelse(is.na(db_code) | !nzchar(trimws(db_code)), "other", trimws(db_code))
  
  # Extract Organism (OS=...) and Gene (GN=...) if present (standard UniProt metadata)
  has_os <- grepl("OS=", desc)
  if (any(has_os)) {
    organism[has_os] <- sub(".*OS=([^=]+?)(?:[[:space:]]+OX=|[[:space:]]+GN=|[[:space:]]+PE=|[[:space:]]+SV=|$).*", "\\1", desc[has_os])
  }
  has_gn <- grepl("GN=", desc)
  if (any(has_gn)) {
    gene[has_gn] <- sub(".*GN=([^[:space:]=]+).*", "\\1", desc[has_gn])
  }
  
  seq_lens <- nchar(seqs)
  
  df <- data.frame(
    Accession   = acc,
    Database    = db_code,
    Entry_Name  = ifelse(nzchar(entry_name), entry_name, "-"),
    Gene        = ifelse(is.na(gene) | !nzchar(gene), "-", gene),
    Organism    = ifelse(is.na(organism) | !nzchar(organism), "-", organism),
    Length      = seq_lens,
    Description = ifelse(nzchar(desc), desc, "-"),
    Sequence    = seqs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Tabulate database counts
  db_table <- sort(table(db_code), decreasing = TRUE)
  db_counts <- data.frame(
    Database = names(db_table),
    Sequences = as.integer(db_table),
    Percentage = round(100 * as.numeric(db_table) / n_seqs, 2),
    stringsAsFactors = FALSE
  )
  
  attr(df, "is_fasta") <- TRUE
  attr(df, "fasta_db_counts") <- db_counts
  attr(df, "total_sequences") <- n_seqs
  
  df
}

# Universal reader supporting all tabular data formats (with auto title skip, FASTA and fread fallback)
read_any_table <- function(file_path, file_name = NULL, sheet = 1, skip = "auto") {
  if (!file.exists(file_path)) {
    stop(sprintf("File does not exist: %s", file_path))
  }
  
  ext <- tolower(tools::file_ext(if (!is.null(file_name) && nzchar(file_name)) file_name else file_path))
  
  skip_n <- if (identical(skip, "auto") || is.null(skip)) {
    detect_title_skip(file_path, ext = ext, sheet = sheet)
  } else {
    as.integer(max(0, skip))
  }
  
  res <- tryCatch({
    switch(ext,
      "csv"      = data.table::fread(file_path, skip = skip_n, data.table = FALSE, check.names = FALSE),
      "tsv"      = data.table::fread(file_path, sep = "\t", skip = skip_n, data.table = FALSE, check.names = FALSE),
      "txt"      = {
        # Check if text file is FASTA formatted
        first_char <- tryCatch({
          con <- file(file_path, "r")
          on.exit(close(con))
          first_line <- ""
          while (length(l <- readLines(con, n = 1, warn = FALSE)) > 0) {
            tl <- trimws(l)
            if (nzchar(tl)) { first_line <- tl; break }
          }
          substr(first_line, 1, 1)
        }, error = function(e) "")
        
        if (identical(first_char, ">")) {
          parse_fasta_file(file_path)
        } else {
          data.table::fread(file_path, skip = skip_n, data.table = FALSE, check.names = FALSE)
        }
      },
      "fasta"    = parse_fasta_file(file_path),
      "fa"       = parse_fasta_file(file_path),
      "faa"      = parse_fasta_file(file_path),
      "fna"      = parse_fasta_file(file_path),
      "fas"      = parse_fasta_file(file_path),
      "xlsx"     = as.data.frame(readxl::read_excel(file_path, sheet = sheet, skip = skip_n)),
      "xls"      = as.data.frame(readxl::read_excel(file_path, sheet = sheet, skip = skip_n)),
      "rds"      = as.data.frame(readRDS(file_path)),
      "dta"      = {
        if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is required to read Stata (.dta) files.")
        as.data.frame(haven::read_dta(file_path))
      },
      "sav"      = {
        if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is required to read SPSS (.sav) files.")
        as.data.frame(haven::read_spss(file_path))
      },
      "sas7bdat" = {
        if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is required to read SAS (.sas7bdat) files.")
        as.data.frame(haven::read_sas(file_path))
      },
      "parquet"  = {
        read_pq <- function(fp) {
          # 1. Try Apache Arrow (Desktop / High Performance)
          if (requireNamespace("arrow", quietly = TRUE)) {
            res <- tryCatch({
              ds <- arrow::open_dataset(fp)
              total_rows <- tryCatch(nrow(ds), error = function(e) NA_integer_)
              if (!is.na(total_rows) && length(total_rows) == 1 && total_rows > 3000000L) {
                out <- as.data.frame(head(ds, 100000L))
                attr(out, "full_n_rows") <- total_rows
                attr(out, "is_sampled")  <- TRUE
                out
              } else {
                as.data.frame(arrow::read_parquet(fp))
              }
            }, error = function(e) NULL)
            if (is.data.frame(res) && ncol(res) > 0) return(res)
          }

          # 2. Try DuckDB if available
          if (requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)) {
            res <- tryCatch({
              con <- DBI::dbConnect(duckdb::duckdb())
              on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
              safe_fp <- gsub("'", "''", normalizePath(fp, winslash = "/", mustWork = FALSE))
              DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", safe_fp))
            }, error = function(e) NULL)
            if (is.data.frame(res) && ncol(res) > 0) return(res)
          }

          # 3. Try nanoparquet (WebAssembly / webR / Lightweight)
          if (requireNamespace("nanoparquet", quietly = TRUE)) {
            # 3a. Standard read
            res <- tryCatch(as.data.frame(nanoparquet::read_parquet(fp)), error = function(e) NULL)
            if (is.data.frame(res) && ncol(res) > 0) return(res)

            # 3b. Read without Arrow metadata (bypasses arrow_schema parsing length-zero bug)
            res <- tryCatch({
              opts <- nanoparquet::parquet_options(use_arrow_metadata = FALSE)
              as.data.frame(nanoparquet::read_parquet(fp, options = opts))
            }, error = function(e) NULL)
            if (is.data.frame(res) && ncol(res) > 0) return(res)

            # 3c. Direct C++ extraction (bypasses post_process_read_result bug in webR / v0.5.1)
            res <- tryCatch({
              opts <- nanoparquet::parquet_options(use_arrow_metadata = FALSE)
              fn <- get("nanoparquet_read2", asNamespace("nanoparquet"))
              raw <- .Call(fn, path.expand(fp), opts, NULL, sys.call())
              if (is.list(raw) && length(raw) >= 1 && is.data.frame(raw[[1]])) {
                df <- as.data.frame(raw[[1]])
                for (j in seq_along(df)) {
                  if (inherits(df[[j]], "POSIXct") || inherits(df[[j]], "hms")) {
                    val <- unclass(df[[j]])
                    if (is.numeric(val) && length(val) > 0 && suppressWarnings(max(val, na.rm = TRUE)) > 1e11) {
                      df[[j]] <- structure(val / 1000, class = class(df[[j]]))
                    }
                  }
                }
                df
              } else {
                NULL
              }
            }, error = function(e) NULL)
            if (is.data.frame(res) && ncol(res) > 0) return(res)
          }

          stop("Unable to parse Parquet file with available readers (arrow, duckdb, nanoparquet).")
        }
        read_pq(file_path)
      },
      "feather"  = {
        if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' is required to read Feather (.feather) files.")
        tab <- arrow::read_feather(file_path, as_data_frame = FALSE)
        if (tab$num_rows > 3000000L) {
          total_rows <- tab$num_rows
          res <- as.data.frame(head(tab, 100000L))
          attr(res, "full_n_rows") <- total_rows
          attr(res, "is_sampled")  <- TRUE
          res
        } else {
          as.data.frame(tab)
        }
      },
      "arrow"    = {
        if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' is required to read Arrow (.arrow) files.")
        tab <- arrow::read_ipc_file(file_path, as_data_frame = FALSE)
        if (tab$num_rows > 3000000L) {
          total_rows <- tab$num_rows
          res <- as.data.frame(head(tab, 100000L))
          attr(res, "full_n_rows") <- total_rows
          attr(res, "is_sampled")  <- TRUE
          res
        } else {
          as.data.frame(tab)
        }
      },
      "fst"      = as.data.frame(fst::read_fst(file_path)),
      "qs"       = as.data.frame(if (requireNamespace("qs2", quietly = TRUE)) qs2::qs_read(file_path) else qs::qread(file_path)),
      "qs2"      = as.data.frame(qs2::qs_read(file_path)),
      # Fallback for custom / bioinformatics / arbitrary extensions (.linear, .assoc, .raw, etc.)
      {
        first_char <- tryCatch({
          con <- file(file_path, "r")
          on.exit(close(con))
          first_line <- ""
          while (length(l <- readLines(con, n = 1, warn = FALSE)) > 0) {
            tl <- trimws(l)
            if (nzchar(tl)) { first_line <- tl; break }
          }
          substr(first_line, 1, 1)
        }, error = function(e) "")
        
        if (identical(first_char, ">")) {
          parse_fasta_file(file_path)
        } else {
          data.table::fread(file_path, skip = skip_n, data.table = FALSE, check.names = FALSE)
        }
      }
    )
  }, error = function(e) {
    display_name <- if (!is.null(file_name) && nzchar(file_name)) file_name else basename(file_path)
    stop(sprintf("Failed to read file '%s'%s: %s",
                 display_name,
                 if (nzchar(ext)) paste0(" (format .", ext, ")") else "",
                 e$message))
  })
  
  if (!is.data.frame(res) || ncol(res) == 0) {
    display_name <- if (!is.null(file_name) && nzchar(file_name)) file_name else basename(file_path)
    stop(sprintf("File '%s' was read but contains no valid tabular data or columns.", display_name))
  }
  
  attr(res, "skip_rows") <- skip_n
  res
}

# ==============================================================================
# 2. STATISTICAL ENGINE: MOMENTS, OUTLIERS & VISUALIZERS (PHASE P1 & P2)
# ==============================================================================

# Fast memory footprint calculator (O(1) sampling avoids recursive object.size freeze on large tables)
format_fast_memory_size <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return("0 B")
  nr <- nrow(df)
  if (nr <= 10000L) {
    return(format(object.size(df), units = "auto"))
  }
  sample_n <- min(1000L, nr)
  sub_sz <- as.numeric(object.size(head(df, sample_n)))
  est_bytes <- (sub_sz / sample_n) * nr
  format(structure(est_bytes, class = "object_size"), units = "auto")
}

# Fisher-Pearson sample skewness (subsampled to 5,000 for high-performance profiling)
calc_skewness <- function(x) {
  v <- x[!is.na(x)]
  n <- length(v)
  if (n < 3) return(NA_real_)
  if (n > 5000) {
    set.seed(42)
    v <- sample(v, 5000)
  }
  m <- mean(v)
  m2 <- mean((v - m)^2)
  m3 <- mean((v - m)^3)
  if (m2 == 0) return(0)
  round(m3 / (m2^1.5), 2)
}

# Tukey's fences outlier detector (1.5 * IQR)
calc_outliers <- function(x) {
  v <- x[!is.na(x)]
  n <- length(v)
  if (n < 4) return(c(count = 0, pct = 0))
  v_sub <- if (n > 10000) sample(v, 10000) else v
  q <- quantile(v_sub, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  n_out <- sum(v < lower | v > upper)
  c(count = n_out, pct = round(100 * n_out / n, 1))
}

# Self-contained SVG distribution sparkline (subsampled to 3,000 for instant 10x rendering)
generate_svg_sparkline <- function(vals, width = 110, height = 24, fill = "#0071E3") {
  v <- vals[!is.na(vals)]
  if (length(v) < 2 || length(unique(v)) == 1) {
    return("<span style='color:#86868B; font-size:11px;'>- (Constant)</span>")
  }
  
  if (length(v) > 3000) {
    set.seed(42)
    v <- sample(v, 3000)
  }
  
  h <- hist(v, breaks = 14, plot = FALSE)
  counts <- h$counts
  max_c <- max(counts)
  if (max_c == 0) max_c <- 1
  
  n_bars <- length(counts)
  bar_w <- round((width - (n_bars - 1) * 1.5) / n_bars, 1)
  
  rects <- character(n_bars)
  for (i in seq_along(counts)) {
    b_h <- round((counts[i] / max_c) * (height - 3), 1)
    b_x <- (i - 1) * (bar_w + 1.5)
    b_y <- height - b_h
    rects[i] <- sprintf("<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='1' fill='%s' />",
                        b_x, b_y, bar_w, b_h, fill)
  }
  
  sprintf("<svg width='%d' height='%d' viewBox='0 0 %d %d' style='display:inline-block; vertical-align:middle;'>%s</svg>",
          width, height, width, height, paste(rects, collapse = ""))
}

# Visual Missingness progress bar (filled in proportion to Missingness percentage)
generate_missing_bar <- function(pct) {
  fill_color <- if (pct == 0) "#34C759" else if (pct < 10) "#0071E3" else if (pct < 50) "#FF9500" else "#FF3B30"
  bar_width <- max(0, min(100, pct))
  sprintf(
    "<div style='display:flex; align-items:center; gap:8px;'>
       <div style='flex:1; background:var(--app-bar-track, #E5E5EA); height:6px; border-radius:980px; overflow:hidden; min-width:45px;'>
         <div style='background:%s; width:%.1f%%; height:100%%; border-radius:980px;'></div>
       </div>
       <span style='font-size:11px; font-variant-numeric:tabular-nums; color:var(--app-text, #1D1D1F); min-width:38px; text-align:right;'>%.1f%%</span>
     </div>",
    fill_color, bar_width, pct
  )
}

# Bivariate scatter plot SVG with linear regression trend (Phase 3)
generate_svg_bivariate <- function(x, y, x_name, y_name, width = 640, height = 240) {
  valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  xv <- as.numeric(x[valid])
  yv <- as.numeric(y[valid])
  n_pts <- length(xv)
  if (n_pts < 3) {
    return("<div style='padding:24px; color:var(--app-text-secondary); text-align:center;'>Insufficient valid numeric points to construct scatter preview.</div>")
  }
  
  # Sample if dataset is large to maintain instant SVG rendering speed
  if (n_pts > 600) {
    set.seed(42)
    idx <- sample(n_pts, 600)
    xv_sub <- xv[idx]
    yv_sub <- yv[idx]
  } else {
    xv_sub <- xv
    yv_sub <- yv
  }
  
  pad_l <- 65
  pad_r <- 25
  pad_t <- 20
  pad_b <- 35
  plot_w <- width - pad_l - pad_r
  plot_h <- height - pad_t - pad_b
  
  min_x <- min(xv)
  max_x <- max(xv)
  min_y <- min(yv)
  max_y <- max(yv)
  if (max_x == min_x) max_x <- min_x + 1
  if (max_y == min_y) max_y <- min_y + 1
  
  scale_x <- function(v) pad_l + ((v - min_x) / (max_x - min_x)) * plot_w
  scale_y <- function(v) pad_t + plot_h - ((v - min_y) / (max_y - min_y)) * plot_h
  
  # Regression line
  fit <- tryCatch(lm(yv ~ xv), error = function(e) NULL)
  line_svg <- ""
  if (!is.null(fit)) {
    cf <- coef(fit)
    if (!any(is.na(cf))) {
      x_start <- min_x
      x_end <- max_x
      y_start <- cf[1] + cf[2] * x_start
      y_end <- cf[1] + cf[2] * x_end
      
      px1 <- scale_x(x_start)
      py1 <- scale_y(y_start)
      px2 <- scale_x(x_end)
      py2 <- scale_y(y_end)
      line_svg <- sprintf("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#FF9500' stroke-width='2.5' stroke-linecap='round' />",
                          px1, py1, px2, py2)
    }
  }
  
  # Dots
  cx <- scale_x(xv_sub)
  cy <- scale_y(yv_sub)
  dots <- sprintf("<circle cx='%.1f' cy='%.1f' r='3.2' fill='#0071E3' fill-opacity='0.45' />", cx, cy)
  
  # Grid lines & Axis labels
  grid_svg <- sprintf(
    "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='var(--app-border-strong)' stroke-width='1' />
     <line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='var(--app-border-strong)' stroke-width='1' />
     <text x='%.1f' y='%.1f' font-size='10' fill='var(--app-text-secondary)' text-anchor='middle'>%.2f</text>
     <text x='%.1f' y='%.1f' font-size='10' fill='var(--app-text-secondary)' text-anchor='middle'>%.2f</text>
     <text x='%.1f' y='%.1f' font-size='10' fill='var(--app-text-secondary)' text-anchor='end'>%.2f</text>
     <text x='%.1f' y='%.1f' font-size='10' fill='var(--app-text-secondary)' text-anchor='end'>%.2f</text>
     <text x='%.1f' y='%.1f' font-size='11' font-weight='600' fill='var(--app-text)' text-anchor='middle'>%s</text>
     <text x='15' y='%.1f' font-size='11' font-weight='600' fill='var(--app-text)' text-anchor='middle' transform='rotate(-90, 15, %.1f)'>%s</text>",
    pad_l, pad_t + plot_h, pad_l + plot_w, pad_t + plot_h,
    pad_l, pad_t, pad_l, pad_t + plot_h,
    pad_l, height - 10, min_x,
    pad_l + plot_w, height - 10, max_x,
    pad_l - 8, pad_t + plot_h + 4, min_y,
    pad_l - 8, pad_t + 10, max_y,
    pad_l + plot_w / 2, height - 2, x_name,
    pad_t + plot_h / 2, pad_t + plot_h / 2, y_name
  )
  
  sprintf(
    "<svg width='100%%' height='%d' viewBox='0 0 %d %d' style='max-width:%dpx; overflow:visible; display:block; margin:0 auto;'>%s%s%s</svg>",
    height, width, height, width, grid_svg, paste(dots, collapse = ""), line_svg
  )
}

# Skeleton table placeholder UI generator
render_skeleton_table <- function(rows = 5, cols = 4, message = "Computing statistics...") {
  col_widths <- c("32%", "18%", "25%", "25%", "15%", "15%", "12%", "18%")
  row_nodes <- lapply(seq_len(rows), function(i) {
    cells <- lapply(seq_len(cols), function(j) {
      w <- if (j <= length(col_widths)) col_widths[j] else "20%"
      tags$div(class = "skeleton-shimmer skeleton-line", style = sprintf("width: %s;", w))
    })
    tags$div(class = "skeleton-row", cells)
  })
  
  tags$div(
    class = "skeleton-table-wrap",
    tags$div(
      style = "display: flex; align-items: center; gap: 8px; margin-bottom: 14px; font-size: 13px; color: var(--app-text-secondary);",
      tags$div(class = "skeleton-shimmer", style = "width: 14px; height: 14px; border-radius: 50%;"),
      tags$span(style = "font-weight: 500;", message)
    ),
    row_nodes
  )
}

# ==============================================================================
# 3. DATA QUALITY & HYGIENE SCREENING ENGINE
# ==============================================================================

check_hygiene_flags <- function(df, col_inv, num_summary = NULL, duplicates_n = NULL) {
  flags <- list()
  n_rows <- nrow(df)
  
  # 1. Zero Variance (Constant Columns)
  const_cols <- col_inv$Column_Name[col_inv$Distinct_N <= 1]
  if (length(const_cols) > 0) {
    flags <- c(flags, list(list(
      level = "critical",
      title = "Zero-Variance Feature",
      color = "#FF3B30",
      bg    = "rgba(255, 59, 48, 0.08)",
      desc  = sprintf("%d column(s) contain only 1 unique value (zero statistical variance): %s",
                      length(const_cols), paste(head(const_cols, 4), collapse = ", "))
    )))
  }
  
  # 2. Severe Missingness (> 50%)
  severe_miss <- col_inv[col_inv$Missing_Pct > 50, , drop = FALSE]
  if (nrow(severe_miss) > 0) {
    flags <- c(flags, list(list(
      level = "warning",
      title = "Severe Missingness (>50%)",
      color = "#FF9500",
      bg    = "rgba(255, 149, 0, 0.08)",
      desc  = sprintf("%d column(s) have more than half of records unrecorded: %s",
                      nrow(severe_miss), paste(head(paste0(severe_miss$Column_Name, " (", severe_miss$Missing_Pct, "%)"), 4), collapse = ", "))
    )))
  }
  
  # 3. High-Cardinality Text (Potential Leak / Dirty ID)
  high_card <- col_inv[col_inv$Data_Type %in% c("Text Variable", "Categorical String") & 
                       col_inv$Distinct_N > (n_rows * 0.8) & 
                       col_inv$Distinct_N != n_rows, , drop = FALSE]
  if (nrow(high_card) > 0) {
    flags <- c(flags, list(list(
      level = "info",
      title = "High-Cardinality Text",
      color = "#0071E3",
      bg    = "rgba(0, 113, 227, 0.08)",
      desc  = sprintf("%d text variable(s) have very high cardinality (>80%% unique): %s",
                      nrow(high_card), paste(head(high_card$Column_Name, 3), collapse = ", "))
    )))
  }
  
  # 4. Zero-Inflated Continuous Features
  if (!is.null(num_summary) && nrow(num_summary) > 0) {
    zero_inf <- num_summary[num_summary$Zero_Count > (n_rows * 0.40), , drop = FALSE]
    if (nrow(zero_inf) > 0) {
      flags <- c(flags, list(list(
        level = "notice",
        title = "Zero-Inflated Continuous",
        color = "#AF52DE",
        bg    = "rgba(175, 82, 222, 0.08)",
        desc  = sprintf("%d continuous feature(s) contain over 40%% exact zero values: %s",
                        nrow(zero_inf), paste(head(paste0(zero_inf$Variable, " (", round(100 * zero_inf$Zero_Count / n_rows, 1), "%)"), 3), collapse = ", "))
      )))
    }
  }
  
  # 5. Duplicate Records (reuse precalculated count to avoid O(N*M) re-scanning)
  dups <- if (!is.null(duplicates_n)) duplicates_n else sum(duplicated(df))
  if (dups > 0) {
    flags <- c(flags, list(list(
      level = "warning",
      title = "Duplicate Rows Detected",
      color = "#FF9500",
      bg    = "rgba(255, 149, 0, 0.08)",
      desc  = sprintf("Found %s exact duplicate row(s) (%.2f%% of cohort).",
                      format(dups, big.mark = ","), 100 * dups / n_rows)
    )))
  }
  
  # 6. Clean Bill of Health
  if (length(flags) == 0) {
    flags <- list(list(
      level = "success",
      title = "Data Quality Verified",
      color = "#34C759",
      bg    = "rgba(52, 199, 89, 0.08)",
      desc  = "All screening checks passed. Zero constant columns, no severe missingness (>50%), and zero duplicate rows."
    ))
  }
  
  flags
}

# ==============================================================================
# 4. UNIVERSAL DATASET PROFILING ENGINE (LAZY EVALUATION CAPABLE)
# ==============================================================================

# Modular compute helpers for lazy evaluation
compute_numeric_profile <- function(df_proc, num_cols) {
  if (length(num_cols) == 0) return(data.frame())
  data.table::rbindlist(lapply(num_cols, function(v) {
    vals <- df_proc[[v]]
    vals_c <- vals[!is.na(vals)]
    if (length(vals_c) == 0) {
      return(list(
        Variable = v, Distribution_SVG = "<span style='color:#86868B;'>-</span>",
        Mean = NA_real_, SD = NA_real_, Min = NA_real_,
        Q1 = NA_real_, Median = NA_real_, Q3 = NA_real_, Max = NA_real_, IQR = NA_real_,
        Skewness = NA_real_,
        Outliers_N = 0L, Outliers_Pct = 0.0,
        Reporting_Hint = "Insufficient data", Zero_Count = 0L
      ))
    }
    q <- unname(quantile(vals_c, probs = c(0.25, 0.50, 0.75), na.rm = TRUE))
    sk <- calc_skewness(vals_c)
    out <- calc_outliers(vals_c)
    
    hint_str <- if (is.na(sk)) {
      "Unknown"
    } else if (abs(sk) <= 0.5) {
      "Parametric: Mean (SD)"
    } else if (abs(sk) <= 1.0) {
      "Mild Skew: Mean (SD)"
    } else {
      "Skewed: Median [IQR]"
    }
    
    list(
      Variable         = v,
      Distribution_SVG = generate_svg_sparkline(vals_c),
      Mean             = round(mean(vals_c), 2),
      SD               = round(sd(vals_c), 2),
      Min              = round(min(vals_c), 2),
      Q1               = round(q[1], 2),
      Median           = round(q[2], 2),
      Q3               = round(q[3], 2),
      Max              = round(max(vals_c), 2),
      IQR              = round(q[3] - q[1], 2),
      Skewness         = sk,
      Outliers_N       = as.integer(out["count"]),
      Outliers_Pct     = as.numeric(out["pct"]),
      Reporting_Hint   = hint_str,
      Zero_Count       = as.integer(sum(vals_c == 0))
    )
  }))
}

compute_categorical_profile <- function(df_proc, cat_cols, n_rows) {
  if (length(cat_cols) == 0) return(data.frame())
  data.table::rbindlist(lapply(cat_cols, function(v) {
    vals <- as.character(df_proc[[v]])
    valid <- vals[!is.na(vals) & vals != ""]
    if (length(valid) == 0) {
      return(list(
        Variable       = v,
        Total_Levels   = 0L,
        Top_Categories = "-"
      ))
    }
    # Fast multi-threaded tally with data.table
    dt_v <- data.table::data.table(val = valid)
    tab <- dt_v[, .N, by = val][order(-N)]
    top_k <- head(tab, 5)
    k_str <- paste(sprintf("%s: %s (%.1f%%)", top_k$val, format(top_k$N, big.mark = ","), 100 * top_k$N / max(1, n_rows)), collapse = "; ")
    if (nrow(tab) > 5) k_str <- paste0(k_str, sprintf(" [and %d more levels]", nrow(tab) - 5))
    list(
      Variable       = v,
      Total_Levels   = nrow(tab),
      Top_Categories = k_str
    )
  }))
}

# Dimensionality guard: Limits correlation matrix to max_vars (default 35) by highest variance
compute_cor_matrix <- function(df_proc, num_cols, max_vars = 35) {
  if (length(num_cols) < 2) {
    return(list(matrix = NULL, total_vars = 0, is_truncated = FALSE, displayed_n = 0))
  }
  
  valid_cols <- num_cols[vapply(num_cols, function(col) length(unique(df_proc[[col]][!is.na(df_proc[[col]])])) > 1, logical(1))]
  total_vars <- length(valid_cols)
  if (total_vars < 2) {
    return(list(matrix = NULL, total_vars = total_vars, is_truncated = FALSE, displayed_n = total_vars))
  }
  
  is_truncated <- FALSE
  if (total_vars > max_vars) {
    is_truncated <- TRUE
    vars_var <- vapply(valid_cols, function(col) {
      v <- as.numeric(df_proc[[col]])
      var(v[!is.na(v)])
    }, numeric(1))
    valid_cols <- names(sort(vars_var, decreasing = TRUE))[seq_len(max_vars)]
  }
  
  c_mat <- round(cor(df_proc[, valid_cols, drop = FALSE], use = "pairwise.complete.obs"), 2)
  list(matrix = c_mat, total_vars = total_vars, is_truncated = is_truncated, displayed_n = length(valid_cols))
}

profile_dataset <- function(df, type_overrides = list(), lazy = FALSE) {
  n_rows  <- nrow(df)
  n_cols  <- ncol(df)
  
  # Work on a working copy of df for downstream type conversion
  df_proc <- df
  # Unpack haven_labelled vectors (SPSS / Stata) to base types
  for (nm in names(df_proc)) {
    x_col <- df_proc[[nm]]
    if (inherits(x_col, "haven_labelled")) {
      df_proc[[nm]] <- tryCatch(as.vector(x_col), error = function(e) as.character(x_col))
    }
  }
  
  if (length(type_overrides) > 0) {
    for (col_name in names(type_overrides)) {
      if (col_name %in% names(df_proc)) {
        target_t <- type_overrides[[col_name]]
        if (target_t == "Continuous Numeric") {
          df_proc[[col_name]] <- suppressWarnings(as.numeric(as.character(df_proc[[col_name]])))
        } else if (target_t %in% c("Categorical String", "Discrete / Categorical", "Factor / Categorical", "Identifier (ID)", "Text Variable")) {
          df_proc[[col_name]] <- as.character(df_proc[[col_name]])
        } else if (target_t == "Date / Time") {
          df_proc[[col_name]] <- suppressWarnings(as.Date(df_proc[[col_name]]))
        }
      }
    }
  }
  
  # Fast duplicate count (sample if massive > 200,000 rows to prevent freeze)
  n_duplicates <- if (n_rows > 200000) {
    set.seed(42)
    s_idx <- sample(n_rows, 50000)
    round(sum(duplicated(df_proc[s_idx, , drop = FALSE])) * (n_rows / 50000))
  } else {
    sum(duplicated(df_proc))
  }
  
  n_cells      <- n_rows * n_cols
  n_missing    <- sum(sapply(df_proc, function(x) sum(is.na(x) | x == "")))
  missing_rate <- if (n_cells > 0) round((n_missing / n_cells) * 100, 2) else 0.0
  
  # Column inventory table with Data type diagnosis & visual missing progress bar
  col_inv <- data.frame(
    Index        = seq_len(n_cols),
    Column_Name  = names(df_proc),
    Data_Type    = sapply(names(df_proc), function(nm) {
      if (!is.null(type_overrides[[nm]])) {
        return(type_overrides[[nm]])
      }
      x <- df_proc[[nm]]
      if (inherits(x, c("Date", "POSIXt"))) {
        "Date / Time"
      } else if (is.numeric(x)) {
        if (all(x == floor(x), na.rm = TRUE) && data.table::uniqueN(x, na.rm = TRUE) <= 6) {
          "Discrete / Categorical"
        } else if (grepl("id$|_id$|^id$", nm, ignore.case = TRUE) && data.table::uniqueN(x, na.rm = TRUE) > 50) {
          "Identifier (ID)"
        } else {
          "Continuous Numeric"
        }
      } else if (is.factor(x) || is.logical(x)) {
        "Factor / Categorical"
      } else if (is.character(x)) {
        if (data.table::uniqueN(x, na.rm = TRUE) <= 25) {
          "Categorical String"
        } else if (data.table::uniqueN(x, na.rm = TRUE) == n_rows) {
          "Unique Key / ID"
        } else {
          "Text Variable"
        }
      } else {
        class(x)[1]
      }
    }),
    Is_Overridden = names(df_proc) %in% names(type_overrides),
    Complete_N   = sapply(df_proc, function(x) sum(!is.na(x) & x != "")),
    Missing_N    = sapply(df_proc, function(x) sum(is.na(x) | x == "")),
    Missing_Pct  = round(100 * sapply(df_proc, function(x) sum(is.na(x) | x == "")) / max(1, n_rows), 1),
    Distinct_N   = sapply(df_proc, function(x) data.table::uniqueN(x, na.rm = TRUE)),
    Sample_Value = sapply(df_proc, function(x) {
      valid <- x[!is.na(x) & x != ""]
      if (length(valid) == 0) return("-")
      val <- as.character(valid[1])
      if (nchar(val) > 28) paste0(substr(val, 1, 25), "...") else val
    }),
    stringsAsFactors = FALSE
  )
  col_inv$Missing_Bar <- sapply(col_inv$Missing_Pct, generate_missing_bar)
  
  # Identify numeric & categorical column candidates
  non_num_types <- c("Identifier (ID)", "Unique Key / ID", "Categorical String", "Text Variable", "Discrete / Categorical", "Date / Time")
  excluded_from_num <- col_inv$Column_Name[col_inv$Data_Type %in% non_num_types]
  num_cols <- names(df_proc)[sapply(df_proc, is.numeric)]
  num_cols <- setdiff(num_cols, excluded_from_num)
  
  non_cat_types <- c("Identifier (ID)", "Unique Key / ID", "Continuous Numeric", "Date / Time")
  cat_candidates <- col_inv$Column_Name[!col_inv$Data_Type %in% non_cat_types]
  cat_cols <- if (length(cat_candidates) > 0) {
    cat_candidates[vapply(cat_candidates, function(v) {
      x <- df_proc[[v]]
      is.character(x) || is.factor(x) || is.logical(x) || (is.numeric(x) && data.table::uniqueN(x) <= 10)
    }, FUN.VALUE = logical(1))]
  } else {
    character(0)
  }
  
  num_summary <- if (!lazy) compute_numeric_profile(df_proc, num_cols) else NULL
  cat_summary <- if (!lazy) compute_categorical_profile(df_proc, cat_cols, n_rows) else NULL
  cor_res     <- if (!lazy) compute_cor_matrix(df_proc, num_cols) else NULL
  
  # Data Quality & Hygiene Screening
  hygiene_flags <- check_hygiene_flags(df_proc, col_inv, num_summary, duplicates_n = n_duplicates)
  
  full_rows <- if (!is.null(attr(df, "full_n_rows"))) attr(df, "full_n_rows") else n_rows
  is_sampled <- isTRUE(attr(df, "is_sampled")) || (full_rows > n_rows)
  
  list(
    rows          = n_rows,
    full_rows     = full_rows,
    is_sampled    = is_sampled,
    cols          = n_cols,
    duplicates    = n_duplicates,
    missing_rate  = missing_rate,
    memory        = format_fast_memory_size(df_proc),
    inventory     = col_inv,
    num_cols      = num_cols,
    cat_cols      = cat_cols,
    numeric       = num_summary,
    categorical   = cat_summary,
    cor_matrix    = if (!is.null(cor_res)) cor_res$matrix else NULL,
    cor_meta      = cor_res,
    hygiene_flags = hygiene_flags,
    is_fasta      = isTRUE(attr(df, "is_fasta")),
    fasta_db_counts = attr(df, "fasta_db_counts"),
    skip_rows     = if (!is.null(attr(df, "skip_rows"))) attr(df, "skip_rows") else 0
  )
}

# Helper to lazily hydrate full profile before generating text or HTML reports
ensure_full_profile <- function(df, p) {
  if (is.null(p$numeric)) {
    p$numeric <- compute_numeric_profile(df, p$num_cols)
  }
  if (is.null(p$categorical)) {
    p$categorical <- compute_categorical_profile(df, p$cat_cols, p$rows)
  }
  if (is.null(p$cor_matrix)) {
    cor_res <- compute_cor_matrix(df, p$num_cols)
    p$cor_matrix <- cor_res$matrix
    p$cor_meta   <- cor_res
  }
  p
}

# ==============================================================================
# 5. STANDALONE HTML REPORT & TEXT REPORT GENERATORS (P1, P2 & P3)
# ==============================================================================

# 1. Plain-text summary report
generate_text_report <- function(file_name, p, df = NULL) {
  if (is.null(p$numeric) && !is.null(df)) {
    p <- ensure_full_profile(df, p)
  }
  lines <- character()
  add_l <- function(...) lines <<- c(lines, sprintf(...))
  add_box <- function(title) {
    add_l("================================================================================")
    add_l(title)
    add_l("================================================================================")
  }
  
  add_box(sprintf("SCIDATAVIEW - DATA PROFILE SUMMARY: %s", basename(file_name)))
  add_l("Generated: %s | Application: SciDataView", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add_l("")
  
  # SECTION 1: OVERVIEW METRICS
  obs_str <- if (isTRUE(p$is_sampled) && !is.null(p$full_rows)) {
    sprintf("%s (Sampled from %s total rows)", format(p$rows, big.mark = ","), format(p$full_rows, big.mark = ","))
  } else {
    format(p$rows, big.mark = ",")
  }
  add_l("Total Rows                : %s", obs_str)
  add_l("Total Columns             : %s", format(p$cols, big.mark = ","))
  add_l("In-Memory Footprint       : %s", p$memory)
  add_l("Total Duplicate Rows      : %s", format(p$duplicates, big.mark = ","))
  add_l("Overall Missing Cell Rate : %.2f%%", p$missing_rate)
  if (isTRUE(p$skip_rows > 0)) {
    add_l("Skipped Title Rows        : %d (Header at row %d)", p$skip_rows, p$skip_rows + 1)
  }
  add_l("")
  
  # FASTA DATABASE BREAKDOWN (IF FASTA)
  if (isTRUE(p$is_fasta) && !is.null(p$fasta_db_counts)) {
    add_box("FASTA SEQUENCE DATABASE BREAKDOWN")
    add_l("%-25s %-15s %-10s", "Database Source", "Sequences", "Percentage")
    add_l("--------------------------------------------------------------------------------")
    for (i in seq_len(nrow(p$fasta_db_counts))) {
      r <- p$fasta_db_counts[i, ]
      add_l("%-25s %-15s %-9.2f%%", r$Database, format(r$Sequences, big.mark = ","), r$Percentage)
    }
    add_l("")
  }
  
  if (!isTRUE(p$is_fasta)) {
    # SECTION 2: DATA QUALITY FLAGS
    add_box("SECTION 2: DATA QUALITY & HYGIENE SCREENING")
    for (flag in p$hygiene_flags) {
      add_l("[%s] %s: %s", toupper(flag$level), flag$title, flag$desc)
    }
    add_l("")
    
    # SECTION 3: COLUMN INVENTORY
    add_box("SECTION 3: COLUMN INVENTORY & CLASSIFICATION")
    add_l("%-4s %-25s %-20s %-10s %-10s %-8s %-12s", "Idx", "Column Name", "Data type", "Complete", "Missing", "Miss %", "Distinct")
    add_l("--------------------------------------------------------------------------------")
    for (i in seq_len(nrow(p$inventory))) {
      r <- p$inventory[i, ]
      add_l("%-4d %-25s %-20s %-10s %-10s %-8.1f %-12s",
            r$Index, substr(r$Column_Name, 1, 24), substr(r$Data_Type, 1, 19),
            format(r$Complete_N, big.mark = ","), format(r$Missing_N, big.mark = ","),
            r$Missing_Pct, format(r$Distinct_N, big.mark = ","))
    }
    add_l("")
    
    # SECTION 4: NUMERIC DISTRIBUTIONS & MOMENTS (P2)
    if (nrow(p$numeric) > 0) {
      add_box("SECTION 4: NUMERIC DISTRIBUTIONS, SKEWNESS & OUTLIERS")
      add_l("%-20s %-16s %-16s %-7s %-12s", "Variable", "Mean (SD)", "Median [IQR]", "Skew", "Outliers")
      add_l("--------------------------------------------------------------------------------")
      for (i in seq_len(nrow(p$numeric))) {
        nm <- p$numeric[i, ]
        msd <- sprintf("%.1f (%.1f)", nm$Mean, nm$SD)
        miqr <- sprintf("%.1f [%.1f]", nm$Median, nm$IQR)
        out_str <- sprintf("%d (%.1f%%)", nm$Outliers_N, nm$Outliers_Pct)
        add_l("%-20s %-16s %-16s %-7.2f %-12s",
              substr(nm$Variable, 1, 19), msd, miqr, nm$Skewness, out_str)
      }
      add_l("")
    }
    
    # SECTION 5: CATEGORICAL DISTRIBUTIONS
    if (nrow(p$categorical) > 0) {
      add_box("SECTION 5: CATEGORICAL & DISCRETE DISTRIBUTIONS")
      for (i in seq_len(nrow(p$categorical))) {
        c_row <- p$categorical[i, ]
        add_l("Variable: %s (Levels: %d)", c_row$Variable, c_row$Total_Levels)
        add_l("  Distribution: %s", c_row$Top_Categories)
        add_l("")
      }
    }
  }
  
  add_l("================================================================================")
  add_l("END OF SCIDATAVIEW SUMMARY REPORT")
  add_l("================================================================================")
  
  paste(lines, collapse = "\n")
}

# 2. Standalone Zero-Dependency HTML Report (Phase P1, P2 & P3)
generate_html_report <- function(file_name, p, df = NULL) {
  if (is.null(p$numeric) && !is.null(df)) {
    p <- ensure_full_profile(df, p)
  }
  # Build hygiene alerts HTML
  hygiene_html <- paste(sapply(p$hygiene_flags, function(f) {
    sprintf(
      "<div style='display:flex; align-items:flex-start; gap:12px; background:%s; border-radius:12px; padding:12px 16px; margin-bottom:8px;'>
         <div style='font-size:12px; font-weight:700; color:%s; min-width:130px; text-transform:uppercase;'>%s</div>
         <div style='font-size:13px; color:#1D1D1F; line-height:1.4;'>%s</div>
       </div>",
      f$bg, f$color, f$title, f$desc
    )
  }), collapse = "\n")
  
  # Build inventory table rows HTML (with Missingness header & bar)
  inv_rows <- paste(sapply(seq_len(nrow(p$inventory)), function(i) {
    r <- p$inventory[i, ]
    type_badge <- if (isTRUE(r$Is_Overridden)) {
      sprintf("<span style='font-size:11px; font-weight:600; background:rgba(0,113,227,0.12); color:#0071E3; padding:3px 8px; border-radius:980px;'>%s (Edited)</span>", r$Data_Type)
    } else {
      sprintf("<span style='font-size:11px; font-weight:600; background:#E8E8ED; color:#444446; padding:3px 8px; border-radius:980px;'>%s</span>", r$Data_Type)
    }
    sprintf(
      "<tr>
         <td style='color:#86868B; text-align:center;'>%d</td>
         <td style='font-weight:600;'>%s</td>
         <td>%s</td>
         <td style='min-width:140px;'>%s</td>
         <td style='text-align:right;'>%s</td>
         <td style='text-align:right;'>%s</td>
         <td style='color:#636366;'>%s</td>
       </tr>",
      r$Index, r$Column_Name, type_badge, r$Missing_Bar,
      format(r$Complete_N, big.mark = ","), format(r$Distinct_N, big.mark = ","), r$Sample_Value
    )
  }), collapse = "\n")
  
  # Build numeric table rows HTML with Sparklines, Skewness & Outliers (P2)
  num_rows <- if (nrow(p$numeric) > 0) {
    paste(sapply(seq_len(nrow(p$numeric)), function(i) {
      r <- p$numeric[i, ]
      out_color <- if (r$Outliers_Pct > 5) "#FF3B30" else if (r$Outliers_Pct > 0) "#FF9500" else "#86868B"
      
      sprintf(
        "<tr>
           <td style='font-weight:600;'>%s</td>
           <td style='text-align:center;'>%s</td>
           <td><strong>%.1f</strong> (%.1f)</td>
           <td><strong>%.1f</strong> [%.1f]</td>
           <td>%.1f</td>
           <td>%.1f</td>
           <td style='font-feature-settings:\"tnum\";'>%.2f</td>
           <td style='color:%s; font-weight:600;'>%s (%.1f%%)</td>
         </tr>",
        r$Variable, r$Distribution_SVG, r$Mean, r$SD, r$Median, r$IQR, r$Min, r$Max,
        r$Skewness, out_color, format(r$Outliers_N, big.mark = ","), r$Outliers_Pct
      )
    }), collapse = "\n")
  } else {
    "<tr><td colspan='8' style='text-align:center; color:#86868B;'>No numeric features found.</td></tr>"
  }
  
  # Build categorical table rows HTML
  cat_rows <- if (nrow(p$categorical) > 0) {
    paste(sapply(seq_len(nrow(p$categorical)), function(i) {
      r <- p$categorical[i, ]
      sprintf(
        "<tr>
           <td style='font-weight:600;'>%s</td>
           <td style='text-align:center;'><span style='background:#E8E8ED; padding:3px 8px; border-radius:980px; font-weight:600; font-size:11px;'>%d</span></td>
           <td style='color:#333336;'>%s</td>
         </tr>",
        r$Variable, r$Total_Levels, r$Top_Categories
      )
    }), collapse = "\n")
  } else {
    "<tr><td colspan='3' style='text-align:center; color:#86868B;'>No categorical features found.</td></tr>"
  }
  
  # Build correlation matrix HTML
  cor_html <- if (!is.null(p$cor_matrix)) {
    c_mat <- p$cor_matrix
    vars <- colnames(c_mat)
    
    header_ths <- paste(sprintf("<th style='font-size:10px; padding:6px 8px; text-align:center;'>%s</th>", substr(vars, 1, 10)), collapse = "")
    
    matrix_rows <- paste(sapply(seq_along(vars), function(row_i) {
      cells <- sapply(seq_along(vars), function(col_j) {
        if (row_i == col_j) {
          return("<td style='background:#F5F5F7; color:#86868B; text-align:center; font-size:12px; font-weight:600;'>-</td>")
        }
        val <- c_mat[row_i, col_j]
        if (is.na(val)) return("<td style='background:#FAFAFC; color:#86868B; text-align:center;'>-</td>")
        
        intensity <- abs(val)
        bg_col <- if (val > 0) {
          sprintf("background:rgba(230, 75, 53, %.2f); color:%s;", intensity * 0.85, if(intensity > 0.5) "#FFF" else "#1D1D1F")
        } else {
          sprintf("background:rgba(0, 160, 135, %.2f); color:%s;", intensity * 0.85, if(intensity > 0.5) "#FFF" else "#1D1D1F")
        }
        
        strong_tag <- if (abs(val) >= 0.70) "font-weight:700; text-decoration:underline;" else ""
        sprintf("<td style='padding:6px 8px; text-align:center; font-size:11px; %s %s'>%.2f</td>", bg_col, strong_tag, val)
      })
      sprintf("<tr><td style='font-weight:600; font-size:11px; padding:6px 10px;'>%s</td>%s</tr>", vars[row_i], paste(cells, collapse = ""))
    }), collapse = "\n")
    
    sprintf(
      "<div style='overflow-x:auto;'>
         <table>
           <thead><tr><th>Feature</th>%s</tr></thead>
           <tbody>%s</tbody>
         </table>
         <div style='font-size:11px; color:#86868B; margin-top:8px;'>
           * Pearson correlation coefficient r [-1.0 to 1.0]. Diagonal indicates self-correlation (-). Color scale: Red (+1.0) and Teal (-1.0). Underlined bold entries indicate strong collinearity (|r| &ge; 0.70).
         </div>
       </div>",
      header_ths, matrix_rows
    )
  } else {
    "<div style='color:#86868B; padding:16px;'>Insufficient numeric columns to compute correlation matrix.</div>"
  }
  
  # Build FASTA database breakdown HTML (if FASTA format)
  fasta_html <- if (isTRUE(p$is_fasta) && !is.null(p$fasta_db_counts)) {
    db_c <- p$fasta_db_counts
    db_rows <- paste(sapply(seq_len(nrow(db_c)), function(i) {
      r <- db_c[i, ]
      sprintf(
        "<tr>
           <td style='font-weight:600;'>%s</td>
           <td style='text-align:right;'>%s</td>
           <td style='text-align:right;'>%.2f%%</td>
         </tr>",
        r$Database, format(r$Sequences, big.mark = ","), r$Percentage
      )
    }), collapse = "\n")
    
    sprintf(
      "<div class='card'>
         <div class='card-title'>FASTA Sequence Database Breakdown</div>
         <div style='overflow-x: auto;'>
           <table>
             <thead><tr><th>Database Source</th><th style='text-align:right;'>Sequences</th><th style='text-align:right;'>Percentage</th></tr></thead>
             <tbody>%s</tbody>
           </table>
         </div>
       </div>",
      db_rows
    )
  } else {
    ""
  }
  
  # Assemble HTML sections based on data type (FASTA vs Tabular)
  if (isTRUE(p$is_fasta)) {
    db_cnt <- if (!is.null(p$fasta_db_counts)) nrow(p$fasta_db_counts) else 0
    kpi_grid_html <- sprintf(
      '<div class="kpi-grid">
         <div class="kpi-card"><div class="kpi-label">Total Sequences (N)</div><div class="kpi-val">%s</div></div>
         <div class="kpi-card"><div class="kpi-label">Databases</div><div class="kpi-val">%d</div></div>
         <div class="kpi-card"><div class="kpi-label">Memory Footprint</div><div class="kpi-val">%s</div></div>
       </div>',
      format(p$rows, big.mark = ","), db_cnt, p$memory
    )
    body_cards_html <- fasta_html
  } else {
    kpi_grid_html <- sprintf(
      '<div class="kpi-grid">
         <div class="kpi-card"><div class="kpi-label">Rows</div><div class="kpi-val">%s</div></div>
         <div class="kpi-card"><div class="kpi-label">Columns</div><div class="kpi-val">%s</div></div>
         <div class="kpi-card"><div class="kpi-label">Memory Footprint</div><div class="kpi-val">%s</div></div>
         <div class="kpi-card"><div class="kpi-label">Missing Cell Rate</div><div class="kpi-val">%.2f%%</div></div>
         <div class="kpi-card"><div class="kpi-label">Duplicate Rows</div><div class="kpi-val">%s</div></div>
       </div>',
      format(p$rows, big.mark = ","), format(p$cols, big.mark = ","), p$memory, p$missing_rate, format(p$duplicates, big.mark = ",")
    )
    body_cards_html <- sprintf(
      '<!-- Column Inventory Table with Missingness Bars -->
    <div class="card">
      <div class="card-title">Column Inventory &amp; Missingness</div>
      <div style="overflow-x: auto;">
        <table>
          <thead>
            <tr>
              <th style="text-align:center;">#</th>
              <th>Column Name</th>
              <th>Data type</th>
              <th>Missingness</th>
              <th style="text-align:right;">Complete N</th>
              <th style="text-align:right;">Distinct</th>
              <th>Sample Value</th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>

    <!-- Numeric Distributions Table with Sparklines, Moments & Outliers (P2) -->
    <div class="card">
      <div class="card-title">Numeric Features, Distribution Sparklines &amp; Outliers</div>
      <div style="overflow-x: auto;">
        <table>
          <thead>
            <tr>
              <th>Variable</th>
              <th style="text-align:center; min-width:120px;">Distribution</th>
              <th>Mean (SD)</th>
              <th>Median [IQR]</th>
              <th>Min</th>
              <th>Max</th>
              <th>Skewness</th>
              <th>Outliers</th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>

    <!-- Categorical Breakdown Table -->
    <div class="card">
      <div class="card-title">Categorical &amp; Discrete Columns</div>
      <div style="overflow-x: auto;">
        <table>
          <thead>
            <tr>
              <th>Variable</th>
              <th style="text-align:center;">Total Levels</th>
              <th>Frequency Breakdown (Top Levels)</th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>

    <!-- Correlation Matrix (P3) -->
    <div class="card">
      <div class="card-title">Correlation Matrix (Pearson)</div>
      %s
    </div>',
      inv_rows, num_rows, cat_rows, cor_html
    )
  }
  
  # Assemble complete standalone HTML
  app_logo_svg <- get_app_icon_svg()
  if (nzchar(app_logo_svg)) {
    app_logo_svg <- sub('<svg ', '<svg width="38" height="38" ', app_logo_svg)
  }
  
  safe_name <- htmltools::htmlEscape(basename(file_name))
  sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>SciDataView Report: %s</title>
  <style>
    :root {
      --app-bg: #F5F5F7;
      --app-card: #FFFFFF;
      --app-blue: #0071E3;
      --app-text: #1D1D1F;
      --app-text-secondary: #86868B;
      --app-border: rgba(0, 0, 0, 0.08);
      --app-radius: 16px;
      --app-shadow: 0 4px 24px rgba(0, 0, 0, 0.04);
    }
    body {
      background: var(--app-bg);
      color: var(--app-text);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      margin: 0;
      padding: 32px 24px;
      -webkit-font-smoothing: antialiased;
    }
    .container { max-width: 1360px; margin: 0 auto; }
    .header {
      background: rgba(255, 255, 255, 0.9);
      backdrop-filter: blur(20px);
      border-radius: var(--app-radius);
      border: 1px solid var(--app-border);
      padding: 20px 24px;
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 24px;
      box-shadow: var(--app-shadow);
    }
    .kpi-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 16px;
      margin-bottom: 24px;
    }
    .kpi-card {
      background: var(--app-card);
      border-radius: 14px;
      border: 1px solid var(--app-border);
      padding: 16px 20px;
      box-shadow: var(--app-shadow);
    }
    .kpi-label { font-size: 11px; font-weight: 600; text-transform: uppercase; color: var(--app-text-secondary); letter-spacing: 0.05em; }
    .kpi-val { font-size: 26px; font-weight: 700; color: var(--app-text); margin: 4px 0 0; font-feature-settings: "tnum"; }
    .card {
      background: var(--app-card);
      border-radius: var(--app-radius);
      border: 1px solid var(--app-border);
      padding: 24px;
      box-shadow: var(--app-shadow);
      margin-bottom: 24px;
    }
    .card-title { font-size: 16px; font-weight: 600; margin: 0 0 16px; color: var(--app-text); }
    table { width: 100%%; border-collapse: collapse; font-size: 13px; text-align: left; }
    th {
      background: #F9FAFB;
      color: #4B5563;
      font-weight: 600;
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      padding: 10px 14px;
      border-bottom: 1px solid var(--app-border);
    }
    td {
      padding: 10px 14px;
      border-bottom: 1px solid rgba(0, 0, 0, 0.04);
      font-feature-settings: "tnum";
      vertical-align: middle;
    }
    tr:nth-child(even) { background: #FAFAFC; }
    tr:hover { background: rgba(0, 113, 227, 0.03); }
    .footer { text-align: center; font-size: 12px; color: var(--app-text-secondary); margin-top: 32px; }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <div style="display: flex; align-items: center; gap: 14px;">
        <div style="width: 38px; height: 38px; display: flex; align-items: center; justify-content: center; flex-shrink: 0; filter: drop-shadow(0 2px 6px rgba(0, 0, 0, 0.22));">
          %s
        </div>
        <div>
          <h1 style="font-size: 20px; margin: 0 0 4px; font-weight: 600;">SciDataView Report</h1>
          <div style="font-size: 13px; color: var(--app-text-secondary);">Source File: <strong>%s</strong> | Generated: %s%s</div>
        </div>
      </div>
    </div>

    <!-- Overview KPIs -->
    %s

    <!-- Report Body Cards -->
    %s

    <div class="footer">
      SciDataView Report | Generated: %s | Standalone Offline HTML
    </div>
  </div>
</body>
</html>',
    safe_name, app_logo_svg, safe_name, format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    if (isTRUE(p$skip_rows > 0)) sprintf(" | Skipped %d title row(s)", p$skip_rows) else "",
    kpi_grid_html, body_cards_html,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ==============================================================================
# 6. APPLICATION STYLING & USER INTERFACE (UI)
# ==============================================================================

app_css <- "
:root {
  --app-bg: #F5F5F7;
  --app-card: #FFFFFF;
  --app-navbar-bg: rgba(255, 255, 255, 0.85);
  --app-blue: #0071E3;
  --app-blue-hover: #0077ED;
  --app-blue-soft: rgba(0, 113, 227, 0.08);
  --app-text: #1D1D1F;
  --app-text-secondary: #86868B;
  --app-border: rgba(0, 0, 0, 0.06);
  --app-border-strong: rgba(0, 0, 0, 0.12);
  --app-radius-lg: 18px;
  --app-radius-md: 12px;
  --app-radius-pill: 980px;
  --app-shadow: 0 4px 24px rgba(0, 0, 0, 0.03), 0 1px 2px rgba(0, 0, 0, 0.02);
  --app-shadow-hover: 0 8px 30px rgba(0, 0, 0, 0.06);
  --app-table-th: #F9FAFB;
  --app-table-th-text: #4B5563;
  --app-table-odd: #FFFFFF;
  --app-table-even: #FAFAFC;
  --app-table-hover: rgba(0, 113, 227, 0.04);
  --app-tabs-bg: #EBEBED;
  --app-tabs-active-bg: #FFFFFF;
  --app-dropzone-bg: #FAFAFC;
  --app-dropzone-border: #D2D2D7;
  --app-btn-secondary-bg: #E8E8ED;
  --app-btn-secondary-text: #1D1D1F;
  --app-badge-bg: #E8E8ED;
  --app-badge-text: #444446;
  --app-input-bg: #FFFFFF;
  --app-subbox-bg: #F5F5F7;
  --app-bar-track: #E5E5EA;
}

[data-theme='dark'] {
  --app-bg: #121214;
  --app-card: #1C1C1E;
  --app-navbar-bg: rgba(28, 28, 30, 0.85);
  --app-blue: #0A84FF;
  --app-blue-hover: #409CFF;
  --app-blue-soft: rgba(10, 132, 255, 0.15);
  --app-text: #F5F5F7;
  --app-text-secondary: #98989D;
  --app-border: rgba(255, 255, 255, 0.08);
  --app-border-strong: rgba(255, 255, 255, 0.15);
  --app-shadow: 0 4px 24px rgba(0, 0, 0, 0.35), 0 1px 2px rgba(0, 0, 0, 0.2);
  --app-shadow-hover: 0 8px 30px rgba(0, 0, 0, 0.45);
  --app-table-th: #252528;
  --app-table-th-text: #A1A1A6;
  --app-table-odd: #1C1C1E;
  --app-table-even: #222225;
  --app-table-hover: rgba(10, 132, 255, 0.12);
  --app-tabs-bg: #2C2C2E;
  --app-tabs-active-bg: #3A3A3C;
  --app-dropzone-bg: #222225;
  --app-dropzone-border: #3A3A3C;
  --app-btn-secondary-bg: #2C2C2E;
  --app-btn-secondary-text: #F5F5F7;
  --app-badge-bg: #2C2C2E;
  --app-badge-text: #D1D1D6;
  --app-input-bg: #222225;
  --app-subbox-bg: #222225;
  --app-bar-track: #3A3A3C;
}

body {
  background-color: var(--app-bg) !important;
  color: var(--app-text) !important;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  -webkit-font-smoothing: antialiased;
  margin: 0;
  padding: 0;
  transition: background-color 0.2s ease, color 0.2s ease;
}

.app-navbar {
  position: sticky;
  top: 0;
  z-index: 1000;
  backdrop-filter: saturate(180%) blur(20px);
  -webkit-backdrop-filter: saturate(180%) blur(20px);
  background: var(--app-navbar-bg);
  border-bottom: 1px solid var(--app-border-strong);
  padding: 12px 32px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  transition: background 0.2s ease, border-color 0.2s ease;
}

.app-brand { display: flex; align-items: center; gap: 12px; }

.app-logo-badge {
  width: 32px;
  height: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  filter: drop-shadow(0 2px 6px rgba(0, 0, 0, 0.22));
}

.app-logo-badge img, .app-logo-badge svg {
  width: 100%;
  height: 100%;
  display: block;
}

.app-title-main { font-size: 18px; font-weight: 600; letter-spacing: -0.02em; color: var(--app-text); margin: 0; }

.app-content-wrap { max-width: 1440px; margin: 0 auto; padding: 24px 32px 48px; }

.app-card {
  background: var(--app-card);
  border-radius: var(--app-radius-lg);
  border: 1px solid var(--app-border);
  box-shadow: var(--app-shadow);
  padding: 24px;
  color: var(--app-text);
  transition: background 0.2s ease, border-color 0.2s ease;
}

/* Empty state vs active dataset view toggle */
body:not(.has-dataset) .app-dataset-view {
  display: none !important;
}

body.has-dataset .app-empty-view {
  display: none !important;
}

/* Tabular data vs FASTA data view toggle */
body.is-fasta .app-tabular-view {
  display: none !important;
}

body:not(.is-fasta) .app-fasta-view {
  display: none !important;
}

.fasta-summary-card {
  background: var(--app-card);
  border: 1px solid var(--app-border);
  border-radius: var(--app-radius-lg);
  padding: 16px 20px;
  margin-bottom: 20px;
  box-shadow: var(--app-shadow);
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.fasta-badge-grid {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  align-items: center;
}

.fasta-db-badge {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 6px 14px;
  border-radius: var(--app-radius-pill);
  font-size: 13px;
  font-weight: 500;
  border: 1px solid var(--app-border-strong);
  background: var(--app-subbox-bg);
  color: var(--app-text);
}

.fasta-db-badge.badge-sp {
  background: rgba(0, 113, 227, 0.08);
  border-color: rgba(0, 113, 227, 0.25);
  color: #0071E3;
}

.fasta-db-badge.badge-tr {
  background: rgba(255, 149, 0, 0.08);
  border-color: rgba(255, 149, 0, 0.25);
  color: #D97706;
}

[data-theme='dark'] .fasta-db-badge.badge-tr {
  color: #F59E0B;
}

.fasta-db-badge.badge-other {
  background: rgba(142, 142, 147, 0.08);
  border-color: rgba(142, 142, 147, 0.25);
  color: var(--app-text-secondary);
}

.fasta-count-pill {
  font-weight: 700;
  font-size: 13px;
  font-feature-settings: 'tnum';
}

.fasta-pct-pill {
  font-size: 11px;
  opacity: 0.8;
}

.app-dropzone {
  border: 2px dashed var(--app-dropzone-border);
  border-radius: var(--app-radius-md);
  padding: 20px 16px;
  text-align: center;
  background: var(--app-dropzone-bg);
  transition: all 0.2s ease;
  cursor: pointer;
}

.app-dropzone * {
  pointer-events: none;
}

.app-dropzone:hover,
.app-dropzone.dragover {
  border-color: var(--app-blue);
  background: var(--app-blue-soft);
}

.app-dropzone.drag-active {
  border-color: var(--app-blue);
  box-shadow: 0 0 0 3px var(--app-blue-soft);
}

.btn-app-primary {
  background: var(--app-blue) !important;
  color: white !important;
  border: none !important;
  border-radius: var(--app-radius-pill) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  padding: 8px 18px !important;
  letter-spacing: -0.01em !important;
  transition: all 0.2s ease !important;
}

.btn-app-primary:hover {
  background: var(--app-blue-hover) !important;
  transform: translateY(-1px);
}

.btn-app-secondary {
  background: var(--app-btn-secondary-bg) !important;
  color: var(--app-btn-secondary-text) !important;
  border: 1px solid var(--app-border) !important;
  border-radius: var(--app-radius-pill) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  padding: 8px 18px !important;
  transition: all 0.2s ease !important;
}

.btn-app-secondary:hover {
  background: var(--app-border-strong) !important;
}

.btn-clear-dataset {
  background: var(--app-btn-secondary-bg) !important;
  color: var(--app-btn-secondary-text) !important;
  border: 1px solid var(--app-border) !important;
  border-radius: var(--app-radius-pill) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  padding: 8px 18px !important;
  transition: all 0.2s ease !important;
  width: 100% !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  gap: 6px !important;
}

.btn-clear-dataset:hover {
  background: rgba(255, 59, 48, 0.08) !important;
  color: #FF3B30 !important;
  border-color: rgba(255, 59, 48, 0.35) !important;
}

.col-sidebar-ingestion {
  align-self: flex-start !important;
  position: sticky !important;
  top: 84px !important;
  z-index: 100;
}

.app-card-ingestion {
  display: flex !important;
  flex-direction: column !important;
  margin-bottom: 24px !important;
  transition: min-height 0.2s ease, background 0.2s ease, border-color 0.2s ease;
}

.app-card-ingestion .app-dropzone {
  flex: none !important;
  height: auto !important;
}

.app-file-meta-wrap {
  padding-top: 14px;
  border-top: 1px solid var(--app-border);
  font-size: 12px;
  color: #86868B;
}

body:not(.has-dataset) .app-card-ingestion {
  min-height: 520px;
}

body:not(.has-dataset) .app-file-meta-wrap {
  margin-top: auto !important;
}

body.has-dataset .app-card-ingestion {
  min-height: unset !important;
  height: auto !important;
}

body.has-dataset .app-file-meta-wrap {
  margin-top: 16px !important;
}


.btn-theme-toggle {
  background: var(--app-card);
  color: var(--app-text);
  border: 1px solid var(--app-border-strong);
  border-radius: var(--app-radius-pill);
  width: 36px;
  height: 36px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  padding: 0;
  transition: all 0.2s ease;
}

.btn-theme-toggle:hover {
  background: var(--app-blue-soft);
  color: var(--app-blue);
  border-color: var(--app-blue);
  transform: scale(1.05);
}

.app-kpi-grid { display: grid; grid-template-columns: repeat(5, 1fr); gap: 16px; margin-bottom: 24px; }
.app-kpi-card { background: var(--app-card); border-radius: var(--app-radius-md); border: 1px solid var(--app-border); padding: 18px 20px; box-shadow: var(--app-shadow); transition: background 0.2s ease, border-color 0.2s ease; }
.app-kpi-label { font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.06em; color: var(--app-text-secondary); margin-bottom: 6px; }
.app-kpi-val { font-size: 26px; font-weight: 700; color: var(--app-text); font-feature-settings: 'tnum'; margin: 0; }
.app-kpi-sub { font-size: 11px; color: var(--app-text-secondary); margin-top: 4px; }

.nav-tabs { border-bottom: none !important; background: var(--app-tabs-bg); border-radius: 12px; padding: 4px; display: inline-flex; gap: 4px; margin-bottom: 20px; transition: background 0.2s ease; }
.nav-tabs > li > a { border: none !important; border-radius: 9px !important; color: var(--app-text-secondary) !important; font-size: 13px !important; font-weight: 500 !important; padding: 7px 16px !important; background: transparent !important; }
.nav-tabs > li.active > a, .nav-tabs > li > a.active { background: var(--app-tabs-active-bg) !important; color: var(--app-text) !important; font-weight: 600 !important; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08) !important; }

.app-table-wrap { border-radius: var(--app-radius-md); border: 1px solid var(--app-border-strong); overflow-x: auto; background: var(--app-card); transition: background 0.2s ease, border-color 0.2s ease; }
.table { margin-bottom: 0 !important; font-size: 13px !important; color: var(--app-text) !important; }
.table > thead > tr > th { background: var(--app-table-th) !important; color: var(--app-table-th-text) !important; font-weight: 600 !important; font-size: 11px !important; text-transform: uppercase !important; letter-spacing: 0.05em !important; padding: 12px 16px !important; border-bottom: 1px solid var(--app-border-strong) !important; border-top: none !important; }
.table > tbody > tr > td { padding: 12px 16px !important; border-bottom: 1px solid var(--app-border) !important; border-top: none !important; font-feature-settings: 'tnum'; vertical-align: middle !important; color: var(--app-text) !important; }
.table > tbody > tr:nth-of-type(odd) { background-color: var(--app-table-odd) !important; }
.table > tbody > tr:nth-of-type(even) { background-color: var(--app-table-even) !important; }
.table > tbody > tr:hover { background-color: var(--app-table-hover) !important; }

.app-terminal { background: #1C1C1E; color: #98D0FF; border-radius: var(--app-radius-md); padding: 20px; font-family: 'SF Mono', Monaco, Menlo, Consolas, monospace !important; font-size: 12px; line-height: 1.5; max-height: 550px; overflow-y: auto; border: 1px solid rgba(255, 255, 255, 0.08); }

/* Form inputs & controls in Dark Mode */
[data-theme='dark'] .form-control,
[data-theme='dark'] .form-select,
[data-theme='dark'] input[type='text'],
[data-theme='dark'] input[type='number'],
[data-theme='dark'] .selectize-input {
  background-color: var(--app-input-bg) !important;
  color: var(--app-text) !important;
  border-color: var(--app-border-strong) !important;
}
[data-theme='dark'] .selectize-input input {
  color: var(--app-text) !important;
}
[data-theme='dark'] .selectize-dropdown {
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
  border: 1px solid var(--app-border-strong) !important;
}
[data-theme='dark'] .selectize-dropdown .active {
  background-color: var(--app-blue) !important;
  color: #FFFFFF !important;
}
[data-theme='dark'] .selectize-dropdown .option {
  color: var(--app-text) !important;
}
[data-theme='dark'] .selectize-dropdown .option:hover {
  background-color: var(--app-blue-soft) !important;
}
[data-theme='dark'] pre.shiny-text-output,
[data-theme='dark'] pre {
  background-color: var(--app-card) !important;
  color: #98D0FF !important;
  border: 1px solid var(--app-border-strong) !important;
}

/* Type Override Toolbar & Select Controls */
.type-override-toolbar {
  background: var(--app-subbox-bg);
  border: 1px solid var(--app-border);
  border-radius: 12px;
  padding: 10px 16px;
  margin-bottom: 18px;
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02);
}

.type-override-controls {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 10px;
  flex: 1;
}

.type-override-label {
  display: inline-flex;
  align-items: center;
  gap: 7px;
  font-weight: 600;
  font-size: 13px;
  color: var(--app-text);
  white-space: nowrap;
  height: 36px;
}

.type-override-label svg {
  color: var(--app-blue);
  flex-shrink: 0;
}

.type-override-arrow {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 36px;
  color: var(--app-text-secondary);
  flex-shrink: 0;
}

.type-override-select-wrap {
  display: inline-flex;
  align-items: center;
}

.type-override-toolbar .shiny-input-container {
  margin-bottom: 0 !important;
  padding: 0 !important;
  display: inline-flex !important;
  align-items: center !important;
  width: 100% !important;
}

.type-override-toolbar .shiny-input-container > div {
  width: 100% !important;
  display: inline-flex !important;
  align-items: center !important;
}

.type-override-toolbar select.shiny-input-select,
.type-override-toolbar select.form-control,
.type-override-toolbar select {
  height: 36px !important;
  min-height: 36px !important;
  padding: 6px 34px 6px 12px !important;
  border-radius: 8px !important;
  border: 1px solid var(--app-border-strong) !important;
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  line-height: 22px !important;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%2386868B' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\") !important;
  background-repeat: no-repeat !important;
  background-position: right 10px center !important;
  background-size: 12px 10px !important;
  appearance: none !important;
  -webkit-appearance: none !important;
  -moz-appearance: none !important;
  cursor: pointer !important;
  transition: all 0.15s ease !important;
}

.type-override-toolbar select:hover {
  border-color: var(--app-blue) !important;
}

.type-override-toolbar select:focus {
  border-color: var(--app-blue) !important;
  box-shadow: 0 0 0 3px var(--app-blue-soft) !important;
  outline: none !important;
}

.type-override-btn {
  height: 36px !important;
  padding: 0 16px !important;
  display: inline-flex !important;
  align-items: center !important;
  justify-content: center !important;
  font-size: 12px !important;
  font-weight: 600 !important;
  border-radius: 8px !important;
  white-space: nowrap !important;
  flex-shrink: 0 !important;
  line-height: 36px !important;
}

[data-theme='dark'] .type-override-toolbar select {
  background-color: var(--app-input-bg) !important;
  border-color: var(--app-border-strong) !important;
  color: var(--app-text) !important;
  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%2398989D' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\") !important;
}

[data-theme='dark'] .type-override-toolbar select option {
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
}

/* Excel Sheet Selector Toolbar (Styled to match Change Data Type) */
.sheet-selector-card {
  background: var(--app-subbox-bg);
  border: 1px solid var(--app-border);
  border-radius: 12px;
  padding: 10px 14px;
  margin-top: 14px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02);
}

.sheet-selector-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
}

.sheet-selector-label {
  display: inline-flex;
  align-items: center;
  gap: 7px;
  font-weight: 600;
  font-size: 13px;
  color: var(--app-text);
  white-space: nowrap;
}

.sheet-selector-label svg {
  color: #107C41;
  flex-shrink: 0;
}

.sheet-badge {
  font-size: 11px;
  font-weight: 600;
  padding: 2px 7px;
  border-radius: 6px;
  background: rgba(16, 124, 65, 0.10);
  color: #107C41;
  letter-spacing: 0.02em;
}

[data-theme='dark'] .sheet-badge {
  background: rgba(52, 199, 89, 0.16);
  color: #34C759;
}

[data-theme='dark'] .sheet-selector-label svg {
  color: #34C759;
}

.sheet-selector-select-wrap {
  width: 100%;
}

.sheet-selector-card .shiny-input-container {
  margin-bottom: 0 !important;
  padding: 0 !important;
  width: 100% !important;
}

.sheet-selector-card select.shiny-input-select,
.sheet-selector-card select.form-control,
.sheet-selector-card select {
  height: 36px !important;
  min-height: 36px !important;
  padding: 6px 34px 6px 12px !important;
  border-radius: 8px !important;
  border: 1px solid var(--app-border-strong) !important;
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  line-height: 22px !important;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%2386868B' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\") !important;
  background-repeat: no-repeat !important;
  background-position: right 10px center !important;
  background-size: 12px 10px !important;
  appearance: none !important;
  -webkit-appearance: none !important;
  -moz-appearance: none !important;
  cursor: pointer !important;
  transition: all 0.15s ease !important;
  width: 100% !important;
}

.sheet-selector-card select:hover {
  border-color: var(--app-blue) !important;
}

.sheet-selector-card select:focus {
  border-color: var(--app-blue) !important;
  box-shadow: 0 0 0 3px var(--app-blue-soft) !important;
  outline: none !important;
}

[data-theme='dark'] .sheet-selector-card select {
  background-color: var(--app-input-bg) !important;
  border-color: var(--app-border-strong) !important;
  color: var(--app-text) !important;
  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%2398989D' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\") !important;
}

[data-theme='dark'] .sheet-selector-card select option {
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
}

/* Skip Title Rows Card (Styled to match Change Data Type & Excel Sheet) */
.skip-rows-card {
  background: var(--app-subbox-bg);
  border: 1px solid var(--app-border);
  border-radius: 12px;
  padding: 10px 14px;
  margin-top: 12px;
  display: flex;
  flex-direction: column;
  gap: 8px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02);
}

.skip-rows-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
}

.skip-rows-label {
  display: inline-flex;
  align-items: center;
  gap: 7px;
  font-weight: 600;
  font-size: 13px;
  color: var(--app-text);
  white-space: nowrap;
}

.skip-rows-label svg {
  color: var(--app-blue);
  flex-shrink: 0;
}

.skip-badge {
  font-size: 11px;
  font-weight: 600;
  padding: 2px 7px;
  border-radius: 6px;
  background: rgba(134, 134, 139, 0.12);
  color: #86868B;
  letter-spacing: 0.02em;
}

.skip-badge.active {
  background: rgba(0, 113, 227, 0.10);
  color: #0071E3;
}

[data-theme='dark'] .skip-badge.active {
  background: rgba(10, 132, 255, 0.18);
  color: #0A84FF;
}

.skip-rows-input-wrap {
  width: 100%;
}

.skip-rows-card .shiny-input-container {
  margin-bottom: 0 !important;
  padding: 0 !important;
  width: 100% !important;
}

.skip-rows-card input.form-control,
.skip-rows-card input[type='number'] {
  height: 36px !important;
  min-height: 36px !important;
  padding: 6px 12px !important;
  border-radius: 8px !important;
  border: 1px solid var(--app-border-strong) !important;
  background-color: var(--app-card) !important;
  color: var(--app-text) !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  line-height: 22px !important;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04) !important;
  transition: all 0.15s ease !important;
  width: 100% !important;
}

.skip-rows-card input[type='number']:hover {
  border-color: var(--app-blue) !important;
}

.skip-rows-card input[type='number']:focus {
  border-color: var(--app-blue) !important;
  box-shadow: 0 0 0 3px var(--app-blue-soft) !important;
  outline: none !important;
}

[data-theme='dark'] .skip-rows-card input[type='number'] {
  background-color: var(--app-input-bg) !important;
  border-color: var(--app-border-strong) !important;
  color: var(--app-text) !important;
}

/* Real-time Busy / Processing Indicator */
.app-busy-indicator {
  display: none;
  align-items: center;
  gap: 7px;
  background: var(--app-card);
  border: 1px solid var(--app-border-strong);
  border-radius: var(--app-radius-pill);
  padding: 5px 12px;
  font-size: 11px;
  font-weight: 600;
  color: var(--app-blue);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
}
html.shiny-busy .app-busy-indicator {
  display: inline-flex !important;
}

.app-dropzone.is-loading {
  border-color: var(--app-blue) !important;
  background: rgba(0, 113, 227, 0.04) !important;
}


/* Interactive Correlation Matrix Cells */
.cell-cor-interactive {
  cursor: pointer !important;
  transition: transform 0.15s ease, box-shadow 0.15s ease !important;
}
.cell-cor-interactive:hover {
  outline: 2px solid var(--app-blue) !important;
  outline-offset: -2px !important;
  transform: scale(1.06) !important;
  z-index: 10 !important;
  position: relative !important;
}

/* Sortable Column Inventory Headers */
.sortable-th {
  cursor: pointer !important;
  user-select: none !important;
  transition: color 0.15s ease, background-color 0.15s ease !important;
}
.sortable-th:hover {
  color: var(--app-blue) !important;
  background-color: var(--app-blue-soft) !important;
}

/* Data Inspector Unified Search & Pagination Bar */
.inspect-toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 14px;
  flex-wrap: wrap;
  gap: 12px;
}

.inspect-search-wrap {
  position: relative;
  display: inline-flex;
  align-items: center;
  width: 280px;
}

.inspect-search-icon {
  position: absolute;
  left: 12px;
  width: 14px;
  height: 14px;
  color: var(--app-text-secondary);
  pointer-events: none;
  z-index: 2;
}

.inspect-search-wrap .form-group {
  margin-bottom: 0 !important;
  width: 100% !important;
}

.inspect-search-wrap input.form-control,
#tbl_search {
  height: 32px !important;
  min-height: 32px !important;
  padding: 0 14px 0 34px !important;
  background-color: var(--app-subbox-bg) !important;
  border: 1px solid var(--app-border-strong) !important;
  border-radius: var(--app-radius-pill) !important;
  color: var(--app-text) !important;
  font-size: 12px !important;
  font-weight: 500 !important;
  line-height: 30px !important;
  box-shadow: none !important;
  transition: all 0.2s ease !important;
}

.inspect-search-wrap input.form-control::placeholder,
#tbl_search::placeholder {
  color: var(--app-text-secondary) !important;
  opacity: 0.8 !important;
}

.inspect-search-wrap input.form-control:focus,
#tbl_search:focus {
  background-color: var(--app-card) !important;
  border-color: var(--app-blue) !important;
  box-shadow: 0 0 0 3px var(--app-blue-soft) !important;
  outline: none !important;
}

.inspect-pagination-group {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}

.btn-inspect-page {
  height: 32px !important;
  min-width: 32px !important;
  padding: 0 11px !important;
  display: inline-flex !important;
  align-items: center !important;
  justify-content: center !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  border-radius: var(--app-radius-pill) !important;
  background: var(--app-btn-secondary-bg) !important;
  color: var(--app-btn-secondary-text) !important;
  border: 1px solid var(--app-border-strong) !important;
  transition: all 0.15s ease !important;
}

.btn-inspect-page:hover:not([disabled]) {
  background: var(--app-blue-soft) !important;
  border-color: var(--app-blue) !important;
  color: var(--app-blue) !important;
}

.btn-inspect-page[disabled] {
  opacity: 0.4 !important;
  cursor: not-allowed !important;
}

.inspect-page-indicator {
  height: 32px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 0 12px;
  background: var(--app-subbox-bg);
  border: 1px solid var(--app-border);
  border-radius: var(--app-radius-pill);
  font-size: 12px;
  font-weight: 500;
  color: var(--app-text-secondary);
  white-space: nowrap;
}

/* Skeleton Shimmer Loaders */
@keyframes app-skeleton-shimmer {
  0% { background-position: -200% 0; }
  100% { background-position: 200% 0; }
}

.skeleton-shimmer {
  background: linear-gradient(
    90deg,
    var(--app-subbox-bg) 25%,
    var(--app-border) 37%,
    var(--app-subbox-bg) 63%
  );
  background-size: 400% 100%;
  animation: app-skeleton-shimmer 1.4s ease infinite;
  border-radius: 4px;
}

.skeleton-line {
  height: 14px;
  margin: 6px 0;
}

.skeleton-table-wrap {
  width: 100%;
  padding: 16px 20px;
  background: var(--app-card-bg);
  border-radius: var(--app-radius-md);
  border: 1px solid var(--app-border);
}

.skeleton-row {
  display: flex;
  gap: 16px;
  padding: 10px 0;
  border-bottom: 1px solid var(--app-border);
}

.skeleton-row:last-child {
  border-bottom: none;
}

/* Liquid Glass Notification Panel & Toasts */
#shiny-notification-panel {
  position: fixed !important;
  bottom: 24px !important;
  right: 24px !important;
  z-index: 999999 !important;
  width: 380px !important;
  max-width: calc(100vw - 48px) !important;
  display: flex !important;
  flex-direction: column !important;
  gap: 12px !important;
  margin: 0 !important;
  padding: 0 !important;
  background: transparent !important;
  pointer-events: none !important;
}

.shiny-notification {
  position: relative !important;
  pointer-events: auto !important;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  -webkit-font-smoothing: antialiased !important;
  background: rgba(255, 255, 255, 0.82) !important;
  backdrop-filter: saturate(180%) blur(20px) !important;
  -webkit-backdrop-filter: saturate(180%) blur(20px) !important;
  border: 1px solid rgba(255, 255, 255, 0.6) !important;
  border-left: 4px solid var(--app-blue) !important;
  border-radius: 14px !important;
  box-shadow: 0 12px 32px rgba(0, 0, 0, 0.12), 0 2px 6px rgba(0, 0, 0, 0.04), inset 0 1px 0 rgba(255, 255, 255, 0.6) !important;
  color: var(--app-text) !important;
  padding: 14px 18px !important;
  font-size: 13px !important;
  line-height: 1.45 !important;
  margin: 0 !important;
  box-sizing: border-box !important;
  transition: all 0.25s ease !important;
  animation: app-toast-enter 0.28s cubic-bezier(0.16, 1, 0.3, 1) forwards !important;
}

[data-theme='dark'] .shiny-notification {
  background: rgba(28, 28, 30, 0.82) !important;
  backdrop-filter: saturate(180%) blur(20px) !important;
  -webkit-backdrop-filter: saturate(180%) blur(20px) !important;
  border: 1px solid rgba(255, 255, 255, 0.12) !important;
  border-left: 4px solid var(--app-blue) !important;
  box-shadow: 0 14px 40px rgba(0, 0, 0, 0.45), 0 2px 8px rgba(0, 0, 0, 0.3), inset 0 1px 0 rgba(255, 255, 255, 0.08) !important;
  color: var(--app-text) !important;
}

.shiny-notification-warning {
  border-left-color: #FF9500 !important;
}

.shiny-notification-error {
  border-left-color: #FF3B30 !important;
}

.shiny-notification-message {
  border-left-color: var(--app-blue) !important;
}

.shiny-notification-close {
  position: absolute !important;
  top: 10px !important;
  right: 12px !important;
  color: var(--app-text-secondary) !important;
  opacity: 0.6 !important;
  font-size: 16px !important;
  font-weight: 400 !important;
  cursor: pointer !important;
  border: none !important;
  background: transparent !important;
  transition: opacity 0.15s ease, transform 0.15s ease !important;
  line-height: 1 !important;
}

.shiny-notification-close:hover {
  opacity: 1 !important;
  color: var(--app-text) !important;
  transform: scale(1.15) !important;
}

.shiny-notification-content {
  padding-right: 18px !important;
  font-family: inherit !important;
}

.shiny-notification .progress {
  height: 5px !important;
  background: rgba(0, 113, 227, 0.12) !important;
  border-radius: 980px !important;
  overflow: hidden !important;
  margin-top: 10px !important;
  margin-bottom: 2px !important;
  box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.04) !important;
}

[data-theme='dark'] .shiny-notification .progress {
  background: rgba(255, 255, 255, 0.1) !important;
}

.shiny-notification .progress-bar {
  background: linear-gradient(90deg, #0071E3, #409CFF) !important;
  border-radius: 980px !important;
  transition: width 0.3s ease !important;
}

[data-theme='dark'] .shiny-notification .progress-bar {
  background: linear-gradient(90deg, #0A84FF, #64D2FF) !important;
}

.shiny-progress-text {
  font-size: 13px !important;
  font-weight: 600 !important;
  color: var(--app-text) !important;
  font-family: inherit !important;
}

.shiny-progress-text .progress-detail {
  font-size: 12px !important;
  font-weight: 400 !important;
  color: var(--app-text-secondary) !important;
  margin-top: 2px !important;
}

@keyframes app-toast-enter {
  from {
    opacity: 0;
    transform: translateY(12px) scale(0.96);
  }
  to {
    opacity: 1;
    transform: translateY(0) scale(1);
  }
}
"

ui <- fluidPage(
  title = "SciDataView",
  theme = bslib::bs_theme(version = 5),
  tags$head(
    tags$title("SciDataView"),
    tags$link(rel = "icon", type = "image/x-icon", href = "icon/app.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "icon/app_icon.png"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "icon/app_icon.svg"),
    tags$style(HTML(app_css)),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML("
      (function() {
        function getTheme() {
          return localStorage.getItem('scidataview_theme') || 
            (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
        }
        function applyTheme(theme) {
          document.documentElement.setAttribute('data-theme', theme);
          document.documentElement.setAttribute('data-bs-theme', theme);
          if (document.body) {
            document.body.setAttribute('data-theme', theme);
            document.body.setAttribute('data-bs-theme', theme);
          }
          var moon = document.getElementById('theme_icon_moon');
          var sun = document.getElementById('theme_icon_sun');
          if (moon && sun) {
            if (theme === 'dark') {
              moon.style.display = 'none';
              sun.style.display = 'block';
            } else {
              moon.style.display = 'block';
              sun.style.display = 'none';
            }
          }
        }
        window.toggleDarkMode = function() {
          var current = document.documentElement.getAttribute('data-theme') || 'light';
          var next = current === 'dark' ? 'light' : 'dark';
          localStorage.setItem('scidataview_theme', next);
          applyTheme(next);
        };
        function syncIngestionHeight() {
          var ingestionCard = document.querySelector('.app-card-ingestion');
          if (!ingestionCard) return;
          if (document.body.classList.contains('has-dataset')) {
            ingestionCard.style.minHeight = '';
            return;
          }
          var tabsCard = document.querySelector('.app-content-wrap .col-sm-9 .app-card');
          if (tabsCard) {
            var tabsBottom = tabsCard.getBoundingClientRect().bottom;
            var ingTop = ingestionCard.getBoundingClientRect().top;
            var matchedHeight = Math.round(tabsBottom - ingTop);
            if (matchedHeight > 300) {
              ingestionCard.style.minHeight = matchedHeight + 'px';
            }
          }
        }
        window.syncIngestionHeight = syncIngestionHeight;
        window.addEventListener('resize', syncIngestionHeight);

        var originalDropzoneHtml = null;
        var uploadPollTimer = null;

        function formatBytes(bytes) {
          if (!bytes || bytes <= 0) return '0 B';
          var k = 1024;
          var sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
          var i = Math.floor(Math.log(bytes) / Math.log(k));
          return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i];
        }

        function showImmediateLoading(file) {
          if (!file) return;
          var fileName = file.name || 'Dataset';
          var fileSize = formatBytes(file.size);

          // 1. Update dropzone UI immediately
          var dropzone = document.getElementById('app_dropzone');
          if (dropzone) {
            if (!originalDropzoneHtml) {
              originalDropzoneHtml = dropzone.innerHTML;
            }
            dropzone.classList.add('is-loading');
            dropzone.innerHTML = 
              '<div class=\"spinner-border\" role=\"status\" style=\"width: 26px; height: 26px; border-width: 2.5px; color: var(--app-blue); margin-bottom: 8px;\"></div>' +
              '<div style=\"font-size: 13px; font-weight: 600; color: var(--app-text);\">Loading data...</div>' +
              '<div style=\"font-size: 11px; color: #0071E3; margin-top: 4px; font-weight: 500; word-break: break-all;\">' + fileName + ' (' + fileSize + ')</div>' +
              '<div id=\"dropzone_upload_meta\" style=\"font-size: 10px; color: #86868B; margin-top: 2px;\">Uploading file, please wait...</div>' +
              '<div style=\"margin-top: 10px; width: 100%; height: 4px; background: rgba(0, 113, 227, 0.15); border-radius: 980px; overflow: hidden;\">' +
                '<div id=\"dropzone_progress_bar\" style=\"width: 15%; height: 100%; background: #0071E3; transition: width 0.25s ease;\"></div>' +
              '</div>';
          }

          // 2. Update header status badge immediately
          var headerStatus = document.getElementById('ui_header_status');
          if (headerStatus) {
            headerStatus.innerHTML = 
              '<span style=\"font-size: 12px; color: #0071E3; background: rgba(0, 113, 227, 0.1); padding: 4px 10px; border-radius: 980px; font-weight: 500; display: inline-flex; align-items: center; gap: 6px;\">' +
                '<span class=\"spinner-border spinner-border-sm\" style=\"width: 11px; height: 11px; border-width: 1.5px;\"></span>' +
                'Loading: ' + (fileName.length > 20 ? fileName.substring(0, 17) + '...' : fileName) +
              '</span>';
          }

          // 3. Poll Shiny upload progress bar
          if (uploadPollTimer) clearInterval(uploadPollTimer);
          uploadPollTimer = setInterval(function() {
            var bar = document.querySelector('#file_upload_progress .progress-bar, .shiny-file-input-progress .progress-bar');
            var dropBar = document.getElementById('dropzone_progress_bar');
            var dropMeta = document.getElementById('dropzone_upload_meta');
            if (bar) {
              var widthPct = bar.style.width || (bar.getAttribute('aria-valuenow') ? bar.getAttribute('aria-valuenow') + '%' : '');
              if (widthPct) {
                if (dropBar) dropBar.style.width = widthPct;
                if (widthPct === '100%') {
                  if (dropMeta) dropMeta.textContent = 'Ingesting & profiling dataset...';
                }
              }
            }
          }, 150);
        }

        function hideImmediateLoading() {
          if (uploadPollTimer) {
            clearInterval(uploadPollTimer);
            uploadPollTimer = null;
          }
          var dropzone = document.getElementById('app_dropzone');
          if (dropzone && originalDropzoneHtml) {
            dropzone.innerHTML = originalDropzoneHtml;
            dropzone.classList.remove('is-loading');
          }
        }

        function handleDroppedFiles(files) {
          if (!files || files.length === 0) return;
          showImmediateLoading(files[0]);
          var fileInput = document.getElementById('file_upload');
          if (!fileInput) return;
          fileInput.value = '';
          try {
            var dt = new DataTransfer();
            dt.items.add(files[0]);
            fileInput.files = dt.files;
          } catch (e) {
            try {
              fileInput.files = files;
            } catch (err) {
              console.error('Failed to set dropped files:', err);
            }
          }
          if (window.jQuery) {
            window.jQuery(fileInput).trigger('change');
          } else {
            fileInput.dispatchEvent(new Event('change', { bubbles: true }));
          }
        }

        function initDropzone() {
          var dropzone = document.getElementById('app_dropzone') || document.querySelector('.app-dropzone');
          if (!dropzone || dropzone._dragInitialized) return;
          dropzone._dragInitialized = true;
          if (!originalDropzoneHtml) {
            originalDropzoneHtml = dropzone.innerHTML;
          }

          var fileInput = document.getElementById('file_upload');
          if (fileInput && !fileInput._changeBound) {
            fileInput._changeBound = true;
            fileInput.addEventListener('change', function(e) {
              if (this.files && this.files.length > 0) {
                showImmediateLoading(this.files[0]);
              }
            });
          }

          // Prevent default browser drag/drop behavior on the entire window (prevents opening file)
          window.addEventListener('dragover', function(e) {
            e.preventDefault();
          }, false);

          window.addEventListener('drop', function(e) {
            e.preventDefault();
          }, false);

          var dragCounter = 0;
          document.addEventListener('dragenter', function(e) {
            if (e.dataTransfer && e.dataTransfer.types && Array.from(e.dataTransfer.types).includes('Files')) {
              dragCounter++;
              dropzone.classList.add('drag-active');
            }
          }, false);

          document.addEventListener('dragleave', function(e) {
            dragCounter--;
            if (dragCounter <= 0) {
              dragCounter = 0;
              dropzone.classList.remove('drag-active');
              dropzone.classList.remove('dragover');
            }
          }, false);

          document.addEventListener('drop', function(e) {
            dragCounter = 0;
            dropzone.classList.remove('drag-active');
            dropzone.classList.remove('dragover');
            if (!document.body.classList.contains('has-dataset') || (e.target && e.target.closest && e.target.closest('.app-card-ingestion'))) {
              var files = e.dataTransfer && e.dataTransfer.files;
              if (files && files.length > 0) {
                handleDroppedFiles(files);
              }
            }
          }, false);

          dropzone.addEventListener('dragover', function(e) {
            e.preventDefault();
            e.stopPropagation();
            dropzone.classList.add('dragover');
          }, false);

          dropzone.addEventListener('dragleave', function(e) {
            e.preventDefault();
            e.stopPropagation();
            dropzone.classList.remove('dragover');
          }, false);

          dropzone.addEventListener('drop', function(e) {
            e.preventDefault();
            e.stopPropagation();
            dropzone.classList.remove('dragover');
            dropzone.classList.remove('drag-active');
            var files = e.dataTransfer && e.dataTransfer.files;
            if (files && files.length > 0) {
              handleDroppedFiles(files);
            }
          }, false);
        }

        applyTheme(getTheme());
        document.addEventListener('DOMContentLoaded', function() {
          applyTheme(getTheme());
          setTimeout(syncIngestionHeight, 50);
          initDropzone();
        });
        if (document.readyState === 'complete' || document.readyState === 'interactive') {
          setTimeout(initDropzone, 50);
        }
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], button[data-bs-toggle=\"tab\"]', function() {
            setTimeout(syncIngestionHeight, 50);
          });
        }

        function registerShinyHandlers() {
          if (window.Shiny && window.Shiny.addCustomMessageHandler) {
            Shiny.addCustomMessageHandler('clearFileInput', function(id) {
              hideImmediateLoading();
              var el = document.getElementById(id);
              if (el) {
                el.value = '';
                var wrap = el.closest ? el.closest('.app-dropzone, .app-file-dropzone, .shiny-input-container') : el.parentElement;
                if (wrap) {
                  var txt = wrap.querySelector('input[type=\"text\"]');
                  if (txt) txt.value = '';
                  var bar = wrap.querySelector('.progress');
                  if (bar) bar.style.display = 'none';
                }
              }
              var dropzone = document.getElementById('app_dropzone') || document.querySelector('.app-dropzone');
              if (dropzone) {
                dropzone.classList.remove('dragover', 'drag-active');
              }
              document.body.classList.remove('has-dataset');
              document.body.classList.remove('is-fasta');
            });
            Shiny.addCustomMessageHandler('setDatasetState', function(payload) {
              hideImmediateLoading();
              var hasData = typeof payload === 'object' && payload !== null ? !!payload.hasData : !!payload;
              var isFasta = typeof payload === 'object' && payload !== null ? !!payload.isFasta : false;
              if (hasData) {
                document.body.classList.add('has-dataset');
              } else {
                document.body.classList.remove('has-dataset');
              }
              if (isFasta) {
                document.body.classList.add('is-fasta');
              } else {
                document.body.classList.remove('is-fasta');
              }
              setTimeout(syncIngestionHeight, 50);
            });
          } else {
            setTimeout(registerShinyHandlers, 100);
          }
        }
        registerShinyHandlers();
      })();
    ")),
    tags$link(rel = "icon", type = "image/x-icon", href = "icon/app.ico"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "icon/app_icon.svg"),
    tags$link(rel = "apple-touch-icon", href = "icon/app_icon.png")
  ),
  
  # Top Navigation Bar (Frosted Glass)
  div(
    class = "app-navbar",
    div(
      class = "app-brand",
      div(
        class = "app-logo-badge",
        tags$img(src = "icon/app_icon.svg", width = "32", height = "32", alt = "SciDataView Icon")
      ),
      div(
        h1(class = "app-title-main", "SciDataView")
      )
    ),
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      div(
        class = "app-busy-indicator",
        tags$span(class = "spinner-border spinner-border-sm", role = "status", style = "width: 12px; height: 12px; border-width: 2px; color: var(--app-blue);"),
        tags$span("Processing...")
      ),
      uiOutput("ui_header_status"),
      uiOutput("ui_download_html_btn"),
      uiOutput("ui_download_txt_btn"),
      tags$button(
        id = "btn_theme_toggle",
        class = "btn-theme-toggle",
        type = "button",
        onclick = "toggleDarkMode()",
        title = "Toggle Light / Dark Mode",
        HTML('<svg id=\"theme_icon_moon\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z\"></path></svg><svg id=\"theme_icon_sun\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" style=\"display:none;\"><circle cx=\"12\" cy=\"12\" r=\"5\"></circle><line x1=\"12\" y1=\"1\" x2=\"12\" y2=\"3\"></line><line x1=\"12\" y1=\"21\" x2=\"12\" y2=\"23\"></line><line x1=\"4.22\" y1=\"4.22\" x2=\"5.64\" y2=\"5.64\"></line><line x1=\"18.36\" y1=\"18.36\" x2=\"19.78\" y2=\"19.78\"></line><line x1=\"1\" y1=\"12\" x2=\"3\" y2=\"12\"></line><line x1=\"21\" y1=\"12\" x2=\"23\" y2=\"12\"></line><line x1=\"4.22\" y1=\"19.78\" x2=\"5.64\" y2=\"18.36\"></line><line x1=\"18.36\" y1=\"5.64\" x2=\"19.78\" y2=\"4.22\"></line></svg>')
      )
    )
  ),
  
  # Main Content Container
  div(
    class = "app-content-wrap",
    
    fluidRow(
      column(
        width = 3,
        class = "col-sidebar-ingestion",
        div(
          class = "app-card app-card-ingestion",
          h4("Dataset Ingestion", style = "font-size: 15px; font-weight: 600; margin-bottom: 14px;"),
          
          # Custom drop-zone
          div(
            id = "app_dropzone",
            class = "app-dropzone",
            onclick = "$('#file_upload').click()",
            div(style = "font-size: 28px; color: #0071E3; margin-bottom: 8px;", icon("cloud-arrow-up")),
            div(style = "font-size: 13px; font-weight: 600; color: var(--app-text);", "Choose a data file"),
            div(style = "font-size: 11px; color: #86868B; margin-top: 4px;", "Drag & drop or browse (CSV, Parquet, Feather, Arrow, FST, QS, Excel, FASTA, etc.)")
          ),
          
          # Hidden raw input (accepts all extensions, fallback to fread)
          div(
            style = "display: none;",
            fileInput(
              inputId = "file_upload",
              label   = NULL,
              accept  = NULL
            )
          ),
          
          uiOutput("ui_sheet_selector"),
          uiOutput("ui_skip_selector"),
          
          div(style = "margin-top: 18px;"),
          actionButton(
            inputId = "btn_clear",
            label   = "Clear Dataset",
            icon    = tags$svg(
              width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
              stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
              tags$polyline(points = "3 6 5 6 21 6"),
              tags$path(d = "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"),
              tags$line(x1 = "10", y1 = "11", x2 = "10", y2 = "17"),
              tags$line(x1 = "14", y1 = "11", x2 = "14", y2 = "17")
            ),
            class   = "btn-clear-dataset",
            title   = "Clear dataset from memory and free RAM/CPU"
          ),
          
          div(
            class = "app-file-meta-wrap",
            uiOutput("ui_file_meta")
          )
        )
      ),
      
      column(
        width = 9,
        
        # 1. Empty State View (Shown ONLY when no dataset is loaded)
        div(
          class = "app-card app-empty-view",
          style = "padding: 80px 24px; text-align: center; color: #86868B;",
          div(style = "font-size: 48px; color: #0071E3; margin-bottom: 16px;", icon("file-lines")),
          div(style = "font-size: 18px; font-weight: 600; color: var(--app-text); margin-bottom: 8px;", "No Dataset Loaded"),
          div(style = "font-size: 13px; max-width: 520px; margin: 0 auto; line-height: 1.6; color: var(--app-text-secondary);", 
              "Upload or drag & drop any data file (CSV, Parquet, Feather, Arrow, FST, QS, Excel, FASTA, etc.) on the left sidebar to generate the interactive profile and summary.")
        ),
        
        # 2. Active Dataset View (Shown ONLY when dataset is loaded)
        div(
          class = "app-dataset-view",
          
          # FASTA View (Shown ONLY when dataset is FASTA)
          div(
            class = "app-fasta-view",
            uiOutput("ui_fasta_view")
          ),
          
          # Tabular View (Shown ONLY when dataset is tabular: CSV, Excel, Parquet, etc.)
          div(
            class = "app-tabular-view",
            
            # 5 Metric KPI Cards
            div(
              class = "app-kpi-grid",
              div(
                class = "app-kpi-card",
                div(class = "app-kpi-label", "Rows"),
                h3(class = "app-kpi-val", textOutput("kpi_rows", inline = TRUE))
              ),
              div(
                class = "app-kpi-card",
                div(class = "app-kpi-label", "Columns"),
                h3(class = "app-kpi-val", textOutput("kpi_cols", inline = TRUE))
              ),
              div(
                class = "app-kpi-card",
                div(class = "app-kpi-label", "Memory"),
                h3(class = "app-kpi-val", textOutput("kpi_memory", inline = TRUE))
              ),
              div(
                class = "app-kpi-card",
                div(class = "app-kpi-label", "Missingness"),
                h3(class = "app-kpi-val", textOutput("kpi_missing", inline = TRUE))
              ),
              div(
                class = "app-kpi-card",
                div(class = "app-kpi-label", "Duplicates"),
                h3(class = "app-kpi-val", textOutput("kpi_dups", inline = TRUE))
              )
            ),
            
            # Segmented Control Tabs
            div(
              class = "app-card",
              tabsetPanel(
                id = "app_tabs",
                tabPanel(
                  title = "Column Inventory",
                  div(style = "margin-top: 18px;",
                      uiOutput("ui_type_override_bar"),
                      uiOutput("ui_html_inventory"))
                ),
                tabPanel(
                  title = "Numeric Distributions",
                  div(style = "margin-top: 18px;",
                      uiOutput("ui_html_numeric"))
                ),
                tabPanel(
                  title = "Categorical Breakdown",
                  div(style = "margin-top: 18px;",
                      uiOutput("ui_html_categorical"))
                ),
                tabPanel(
                  title = "Correlation Matrix (Pearson)",
                  div(style = "margin-top: 18px;",
                      uiOutput("ui_html_correlation"),
                      uiOutput("ui_bivariate_scatter"))
                ),
                tabPanel(
                  title = "Data Inspector",
                  div(
                    style = "margin-top: 18px;",
                    div(
                      class = "inspect-toolbar",
                      div(
                        style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                        div(
                          class = "inspect-search-wrap",
                          tags$svg(
                            class = "inspect-search-icon", viewBox = "0 0 24 24", fill = "none",
                            stroke = "currentColor", strokeWidth = "2.2", strokeLinecap = "round", strokeLinejoin = "round",
                            tags$circle(cx = "11", cy = "11", r = "8"),
                            tags$line(x1 = "21", y1 = "21", x2 = "16.65", y2 = "16.65")
                          ),
                          textInput("tbl_search", NULL, placeholder = "Search all rows & columns...", width = "100%")
                        ),
                        uiOutput("ui_inspect_pagination")
                      ),
                      div(style = "font-size: 12px; color: var(--app-text-secondary);", textOutput("inspector_status", inline = TRUE))
                    ),
                    div(class = "app-table-wrap", tableOutput("tbl_preview"))
                  )
                ),
                tabPanel(
                  title = "Text Report View",
                  div(style = "margin-top: 18px;",
                      verbatimTextOutput("tbl_report_text"))
                )
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# 7. APPLICATION SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  # Gracefully terminate R process when desktop window is closed
  session$onSessionEnded(function() {
    if (!isTRUE(getOption("shinylive.active", FALSE))) {
      message("SciDataView window closed. Terminating background process...")
      try(stopApp(), silent = TRUE)
    }
  })
  
  rv <- reactiveValues(
    raw_df            = NULL,
    original_df       = NULL,
    type_overrides    = list(),
    profile           = NULL,
    file_name         = NULL,
    file_path         = NULL,
    current_sheet     = NULL,
    skip_rows         = 0,
    report_txt        = NULL,
    report_html       = NULL,
    selected_cor_pair = NULL,
    inspect_page      = 1,
    inv_sort_col      = "Index",
    inv_sort_dir      = "asc"
  )
  
  # Broadcast dataset state to client DOM for adaptive sidebar sizing and fasta view toggle
  observe({
    has_data <- !is.null(rv$raw_df)
    is_fa    <- isTRUE(attr(rv$raw_df, "is_fasta"))
    session$sendCustomMessage("setDatasetState", list(hasData = has_data, isFasta = is_fa))
  })
  
  # Dynamic Excel sheet selector (matching Change Data Type UI)
  output$ui_sheet_selector <- renderUI({
    req(input$file_upload, rv$raw_df)
    ext <- tolower(tools::file_ext(input$file_upload$name))
    if (ext %in% c("xlsx", "xls")) {
      sheets <- readxl::excel_sheets(input$file_upload$datapath)
      if (length(sheets) > 1) {
        selected_val <- if (!is.null(rv$current_sheet) && rv$current_sheet %in% sheets) {
          rv$current_sheet
        } else if (!is.null(input$excel_sheet) && input$excel_sheet %in% sheets) {
          input$excel_sheet
        } else {
          sheets[1]
        }
        
        div(
          class = "sheet-selector-card",
          div(
            class = "sheet-selector-header",
            div(
              class = "sheet-selector-label",
              tags$svg(
                width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
                stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
                tags$path(d = "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"),
                tags$polyline(points = "14 2 14 8 20 8"),
                tags$line(x1 = "8", y1 = "13", x2 = "16", y2 = "13"),
                tags$line(x1 = "8", y1 = "17", x2 = "16", y2 = "17")
              ),
              tags$span("Select Excel Sheet:")
            ),
            tags$span(
              class = "sheet-badge",
              sprintf("%d Sheets", length(sheets))
            )
          ),
          div(
            class = "sheet-selector-select-wrap",
            selectInput(
              inputId  = "excel_sheet",
              label    = NULL,
              choices  = sheets,
              selected = selected_val,
              selectize = FALSE,
              width    = "100%"
            )
          )
        )
      }
    }
  })
  
  # Dynamic title row skip selector (matching Change Data Type UI)
  output$ui_skip_selector <- renderUI({
    req(input$file_upload, rv$raw_df)
    ext <- tolower(tools::file_ext(input$file_upload$name[1]))
    if (ext %in% c("parquet", "feather", "arrow", "fst", "qs", "qs2", "rds", "dta", "sav", "sas7bdat",
                   "fasta", "fa", "faa", "fna", "fas") || isTRUE(attr(rv$raw_df, "is_fasta"))) {
      return(NULL)
    }
    sheet_sel <- if (!is.null(rv$current_sheet)) rv$current_sheet else 1
    detected  <- detect_title_skip(input$file_upload$datapath[1], ext = ext, sheet = sheet_sel)
    
    current_val <- if (!is.null(rv$skip_rows)) rv$skip_rows else detected
    
    div(
      class = "skip-rows-card",
      div(
        class = "skip-rows-header",
        div(
          class = "skip-rows-label",
          tags$svg(
            width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
            stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
            tags$path(d = "M4 6h16"),
            tags$path(d = "M4 12h10"),
            tags$path(d = "M4 18h14"),
            tags$polyline(points = "15 9 18 12 15 15")
          ),
          tags$span("Skip Title Rows:")
        ),
        tags$span(
          class = if (detected > 0) "skip-badge active" else "skip-badge",
          sprintf("Auto: %d", detected)
        )
      ),
      div(
        class = "skip-rows-input-wrap",
        numericInput(
          inputId = "num_skip_rows",
          label   = NULL,
          value   = current_val,
          min     = 0,
          max     = 100,
          step    = 1,
          width   = "100%"
        )
      )
    )
  })
  
  # Dedicated FASTA View (KPIs & Database Breakdown)
  output$ui_fasta_view <- renderUI({
    req(rv$raw_df)
    if (!isTRUE(attr(rv$raw_df, "is_fasta"))) return(NULL)
    db_counts <- attr(rv$raw_df, "fasta_db_counts")
    if (is.null(db_counts) || nrow(db_counts) == 0) return(NULL)
    
    total_seqs <- sum(db_counts$Sequences)
    lens <- rv$raw_df$Length
    lens_c <- lens[!is.na(lens)]
    avg_len <- if (length(lens_c) > 0) round(mean(lens_c), 1) else 0
    min_len <- if (length(lens_c) > 0) min(lens_c) else 0
    max_len <- if (length(lens_c) > 0) max(lens_c) else 0
    mem_str <- format_fast_memory_size(rv$raw_df)
    
    # 1. FASTA KPI Grid
    kpi_grid <- div(
      class = "app-kpi-grid",
      div(
        class = "app-kpi-card",
        div(class = "app-kpi-label", "Total Sequences"),
        h3(class = "app-kpi-val", format(total_seqs, big.mark = ",")),
        div(class = "app-kpi-sub", "FASTA records (N)")
      ),
      div(
        class = "app-kpi-card",
        div(class = "app-kpi-label", "Databases"),
        h3(class = "app-kpi-val", format(nrow(db_counts), big.mark = ",")),
        div(class = "app-kpi-sub", "Distinct source databases")
      ),
      div(
        class = "app-kpi-card",
        div(class = "app-kpi-label", "Memory"),
        h3(class = "app-kpi-val", mem_str)
      ),
      div(
        class = "app-kpi-card",
        div(class = "app-kpi-label", "Average Length"),
        h3(class = "app-kpi-val", sprintf("%s aa", format(avg_len, big.mark = ","))),
        div(class = "app-kpi-sub", "Mean sequence length")
      ),
      div(
        class = "app-kpi-card",
        div(class = "app-kpi-label", "Length Range"),
        h3(class = "app-kpi-val", sprintf("%s - %s", format(min_len, big.mark = ","), format(max_len, big.mark = ","))),
        div(class = "app-kpi-sub", "Min - Max sequence length")
      )
    )
    
    # 2. Database Badges
    badges <- lapply(seq_len(nrow(db_counts)), function(i) {
      row <- db_counts[i, ]
      db_nm <- row$Database
      badge_class <- if (grepl("^sp\\b", db_nm)) {
        "fasta-db-badge badge-sp"
      } else if (grepl("^tr\\b", db_nm)) {
        "fasta-db-badge badge-tr"
      } else {
        "fasta-db-badge badge-other"
      }
      
      div(
        class = badge_class,
        tags$span(style = "font-weight: 600;", db_nm),
        tags$span(class = "fasta-count-pill", format(row$Sequences, big.mark = ",")),
        tags$span(class = "fasta-pct-pill", sprintf("(%.1f%%)", row$Percentage))
      )
    })
    
    # 3. Database Table Rows
    table_rows <- lapply(seq_len(nrow(db_counts)), function(i) {
      row <- db_counts[i, ]
      pct <- row$Percentage
      bar_color <- if (grepl("^sp\\b", row$Database)) "#0071E3" else if (grepl("^tr\\b", row$Database)) "#FF9500" else "#8E8E93"
      
      tags$tr(
        tags$td(
          style = "font-weight: 600; padding: 12px 16px; font-size: 13px;",
          row$Database
        ),
        tags$td(
          style = "text-align: right; padding: 12px 16px; font-weight: 600; font-feature-settings: 'tnum';",
          format(row$Sequences, big.mark = ",")
        ),
        tags$td(
          style = "text-align: right; padding: 12px 16px; font-weight: 600; font-feature-settings: 'tnum';",
          sprintf("%.2f%%", pct)
        ),
        tags$td(
          style = "padding: 12px 16px; min-width: 140px; width: 35%;",
          div(
            style = "height: 8px; width: 100%; background: var(--app-bar-track); border-radius: 980px; overflow: hidden;",
            div(style = sprintf("height: 100%%; width: %.1f%%; background: %s; border-radius: 980px;", max(1, pct), bar_color))
          )
        )
      )
    })
    
    breakdown_card <- div(
      class = "app-card",
      div(
        style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px; margin-bottom: 16px;",
        div(
          style = "display: flex; align-items: center; gap: 8px; font-weight: 600; font-size: 15px; color: var(--app-text);",
          tags$svg(
            width = "18", height = "18", viewBox = "0 0 24 24", fill = "none",
            stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
            style = "color: var(--app-blue);",
            tags$path(d = "M12 2v20"),
            tags$path(d = "M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6")
          ),
          tags$span("FASTA Database Sequence Breakdown")
        ),
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          tags$span(
            style = "font-size: 11px; font-weight: 600; background: var(--app-blue-soft); color: var(--app-blue); padding: 3px 10px; border-radius: 980px;",
            sprintf("%s Sequences Total", format(total_seqs, big.mark = ","))
          )
        )
      ),
      
      div(
        class = "fasta-badge-grid",
        style = "margin-bottom: 20px;",
        badges
      ),
      
      div(
        class = "app-table-wrap",
        tags$table(
          class = "table app-table",
          style = "width: 100%; margin-bottom: 0;",
          tags$thead(
            tags$tr(
              tags$th(style = "text-align: left; padding: 10px 16px; font-size: 11px; text-transform: uppercase; color: var(--app-table-th-text);", "Database Source"),
              tags$th(style = "text-align: right; padding: 10px 16px; font-size: 11px; text-transform: uppercase; color: var(--app-table-th-text);", "Sequences"),
              tags$th(style = "text-align: right; padding: 10px 16px; font-size: 11px; text-transform: uppercase; color: var(--app-table-th-text);", "Percentage"),
              tags$th(style = "text-align: left; padding: 10px 16px; font-size: 11px; text-transform: uppercase; color: var(--app-table-th-text);", "Proportion")
            )
          ),
          tags$tbody(table_rows)
        )
      )
    )
    
    tagList(kpi_grid, breakdown_card)
  })
  
  # Core helper to ingest and profile dataset
  load_dataset <- function(sheet_sel = NULL, skip_sel = NULL) {
    req(input$file_upload)
    file_info <- input$file_upload
    
    if (is.null(sheet_sel)) {
      sheet_sel <- if (!is.null(rv$current_sheet)) rv$current_sheet else 1
    }
    if (is.null(skip_sel)) {
      skip_sel <- if (!is.null(rv$skip_rows)) rv$skip_rows else "auto"
    }
    
    file_path <- file_info$datapath[1]
    file_name <- file_info$name[1]
    
    withProgress(message = "Reading dataset...", detail = "Please wait", value = 0.3, {
      tryCatch({
        df <- read_any_table(file_path, file_name = file_name, sheet = sheet_sel, skip = skip_sel)
        skip_actual <- attr(df, "skip_rows")
        rv$skip_rows <- if (!is.null(skip_actual)) skip_actual else 0
        
        # Smart Sampling threshold: if rows > 3,000,000, sample 100,000 rows
        full_n <- attr(df, "full_n_rows")
        if (is.null(full_n)) full_n <- nrow(df)
        
        if (nrow(df) > 3000000L) {
          set.seed(42)
          s_idx <- sort(sample.int(nrow(df), 100000L))
          df <- df[s_idx, , drop = FALSE]
          attr(df, "full_n_rows") <- full_n
          attr(df, "is_sampled")  <- TRUE
        }
        
        setProgress(value = 0.7, message = "Classifying columns & screening data...")
        p <- profile_dataset(df, type_overrides = list(), lazy = TRUE)
        
        rv$original_df       <- df
        rv$raw_df            <- df
        rv$type_overrides    <- list()
        rv$file_name         <- file_name
        rv$file_path         <- file_path
        rv$current_sheet     <- sheet_sel
        rv$profile           <- p
        rv$report_txt        <- NULL
        rv$report_html       <- NULL
        rv$selected_cor_pair <- NULL
        rv$inspect_page      <- 1
        rv$inv_sort_col      <- "Index"
        rv$inv_sort_dir      <- "asc"
        
        if (isTRUE(rv$skip_rows > 0)) {
          showNotification(sprintf("Auto-skipped %d title row(s). Header at row %d.", rv$skip_rows, rv$skip_rows + 1),
                           type = "message", duration = 4)
        }
        
        if (isTRUE(p$is_sampled)) {
          showNotification(
            sprintf("Large dataset (%s rows) — Sampled %s random rows for fast profiling.",
                    format(p$full_rows, big.mark = ","),
                    format(p$rows, big.mark = ",")),
            type = "message", duration = 6
          )
        }
        
        setProgress(value = 1.0, message = "Complete!")
      }, error = function(e) {
        msg <- e$message
        if (!startsWith(msg, "Failed to read file")) {
          msg <- paste("Failed to read file:", msg)
        }
        showNotification(
          msg,
          type = "error",
          duration = 8
        )
      })
    })
  }
  
  # Ingest and profile file upon initial upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tolower(tools::file_ext(input$file_upload$name[1]))
    first_sheet <- 1
    if (ext %in% c("xlsx", "xls")) {
      sheets <- tryCatch(readxl::excel_sheets(input$file_upload$datapath[1]), error = function(e) NULL)
      if (length(sheets) > 0) {
        first_sheet <- sheets[1]
      }
    }
    load_dataset(sheet_sel = first_sheet, skip_sel = "auto")
  })
  
  # Automatically reload data when user switches Excel sheet
  observeEvent(input$excel_sheet, {
    req(input$file_upload, rv$raw_df, input$excel_sheet)
    if (!is.null(rv$current_sheet) && identical(as.character(input$excel_sheet), as.character(rv$current_sheet))) {
      return()
    }
    load_dataset(sheet_sel = input$excel_sheet, skip_sel = if (!is.null(rv$skip_rows)) rv$skip_rows else "auto")
  }, ignoreInit = TRUE)
  
  # Automatically reload data when user changes skip rows (debounced)
  skip_debounced <- debounce(reactive(input$num_skip_rows), 500)
  observeEvent(skip_debounced(), {
    req(input$file_upload, rv$raw_df)
    val <- skip_debounced()
    if (is.null(val) || is.na(val)) return()
    if (!is.null(rv$skip_rows) && identical(as.integer(val), as.integer(rv$skip_rows))) {
      return()
    }
    sheet_sel <- if (!is.null(rv$current_sheet)) rv$current_sheet else 1
    load_dataset(sheet_sel = sheet_sel, skip_sel = val)
  }, ignoreInit = TRUE)
  
  # Clear dataset from memory and force garbage collection
  observeEvent(input$btn_clear, {
    if (is.null(rv$raw_df) && is.null(rv$file_name)) {
      showNotification("No dataset loaded in memory.", type = "message", duration = 3)
      return()
    }
    
    rv$raw_df            <- NULL
    rv$original_df       <- NULL
    rv$type_overrides    <- list()
    rv$profile           <- NULL
    rv$file_name         <- NULL
    rv$file_path         <- NULL
    rv$current_sheet     <- NULL
    rv$skip_rows         <- 0
    rv$report_txt        <- NULL
    rv$report_html       <- NULL
    rv$selected_cor_pair <- NULL
    rv$inspect_page      <- 1
    rv$inv_sort_col      <- "Index"
    rv$inv_sort_dir      <- "asc"
    
    session$sendCustomMessage("clearFileInput", "file_upload")
    gc()
    
    showNotification("Dataset cleared from memory. RAM released.", type = "message", duration = 3)
  })
  
  # Lazy Profile Evaluators (Computed on demand when switching to respective tabs)
  numeric_profile <- reactive({
    req(rv$raw_df, rv$profile)
    if (!is.null(rv$profile$numeric)) return(rv$profile$numeric)
    compute_numeric_profile(rv$raw_df, rv$profile$num_cols)
  })
  
  categorical_profile <- reactive({
    req(rv$raw_df, rv$profile)
    if (!is.null(rv$profile$categorical)) return(rv$profile$categorical)
    compute_categorical_profile(rv$raw_df, rv$profile$cat_cols, rv$profile$rows)
  })
  
  cor_profile <- reactive({
    req(rv$raw_df, rv$profile)
    if (!is.null(rv$profile$cor_meta)) return(rv$profile$cor_meta)
    compute_cor_matrix(rv$raw_df, rv$profile$num_cols, max_vars = 35)
  })
  
  # File metadata indicator in sidebar
  output$ui_file_meta <- renderUI({
    if (is.null(rv$file_name)) {
      div("No file loaded. Drop a table to begin profiling.")
    } else {
      is_fa <- isTRUE(attr(rv$raw_df, "is_fasta"))
      db_counts <- attr(rv$raw_df, "fasta_db_counts")
      
      div(
        div(style = "font-weight: 600; color: var(--app-text); margin-bottom: 2px;", rv$file_name),
        div(sprintf("Format: %s", if (is_fa) "FASTA Sequence" else toupper(tools::file_ext(rv$file_name)))),
        if (is_fa && !is.null(db_counts)) {
          tagList(
            div(style = "font-size: 11px; color: var(--app-text); margin-top: 6px; font-weight: 600;",
                sprintf("Databases (%d):", nrow(db_counts))),
            tags$ul(
              style = "margin: 4px 0 0 16px; padding: 0; font-size: 11px; line-height: 1.4;",
              lapply(seq_len(nrow(db_counts)), function(i) {
                tags$li(sprintf("%s: %s (%.1f%%)", db_counts$Database[i], format(db_counts$Sequences[i], big.mark = ","), db_counts$Percentage[i]))
              })
            )
          )
        },
        if (isTRUE(rv$skip_rows > 0)) {
          div(style = "color: #0071E3; font-weight: 500; font-size: 11px; margin-top: 4px;",
              sprintf("Header at row %d (Skipped %d title rows)", rv$skip_rows + 1, rv$skip_rows))
        },
        if (isTRUE(rv$profile$is_sampled) && !is.null(rv$profile$full_rows)) {
          div(style = "color: #0071E3; font-weight: 500; font-size: 11px; margin-top: 4px;",
              sprintf("⚡ Sampled %s of %s rows",
                      format(rv$profile$rows, big.mark = ","),
                      format(rv$profile$full_rows, big.mark = ",")))
        }
      )
    }
  })
  
  # Header Status Badge
  output$ui_header_status <- renderUI({
    if (is.null(rv$raw_df)) {
      span(style = "font-size: 12px; color: var(--app-badge-text); background: var(--app-badge-bg); padding: 4px 10px; border-radius: 980px;", "Ready")
    } else {
      span(style = "font-size: 12px; color: #0071E3; background: rgba(0, 113, 227, 0.1); padding: 4px 10px; border-radius: 980px; font-weight: 500;",
           sprintf("Active: %s", rv$file_name))
    }
  })
  
  # Top HTML Export Button
  output$ui_download_html_btn <- renderUI({
    req(rv$raw_df)
    downloadButton(
      outputId = "btn_download_html",
      label    = "Export HTML (.html)",
      class    = "btn-app-primary"
    )
  })
  
  output$btn_download_html <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(rv$file_name), "_scidataview.html")
    },
    content = function(file) {
      req(rv$raw_df, rv$profile)
      full_p <- ensure_full_profile(rv$raw_df, rv$profile)
      writeLines(generate_html_report(rv$file_name, full_p), con = file, useBytes = TRUE)
    },
    contentType = "text/html"
  )
  
  # Top TXT Export Button
  output$ui_download_txt_btn <- renderUI({
    req(rv$raw_df)
    downloadButton(
      outputId = "btn_download_txt",
      label    = "Export Text (.txt)",
      class    = "btn-app-secondary"
    )
  })
  
  output$btn_download_txt <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(rv$file_name), "_scidataview.txt")
    },
    content = function(file) {
      req(rv$raw_df, rv$profile)
      full_p <- ensure_full_profile(rv$raw_df, rv$profile)
      writeLines(generate_text_report(rv$file_name, full_p), con = file, useBytes = TRUE)
    },
    contentType = "text/plain"
  )
  
  # Metric KPI Cards
  output$kpi_rows    <- renderText({
    if (is.null(rv$profile)) return("-")
    if (isTRUE(rv$profile$is_sampled) && !is.null(rv$profile$full_rows)) {
      sprintf("%s (Sample)", format(rv$profile$rows, big.mark = ","))
    } else {
      format(rv$profile$rows, big.mark = ",")
    }
  })
  output$kpi_cols    <- renderText({ if (is.null(rv$profile)) "-" else format(rv$profile$cols, big.mark = ",") })
  output$kpi_memory  <- renderText({ if (is.null(rv$profile)) "-" else rv$profile$memory })
  output$kpi_missing <- renderText({ if (is.null(rv$profile)) "-" else paste0(rv$profile$missing_rate, "%") })
  output$kpi_dups    <- renderText({ if (is.null(rv$profile)) "-" else format(rv$profile$duplicates, big.mark = ",") })
  
  # Type Override Toolbar (Tab 1)
  output$ui_type_override_bar <- renderUI({
    req(rv$profile)
    cols <- rv$profile$inventory$Column_Name
    has_overrides <- length(rv$type_overrides) > 0
    
    div(
      class = "type-override-toolbar",
      div(
        class = "type-override-controls",
        div(
          class = "type-override-label",
          tags$svg(
            width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
            stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
            tags$path(d = "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"),
            tags$path(d = "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z")
          ),
          tags$span("Change Data Type:")
        ),
        div(
          class = "type-override-select-wrap", style = "min-width: 170px; max-width: 250px;",
          selectInput("sel_col_override", NULL, choices = cols, selected = input$sel_col_override, selectize = FALSE, width = "100%")
        ),
        div(
          class = "type-override-arrow",
          tags$svg(
            width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
            stroke = "currentColor", strokeWidth = "2.2", strokeLinecap = "round", strokeLinejoin = "round",
            tags$line(x1 = "5", y1 = "12", x2 = "19", y2 = "12"),
            tags$polyline(points = "12 5 19 12 12 19")
          )
        ),
        div(
          class = "type-override-select-wrap", style = "min-width: 190px; max-width: 230px;",
          selectInput("sel_type_target", NULL, choices = c(
            "Continuous Numeric",
            "Categorical String",
            "Discrete / Categorical",
            "Date / Time",
            "Identifier (ID)"
          ), selectize = FALSE, width = "100%")
        ),
        actionButton(
          "btn_apply_type",
          label = tags$span(
            tags$svg(
              width = "12", height = "12", viewBox = "0 0 24 24", fill = "none",
              stroke = "currentColor", strokeWidth = "2.5", strokeLinecap = "round", strokeLinejoin = "round",
              style = "margin-right: 5px; vertical-align: -1px;",
              tags$polyline(points = "20 6 9 17 4 12")
            ),
            "Apply Change"
          ),
          class = "btn-app-primary type-override-btn"
        )
      ),
      if (has_overrides) {
        actionButton(
          "btn_reset_types",
          label = tags$span(
            tags$svg(
              width = "12", height = "12", viewBox = "0 0 24 24", fill = "none",
              stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
              style = "margin-right: 5px; vertical-align: -1px;",
              tags$path(d = "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"),
              tags$path(d = "M3 3v5h5")
            ),
            sprintf("Reset Defaults (%d)", length(rv$type_overrides))
          ),
          class = "btn-app-secondary type-override-btn"
        )
      }
    )
  })
  
  observeEvent(input$btn_apply_type, {
    req(rv$original_df, input$sel_col_override, input$sel_type_target)
    col <- input$sel_col_override
    target <- input$sel_type_target
    
    overrides <- rv$type_overrides
    overrides[[col]] <- target
    rv$type_overrides <- overrides
    
    p <- profile_dataset(rv$original_df, type_overrides = rv$type_overrides, lazy = TRUE)
    rv$profile     <- p
    rv$report_txt  <- NULL
    rv$report_html <- NULL
    showNotification(sprintf("Updated '%s' Data type to '%s'", col, target), type = "message", duration = 3)
  })
  
  observeEvent(input$btn_reset_types, {
    req(rv$original_df)
    rv$type_overrides <- list()
    p <- profile_dataset(rv$original_df, type_overrides = list(), lazy = TRUE)
    rv$profile     <- p
    rv$report_txt  <- NULL
    rv$report_html <- NULL
    showNotification("Reset all columns to original detected data types.", type = "message", duration = 3)
  })
  
  observeEvent(input$inv_sort_click, {
    col <- input$inv_sort_click
    if (identical(rv$inv_sort_col, col)) {
      rv$inv_sort_dir <- if (rv$inv_sort_dir == "asc") "desc" else "asc"
    } else {
      rv$inv_sort_col <- col
      if (col %in% c("Missing_Pct", "Complete_N", "Distinct_N")) {
        rv$inv_sort_dir <- "desc"
      } else {
        rv$inv_sort_dir <- "asc"
      }
    }
  })
  
  observeEvent(input$btn_reset_inv_sort, {
    rv$inv_sort_col <- "Index"
    rv$inv_sort_dir <- "asc"
  })
  
  # Tab 1: Column Inventory (with Missingness header, bar & interactive column sorting)
  output$ui_html_inventory <- renderUI({
    if (is.null(rv$profile)) return(NULL)
    inv <- rv$profile$inventory
    
    # Sort inventory by active column & direction
    sort_col <- rv$inv_sort_col
    sort_dir <- rv$inv_sort_dir
    if (!is.null(sort_col) && sort_col %in% names(inv)) {
      ord <- order(inv[[sort_col]], decreasing = (sort_dir == "desc"), na.last = TRUE)
      inv <- inv[ord, , drop = FALSE]
    }
    
    render_th <- function(label, col_id, align = "left", style_extra = "") {
      is_active <- identical(rv$inv_sort_col, col_id)
      indicator <- if (is_active) {
        if (rv$inv_sort_dir == "asc") " ▲" else " ▼"
      } else {
        " ⇅"
      }
      ind_style <- if (is_active) "color: var(--app-blue); font-weight: 700;" else "color: var(--app-text-secondary); opacity: 0.35;"
      
      tags$th(
        class = "sortable-th",
        style = sprintf("cursor: pointer; user-select: none; text-align: %s; %s", align, style_extra),
        onclick = sprintf("Shiny.setInputValue('inv_sort_click', '%s', {priority: 'event'})", col_id),
        title = sprintf("Sort by %s (%s)", label, if (is_active && rv$inv_sort_dir == "asc") "Ascending -> click for Descending" else "Descending -> click for Ascending"),
        label,
        tags$span(style = sprintf("font-size: 10px; margin-left: 4px; display: inline-block; %s", ind_style), indicator)
      )
    }
    
    rows_html <- lapply(seq_len(nrow(inv)), function(i) {
      r <- inv[i, ]
      type_badge <- if (isTRUE(r$Is_Overridden)) {
        span(style = "font-size: 11px; font-weight: 600; background: rgba(0, 113, 227, 0.12); color: #0071E3; padding: 3px 8px; border-radius: 980px; border: 1px solid rgba(0, 113, 227, 0.25);",
             paste0(r$Data_Type, " (Edited)"))
      } else {
        span(style = "font-size: 11px; font-weight: 600; background: var(--app-badge-bg); color: var(--app-badge-text); padding: 3px 8px; border-radius: 980px;", r$Data_Type)
      }
      tags$tr(
        tags$td(style = "color: #86868B; text-align: center;", r$Index),
        tags$td(style = "font-weight: 600;", r$Column_Name),
        tags$td(type_badge),
        tags$td(style = "min-width: 140px;", HTML(r$Missing_Bar)),
        tags$td(style = "text-align: right;", format(r$Complete_N, big.mark = ",")),
        tags$td(style = "text-align: right;", format(r$Distinct_N, big.mark = ",")),
        tags$td(style = "color: var(--app-text-secondary);", r$Sample_Value)
      )
    })
    
    div(
      div(
        class = "app-table-wrap",
        tags$table(
          class = "table",
          tags$thead(
            tags$tr(
              render_th("#", "Index", align = "center", style_extra = "width: 50px;"),
              render_th("Column Name", "Column_Name"),
              render_th("Data type", "Data_Type"),
              render_th("Missingness", "Missing_Pct", style_extra = "min-width: 140px;"),
              render_th("Complete N", "Complete_N", align = "right"),
              render_th("Distinct N", "Distinct_N", align = "right"),
              tags$th("Sample Value")
            )
          ),
          tags$tbody(rows_html)
        )
      ),
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-top: 10px; font-size: 12px; color: var(--app-text-secondary);",
        tags$span(sprintf("Total %d columns. Click any column header (▲/▼) to sort.", nrow(inv))),
        if (!identical(rv$inv_sort_col, "Index")) {
          actionLink(
            "btn_reset_inv_sort",
            label = "Reset to original order (#)",
            style = "font-size: 12px; color: var(--app-blue); text-decoration: none; cursor: pointer;"
          )
        }
      )
    )
  })
  
  # Tab 2: Numeric Distributions (with Sparklines, Skewness & Outliers)
  output$ui_html_numeric <- renderUI({
    if (is.null(rv$profile)) {
      return(div(style = "padding: 56px 20px; text-align: center; color: #86868B;", "Upload a dataset to view numeric distributions, sparklines, and outliers."))
    }
    num <- numeric_profile()
    if (is.null(num)) {
      return(render_skeleton_table(rows = 6, cols = 8, message = "Computing numeric distributions, sparklines, and moments..."))
    }
    if (nrow(num) == 0) {
      return(div(style = "padding: 20px; color: #86868B; text-align: center;", "No numeric features found in this dataset."))
    }
    
    rows_html <- lapply(seq_len(nrow(num)), function(i) {
      r <- num[i, ]
      out_color <- if (r$Outliers_Pct > 5) "#FF3B30" else if (r$Outliers_Pct > 0) "#FF9500" else "#86868B"
      
      tags$tr(
        tags$td(style = "font-weight: 600;", r$Variable),
        tags$td(style = "text-align: center;", HTML(r$Distribution_SVG)),
        tags$td(HTML(sprintf("<strong>%.1f</strong> (%.1f)", r$Mean, r$SD))),
        tags$td(HTML(sprintf("<strong>%.1f</strong> [%.1f]", r$Median, r$IQR))),
        tags$td(sprintf("%.1f", r$Min)),
        tags$td(sprintf("%.1f", r$Max)),
        tags$td(sprintf("%.2f", r$Skewness)),
        tags$td(style = sprintf("color: %s; font-weight: 600;", out_color), sprintf("%s (%.1f%%)", format(r$Outliers_N, big.mark = ","), r$Outliers_Pct))
      )
    })
    
    div(
      class = "app-table-wrap",
      tags$table(
        class = "table",
        tags$thead(
          tags$tr(
            tags$th("Variable"),
            tags$th(style = "text-align: center; min-width: 120px;", "Distribution"),
            tags$th("Mean (SD)"),
            tags$th("Median [IQR]"),
            tags$th("Min"),
            tags$th("Max"),
            tags$th("Skewness"),
            tags$th("Outliers")
          )
        ),
        tags$tbody(rows_html)
      )
    )
  })
  
  # Tab 3: Categorical Breakdown
  output$ui_html_categorical <- renderUI({
    if (is.null(rv$profile)) {
      return(div(style = "padding: 56px 20px; text-align: center; color: #86868B;", "Upload a dataset to view categorical frequency breakdowns."))
    }
    cat_df <- categorical_profile()
    if (is.null(cat_df)) {
      return(render_skeleton_table(rows = 5, cols = 3, message = "Computing categorical frequency breakdowns..."))
    }
    if (nrow(cat_df) == 0) {
      return(div(style = "padding: 20px; color: #86868B; text-align: center;", "No categorical features found in this dataset."))
    }
    
    rows_html <- lapply(seq_len(nrow(cat_df)), function(i) {
      r <- cat_df[i, ]
      tags$tr(
        tags$td(style = "font-weight: 600;", r$Variable),
        tags$td(style = "text-align: center;", span(style = "background: var(--app-badge-bg); color: var(--app-badge-text); padding: 3px 8px; border-radius: 980px; font-weight: 600; font-size: 11px;", r$Total_Levels)),
        tags$td(style = "color: var(--app-text);", r$Top_Categories)
      )
    })
    
    div(
      class = "app-table-wrap",
      tags$table(
        class = "table",
        tags$thead(
          tags$tr(
            tags$th("Variable"),
            tags$th(style = "text-align: center;", "Total Levels"),
            tags$th("Frequency Breakdown (Top Levels)")
          )
        ),
        tags$tbody(rows_html)
      )
    )
  })
  
  # Tab 4: Correlation Matrix
  output$ui_html_correlation <- renderUI({
    if (is.null(rv$profile)) {
      return(div(style = "padding: 56px 20px; text-align: center; color: #86868B;", "Upload a dataset to compute the Pearson correlation matrix."))
    }
    cor_res <- cor_profile()
    if (is.null(cor_res)) {
      return(render_skeleton_table(rows = 6, cols = 6, message = "Computing Pearson correlation matrix..."))
    }
    c_mat <- cor_res$matrix
    if (is.null(c_mat)) {
      return(div(style = "padding: 20px; color: #86868B; text-align: center;", "Insufficient numeric columns to construct correlation matrix."))
    }
    
    vars <- colnames(c_mat)
    header_ths <- lapply(vars, function(v) tags$th(style = "font-size: 10px; padding: 8px 6px; text-align: center;", substr(v, 1, 9)))
    
    matrix_rows <- lapply(seq_along(vars), function(row_i) {
      cells <- lapply(seq_along(vars), function(col_j) {
        if (row_i == col_j) {
          return(tags$td(style = "background: var(--app-subbox-bg); color: var(--app-text-secondary); text-align: center; font-size: 12px; font-weight: 600;", "-"))
        }
        val <- c_mat[row_i, col_j]
        if (is.na(val)) return(tags$td(style = "background: var(--app-subbox-bg); color: var(--app-text-secondary); text-align: center;", "-"))
        
        intensity <- abs(val)
        bg_col <- if (val > 0) {
          sprintf("background: rgba(230, 75, 53, %.2f); color: %s;", intensity * 0.85, if(intensity > 0.5) "#FFF" else "var(--app-text)")
        } else {
          sprintf("background: rgba(0, 160, 135, %.2f); color: %s;", intensity * 0.85, if(intensity > 0.5) "#FFF" else "var(--app-text)")
        }
        
        strong_tag <- if (abs(val) >= 0.70) "font-weight: 700; text-decoration: underline;" else ""
        tags$td(
          class = "cell-cor-interactive",
          title = sprintf("Click to inspect scatter: %s vs %s (r = %.2f)", vars[row_i], vars[col_j], val),
          onclick = sprintf("Shiny.setInputValue('sel_cor_pair', '%s:::%s', {priority: 'event'})", vars[row_i], vars[col_j]),
          style = sprintf("padding: 8px 6px; text-align: center; font-size: 11px; %s %s", bg_col, strong_tag),
          sprintf("%.2f", val)
        )
      })
      tags$tr(tags$td(style = "font-weight: 600; font-size: 11px; padding: 8px 10px;", vars[row_i]), cells)
    })
    
    guard_badge <- if (isTRUE(cor_res$is_truncated)) {
      div(
        style = "display: inline-flex; align-items: center; gap: 8px; background: rgba(0, 113, 227, 0.08); border: 1px solid rgba(0, 113, 227, 0.2); border-radius: var(--app-radius-pill); padding: 5px 14px; margin-bottom: 14px; font-size: 12px; color: var(--app-blue); font-weight: 500;",
        tags$svg(
          width = "14", height = "14", viewBox = "0 0 24 24", fill = "none",
          stroke = "currentColor", strokeWidth = "2", strokeLinecap = "round", strokeLinejoin = "round",
          tags$circle(cx = "12", cy = "12", r = "10"),
          tags$line(x1 = "12", y1 = "16", x2 = "12", y2 = "12"),
          tags$line(x1 = "12", y1 = "8", x2 = "12.01", y2 = "8")
        ),
        sprintf("Dimensionality Guard: Showing top %d features by variance (out of %d numeric columns) for optimal performance.", cor_res$displayed_n, cor_res$total_vars)
      )
    } else {
      NULL
    }
    
    div(
      guard_badge,
      div(
        class = "app-table-wrap",
        tags$table(
          class = "table",
          tags$thead(tags$tr(tags$th("Feature"), header_ths)),
          tags$tbody(matrix_rows)
        )
      ),
      div(style = "font-size: 12px; color: #86868B; margin-top: 10px;",
          "* Pearson correlation coefficient r [-1.0 to 1.0]. Diagonal indicates self-correlation (-). Color scale: Red (+1.0) and Teal (-1.0). Underlined bold entries indicate strong collinearity (|r| >= 0.70). Click any cell to view scatter & trendline.")
    )
  })
  
  # Bivariate scatter viewer (Phase 3)
  observeEvent(input$sel_cor_pair, {
    rv$selected_cor_pair <- input$sel_cor_pair
  })
  
  observeEvent(input$btn_close_scatter, {
    rv$selected_cor_pair <- NULL
  })
  
  output$ui_bivariate_scatter <- renderUI({
    req(rv$raw_df)
    pair_str <- rv$selected_cor_pair
    if (is.null(pair_str) || !nzchar(pair_str)) {
      return(NULL)
    }
    parts <- strsplit(pair_str, ":::")[[1]]
    if (length(parts) != 2) return(NULL)
    v1 <- parts[1]
    v2 <- parts[2]
    
    if (!all(c(v1, v2) %in% names(rv$raw_df))) return(NULL)
    
    x_val <- rv$raw_df[[v1]]
    y_val <- rv$raw_df[[v2]]
    if (!is.numeric(x_val) || !is.numeric(y_val)) return(NULL)
    
    valid <- !is.na(x_val) & !is.na(y_val) & is.finite(x_val) & is.finite(y_val)
    n_pts <- sum(valid)
    if (n_pts < 3) return(NULL)
    
    r_val <- cor(x_val[valid], y_val[valid])
    r2_val <- r_val^2
    
    svg_html <- generate_svg_bivariate(x_val, y_val, v1, v2, width = 640, height = 240)
    
    div(
      style = "margin-top: 20px; padding: 20px; background: var(--app-subbox-bg); border-radius: var(--app-radius-md); border: 1px solid var(--app-border);",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap; gap: 8px;",
        div(
          tags$span(style = "font-weight: 600; font-size: 14px; color: var(--app-text);", 
                    sprintf("Bivariate Relationship: %s vs %s", v1, v2)),
          tags$span(style = "margin-left: 12px; font-size: 12px; color: var(--app-text-secondary);",
                    sprintf("Pearson r = %.3f | R² = %.3f | N = %s", r_val, r2_val, format(n_pts, big.mark = ",")))
        ),
        actionButton(
          inputId = "btn_close_scatter",
          label = "Close Preview",
          class = "btn-app-secondary",
          style = "font-size: 11px; padding: 3px 10px; height: 26px; line-height: 1;"
        )
      ),
      HTML(svg_html)
    )
  })
  
  # Tab 5: Data Inspector (with vectorized search filter & pagination)
  search_debounced <- debounce(reactive(input$tbl_search), 300)
  
  observeEvent(search_debounced(), {
    rv$inspect_page <- 1
  })
  
  observeEvent(input$btn_inspect_prev, {
    if (isTRUE(rv$inspect_page > 1)) {
      rv$inspect_page <- rv$inspect_page - 1
    }
  })
  
  observeEvent(input$btn_inspect_next, {
    m_df <- matched_preview()
    total_m <- nrow(m_df)
    max_p <- max(1, ceiling(total_m / 25))
    if (isTRUE(rv$inspect_page < max_p)) {
      rv$inspect_page <- rv$inspect_page + 1
    }
  })
  
  matched_preview <- reactive({
    req(rv$raw_df)
    df <- rv$raw_df
    q <- search_debounced()
    if (!is.null(q) && nzchar(trimws(q))) {
      q_clean <- trimws(q)
      # Fast vectorized search: use fixed=TRUE if no regex characters for 5-10x faster Boyer-Moore search
      is_regex <- grepl("[.\\\\+*?\\[^\\]$(){}=!<>|:-]", q_clean)
      col_matches <- lapply(df, function(col) {
        grepl(q_clean, as.character(col), ignore.case = TRUE, fixed = !is_regex)
      })
      matched_rows <- Reduce(`|`, col_matches)
      df <- df[matched_rows, , drop = FALSE]
    }
    df
  })
  
  output$ui_inspect_pagination <- renderUI({
    req(rv$raw_df)
    total_m <- nrow(matched_preview())
    page_size <- 25
    total_pages <- max(1, ceiling(total_m / page_size))
    curr_page <- min(max(1, rv$inspect_page), total_pages)
    
    if (total_pages <= 1) return(NULL)
    
    div(
      class = "inspect-pagination-group",
      actionButton(
        "btn_inspect_prev", "◀",
        class = "btn-inspect-page",
        disabled = if (curr_page <= 1) "disabled" else NULL,
        title = "Previous page"
      ),
      div(class = "inspect-page-indicator", sprintf("Page %d / %d", curr_page, total_pages)),
      actionButton(
        "btn_inspect_next", "▶",
        class = "btn-inspect-page",
        disabled = if (curr_page >= total_pages) "disabled" else NULL,
        title = "Next page"
      )
    )
  })
  
  output$inspector_status <- renderText({
    if (is.null(rv$raw_df)) {
      return("Waiting for data upload...")
    }
    total_n <- nrow(rv$raw_df)
    is_smp  <- isTRUE(rv$profile$is_sampled) && !is.null(rv$profile$full_rows)
    total_str <- if (is_smp) {
      sprintf("%s rows (Sampled from %s total rows)", format(total_n, big.mark = ","), format(rv$profile$full_rows, big.mark = ","))
    } else {
      sprintf("%s rows", format(total_n, big.mark = ","))
    }
    
    m_df <- matched_preview()
    total_m <- nrow(m_df)
    page_size <- 25
    total_pages <- max(1, ceiling(total_m / page_size))
    curr_page <- min(max(1, rv$inspect_page), total_pages)
    
    if (total_m == 0) {
      return(sprintf("0 matches found for '%s' (of %s rows)", input$tbl_search, format(total_n, big.mark = ",")))
    }
    
    start_i <- (curr_page - 1) * page_size + 1
    end_i   <- min(start_i + page_size - 1, total_m)
    
    q <- input$tbl_search
    if (!is.null(q) && nzchar(trimws(q))) {
      sprintf("Showing rows %d–%d of %s matches for query '%s' (%s)",
              start_i, end_i, format(total_m, big.mark = ","), q, total_str)
    } else {
      sprintf("Showing rows %d–%d of %s",
              start_i, end_i, total_str)
    }
  })
  
  output$tbl_preview <- renderTable({
    req(rv$raw_df)
    m_df <- matched_preview()
    if (nrow(m_df) == 0) return(data.frame())
    page_size <- 25
    total_pages <- max(1, ceiling(nrow(m_df) / page_size))
    curr_page <- min(max(1, rv$inspect_page), total_pages)
    start_i <- (curr_page - 1) * page_size + 1
    end_i   <- min(start_i + page_size - 1, nrow(m_df))
    m_df[start_i:end_i, , drop = FALSE]
  }, striped = FALSE, hover = TRUE, bordered = FALSE, spacing = "s", width = "100%")
  
  # Tab 6: Formatted Text Report
  output$tbl_report_text <- renderPrint({
    if (is.null(rv$raw_df) || is.null(rv$profile)) {
      cat("No report generated yet. Upload a data file on the left panel to begin profiling.")
    } else {
      full_p <- ensure_full_profile(rv$raw_df, rv$profile)
      cat(generate_text_report(rv$file_name, full_p))
    }
  })
}

# ==============================================================================
# 8. HEADLESS BATCH CLI EXECUTION (NON-INTERACTIVE MODE)
# ==============================================================================

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && file.exists(args[1])) {
    input_file <- args[1]
    output_dir <- "."
    
    cat("==============================================================================\n")
    cat("SCIDATAVIEW - UNIVERSAL DATASET PROFILER (CLI BATCH MODE)\n")
    cat("==============================================================================\n")
    cat("Input File :", input_file, "\n")
    
    cat("Reading file and profiling data...\n")
    df <- read_any_table(input_file, skip = "auto")
    skip_n <- attr(df, "skip_rows")
    if (!is.null(skip_n) && skip_n > 0) {
      cat(sprintf("Auto-detected and skipped %d title row(s) (Header at row %d)\n", skip_n, skip_n + 1))
    }
    p  <- profile_dataset(df)
    
    base_nm <- tools::file_path_sans_ext(basename(input_file))
    
    # 1. Output ASCII Text Report
    txt_file <- file.path(output_dir, paste0(base_nm, "_scidataview.txt"))
    writeLines(generate_text_report(input_file, p), con = txt_file, useBytes = TRUE)
    cat(sprintf("Success! Text report written to: %s\n", txt_file))
    
    # 2. Output Standalone Offline HTML Report
    html_file <- file.path(output_dir, paste0(base_nm, "_scidataview.html"))
    writeLines(generate_html_report(input_file, p), con = html_file, useBytes = TRUE)
    cat(sprintf("Success! HTML report written to: %s\n", html_file))
    
    cat("==============================================================================\n")
    quit(save = "no", status = 0)
  }
}

# Standalone Shiny App call
# This exact top-level expression is scanned by RStudio IDE to display the green 'Run App' button
shinyApp(ui = ui, server = server)
