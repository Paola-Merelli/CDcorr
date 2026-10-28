# ESA CCI Biomass v6.0 -- annual spatial aggregation onto Sabatini grid
# 10-band raster for each year:
# 7 AGB statistics: q85, q90, q95, mean all, median all, mean positive, median positive
# + 3 source-pixel counts: n zeros, n NAs, n positive
#
# PARALLEL VERSION: each worker processes one band of `block_rows` Sabatini rows
# (full width, split in column blocks of `block_cols`); the main process writes
# the bands in order.

library(terra)
library(rvest)
library(stringr)
library(parallel)
library(matrixStats)

base_url <- paste0(
  "https://dap.ceda.ac.uk/neodc/esacci/",
  "biomass/data/agb/maps/v6.0/geotiff"
)

years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

download_dir <- "03.Data/in/AGB_v6_download"
output_dir   <- "03.Data/out/agb_mosaic_yy"
temp_dir     <- "03.Data/tmp/agb_processing"
AD_path      <- "03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif"

block_rows <- 8L
block_cols <- 480L
n_workers  <- 8L        
bands_per_worker <- 8L
delete_tiles <- TRUE 

dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

terraOptions(memfrac = 0.5, tempdir = temp_dir)
AD_map <- rast(AD_path)

metric_names <- c(
  "q85", "q90", "q95",
  "mean_positive", "median_positive",
  "mean_all", "median_all",
  "n_positive", "n_zero", "n_NA"
)

#####################
# FUNCTIONS
#####################

# filter only AGB tiles and exclude SD
is_agb_tile <- function(filenames) {
  grepl("AGB-MERGED", basename(filenames), fixed = TRUE) &
    grepl("\\.tif(f)?$", filenames, ignore.case = TRUE)
}

# get complete tile list from the CEDA archive for a given year
get_expected_tiles <- function(year, base_url) {
  year_url <- paste0(base_url, "/", year, "/")
  page <- rvest::read_html(year_url)
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  
  filenames <- basename(URLdecode(links))
  expected <- unique(filenames[is_agb_tile(filenames)])
  
  if (length(expected) == 0) {
    stop("Could not download tiles for ", year)
  }
  expected
}

# calculate 10 bands for each target cell 
calculate_metrics <- function(v, g, n_cells) {
  assigned <- !is.na(g) & g >= 1 & g <= n_cells
  g <- as.integer(g[assigned])
  v <- v[assigned]
  
  ans <- setNames(
    replicate(length(metric_names), rep(NA_real_, n_cells), simplify = FALSE),
    metric_names
  )
  
  n_total <- tabulate(g, nbins = n_cells)
  is_na <- is.na(v)
  ans$n_NA <- as.numeric(tabulate(g[is_na], nbins = n_cells))
  ans$n_zero <- as.numeric(tabulate(g[!is_na & v == 0], nbins = n_cells))
  ans$n_positive <- as.numeric(tabulate(g[!is_na & v > 0], nbins = n_cells))
  
  keep_valid <- !is_na
  va <- v[keep_valid]
  ga <- g[keep_valid]
  n_valid <- tabulate(ga, nbins = n_cells)
  
  keep_pos <- va > 0
  vp <- va[keep_pos]
  gp <- ga[keep_pos]
  n_pos <- tabulate(gp, nbins = n_cells)
  
  # Internal consistency for all source pixels assigned to a target cell
  if (any(n_total != n_valid + ans$n_NA) ||
      any(n_valid != ans$n_positive + ans$n_zero)) {
    stop("Inconsistent source-pixel counts in a block")
  }
  
  group_quantile <- function(sorted_values, counts, idx, p) {
    starts <- cumsum(counts) - counts
    h <- (counts[idx] - 1) * p
    lower <- floor(h)
    fraction <- h - lower
    i1 <- starts[idx] + lower + 1
    i2 <- pmin(i1 + 1, starts[idx] + counts[idx])
    sorted_values[i1] + fraction * (sorted_values[i2] - sorted_values[i1])
  }
  
  # Summaries over all non-NA source pixels (including zero)
  observed <- which(n_valid > 0L)
  if (length(observed)) {
    sums <- rowsum(va, ga, reorder = TRUE)
    group_ids <- as.integer(rownames(sums))
    ans$mean_all[group_ids] <- as.numeric(sums[, 1]) / n_valid[group_ids]
    
    va_sorted <- va[order(ga, va, method = "radix")]
    ans$median_all[observed] <- group_quantile(
      va_sorted, n_valid, observed, 0.50
    )
  }
  
  # Summaries over strictly positive source pixels; NO 10% threshold
  positive_cells <- which(n_pos > 0L)
  if (length(positive_cells)) {
    sums <- rowsum(vp, gp, reorder = TRUE)
    group_ids <- as.integer(rownames(sums))
    ans$mean_positive[group_ids] <- as.numeric(sums[, 1]) / n_pos[group_ids]
    
    vp_sorted <- vp[order(gp, vp, method = "radix")]
    ans$q85[positive_cells] <- group_quantile(vp_sorted, n_pos, positive_cells, 0.85)
    ans$q90[positive_cells] <- group_quantile(vp_sorted, n_pos, positive_cells, 0.90)
    ans$q95[positive_cells] <- group_quantile(vp_sorted, n_pos, positive_cells, 0.95)
    ans$median_positive[positive_cells] <- group_quantile(
      vp_sorted, n_pos, positive_cells, 0.50
    )
  }
  
  ans
}

# each worker: one band of `block_rows` Sabatini rows over the full width.
# Returns a (cells x 10 layers) matrix, already in writeValues order.
process_band <- function(row_start, geom, block_rows, block_cols) {
  agb <- get(".agb_worker", envir = .GlobalEnv)
  rx <- geom$rx; ry <- geom$ry; nc_total <- geom$ncol
  count_names <- c("n_positive", "n_zero", "n_NA")
  
  row_end  <- min(row_start + block_rows - 1L, geom$nrow)
  nr       <- row_end - row_start + 1L
  y_top    <- geom$y_top_all[row_start]
  y_bottom <- geom$y_bottom_all[row_end]
  
  buffers <- setNames(lapply(metric_names, function(nm) {
    matrix(if (nm %in% count_names) 0 else NA_real_, nrow = nr, ncol = nc_total)
  }), metric_names)
  
  for (col_start in seq.int(1L, nc_total, by = block_cols)) {
    col_end <- min(col_start + block_cols - 1L, nc_total)
    nc      <- col_end - col_start + 1L
    x_left  <- geom$x_left_all[col_start]
    x_right <- geom$x_right_all[col_end]
    
    # skip blocks completely outside the AGB extent
    if (terra::xmax(agb) <= x_left || terra::xmin(agb) >= x_right ||
        terra::ymax(agb) <= y_bottom || terra::ymin(agb) >= y_top) next
    
    agb_block <- terra::crop(agb, terra::ext(x_left, x_right, y_bottom, y_top),
                             snap = "out")
    v <- terra::values(agb_block, mat = FALSE)
    
    # assign source-pixel centres to Sabatini cells
    tcol <- floor((terra::xFromCol(agb_block, seq_len(ncol(agb_block))) - x_left) / rx) + 1L
    trow <- floor((y_top - terra::yFromRow(agb_block, seq_len(nrow(agb_block)))) / ry) + 1L
    tcol[tcol < 1L | tcol > nc] <- NA_integer_
    trow[trow < 1L | trow > nr] <- NA_integer_
    g <- (rep(trow, each = ncol(agb_block)) - 1L) * nc +
      rep(tcol, times = nrow(agb_block))
    
    metrics <- calculate_metrics(v, g, n_cells = nr * nc)
    for (nm in metric_names) {
      buffers[[nm]][, col_start:col_end] <- matrix(
        metrics[[nm]], nrow = nr, ncol = nc, byrow = TRUE
      )
    }
    rm(agb_block, v, tcol, trow, g, metrics)
  }
  
  do.call(cbind, lapply(buffers, function(x) as.vector(t(x))))
}

# Parallel aggregation of AGB (VRT path) onto the AD grid
aggregate_agb_metrics <- function(agb_path, AD_map, filename,
                                  block_rows = 8L, block_cols = 480L,
                                  n_workers = 12L, bands_per_worker = 8L) {
  
  rx <- res(AD_map)[1]
  ry <- res(AD_map)[2]
  
  geom <- list(
    nrow = nrow(AD_map), ncol = ncol(AD_map), rx = rx, ry = ry,
    # edges computed with the same formulas as the original serial code
    y_top_all    = yFromRow(AD_map, seq_len(nrow(AD_map))) + ry / 2,
    y_bottom_all = yFromRow(AD_map, seq_len(nrow(AD_map))) - ry / 2,
    x_left_all   = xFromCol(AD_map, seq_len(ncol(AD_map))) - rx / 2,
    x_right_all  = xFromCol(AD_map, seq_len(ncol(AD_map))) + rx / 2
  )
  
  target <- rast(nrows = geom$nrow, ncols = geom$ncol,
                 nlyrs = length(metric_names),
                 ext = ext(AD_map), crs = crs(AD_map))
  names(target) <- metric_names
  
  # cluster: each worker reopens the VRT itself (SpatRaster can't be serialized)
  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, c("calculate_metrics", "metric_names", "process_band"),
                          envir = globalenv())
  invisible(parallel::clusterCall(cl, function(p) {
    library(terra)
    terraOptions(threads = 1)                  # no nested threading
    setGDALconfig("GDAL_CACHEMAX", "512")      # MB per worker
    assign(".agb_worker", rast(p), envir = .GlobalEnv)
    NULL
  }, agb_path))
  
  terra::writeStart(target, filename = filename, overwrite = TRUE,
                    datatype = "FLT4S",
                    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))
  opened <- TRUE
  on.exit({if (opened) try(terra::writeStop(target), silent = TRUE)}, add = TRUE)
  
  row_starts <- seq.int(1L, geom$nrow, by = block_rows)
  chunk_size <- n_workers * bands_per_worker
  chunks <- split(row_starts, ceiling(seq_along(row_starts) / chunk_size))
  
  done <- 0L
  for (ch in chunks) {
    out <- parallel::parLapplyLB(cl, ch, process_band, geom = geom,
                                 block_rows = block_rows, block_cols = block_cols,
                                 chunk.size = 1L)
    # sequential, ordered writing
    for (k in seq_along(ch)) {
      terra::writeValues(target, out[[k]], start = ch[k],
                         nrows = nrow(out[[k]]) / geom$ncol)
    }
    done <- done + length(ch)
    message("Spatial metrics: ", round(100 * done / length(row_starts), 1), "%")
    rm(out); gc(verbose = FALSE)
  }
  
  terra::writeStop(target)
  opened <- FALSE
  invisible(filename)
}

# function for processing one year
process_year <- function(year) {
  message("\n========== YEAR ", year, " ==========")
  
  final_file <- file.path(output_dir, paste0("AGBmetrics_", year, ".tif"))
  
  if (file.exists(final_file)) {
    message("Already processed: ", year)
    return(invisible(final_file))
  }
  
  year_dir <- file.path(download_dir, as.character(year))
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  year_url <- paste0(base_url, "/", year, "/")
  marker   <- file.path(year_dir, ".download_complete")
  
  list_local_tiles <- function() {
    f <- list.files(year_dir, pattern = "\\.tif(f)?$", full.names = TRUE,
                    recursive = TRUE, ignore.case = TRUE)
    f[is_agb_tile(f)]
  }
  
  # ---- 1) download: saltato se una run precedente l'ha già verificato
  skip_download <- FALSE
  if (file.exists(marker)) {
    expected_files <- readLines(marker)
    agb_files <- list_local_tiles()
    if (length(setdiff(expected_files, basename(agb_files))) == 0) {
      skip_download <- TRUE
      message("Tiles already downloaded and verified (", length(agb_files), "): skipping download")
    } else {
      message("Marker found but some tiles are missing: re-running download")
    }
  }
  
  if (!skip_download) {
    expected_files <- get_expected_tiles(year, base_url)
    
    # solo tile AGB (niente AGB_SD), lista di URL esplicita, ripresa con -c.
    # tile_dir = year_dir/<anno>: stesso layout del vecchio --mirror --cut-dirs=8
    tile_dir <- file.path(year_dir, as.character(year))
    dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)
    url_file <- file.path(temp_dir, paste0("urls_", year, ".txt"))
    writeLines(paste0(year_url, expected_files), url_file)
    log_file <- file.path(output_dir, paste0("wget_", year, ".log"))
    
    wget_args <- c("-nv", "-c", "--tries=10", "--retry-connrefused",
                   "--retry-on-http-error=503", "--waitretry=5",
                   "--read-timeout=120", "--timeout=120",
                   "-i", url_file, "-P", tile_dir, "-a", log_file)
    
    # wget esce != 0 se ANCHE UN SOLO file fallisce (es. 503 transitori):
    # ritenta, e considera il download riuscito solo con exit 0 e tutti i tile presenti
    ok_dl <- FALSE
    for (attempt in 1:3) {
      message("Downloading year ", year, " (attempt ", attempt, ")")
      status <- system2("wget", args = wget_args, stdout = FALSE, stderr = FALSE)
      agb_files <- list_local_tiles()
      missing_files <- setdiff(expected_files, basename(agb_files))
      if (status == 0 && length(missing_files) == 0) { ok_dl <- TRUE; break }
      message("  wget status ", status, ", ", length(missing_files), " tiles missing; retrying")
      Sys.sleep(30)
    }
    if (!ok_dl) {
      stop("Download not completed for ", year, " (wget status ", status, ", ",
           length(missing_files), " tiles missing). Check ", log_file,
           ". Files retained for retry.")
    }
    
    message("AGB tiles found: ", length(agb_files))
    writeLines(expected_files, marker)
    message("All AGB tiles downloaded")
    unlink(url_file)
  }
  
  # ---- 2) aggregazione
  vrt_file <- file.path(temp_dir, paste0("AGB_", year, ".vrt"))
  terra::vrt(agb_files, filename = vrt_file, overwrite = TRUE)
  
  writing_file <- file.path(output_dir, paste0("AGB_", year, "_writing.tif"))
  aggregate_agb_metrics(agb_path = vrt_file, AD_map = AD_map, filename = writing_file,
                        block_rows = block_rows, block_cols = block_cols,
                        n_workers = n_workers, bands_per_worker = bands_per_worker)
  
  gc()
  if (!file.rename(writing_file, final_file)) stop("Cannot finalize ", final_file)
  unlink(vrt_file)
  
  # ---- 3) valida l'output, POI cancella i tile
  chk <- rast(final_file)
  ok <- nlyr(chk) == length(metric_names) &&
    identical(names(chk), metric_names) &&
    nrow(chk) == nrow(AD_map) && ncol(chk) == ncol(AD_map) &&
    is.finite(global(chk[["q95"]], "max", na.rm = TRUE)[1, 1])
  rm(chk)
  if (!ok) stop("Output validation failed for ", year, ": tiles NOT deleted (", final_file, ")")
  message("Saved and validated: ", final_file)
  
  if (delete_tiles) {
    unlink(year_dir, recursive = TRUE)
    message("Tiles removed: ", year_dir)
  }
  
  invisible(final_file)
}



#####################
# MAIN
#####################
for (yr in years) process_year(yr)


##########################
# temporal aggregation q90
##########################

files <- list.files("03.Data/out/agb_mosaic_yy",
                    pattern = "\\.tif$", full.names = TRUE)

out_file <- "03.Data/out/potential_AGB_final.tif"
n_workers <- 8L
strip <- 50L

tmpl <- rast(files[1], lyrs = "q90")
nr <- nrow(tmpl)
nc <- ncol(tmpl)

# Process one strip
q90_strip <- function(r0) {
  n <- min(strip, nr - r0 + 1L)
  
  m <- vapply(files, function(f) {
    r <- rast(f, lyrs = "q90")
    readStart(r)
    x <- readValues(r, row = r0, nrows = n)
    readStop(r)
    x
  }, numeric(n * nc))
  
  rowQuantiles(m, probs = 0.90, na.rm = TRUE, drop = TRUE)
}

# Initialize output
out <- rast(tmpl)
names(out) <- "AGB_q90_temporal"

writeStart(out, out_file, overwrite = TRUE,
           datatype = "FLT4S",
           gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))

# Parallel processing and sequential writing
starts <- seq.int(1L, nr, by = strip)
batches <- split(starts, ceiling(seq_along(starts) / (n_workers * 2L)))

for (b in batches) {
  res <- mclapply(b, q90_strip, mc.cores = n_workers)
  
  if (any(vapply(res, inherits, logical(1), "try-error")))
    stop("Error in parallel processing")
  
  for (k in seq_along(b)) {
    writeValues(out, res[[k]], b[k],
                min(strip, nr - b[k] + 1L))
  }
}

out <- writeStop(out)

