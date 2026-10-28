library(terra)
library(stringr)
library(rvest)


base_url <- paste0(
  "https://dap.ceda.ac.uk/neodc/esacci/",
  "biomass/data/agb/maps/v6.0/geotiff"
)
# set according to the years available in CEDA v6.0
years <- c(2007, 2010)

download_dir <- "03.Data/in/AGB_v6_download"
output_dir   <- "03.Data/out/agb_mosaic_yy"
temp_dir     <- "03.Data/tmp/agb_processing"

dir.create(download_dir, recursive = TRUE,
           showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE,
           showWarnings = FALSE)
dir.create(temp_dir, recursive = TRUE,
           showWarnings = FALSE)

terraOptions(memfrac = 0.3, tempdir = temp_dir)

AD_map <- rast(
  "03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif"
)

# function for extracting complete tiles list from CEDA archive for each year and aggregating with q90 approach
is_agb_tile <- function(x) {
  grepl(
    "-AGB-MERGED-100m-.*\\.tif$",
    basename(x),
    ignore.case = TRUE
  )
}

get_expected_tiles <- function(year, base_url) {
  
  year_url <- paste0(base_url, "/", year, "/")
  
  # Read CEDA online directory
  page <- rvest::read_html(year_url)
  
  # Extract file links
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  
  # Extract filenames
  filenames <- basename(URLdecode(links))
  
  expected <- unique(filenames[is_agb_tile(filenames)])
  
  if (length(expected) == 0) {
    stop("Could not retrieve expected AGB tiles for ", year)
  }
  
  return(expected)
}

q90_grouped <- function(v, g, n_cells, min_frac = 0.10, p = 0.90) {
  
  keep <- !is.na(v) & !is.na(g)
  v <- v[keep]
  g <- as.integer(g[keep])
  
  n_all <- tabulate(g, nbins = n_cells)
  
  pos   <- v > 0
  vp    <- v[pos]
  gp    <- g[pos]
  n_pos <- tabulate(gp, nbins = n_cells)
  
  vp    <- vp[order(gp, vp, method = "radix")]
  start <- cumsum(n_pos) - n_pos            # offset iniziale di ogni gruppo
  
  out <- rep(NA_real_, n_cells)
  
  idx <- which(n_pos > 0L)
  idx <- idx[n_pos[idx] / n_all[idx] >= min_frac]
  
  if (length(idx) > 0) {
    h  <- (n_pos[idx] - 1) * p
    lo <- floor(h)
    fr <- h - lo
    i1 <- start[idx] + lo + 1
    i2 <- pmin(i1 + 1, start[idx] + n_pos[idx])
    out[idx] <- vp[i1] + fr * (vp[i2] - vp[i1])
  }
  
  out
}

aggregate_agb <- function(agb, AD_map, filename,
                          block_rows = 8, block_cols = 960) {
  
  stopifnot(
    same.crs(agb, AD_map),
    nlyr(agb) == 1,
    block_rows >= 1,
    block_cols >= 1
  )
  
  target <- rast(AD_map[[1]])
  names(target) <- "AGB_q90"
  
  nr_total <- nrow(target)
  nc_total <- ncol(target)
  rx <- res(target)[1]
  ry <- res(target)[2]
  
  terra::writeStart(
    target,
    filename  = filename,
    overwrite = TRUE,
    datatype  = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
  
  writing <- TRUE
  on.exit({
    if (writing) try(terra::writeStop(target), silent = TRUE)
  }, add = TRUE)
  
  row_starts <- seq(1, nr_total, by = block_rows)
  col_starts <- seq(1, nc_total, by = block_cols)
  
  for (i in seq_along(row_starts)) {
    
    row_start <- row_starts[i]
    row_end   <- min(row_start + block_rows - 1, nr_total)
    nr        <- row_end - row_start + 1
    
    row_buffer <- matrix(NA_real_, nrow = nr, ncol = nc_total)
    
    y_top    <- yFromRow(target, row_start) + ry / 2
    y_bottom <- yFromRow(target, row_end)   - ry / 2
    
    for (col_start in col_starts) {
      
      col_end <- min(col_start + block_cols - 1, nc_total)
      nc      <- col_end - col_start + 1
      
      x_left  <- xFromCol(target, col_start) - rx / 2
      x_right <- xFromCol(target, col_end)   + rx / 2
      
      block_ext <- ext(x_left, x_right, y_bottom, y_top)
      
      has_overlap <- !(
        xmax(agb) <= xmin(block_ext) || xmin(agb) >= xmax(block_ext) ||
          ymax(agb) <= ymin(block_ext) || ymin(agb) >= ymax(block_ext)
      )
      if (!has_overlap) next
      
      agb_block <- crop(agb, block_ext, snap = "out")
      v <- values(agb_block, mat = FALSE)
      
      if (all(is.na(v))) next
      
      # Riga/colonna target per ogni colonna/riga SORGENTE (vettori corti)
      tcol <- floor((xFromCol(agb_block, seq_len(ncol(agb_block))) - x_left) / rx) + 1
      trow <- floor((y_top - yFromRow(agb_block, seq_len(nrow(agb_block)))) / ry) + 1
      tcol[tcol < 1 | tcol > nc] <- NA
      trow[trow < 1 | trow > nr] <- NA
      
      # Cella target locale per ogni pixel (ordine row-major, come values())
      g <- (rep(trow, each = ncol(agb_block)) - 1) * nc +
        rep(tcol, times = nrow(agb_block))
      
      q <- q90_grouped(v, g, n_cells = nr * nc)
      
      row_buffer[, col_start:col_end] <- matrix(q, nrow = nr, ncol = nc, byrow = TRUE)
      
      rm(agb_block, v, tcol, trow, g, q)
    }
    
    terra::writeValues(target, as.vector(t(row_buffer)),
                       start = row_start, nrows = nr)
    
    if (i %% 25 == 0 || i == length(row_starts)) {
      message("Spatial Q90: ", round(100 * i / length(row_starts), 1), "% completed")
    }
    rm(row_buffer)
  }
  
  terra::writeStop(target)
  writing <- FALSE
  invisible(filename)
}


process_year <- function(year) {
  
  message("\n========== YEAR ", year, " ==========")
  
  final_file <- file.path(
    output_dir,
    paste0("AGBmosaic_", year, ".q90.for.tif")
  )
  
  # Skip completed years
  if (file.exists(final_file)) {
    existing <- rast(final_file)
    
    if (!compareGeom(existing, AD_map,
                     stopOnError = FALSE)) {
      stop("Existing raster has incorrect geometry: ", year)
    }
    
    message("Already processed: ", year)
    return(invisible(final_file))
  }
  
  year_dir <- file.path(download_dir, as.character(year))
  dir.create(year_dir, recursive = TRUE,
             showWarnings = FALSE)
  
  year_url <- paste0(base_url, "/", year, "/")
  
  # ==========================================================
  # STEP 1. DOWNLOAD
  # ==========================================================
  
  message("Downloading year ", year)
  
  wget_args <- c(
    "-e", "robots=off",
    "--mirror",
    "--no-parent",
    "-r",
    "-c",
    "--tries=10",
    "--retry-connrefused",
    "--waitretry=5",
    "--read-timeout=120",
    "--timeout=120",
    "--no-host-directories",
    "--cut-dirs=8",
    "--directory-prefix", shQuote(year_dir),
    "--accept", shQuote("*AGB-MERGED-100m-*.tif"),
    year_url
  )
  
  log_file <- file.path(
    output_dir, paste0("wget_", year, ".log")
  )
  
  status <- system2(
    "wget",
    args = wget_args,
    stdout = log_file,
    stderr = log_file
  )
  
  if (status != 0) {
    stop("Download failed for ", year,
         ". Check ", log_file)
  }
  
  # ==========================================================
  # STEP 2. VERIFY DOWNLOADED TILES
  # ==========================================================
  
  all_files <- list.files(
    year_dir,
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  agb_files <- all_files[is_agb_tile(all_files)]
  
  expected_files <- get_expected_tiles(year, base_url)
  
  missing_files <- setdiff(
    expected_files,
    basename(agb_files)
  )
  
  unexpected_files <- setdiff(
    basename(agb_files),
    expected_files
  )
  
  if (length(agb_files) == 0 ||
      anyDuplicated(basename(agb_files)) ||
      length(missing_files) > 0 ||
      length(unexpected_files) > 0) {
    
    stop(
      "AGB tile inventory mismatch for ", year,
      "\nMissing: ", length(missing_files),
      "\nUnexpected: ", length(unexpected_files)
    )
  }
  
  message(
    "AGB tile filenames verified: ",
    length(agb_files), "/",
    length(expected_files)
  )
  
  # ==========================================================
  # STEP 3. CREATE VIRTUAL MOSAIC
  # ==========================================================
  
  vrt_file <- file.path(
    temp_dir,
    paste0("AGB_", year, ".vrt")
  )
  
  terra::vrt(
    agb_files,
    filename = vrt_file,
    overwrite = TRUE
  )
  
  agb <- rast(vrt_file)
  
  # ==========================================================
  # STEP 4. SPATIAL Q90
  # ==========================================================
  
  aligned_tmp <- file.path(
    output_dir,
    paste0("AGB_", year, "_writing.tif")
  )
  
  message("Calculating spatial Q90 for ", year)
  
  aggregate_agb(
    agb = agb,
    AD_map = AD_map,
    filename = aligned_tmp,
    block_rows = 8,
    block_cols = 960
  )
  
  # ==========================================================
  # STEP 5. VERIFY OUTPUT
  # ==========================================================
  
  if (!file.exists(aligned_tmp) ||
      is.na(file.info(aligned_tmp)$size) ||
      file.info(aligned_tmp)$size == 0) {
    stop("Invalid output file for ", year)
  }
  
  agb_final <- rast(aligned_tmp)
  
  # Check geometry
  if (!compareGeom(
    agb_final,
    AD_map,
    stopOnError = FALSE
  )) {
    stop("Output geometry mismatch for ", year)
  }
  
  # Check AGB values
  agb_stats <- global(
    agb_final,
    fun = c("min", "max"),
    na.rm = TRUE
  )
  
  n_valid <- global(
    !is.na(agb_final),
    fun = "sum"
  )[1, 1]
  
  if (!is.finite(n_valid) || n_valid == 0 ||
      any(!is.finite(as.numeric(agb_stats))) ||
      agb_stats[1, 1] < 0) {
    stop("Invalid AGB values for ", year)
  }
  
  message("Geometry and AGB values verified")
  message("Valid cells: ", n_valid)
  print(agb_stats)
  
  # Release raster references
  rm(agb_final, agb)
  gc()
  
  # Rename completed output
  if (!file.rename(aligned_tmp, final_file)) {
    stop("Could not finalize output for ", year)
  }
  
  message("Saved: ", final_file)
  
  # ==========================================================
  # STEP 6. CLEAN UP
  # ==========================================================
  
  unlink(vrt_file)
  
  # Enable ONLY after testing the full workflow and
  # verifying downloaded tile integrity and output.
  # unlink(year_dir, recursive = TRUE)
  
  gc()
  
  message("Finished year ", year)
  
  invisible(final_file)
}

for (yr in years) {
  
  tryCatch(
    process_year(yr),
    error = function(e) {
      message("ERROR in year ", yr, ": ",
              conditionMessage(e))
    }
  )
}




for (year in c(2007, 2010)) {
  
  r <- rast(file.path(
    output_dir,
    paste0("AGBmosaic_", year, ".q90.for.tif")
  ))
  
  cat("\nYEAR:", year, "\n")
  
  print(global(
    r,
    fun = function(x, ...) {
      quantile(
        x,
        probs = c(0.5, 0.90, 0.95, 0.99, 0.999),
        na.rm = TRUE
      )
    }
  ))
}


