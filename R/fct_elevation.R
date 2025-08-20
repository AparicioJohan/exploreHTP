#' Align and write DSM rasters using ground-control points
#'
#' @description
#' Aligns a time series of DSM (Digital Surface Model) rasters by estimating
#' per-time vertical offsets from ground-control (GC) points, writes the
#' adjusted rasters to disk, and returns a summary (including diagnostic plots).
#'
#' @details
#' For each raster in `path_in`, values are extracted at GC point locations
#' (`gc_points`). Using the first element of `times` as the baseline time,
#' the function computes, for every other time, the mean difference
#' \eqn{E[DSM_\mathrm{baseline} - DSM_t]} over GC points that have non-NA
#' baseline values. Each raster is then shifted by its estimated offset and
#' written to `path_out` with the original file name.
#'
#' Two ggplot objects are returned to help diagnose alignment:
#' a "Before" plot (raw DSM at GC points over time) and an "After" plot
#' (offset-adjusted DSM at GC points over time).
#'
#' @param path_in `character(1)`. Directory containing input DSM rasters.
#' @param path_out `character(1)`. Output directory where adjusted rasters
#'   will be written. Created recursively if it does not exist.
#' @param gc_points `character(1)`. Path to a vector file readable by
#'   [sf::read_sf()] containing **point** features for ground control.
#'   Must include a unique identifier column named `ID`.
#' @param times `character()` or `numeric()`. A vector of time labels
#'   (e.g., acquisition times or survey indices) with **length equal to the
#'   number of DSM files** matched by `pattern`. The first element is treated
#'   as the baseline.
#' @param pattern `character(1)`. Regular expression used by [base::list.files()]
#'   to find input rasters (default `\\.tif$`).
#'
#' @return
#' A `list` with components:
#' \itemize{
#'   \item `files_in` — full paths to the input rasters.
#'   \item `times` — the time labels provided.
#'   \item `baseline` — the baseline time label.
#'   \item `offsets` — a data frame with columns `Time` and `offset`
#'     (mean baseline minus current).
#'   \item `out_dir` — normalized output directory path.
#'   \item `plot` — a length-2 list of ggplot objects: before/after alignment.
#' }
#'
#' @section Assumptions:
#' \itemize{
#'   \item Each raster is a **single-layer** DSM.
#'   \item The GC points layer contains a unique identifier column named `ID`.
#'   \item The length of `times` matches the number of raster files found by
#'     `list.files(path_in, pattern)`.
#' }
#'
#' @section Output files:
#' One adjusted GeoTIFF per input file is written to `path_out` with
#' the same base file name. Existing files are overwritten.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Inputs
#' in_dir  <- "data/dsm_raw"
#' out_dir <- "data/dsm_aligned"
#' gcp     <- "data/ground_control.gpkg"  # must have column ID
#'
#' # Time labels (first is baseline)
#' tvec <- c("2025-06-01", "2025-06-10", "2025-06-20")
#'
#' res <- align_write_dsms(
#'   path_in   = in_dir,
#'   path_out  = out_dir,
#'   gc_points = gcp,
#'   times     = tvec,
#'   pattern   = "\\.tif$"
#' )
#'
#' # Inspect offsets and plots
#' res$offsets
#' res$plot[[1]]  # before
#' res$plot[[2]]  # after
#' }
#'
#' @seealso [terra::rast()], [terra::extract()], [sf::read_sf()], [ggplot2::ggplot]
#'
#' @importFrom terra rast extract writeRaster
#' @export
align_write_dsms <- function(path_in,
                             path_out,
                             gc_points,
                             times,
                             pattern = "\\.tif$") {
  stopifnot(dir.exists(path_in))
  if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)
  files <- list.files(path_in, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No raster files found in 'path_in'")
  if (length(times) != length(files)) {
    stop(sprintf(
      "Length of 'times' (%d) must match number of files (%d).",
      length(times), length(files)
    ))
  }
  baseline_time <- times[1]
  rs <- lapply(files, \(x) {
    new_x <- rast(x)
    names(new_x) <- "dsm"
    new_x
  })
  names(rs) <- times
  gc <- sf::read_sf(gc_points) # sf points
  vals <- do.call(
    rbind,
    lapply(seq_along(rs), function(i) {
      df <- terra::extract(rs[[i]], gc)
      df$Time <- times[i]
      df
    })
  )
  baseline <- vals |>
    dplyr::filter(Time == baseline_time) |>
    dplyr::select(ID, DSM_baseline = dsm)
  joined <- dplyr::left_join(vals, baseline, by = "ID")
  offsets <- joined |>
    dplyr::filter(Time != baseline_time, !is.na(DSM_baseline)) |>
    dplyr::group_by(Time) |>
    dplyr::summarise(offset = mean(DSM_baseline - dsm), .groups = "drop")
  offsets <- dplyr::bind_rows(
    data.frame(Time = baseline_time, offset = 0),
    offsets
  )
  off_map <- setNames(offsets$offset, offsets$Time)
  for (i in seq_along(files)) {
    .y <- times[i]
    r_in <- rs[[i]]
    r_out <- r_in + off_map[[as.character(.y)]]
    out_fn <- file.path(path_out, basename(files[i]))
    writeRaster(r_out, filename = out_fn, overwrite = TRUE)
  }
  df_with_offsets <- vals %>%
    dplyr::left_join(offsets, by = "Time") %>%
    dplyr::mutate(DSM_adj = dsm + offset) %>%
    dplyr::mutate(ID = as.factor(ID))
  # Figures
  p1 <- df_with_offsets |>
    ggplot(
      mapping = aes(x = Time, y = dsm, color = ID, group = ID)
    ) +
    geom_point() +
    geom_line() +
    theme_classic() +
    labs(title = "Before") +
    ylim(c(range(df_with_offsets$dsm) + c(-0.5, 0.5)))
  p1
  p2 <- df_with_offsets |>
    ggplot(
      mapping = aes(x = Time, y = DSM_adj, color = ID, group = ID)
    ) +
    geom_point() +
    geom_line() +
    theme_classic() +
    labs(title = "After") +
    ylim(c(range(df_with_offsets$dsm) + c(-0.5, 0.5)))
  p2
  list(
    files_in   = files,
    times      = times,
    baseline   = baseline_time,
    offsets    = offsets,
    out_dir    = normalizePath(path_out),
    plot       = list(p1, p2)
  )
}
