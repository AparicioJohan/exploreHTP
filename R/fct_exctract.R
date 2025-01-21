#' Extract Plot-Level Information from Field Trial Images
#'
#' This function processes and analyzes images from field trials to extract plot-level data, including vegetation indices, canopy cover, and other metrics. It supports images collected over time and optionally includes DSM data for calculating canopy height and volume.
#'
#' @param path_rgb Character. Path to the directory containing RGB images (GeoTIFF format).
#' @param path_dsm Character (optional). Path to the directory containing DSM images (GeoTIFF format). Required for canopy height and volume calculations.
#' @param area_of_interest `sf` or `Spatial` object. Defines the region of interest for analysis.
#' @param plot_shape `sf` or `Spatial` object. Shapefile defining the experimental plots.
#' @param plot_shape_crop `sf` or `Spatial` object (optional). Cropped shapefile for experimental plots. Defaults to `plot_shape`.
#' @param indices Character vector. Vegetation indices to calculate. Default is `c("NGRDI", "BGI", "GLI")`.
#' @param bands Character vector. Color bands to use. Default is `c("Red", "Green", "Blue")`.
#' @param index_mask Character. Index used for soil masking. Default is `"HUE"`.
#' @param mask_above Logical. If `TRUE`, masks areas with values above the threshold; otherwise, masks below. Default is `TRUE`.
#' @param threshold Numeric. Threshold value for soil masking. Must be provided.
#' @param time Character vector. Timestamps corresponding to each image in `path_rgb`.
#' @param plot_id Character (optional). Column name in `plot_shape` representing unique plot identifiers.
#' @param save_plots Logical. If `TRUE`, saves individual plot-level images for each date. Default is `FALSE`.
#' @param save_masked_plots Logical. If `TRUE`, saves soil-masked plot-level images for each date. Default is `FALSE`.
#' @param save_shape Logical. If `TRUE`, saves the extracted data as shapefiles. Default is `TRUE`.
#' @param time_serie Logical. If `TRUE`, generates time-series plots. Default is `FALSE`.
#' @param trial_name Character. Name of the trial for naming output files. Default is `"HARS22_chips"`.
#' @param path_out Character. Directory path where output files will be saved.
#' @param update_progress Function (optional). Callback function to track progress, receiving current and total image counts as arguments.
#'
#' @return A list containing:
#'   \item{dt}{A data frame with extracted metrics for each plot across all dates.}
#'   \item{info}{A data frame with metadata about the images processed.}
#'
#' @details
#' The function performs the following steps:
#' 1. Reads and processes RGB and optionally DSM images.
#' 2. Applies soil masking using the specified index and threshold.
#' 3. Calculates vegetation indices and extracts plot-level data.
#' 4. Optionally calculates canopy height and volume using DSM data.
#' 5. Saves extracted data and optional visualizations to the specified output directory.
#'
#' @examples
#' \dontrun{
#' auto_extract(
#'   path_rgb = "path/to/rgb/images",
#'   path_dsm = "path/to/dsm/images",
#'   area_of_interest = aoi_sf,
#'   plot_shape = plots_sf,
#'   indices = c("NGRDI", "BGI"),
#'   time = c("2023-05-01", "2023-06-01"),
#'   threshold = 0.2,
#'   path_out = "output/directory"
#' )
#' }
#'
#' @export
auto_extract <- function(path_rgb = NULL,
                         path_dsm = NULL,
                         area_of_interest,
                         plot_shape,
                         plot_shape_crop,
                         indices = c("NGRDI", "BGI", "GLI"),
                         bands = c("Red", "Green", "Blue"),
                         index_mask = "HUE",
                         mask_above = TRUE,
                         threshold = 0,
                         time = NULL,
                         plot_id = NULL,
                         save_plots = FALSE,
                         save_masked_plots = FALSE,
                         save_shape = TRUE,
                         time_serie = FALSE,
                         trial_name = "test",
                         path_out = NULL,
                         update_progress = NULL) {
  if (is.null(path_rgb)) {
    stop("Missing argument 'path_rgb'")
  }
  path_rgb <- list.files(path_rgb, pattern = "\\.tif$", full.names = TRUE)
  total_imgs <- length(path_rgb)
  if (total_imgs != length(time)) {
    stop("Number of images should match length of time.")
  }
  if (!is.null(path_dsm)) {
    path_dsm <- list.files(path_dsm, pattern = "\\.tif$", full.names = TRUE)
  }
  data_total <- tt <- list()
  msg <- sprintf(" [%d/%d]", 1, total_imgs)
  cli_progress_step("Extracting information {msg}", spinner = TRUE)
  for (i in seq_along(path_rgb)) {
    cli_h1("Starting: Mosaic {i}")
    msg <- sprintf(" (%d/%d)", i, total_imgs)
    cli_progress_update()
    if (!is.null(update_progress)) update_progress(i, length(path_rgb))
    # Reading raster
    t1 <- read_rast(path_rgb[i], area_of_interest)
    # Detecting soil
    cli_alert_info("Removing soil")
    if (is.null(threshold)) {
      stop("You need to provide a threshold") # calc_mask_auto
    } else {
      threshold <- as.numeric(threshold)
      t1_ns <- calc_mask(
        mosaic = t1,
        index = index_mask,
        value = threshold,
        crop_above = mask_above
      )
    }
    cli_alert_success("Soil removed with {index_mask}")
    # Save individual plots per date
    if (is.null(plot_shape_crop)) plot_shape_crop <- plot_shape
    if (save_plots) {
      path <- paste0(path_out, "/", trial_name, "/plot/", time[i], "/")
      create_folder_if_not_exists(path)
      crop_grid(t1, plot_shape_crop, plot_id = plot_id, out_dir = path)
    }
    # Save individual masked plots
    if (save_masked_plots) {
      path <- paste0(path_out, "/", trial_name, "/plot_mask/", time[i], "/")
      create_folder_if_not_exists(path)
      crop_grid(t1_ns$new, plot_shape_crop, plot_id = plot_id, out_dir = path)
    }
    # Calculating Indices
    cli_alert_info("Calculating indices")
    t1_indices <- calc_index(mosaic = t1_ns$new, index = indices)
    # Extracting Information
    cli_alert_info("Extracting information")
    t1_info <- extract_shp(
      mosaic = t1_indices[[c(bands, indices)]],
      shp = plot_shape
    )
    # Calculating GLI without mask
    cli_alert_info("Calculating indice without mask")
    t1_gli <- calc_index(t1, index = "GLI")
    # Extracting GLI Information
    t1_info_gli <- extract_shp(t1_gli[["GLI"]], shp = plot_shape)
    # Image Metadata
    tt[[i]] <- t1 |>
      mosaic_info(name = paste0("rgb_", time[i])) |>
      mutate(Time = time[i], .before = image)
    # Area Pixel
    area_pixel <- extract_shp(
      mosaic = cellSize(t1_ns$new),
      shp = plot_shape,
      fun = "mean"
    )
    # Total Area m2
    plot_area <- st_area(plot_shape)
    # Canopy
    cli_alert_info("Calculating canopy cover")
    coverage <- calc_area(mosaic = t1_ns$new, shp = plot_shape, field = plot_id)
    # Data
    dt_tmp <- t1_info |>
      mutate(Time = time[i], .before = all_of(plot_id)) |>
      mutate(canopy = coverage[["area_percentage"]]) |>
      mutate(total_pixels = coverage[["pixel_count"]]) |>
      mutate(plot_area = plot_area) |>
      mutate(area_pixel = area_pixel[["area_mean"]]) |>
      mutate(GLI_2 = t1_info_gli[["GLI_mean"]], .after = GLI_mean)
    # Digital Surface Model (DSM)
    if (!is.null(path_dsm)) {
      cli_alert_info("Digital surface model")
      if (i == 1) {
        dsm_base <- dsm_k <- read_rast(path_dsm[i], area_of_interest)
      } else {
        dsm_k <- read_rast(path_dsm[i], area_of_interest)
      }
      # Check CRS
      if (crs(dsm_base) != crs(dsm_k)) crs(dsm_k) <- crs(dsm_base)
      # Images info
      tt[[i]] <- rbind.data.frame(
        mosaic_info(t1, name = paste("rgb", time[i])),
        mosaic_info(dsm_k, name = "dsm")
      ) |>
        mutate(Time = time[i], .before = image)
      # Canopy Height Model (CHM) and Canopy Volume Model (CVM)
      cli_alert_info("Canopy height model")
      chm <- calc_height(dsm_base, dsm_k)
      chm_rem_soil <- calc_mask(chm, mask = t1_ns$mask)
      ph <- extract_shp(chm_rem_soil$new$height, shp = t1_info)
      names(ph)[names(ph) == "height_mean"] <- "ph"
      ph_vol <- extract_shp(
        mosaic = chm_rem_soil$new$volume,
        shp = ph,
        fun = "sum"
      )
      names(ph_vol)[names(ph_vol) == "volume_sum"] <- "volume"
      # Data
      dt_tmp <- ph_vol |>
        mutate(Time = time[i], .before = all_of(plot_id)) |>
        mutate(canopy = coverage[["area_percentage"]]) |>
        mutate(total_pixel = coverage[["pixel_count"]]) |>
        mutate(plot_area = plot_area) |>
        mutate(area_pixel = area_pixel[["area_mean"]]) |>
        mutate(volume = total_pixel * area_pixel * ph) |>
        mutate(GLI_2 = t1_info_gli[["GLI_mean"]], .after = GLI_mean)
    }
    # Saving Data Frame
    data_total[[i]] <- data.frame(dt_tmp)
    # Saving shape file
    if (save_shape) {
      path <- paste0(path_out, "/", trial_name, "/shape_files/")
      create_folder_if_not_exists(path)
      dt_tmp <- mutate(dt_tmp, Trial = trial_name, .before = Time)
      names(dt_tmp) <- gsub("_mean", "", names(dt_tmp))
      w_save <- paste0(path, "shape_", trial_name, ".gpkg")
      st_write(dt_tmp, w_save, layer = time[i], append = FALSE, quiet = TRUE)
      cli_alert_info(".gpkg file exported: {.path {w_save}}.")
    }
    cli_alert_success("Extraction succeded")
  }
  cli_h1("")
  # Data with all indices across dates
  dt <- do.call(what = rbind, args = data_total) |>
    as.data.frame() |>
    dplyr::select(-geom) |>
    mutate_all(~ ifelse(is.nan(.), NA, .)) |>
    mutate(Trial = trial_name, .before = Time)
  names(dt) <- gsub("_mean", "", names(dt))
  # Saving data
  path <- paste0(path_out, "/", trial_name, "/extracted_data/")
  create_folder_if_not_exists(path)
  w_save <- paste0(path, trial_name, ".csv")
  write.csv(dt, file = w_save, row.names = FALSE, na = "")
  cli_alert_info("CSV file exported: {.path {w_save}}.")
  # Metadata
  info_imgs <- as.data.frame(do.call(what = rbind, args = tt))
  # Plot Time Series
  if (time_serie) {
    plot_time_series(
      plot_shape = plot_shape,
      plot_id = plot_id,
      path_out = path_out,
      trial_name = trial_name,
      base_size = 14,
      save_plots = save_plots,
      save_masked_plots = save_masked_plots
    )
    cli_alert_success("Time series plots saved successfully.")
  }
  return(list(dt = dt, info = info_imgs))
}


mosaic_info <- function(mosaic, name = "t1") {
  dm <- dim(mosaic)
  names(dm) <- c("nrow", "ncol", "nlyr")
  res <- res(mosaic)
  names(res) <- c("res_x", "res_y")
  # coord_crs <- crs(mosaic)
  var_name <- varnames(mosaic)
  info_imgs <- data.frame(image = name, t(dm), t(res), var_name)
  return(info_imgs)
}

#' @import cli
create_folder_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cli_alert_info("Folder created: {.path {path}}.")
  }
}

crop_grid <- function(mosaic,
                      shp,
                      plot_id = NULL,
                      out_dir = "./") {
  # msgs <- "..."
  # cli_progress_step("Saving mosaics {msgs}", spinner = TRUE)
  cli_progress_message(msg = paste0("Saving mosaics ..."))
  stars_object <- mosaic
  if (class(mosaic) %in% c("RasterStack", "RasterLayer", "RasterBrick")) {
    mosaic <- terra::rast(mosaic)
  }
  nbands <- nlyr(mosaic)
  if (!inherits(stars_object, "stars")) {
    stars_object <- stars::st_as_stars(mosaic)
  }
  if (!sf::st_is_longlat(stars_object) && nbands > 2) {
    stars_object <- stars::st_warp(stars_object, crs = 4326)
  }
  if (!sf::st_is_longlat(shp)) {
    shp <- sf::st_transform(shp, crs = 4326)
  }
  total_ids <- nrow(shp)
  cli_progress_bar(
    format = paste0("Saving mosaics [{pb_current}/{pb_total}]"),
    total = total_ids
  )
  for (i in seq_len(total_ids)) {
    msgs <- sprintf(" (%d/%d)", i, total_ids)
    cli_progress_update()
    grid_poly <- shp[i, ]
    grid_poly <- sf::st_transform(grid_poly, crs = sf::st_crs(stars_object))
    plot_raster <- sf::st_crop(stars_object, grid_poly)
    if (nbands > 2) {
      plot_raster <- stars::st_warp(plot_raster, crs = sf::st_crs(mosaic))
      plot_raster[is.na(plot_raster)] <- 0
    }
    if (nbands == 1) {
      plot_raster <- stars::st_warp(plot_raster, crs = sf::st_crs(mosaic))
      plot_raster[is.na(plot_raster)] <- NA
    }
    file_name <- paste0("ID_", shp[[plot_id]][i], ".tif")
    file_path <- file.path(out_dir, file_name)
    plot_raster <- methods::as(plot_raster, "Raster")
    plot_raster <- terra::rast(plot_raster)
    terra::writeRaster(plot_raster, file_path, overwrite = TRUE)
  }
}

plot_organizer <- function(id,
                           plot_id = NULL,
                           path,
                           path_shape,
                           angle = NULL,
                           color = "white",
                           base_size = 12,
                           color_grid = "red",
                           remove_border = TRUE) {
  df <- info <- list()
  folders <- sort(as.numeric(list.files(path)))
  for (i in folders) {
    path_tmp <- list.files(
      path = paste0(path, i),
      pattern = paste0("_", id, ".tif"),
      full.names = TRUE
    )
    mosaic <- terra::rast(path_tmp)
    names(mosaic)[1:3] <- c("Red", "Green", "Blue")
    grid_shape <- path_shape |>
      st_read(layer = paste(i), quiet = TRUE) |>
      filter(.data[[plot_id]] %in% id) |>
      rename(geometry = geom)
    if (!is.null(angle)) {
      y <- raster::stack(mosaic)
      raster::crs(y) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
      mosaic <- raster::projectRaster(
        from = y,
        res = res(y),
        crs = paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", angle)
      )
      suppressWarnings(
        st_crs(grid_shape) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
      )
      grid_shape <- st_transform(grid_shape, crs = st_crs(mosaic))
    }
    p <- paste0(i)
    df[[p]] <- mosaic |>
      as.data.frame(xy = TRUE) |>
      dplyr::select(x, y, Red, Green, Blue) |>
      dplyr::mutate(Time = i, Plot = id)
    info[[p]] <- grid_shape
  }
  info <- sf::st_as_sf(do.call(what = rbind, args = info))
  df <- do.call(what = rbind, args = df) |>
    dplyr::filter(Red >= 0 & Blue >= 0 & Green >= 0) |>
    dplyr::select(x, y, Red:Blue, Time, Plot) |>
    na.omit()
  if (remove_border) {
    df <- df |>
      dplyr::mutate(
        RGB = rgb(
          red = Red,
          green = Green,
          blue = Blue,
          maxColorValue = 255
        )
      ) |>
      dplyr::filter(!RGB %in% "#000000")
  }
  p0 <- df |>
    ggplot() +
    geom_raster(
      mapping = aes(
        x = x,
        y = y,
        fill = rgb(red = Red, green = Green, blue = Blue, maxColorValue = 255)
      ),
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_sf(data = info, color = color_grid, fill = NA) +
    theme_void(base_size = base_size) +
    facet_wrap(Plot ~ Time, nrow = 1, labeller = label_both) +
    theme(strip.text = element_text(colour = color))
  out <- list(df = df, info = info, figure = p0)
  return(out)
}

plot_time_series <- function(plot_shape,
                             plot_id,
                             path_out,
                             trial_name,
                             base_size = 14,
                             save_plots = TRUE,
                             save_masked_plots = TRUE) {
  if (!save_plots & !save_masked_plots) {
    return()
  }
  unique_ids <- sort(unique(plot_shape[[plot_id]]))
  msgs <- "..."
  k <- 1
  cli_progress_step("Saving time series {msgs}", spinner = TRUE)
  for (w in unique_ids) {
    msgs <- sprintf(" (%d/%d)", k, length(unique_ids))
    cli_progress_update()
    path_shape <- paste0(
      path_out, "/", trial_name,
      "/shape_files/shape_", trial_name, ".gpkg"
    )
    if (save_plots) {
      p0 <- plot_organizer(
        id = w,
        plot_id = plot_id,
        path = paste0(path_out, "/", trial_name, "/plot/"),
        path_shape = path_shape,
        base_size = 14
      )
      create_folder_if_not_exists(
        path = paste0(path_out, "/", trial_name, "/plots_time/")
      )
      ggsave(
        filename = paste0(
          path_out, "/", trial_name, "/plots_time/ID_", w, ".png"
        ),
        plot = p0$figure,
        units = "in",
        dpi = 300,
        width = 8,
        height = 6
      ) |> suppressWarnings()
    }
    if (save_masked_plots) {
      p1 <- plot_organizer(
        id = w,
        plot_id = plot_id,
        path = paste0(path_out, "/", trial_name, "/plot_mask/"),
        path_shape = path_shape,
        base_size = 14
      )
      ggsave(
        filename = paste0(
          path_out, "/", trial_name, "/plots_time/ID_", w, "_mask.png"
        ),
        plot = p1$figure,
        units = "in",
        dpi = 300,
        width = 8,
        height = 6
      ) |> suppressWarnings()
    }
    k <- k + 1
  }
}



extract_shp <- function(mosaic, shp = NULL, fun = "mean", progress = FALSE) {
  # Validate inputs
  if (is.null(mosaic)) {
    stop("The input 'mosaic' cannot be NULL.")
  }
  if (is.null(shp)) {
    stop("The input 'shp' (shapefile or spatial object) cannot be NULL.")
  }
  if (inherits(mosaic, c("RasterStack", "RasterLayer", "RasterBrick"))) {
    mosaic <- terra::rast(mosaic)
  }
  # Valid functions
  valid_functions <- c(
    "mean", "sum", "max", "min", "mode", "stdev", "variance",
    "coefficient_of_variation", "majority", "minority", "summary"
  )
  # Validate function argument
  if (!fun %in% valid_functions) {
    stop(paste(
      "Invalid 'fun' argument. Use one of:",
      paste(valid_functions, collapse = ", ")
    ))
  }
  # Expand "summary" into all statistical functions
  if (fun == "summary") {
    fun <- setdiff(valid_functions, "summary") # Exclude "summary" itself
  }
  # Perform exact extraction
  extracted_data <- exactextractr::exact_extract(
    x = mosaic,
    y = shp,
    fun = fun,
    progress = progress,
    force_df = TRUE
  ) |> as.data.frame()
  # Assign appropriate column names
  colnames(extracted_data) <- paste0(names(mosaic), "_", fun)
  # Combine shapefile and extracted data
  output <- cbind(
    shp,
    extracted_data[, !colnames(extracted_data) %in% "ID", drop = FALSE]
  )
  return(output)
}


calc_area <- function(mosaic, shp, field = NULL) {
  # Validate inputs
  if (is.null(mosaic)) {
    stop("The input 'mosaic' cannot be NULL.")
  }
  if (is.null(shp)) {
    stop("The input 'shp' (shapefile or spatial object) cannot be NULL.")
  }
  # Convert shp to terra vector
  terra_vect <- terra::vect(shp)
  # Determine the rasterization field
  raster_field <- if (is.null(field)) stop("field can not be NULL") else field
  if (!(raster_field %in% names(terra_vect))) {
    stop(paste(
      "The field", raster_field, "is not found in the shapefile attributes."
    ))
  }
  # Rasterize using the specified field
  terra_rast <- terra::rasterize(terra_vect, mosaic, field = raster_field)
  polygons_sf <- sf::st_as_sf(terra::as.polygons(terra_rast))
  # Calculate total pixel count and area pixels
  total_pixel_count <- exactextractr::exact_extract(
    x = terra_rast,
    y = polygons_sf,
    fun = "count",
    force_df = TRUE
  )
  area_pixel <- exactextractr::exact_extract(
    x = mosaic[[1]],
    y = polygons_sf,
    fun = "count",
    force_df = TRUE
  )
  # Calculate area percentage
  area_percentage <- round(area_pixel / total_pixel_count * 100, 3)
  colnames(area_percentage) <- "area_percentage"
  colnames(area_pixel) <- "pixel_count"
  # Combine results with geometry
  result <- cbind(polygons_sf, area_pixel, area_percentage)
  return(result)
}


calc_index <- function(mosaic,
                       red = 1,
                       green = 2,
                       blue = 3,
                       rededge = NULL,
                       nir = NULL,
                       index = "HUE") {
  catalog <- exploreHTP::indices
  num_band <- nlyr(mosaic)
  msgs <- "..."
  cli_progress_step("Calculating indice {msgs}", spinner = TRUE)
  if (num_band < 3) {
    stop("At least 3 bands (RGB) are necessary to calculate indices")
  }
  if (!is.null(rededge) || !is.null(nir)) {
    if (num_band < 4) {
      stop("RedEdge and/or NIR is/are not available in your mosaic")
    }
  }
  lister <- as.character(catalog$index)
  if (is.null(index)) {
    stop("Choose one or more indices")
  }
  if (!all(index %in% lister)) {
    stop(paste("Index: ", index[!index %in% lister], " is not available"))
  }
  nir_re <- as.character(catalog$index[catalog$band %in% c("RedEdge", "NIR")])
  if (any(nir_re %in% index) && is.null(nir)) {
    stg <- paste("Index: ", paste(nir_re[nir_re %in% index], collapse = ", "))
    stop(paste0(stg, " needs NIR/RedEdge band to be calculated"))
  }
  B <- mosaic[[blue]]
  cli_progress_update()
  G <- mosaic[[green]]
  cli_progress_update()
  R <- mosaic[[red]]
  names(mosaic)[c(blue, green, red)] <- c("Blue", "Green", "Red")
  if (!is.null(rededge)) {
    RE <- mosaic[[rededge]]
    names(mosaic)[rededge] <- "RedEdge"
  }
  if (!is.null(nir)) {
    NIR1 <- mosaic[[nir]]
    names(mosaic)[nir] <- "NIR"
  }
  for (i in seq_along(index)) {
    value <- index[i]
    msgs <- sprintf("%s (%d/%d)", value, i, length(index))
    cli_progress_update()
    new_layer <- eval(parse(text = paste(catalog$eq[catalog$index == value])))
    mosaic <- append(mosaic, new_layer)
    names(mosaic)[num_band + i] <- as.character(value)
  }
  return(mosaic)
}

calc_mask <- function(mosaic,
                      red = 1,
                      green = 2,
                      blue = 3,
                      rededge = NULL,
                      nir = NULL,
                      index = "HUE",
                      value = 0,
                      crop_above = TRUE,
                      mask = NULL) {
  num_band <- nlyr(mosaic)
  cli_progress_message("Masking images ...")
  if (is.null(mask)) {
    mr <- calc_index(
      mosaic = mosaic,
      red = red,
      green = green,
      blue = blue,
      rededge = rededge,
      nir = nir,
      index = index
    )[[num_band + 1]]
    if (crop_above) {
      m <- mr > value
    } else {
      m <- mr < value
    }
    mosaic <- terra::mask(mosaic, m, maskvalue = TRUE)
  } else {
    if (nlyr(mask) > 1) {
      stop("Mask must have only one band.")
    }
    mr <- mask
    mosaic <- terra::crop(x = mosaic, y = mr)
    mosaic <- terra::project(mosaic, mask, method = "near")
    if (crop_above) {
      m <- mr > value
    } else {
      m <- mr < value
    }
    mosaic <- terra::mask(mosaic, m, maskvalue = TRUE)
  }
  out <- list(new = mosaic, mask = m)
  return(out)
}

# calc_mask_auto <- function(mosaic, index, above = TRUE) {
#   ensure_ebimage()
#   ind <- calc_index(mosaic = mosaic, index = index)
#   tr <- as.array(ind[[index]])[, , 1]
#   tr[is.nan(tr)] <- NA
#   tr[is.infinite(tr)] <- NA
#   tr <- EBImage::otsu(
#     x = tr,
#     range = c(min(tr, na.rm = TRUE), max(tr, na.rm = TRUE))
#   )
#   if (above) {
#     m <- ind[[index]] > tr
#   } else {
#     m <- ind[[index]] < tr
#   }
#   new <- terra::mask(mosaic, m, maskvalue = TRUE)
#   return(list(new = new, mask = m))
# }

calc_height <- function(dsm_before, dsm_after) {
  if (!inherits(dsm_before, "SpatRaster") || !nlyr(dsm_before) ==
    1 || terra::is.bool(dsm_before) || is.list(dsm_before)) {
    stop("Error: Invalid 'dsm_before' raster object.")
  }
  if (!inherits(dsm_after, "SpatRaster") || !nlyr(dsm_after) ==
    1 || terra::is.bool(dsm_after) || is.list(dsm_after)) {
    stop("Error: Invalid 'dsm_after' raster object.")
  }
  dsm_ini <- resample(dsm_before, dsm_after)
  height <- dsm_after - dsm_ini
  names(height) <- "height"
  volume <- terra::cellSize(height) * height
  names(volume) <- "volume"
  mosaic <- append(height, volume)
  return(mosaic)
}

read_rast <- function(path, area_of_interest = NULL) {
  if (!is.null(area_of_interest)) {
    img <- crop(rast(path), area_of_interest)
  } else {
    img <- rast(path)
  }
  return(img)
}
