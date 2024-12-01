auto_extract <- function(path_rgb = NULL,
                         path_dsm = NULL,
                         area_of_interest,
                         plot_shape,
                         plot_shape_crop,
                         indices = c("NGRDI", "BGI", "GLI"),
                         bands = c("Red", "Green", "Blue"),
                         dap,
                         plot_id = NULL,
                         save_plots = FALSE,
                         save_shape = TRUE,
                         time_serie = FALSE,
                         name_experiment = "HARS22_chips",
                         path_out = NULL,
                         update_progress = NULL) {
  if (is.null(path_rgb)) {
    stop("Missing argument 'path_rgb'")
  }
  path_rgb <- list.files(path_rgb, pattern = "\\.tif$", full.names = TRUE)
  if (!is.null(path_dsm)) {
    path_dsm <- list.files(path_dsm, pattern = "\\.tif$", full.names = TRUE)
  }
  data_total <- list()
  tt <- list()
  for (i in seq_along(path_rgb)) {
    message("#------------------- Starting: Mosaic ", i, " ------------------#")
    if (!is.null(update_progress)) {
      update_progress(i, length(path_rgb))
    }
    # If area of interest is provided
    if (!is.null(area_of_interest)) {
      t1 <- crop(rast(path_rgb[i]), area_of_interest)
    } else {
      t1 <- rast(path_rgb[i])
    }
    # Shape 2 is used to crop plots
    if (is.null(plot_shape_crop)) {
      plot_shape_crop <- plot_shape
    }
    # Save individual plots per date
    if (save_plots) {
      path <- paste0(path_out, "/", name_experiment, "/plot/", dap[i], "/")
      create_folder_if_not_exists(path)
      crop_grid(
        mosaic = t1,
        plot_shape = plot_shape_crop,
        plot_id = plot_id,
        output_dir = path
      )
    }
    # Applying masking
    message("Removing Soil")
    t1_no_soil <- fieldMask(
      mosaic = t1,
      plot = FALSE,
      index = "HUE",
      cropValue = 0,
      cropAbove = TRUE
    )
    # Save individual plots per date no-soil
    if (save_plots) {
      path <- paste0(path_out, "/", name_experiment, "/plot_hue/", dap[i], "/")
      create_folder_if_not_exists(path)
      crop_grid(
        mosaic = t1_no_soil$newMosaic,
        plot_shape = plot_shape_crop,
        plot_id = plot_id,
        output_dir = path
      )
    }
    # Calculating Indices
    message("Calculating Indices")
    t1_indices <- fieldIndex(
      mosaic = t1_no_soil$newMosaic,
      index = indices,
      plot = FALSE
    )
    # Extracting Information
    message("Extracting Information")
    t1_info <- field_extract(
      mosaic = t1_indices[[c(bands, indices)]],
      plot_shape = plot_shape
    )
    # Calculating GLI without mask
    message("Calculating GLI")
    t1_gli <- fieldIndex(t1, index = "GLI", plot = FALSE)
    # Extracting GLI Information
    message("Extracting GLI")
    t1_info_gli <- field_extract(t1_gli[["GLI"]], plot_shape = plot_shape)
    # Image Metadata
    tt[[i]] <- t1 |>
      mos_info(name = paste0("rgb_", dap[i])) |>
      mutate(DAP = dap[i], .before = image)
    # Area Pixel
    area_pixel <- field_extract(
      mosaic = cellSize(t1_no_soil$newMosaic),
      plot_shape = plot_shape,
      fun = "mean"
    )
    # Total Area m2
    plot_area <- st_area(plot_shape)
    # Canopy
    message("Canopy Cover")
    coverage <- field_area(
      mosaic = t1_no_soil$newMosaic,
      field_shape = plot_shape,
      field = plot_id
    )
    # Data
    dt_tmp <- t1_info |>
      mutate(canopy = coverage[["AreaPercentage"]], .after = GLI_mean) %>%
      mutate(total_pixels = coverage[["PixelCount"]], .after = canopy) %>%
      mutate(DAP = dap[i], .before = all_of(plot_id)) |>
      mutate(plot_area = plot_area, .after = canopy) |>
      mutate(area_pixel = area_pixel[["area_mean"]], .after = canopy) |>
      mutate(GLI_2 = t1_info_gli[["GLI_mean"]], .after = GLI_mean)
    # Digital Surface Model (DSM)
    if (!is.null(path_dsm)) {
      message("Digital Surface Model")
      if (!is.null(area_of_interest)) {
        if (i == 1) {
          dsm_0_crop <- dsm_1_crop <- crop(rast(path_dsm[1]), area_of_interest)
        } else {
          dsm_1_crop <- crop(rast(path_dsm[i]), area_of_interest)
        }
      } else {
        if (i == 1) {
          dsm_0_crop <- dsm_1_crop <- rast(path_dsm[1])
        } else {
          dsm_1_crop <- rast(path_dsm[i])
        }
      }
      # Check CRS
      if (crs(dsm_0_crop) != crs(dsm_1_crop)) {
        crs(dsm_1_crop) <- crs(dsm_0_crop)
      }
      # Images info
      tt[[i]] <- rbind.data.frame(
        mos_info(t1, name = paste("rgb", dap[i])),
        mos_info(dsm_1_crop, name = "dsm")
      ) |>
        mutate(DAP = dap[i], .before = image)
      # Canopy Height Model (CHM) and Canopy Volume Model (CVM)
      message("Canopy Height Model")
      chm <- fieldHeight(dsm_0_crop, dsm_1_crop)
      chm_rem_soil <- fieldMask(chm, mask = t1_no_soil$mask, plot = FALSE)
      ph <- field_extract(chm_rem_soil$newMosaic$height, plot_shape = t1_info)
      names(ph)[names(ph) == "height_mean"] <- "ph"
      ph_vol <- field_extract(
        mosaic = chm_rem_soil$newMosaic$volume,
        plot_shape = ph,
        fun = "sum"
      )
      names(ph_vol)[names(ph_vol) == "volume_sum"] <- "volume"
      # Data
      dt_tmp <- ph_vol %>%
        mutate(canopy = coverage[["AreaPercentage"]], .after = volume) %>%
        mutate(total_pixel = coverage[["PixelCount"]], .after = canopy) %>%
        mutate(DAP = dap[i], .before = all_of(plot_id)) |>
        mutate(plot_area = plot_area, .after = canopy) |>
        mutate(area_pixel = area_pixel[["area_mean"]], .after = canopy) |>
        mutate(volume = total_pixel * area_pixel * ph) |>
        mutate(GLI_2 = t1_info_gli[["GLI_mean"]], .after = GLI_mean)
    }
    # Saving Data Frame
    data_total[[i]] <- data.frame(dt_tmp)
    # Saving shape file
    if (save_shape) {
      path <- paste0(path_out, "/", name_experiment, "/shape_files/")
      create_folder_if_not_exists(path)
      st_write(
        obj = dt_tmp,
        dsn = paste0(path, "shape_", name_experiment, ".gpkg"),
        layer = dap[i],
        append = FALSE,
        quiet = TRUE
      )
    }
    message("#------------------ Completed: Mosaic ", i, " ------------------#")
  }
  # Data with all indices across dates
  dt <- data_total |>
    data.table::rbindlist(fill = TRUE) |>
    as.data.frame() |>
    dplyr::select(-geom) |>
    mutate_all(~ ifelse(is.nan(.), NA, .)) |>
    mutate(trial = name_experiment, .before = DAP)
  names(dt) <- gsub("_mean", "", names(dt))
  # Saving data
  path <- paste0(path_out, "/", name_experiment, "/extracted_data/")
  create_folder_if_not_exists(path)
  dt |>
    write.csv(
      file = paste0(path, name_experiment, ".csv"),
      row.names = FALSE,
      na = ""
    )
  # Metadata
  info_imgs <- tt |>
    data.table::rbindlist(fill = TRUE) |>
    as.data.frame()
  # Plot Time Series
  if (time_serie) {
    for (w in sort(unique(plot_shape[[plot_id]]))) {
      path_shape <- paste0(
        path_out, "/",
        name_experiment, "/shape_files/shape_", name_experiment,
        ".gpkg"
      )
      p0 <- plot_organizer(
        id = w,
        plot_id = plot_id,
        path = paste0(path_out, "/", name_experiment, "/plot/"),
        path_shape = path_shape,
        base_size = 14
      )
      create_folder_if_not_exists(
        path = paste0(path_out, "/", name_experiment, "/plots_time/")
      )
      ggsave(
        filename = paste0(
          path_out, "/", name_experiment, "/plots_time/ID_", w, ".png"
        ),
        plot = p0$figure,
        units = "in",
        dpi = 300,
        width = 8,
        height = 6
      ) |> suppressWarnings()
      p1 <- plot_organizer(
        id = w,
        plot_id = plot_id,
        path = paste0(path_out, "/", name_experiment, "/plot_hue/"),
        path_shape = path_shape,
        base_size = 14
      )
      ggsave(
        filename = paste0(
          path_out, "/", name_experiment, "/plots_time/ID_", w, "_mask.png"
        ),
        plot = p1$figure,
        units = "in",
        dpi = 300,
        width = 8,
        height = 6
      ) |> suppressWarnings()
    }
  }
  return(list(dt = dt, info = info_imgs))
}


mos_info <- function(mosaic, name = "t1") {
  dm <- dim(mosaic)
  names(dm) <- c("nrow", "ncol", "nlyr")
  res <- res(mosaic)
  names(res) <- c("res_x", "res_y")
  # coord_crs <- crs(mosaic)
  var_name <- varnames(mosaic)
  info_imgs <- data.frame(image = name, t(dm), t(res), var_name)
  return(info_imgs)
}

create_folder_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Folder created: ", path)
  }
}

crop_grid <- function(mosaic,
                      plot_shape,
                      plot_id = NULL,
                      output_dir = "./") {
  message("Cropping...")
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
  if (!sf::st_is_longlat(plot_shape)) {
    plot_shape <- sf::st_transform(plot_shape, crs = 4326)
  }
  for (i in seq_len(nrow(plot_shape))) {
    grid_poly <- plot_shape[i, ]
    grid_poly <- sf::st_transform(grid_poly, crs = sf::st_crs(stars_object))
    plot_raster <- sf::st_crop(stars_object, grid_poly)
    if (nbands > 2) {
      plot_raster <- stars::st_warp(plot_raster, crs = st_crs(mosaic))
      plot_raster[is.na(plot_raster)] <- 0
    }
    if (nbands == 1) {
      plot_raster <- st_warp(plot_raster, crs = st_crs(mosaic))
      plot_raster[is.na(plot_raster)] <- NA
    }
    file_name <- paste0("ID_", plot_shape[[plot_id]][i], ".tif")
    file_path <- file.path(output_dir, file_name)
    plot_raster <- as(plot_raster, "Raster")
    plot_raster <- rast(plot_raster)
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
                           color_grid = "red") {
  plot <- id
  path <- path
  path_shape <- path_shape
  k <- sort(as.numeric(list.files(path)))
  df <- info <- list()
  for (i in k) {
    tmp <- list.files(paste0(path, i), pattern = paste0("_", plot, ".tif"))
    obj <- terra::rast(paste0(path, i, "/", tmp))
    names(obj)[1:3] <- c("Red", "Green", "Blue")
    grid_shape <- st_read(path_shape, layer = paste(i), quiet = TRUE) |>
      filter(.data[[plot_id]] %in% plot) |>
      rename(geometry = geom)
    if (!is.null(angle)) {
      y <- raster::stack(obj)
      raster::crs(y) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
      obj <- raster::projectRaster(
        from = y,
        res = res(y),
        crs = paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", angle)
      )
      suppressWarnings(
        st_crs(grid_shape) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
      )
      grid_shape <- st_transform(grid_shape, crs = st_crs(obj))
    }
    data <- mutate(as.data.frame(obj, xy = TRUE), DAP = i, Plot = plot)
    df[[paste0(i)]] <- data
    info[[paste0(i)]] <- grid_shape
  }
  df <- data.table::rbindlist(df, fill = TRUE)
  info <- sf::st_as_sf(data.table::rbindlist(info))

  if (!is.null(angle)) {
    df <- df |>
      filter(Red > 0 & Blue > 0 & Green > 0) |>
      select(x, y, Red:Blue, DAP, Plot) |>
      mutate(
        Red = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Red),
        Green = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Green),
        Blue = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Blue)
      ) |>
      na.omit() |>
      mutate(
        RGB = rgb(
          red = Red,
          green = Green,
          blue = Blue,
          maxColorValue = 255
        )
      ) |>
      filter(!RGB %in% "#000000")
  } else {
    df <- df |>
      select(x, y, Red:Blue, DAP, Plot) |>
      mutate(
        Red = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Red),
        Green = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Green),
        Blue = ifelse(Red == 0 & Green == 0 & Blue == 0, NA, Blue)
      ) |>
      na.omit()
  }
  p0 <- df |>
    ggplot() +
    geom_raster(
      aes(
        x = x,
        y = y,
        fill = rgb(
          red = Red,
          green = Green,
          blue = Blue,
          maxColorValue = 255
        )
      ),
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_sf(data = info, color = color_grid, fill = NA) +
    theme_void(base_size = base_size) +
    facet_wrap(Plot ~ DAP, nrow = 1, labeller = label_both) +
    theme(strip.text = element_text(colour = color))
  out <- list(df = df, info = info, figure = p0)
  return(out)
}


field_extract <- function(mosaic,
                          plot_shape,
                          fun = "mean",
                          progress = FALSE) {
  message("Starting data extraction per plot ...")
  if (is.null(mosaic)) {
    stop("The input 'mosaic' object is NULL.")
  }
  if (class(mosaic) %in% c("RasterStack", "RasterLayer", "RasterBrick")) {
    mosaic <- terra::rast(mosaic)
  }
  valid_functions <- c(
    "mean", "sum", "max", "min", "mode",
    "stdev", "variance", "coefficient_of_variation", "majority",
    "minority", "summary"
  )
  if (!(fun %in% valid_functions)) {
    stop("Use one of the functions from
         ('mean', 'sum', 'max', 'min',
         'mode', 'stdev', 'variance',
         'coefficient_of_variation', 'majority',
         'minority', 'summary').")
  }
  if (fun == "summary") {
    fun <- c(
      "mean", "sum", "max", "min", "mode", "stdev",
      "variance", "coefficient_of_variation", "majority",
      "minority"
    )
  }
  plot_info <- exactextractr::exact_extract(
    x = mosaic,
    y = plot_shape,
    fun = fun,
    progress = progress,
    force_df = TRUE
  ) |> as.data.frame()
  if (length(fun) == 1) {
    colnames(plot_info) <- paste0(names(mosaic), "_", fun)
  } else {
    colnames(plot_info) <- paste0(names(mosaic), "_", fun)
  }
  out <- plot_shape |>
    cbind(plot_info[, !colnames(plot_info) %in% "ID", drop = FALSE])
  return(out)
}


field_area <- function(mosaic, field_shape, field = NULL) {
  if (is.null(field)) {
    terra_vect <- vect(field_shape)
    terra_rast <- rasterize(terra_vect, mosaic, field = "PlotID")
    total_pixelcount <- exactextractr::exact_extract(
      x = terra_rast,
      y = sf::st_as_sf(as.polygons(terra_rast)),
      fun = "count",
      force_df = TRUE
    )
    area_pixel <- exactextractr::exact_extract(
      x = mosaic[[1]],
      y = sf::st_as_sf(as.polygons(terra_rast)),
      fun = "count",
      force_df = TRUE
    )
  } else {
    terra_vect <- vect(field_shape)
    terra_rast <- rasterize(terra_vect, mosaic, field = field)
    total_pixelcount <- exactextractr::exact_extract(
      x = terra_rast,
      y = st_as_sf(as.polygons(terra_rast)),
      fun = "count",
      force_df = TRUE
    )
    area_pixel <- exactextractr::exact_extract(
      x = mosaic[[1]],
      y = st_as_sf(as.polygons(terra_rast)),
      fun = "count",
      force_df = TRUE
    )
  }
  area_percentage <- round(area_pixel / total_pixelcount * 100, 3)
  names(area_percentage) <- "AreaPercentage"
  names(area_pixel) <- "PixelCount"
  area_percentage <- cbind(
    sf::st_as_sf(as.polygons(terra_rast)),
    AreaPixel = area_pixel,
    area_percentage
  )
  return(area_percentage)
}
