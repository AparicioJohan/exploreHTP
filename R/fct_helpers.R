multi_save <- function(x,
                       path = NULL,
                       type = 1,
                       color = "#007bc2",
                       color_points = "black",
                       base_size = 16,
                       n_points = 100,
                       color_ci = "black",
                       color_pi = "red",
                       width = 8,
                       height = 6,
                       file_type = "png") {
  create_folder_if_not_exists(path = paste0(path, "/groups/"))
  ids <- x[["param"]]$uid
  for (j in ids) {
    tmp <- plot(
      x = x,
      id = j,
      type = type,
      color = color,
      color_points = color_points,
      title = paste("Group:", j),
      n_points = n_points,
      base_size = base_size,
      color_ci = color_ci,
      color_pi = color_pi
    )
    ggsave(
      filename = paste0(path, "/groups/ID_", j, ".", file_type),
      plot = tmp,
      units = "in",
      dpi = 300,
      width = width,
      height = height
    ) |> suppressWarnings()
  }
}

from_input_to_list <- function(input) {
  if (is.null(input) || length(input) == 0) {
    return()
  }
  # Split the input string into individual key-value pairs
  pairs <- strsplit(input, ";\\s*")[[1]]
  # Create a named list by splitting each key-value pair
  result <- list()
  for (pair in pairs) {
    key_value <- strsplit(pair, "\\s*=\\s*")[[1]]
    if (length(key_value) == 2) {
      key <- trimws(key_value[1])
      value <- trimws(key_value[2])
      # Check if value is numeric
      if (!is.na(suppressWarnings(as.numeric(value)))) {
        result[[key]] <- as.numeric(value)
      } else {
        result[[key]] <- value
      }
    }
  }
  if (length(result) == 0) {
    return()
  } else {
    result
  }
}


resize <- function(plot_shape, mosaic, angle = 0, xsize = 0.8, ysize = 4) {
  # Changing dimensions of the shape
  cen <- st_geometry(plot_shape)
  bbox_list <- lapply(cen, st_bbox)
  points_list <- lapply(bbox_list, st_as_sfc)
  boxes <- lapply(points_list, \(pt) rect_funct(pt, xsize, ysize))
  points <- boxes[[1]]
  if (length(boxes) > 1) {
    for (i in 2:length(boxes)) {
      points <- c(points, boxes[[i]])
    }
  }
  st_crs(points) <- st_crs(cen)
  grid <- st_as_sf(points)
  if (!is.null(mosaic)) st_crs(grid) <- st_crs(mosaic)
  # Applying rotation
  angle <- angle * pi / 180
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  geom <- st_geometry(grid)
  centroid <- st_centroid(geom)
  grid_rotated <- (geom - centroid) * rot(angle) + centroid
  st_crs(grid_rotated) <- st_crs(cen)
  grid_rotated <- st_as_sf(grid_rotated)
  if (!is.null(mosaic)) st_crs(grid_rotated) <- st_crs(mosaic)
  plot_shape$geom <- grid_rotated$x
  return(plot_shape)
}

rect_around_point <- function(x, xsize, ysize) {
  bbox <- st_bbox(x)
  bbox <- bbox + c(xsize / 2, ysize / 2, -xsize / 2, -ysize / 2)
  bbox <- st_as_sfc(st_bbox(bbox))
  return(bbox)
}

rect_funct <- function(x, xsize = 0.85, ysize = 4.5) {
  bbox <- st_bbox(x)
  ys <- c(ysize, bbox["ymax"] + bbox["ymin"])
  ys <- solve(matrix(c(1, 1, -1, 1), ncol = 2)) %*% ys
  xs <- c(xsize, bbox["xmax"] + bbox["xmin"])
  xs <- solve(matrix(c(1, 1, -1, 1), ncol = 2)) %*% xs
  bbox["xmax"] <- xs[1]
  bbox["xmin"] <- xs[2]
  bbox["ymax"] <- ys[1]
  bbox["ymin"] <- ys[2]
  bbox <- st_as_sfc(st_bbox(bbox))
  return(bbox)
}


ensure_ebimage <- function() {
  # Check if EBImage is installed
  if (!requireNamespace("EBImage", quietly = TRUE)) {
    # Interactive session prompt
    if (interactive()) {
      cat("Package {EBImage} is required but not installed.\n")
      choice <- menu(c("Yes", "No"), title = "Would you like to install it now?")
      if (choice == 1) { # User chose "Yes"
        # Ensure BiocManager is available for installation
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager", quiet = TRUE)
        }
        # Install EBImage
        BiocManager::install("EBImage", update = FALSE, ask = FALSE, quiet = TRUE)
        # Verify successful installation
        if (!requireNamespace("EBImage", quietly = TRUE)) {
          message("Failed to install {EBImage}. Please try manual installation.")
          return(FALSE)
        }
      } else { # User chose "No"
        message("Pease manually install {EBImage}:\nhttps://bioconductor.org/packages/EBImage")
        return(FALSE)
      }
    } else { # Non-interactive session
      message("Package {EBImage} is required. Install it from:\nhttps://bioconductor.org/packages/EBImage")
      return(FALSE)
    }
  }
  return(TRUE)
}
