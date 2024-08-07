#' Print an object of class \code{modeler_HTP}
#'
#' @description Prints information about \code{modeler_HTP} function.
#'
#' @aliases print.modeler_HTP
#' @usage \method{print}{modeler_HTP}(x, ...)
#' @param x An object fitted with the function \code{modeler_HTP()}.
#' @param ... Options used by the tibble package to format the output. See
#' `tibble::print()` for more details.
#' @author Johan Aparicio [aut]
#' @method print modeler_HTP
#' @return an object inheriting from class \code{modeler_HTP}.
#' @importFrom utils head
#' @export
#' @examples
#' library(exploreHTP)
#' data(dt_potato)
#' results <- read_HTP(
#'   data = dt_potato,
#'   genotype = "Gen",
#'   time = "DAP",
#'   plot = "Plot",
#'   traits = c("Canopy", "PH"),
#'   row = "Row",
#'   range = "Range"
#' )
#' out <- canopy_HTP(x = results, index = "Canopy", plot_id = c(1:5))
#' plot(out, plot_id = c(1:5))
#' print(out)
print.modeler_HTP <- function(x, ...) {
  param <- select(x$param, -c(row, range))
  trait <- unique(x$dt$trait)
  cat("\nCall:\n")
  cat(paste(trait, "~", deparse(x$fn)), "\n")
  cat("\n")
  if (nrow(param) < 10) {
    cat("Sum of Squares Error:\n")
    resum <- summary(param$sse)
  } else {
    cat("Sum of Squares Error `scale()`:\n")
    resum <- summary(as.numeric(scale(param$sse)))
  }
  print(resum)
  cat("\n")
  cat("Optimization Results `head()`:\n")
  print(as.data.frame(head(param, 4)), digits = 3, row.names = FALSE)
  cat("\n")
  cat("Metrics:\n")
  total_time <- x$execution
  dt <- x$metrics |>
    group_by(plot, genotype) |>
    arrange(sse) |>
    slice(1) |>
    ungroup()
  conv <- dt |>
    summarise(conv = round(sum(convergence %in% 0) / n() * 100, 2)) |>
    mutate(conv = paste0(conv, "%")) |>
    pull(conv)
  ite <- dt |>
    summarise(ite = round(mean(fevals, na.rm = TRUE), 2)) |>
    mutate(ite = paste0(ite, " (plot)")) |>
    pull(ite)
  info <- data.frame(
    Plots = nrow(dt),
    `Timing` = round(total_time, 4),
    Convergence = conv,
    `Iterations` = ite,
    check.names = FALSE
  )
  print(info, row.names = FALSE)
  cat("\n")
}

#' @noRd
comparison_HTP <- function(x, y, value = 5) {
  x <- x$param
  y <- y$param
  xy <- full_join(
    x = select(x, c(plot, sse)),
    y = select(y, c(plot, sse)),
    by = "plot"
  ) |>
    mutate_if(is.numeric, round, value) |>
    mutate(table = sse.x > sse.y)
  plots_to_check <- xy |>
    filter(table %in% TRUE) |>
    pull(plot)
  summ <- xy |>
    mutate(table = sse.x > sse.y) |>
    summarise(
      `y_better_than_x` = round(sum(table %in% TRUE) / n(), 3),
      `y_worse_than_x` = round(sum(table %in% FALSE) / n(), 3)
    )
  res <- data.frame(
    `mean(sse_x) > mean(sse_y)` = mean(x$sse) > mean(y$sse),
    check.names = FALSE
  ) |>
    cbind(summ)
  objt <- list(
    summ = res,
    join = filter(xy, table %in% TRUE),
    plots_to_check = plots_to_check
  )
  return(objt)
}
