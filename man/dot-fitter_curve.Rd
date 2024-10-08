% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_modeler.R
\name{.fitter_curve}
\alias{.fitter_curve}
\title{General-purpose optimization}
\usage{
.fitter_curve(data, id, fn, method, lower, upper, control, .keep)
}
\arguments{
\item{data}{A nested data.frame with columns <plot, genotype, row, range, data, initials, fx_params>.}

\item{id}{An optional vector of IDs to filter the data. Default is \code{NULL}, meaning all ids are used.}

\item{fn}{A string specifying the name of the function to be used for the curve fitting. Default is \code{"fn_piwise"}.}

\item{method}{A character vector specifying the optimization methods to be used. See \code{optimx} package for available methods. Default is \code{c("subplex", "pracmanm", "anms")}.}

\item{lower}{Numeric vector specifying the lower bounds for the parameters. Default is \code{-Inf} for all parameters.}

\item{upper}{Numeric vector specifying the upper bounds for the parameters. Default is \code{Inf} for all parameters.}

\item{control}{A list of control parameters to be passed to the optimization function. For example, \code{list(maxit = 500)}.}
}
\description{
The function .fitter_curve is used internally to find the parameters requested.
}
\examples{
library(flexFitR)
data(dt_potato)
explorer <- explorer(dt_potato, x = DAP, y = c(Canopy, GLI_2), id = Plot)
mod_1 <- dt_potato |>
  modeler(
    x = DAP,
    y = GLI_2,
    grp = Plot,
    fn = "fn_lin_pl_lin",
    parameters = c(t1 = 38.7, t2 = 62, t3 = 90, k = 0.32, beta = -0.01),
    subset = 195,
    add_zero = TRUE
  )
}
\keyword{internal}
