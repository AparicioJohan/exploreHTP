% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08_comparison.R
\name{logLik.modeler}
\alias{logLik.modeler}
\title{Extract Log-Likelihood for an object of class \code{modeler}}
\usage{
\method{logLik}{modeler}(object, ...)
}
\arguments{
\item{object}{An object inheriting from class \code{modeler} resulting of
executing the function \code{modeler()}}

\item{...}{Further parameters. For future improvements.}
}
\value{
A \code{tibble} with the Log-Likelihood for the fitted models.
}
\description{
logLik for an object of class \code{modeler}
}
\examples{
library(flexFitR)
dt <- data.frame(X = 1:6, Y = c(12, 16, 44, 50, 95, 100))
mo_1 <- modeler(dt, X, Y, fn = "fn_lin", param = c(m = 10, b = -5))
plot(mo_1)
logLik(mo_1)
}
\author{
Johan Aparicio [aut]
}
