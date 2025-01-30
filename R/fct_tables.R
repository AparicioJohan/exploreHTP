info_criteria <- function(object) {
  if (!inherits(object, "modeler")) {
    stop("The object should be of class 'modeler'.")
  }
  table <- AIC(object) |>
    full_join(BIC(object), by = c("uid", "logLik", "df", "nobs", "p")) |>
    full_join(select(object$param, uid, sse), by = "uid") |>
    mutate(Sigma = sqrt(sse / (nobs - p))) |>
    select(-sse)
  return(table)
}
