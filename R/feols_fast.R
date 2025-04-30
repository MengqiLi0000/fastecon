#' feols_fast
#'
#' Estimate a linear model with fixed effects using within-transformation.
#'
#' @param data A data.table or data.frame
#' @param y Dependent variable name
#' @param x Independent variable names (vector)
#' @param fe Fixed effect variable(s): one or two character strings
#' @return A list with coefficients and residuals
#' @export

feols_fast <- function(data, y, x, fe) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required.")
  library(data.table)
  data <- as.data.table(data)

  if (length(fe) == 1) {
    # One-way fixed effects
    data[, y_demeaned := get(y) - mean(get(y)), by = fe]
    for (var in x) {
      data[, paste0(var, "_dm") := get(var) - mean(get(var)), by = fe]
    }

  } else if (length(fe) == 2) {
    # Two-way fixed effects (iterative approximation)
    for (iter in 1:2) {
      data[, y := get(y) - ave(get(y), data[[fe[1]]], FUN = mean)]
      data[, y := get(y) - ave(get(y), data[[fe[2]]], FUN = mean)]
      for (var in x) {
        data[, (var) := get(var) - ave(get(var), data[[fe[1]]], FUN = mean)]
        data[, (var) := get(var) - ave(get(var), data[[fe[2]]], FUN = mean)]
      }
    }
    data[, y_demeaned := get(y)]
    for (var in x) data[, paste0(var, "_dm") := get(var)]
  } else {
    stop("Only one or two fixed effects supported.")
  }

  X <- as.matrix(data[, paste0(x, "_dm"), with = FALSE])
  y_vec <- data[["y_demeaned"]]

  coef_hat <- tryCatch(solve(t(X) %*% X, t(X) %*% y_vec), error = function(e) rep(NA, length(x)))
  names(coef_hat) <- x

  return(list(coefficients = coef_hat, residuals = y_vec - X %*% coef_hat))
}
