#' forecast_fast
#'
#' Make fast 1-step-ahead forecasts from regression models or coefficient output.
#'
#' @param model A model object (`lm`, or list of coefficients as from `rollreg_fast`)
#' @param newdata A data.frame or data.table with the new X values (one row or more)
#' @param xvars Character vector of variable names used in model
#' @param ci Logical, whether to return a 95% confidence interval
#' @param sigma Numeric, optional residual std deviation for CI (if model does not contain it)
#' @return A vector of forecasts (or data.table with CIs if `ci = TRUE`)
#' @export

forecast_fast <- function(model, newdata, xvars, ci = FALSE, sigma = NULL) {
  library(data.table)
  newdata <- as.data.table(newdata)
  X_new <- as.matrix(newdata[, ..xvars])

  # Get coefficients
  if ("lm" %in% class(model)) {
    coef_vec <- coef(model)
    if (is.null(sigma)) sigma <- summary(model)$sigma
  } else if (is.numeric(model)) {
    coef_vec <- model
    if (is.null(sigma)) sigma <- 1  # fallback default
  } else {
    stop("Model must be lm or numeric vector of coefficients.")
  }

  pred <- as.vector(X_new %*% coef_vec[xvars])

  if (!ci) return(pred)

  # Basic constant-variance CI: assumes homoskedasticity
  se_pred <- sqrt(rowSums(X_new^2)) * sigma
  lower <- pred - 1.96 * se_pred
  upper <- pred + 1.96 * se_pred

  return(data.table(pred = pred, lower = lower, upper = upper))
}
