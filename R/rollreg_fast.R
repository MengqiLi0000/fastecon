#' rollreg_fast
#'
#' Fast rolling window regression using matrix caching and optional parallel computation.
#'
#' @param data A data.frame or data.table with time-series or panel data
#' @param y The name of the dependent variable (as a string)
#' @param x A character vector of independent variable names
#' @param time The name of the time variable (used for ordering)
#' @param window Size of the rolling window (number of observations per window)
#' @param step Step size for the rolling window (default is 1)
#' @param parallel Logical, whether to use multiple cores (default TRUE)
#' @return A data.table with time and estimated coefficients for each window
#' @export

rollreg_fast <- function(data, y, x, time, window = 60, step = 1, parallel = TRUE) {
  library(data.table)
  library(parallel)

  data <- as.data.table(data)
  setorder(data, get(time))  # ensure sorted by time

  yvec <- data[[y]]
  xmat <- as.matrix(data[, ..x])
  n <- nrow(data)
  K <- length(x)

  # number of windows
  n_windows <- floor((n - window) / step) + 1
  roll_results <- vector("list", n_windows)

  # initial matrices
  XTX <- crossprod(xmat[1:window, ])
  XTy <- crossprod(xmat[1:window, ], yvec[1:window])

  ols_estimate <- function(XTX, XTy) {
    solve(XTX, XTy)
  }

  compute_one <- function(i) {
    start <- 1 + (i - 1) * step
    end <- start + window - 1
    if (end > n) return(NULL)

    if (i > 1) {
      # update matrix products instead of recomputing
      x_old <- matrix(xmat[start - 1, ], ncol = K)
      x_new <- matrix(xmat[end, ], ncol = K)
      y_old <- yvec[start - 1]
      y_new <- yvec[end]

      XTX <<- XTX - tcrossprod(x_old) + tcrossprod(x_new)
      XTy <<- XTy - x_old * y_old + x_new * y_new
    }

    beta <- tryCatch(ols_estimate(XTX, XTy), error = function(e) rep(NA, K))
    list(time = data[[time]][end], coef = beta)
  }

  if (parallel) {
    cl <- makeCluster(detectCores() - 1)
    clusterExport(cl, varlist = c("xmat", "yvec", "K", "n", "data", "time",
                                  "window", "step", "XTX", "XTy", "ols_estimate"),
                  envir = environment())
    roll_results <- parLapply(cl, 1:n_windows, compute_one)
    stopCluster(cl)
  } else {
    for (i in 1:n_windows) {
      roll_results[[i]] <- compute_one(i)
    }
  }

  # format output
  coefs <- do.call(rbind, lapply(roll_results, function(r) if (!is.null(r)) r$coef else rep(NA, K)))
  times <- sapply(roll_results, function(r) if (!is.null(r)) r$time else NA)

  result <- data.table(time = times, coefs)
  setnames(result, c("time", paste0("beta_", x)))
  return(result)
}
