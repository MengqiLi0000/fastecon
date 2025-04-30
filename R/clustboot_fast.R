#' clustboot_fast
#'
#' Fast clustered bootstrap for linear models.
#'
#' @param data A data.frame or data.table
#' @param formula A regression formula
#' @param cluster Name of the clustering variable
#' @param B Number of bootstrap replications
#' @param type Type of bootstrap: "pairs" (default) or "wild"
#' @param seed Random seed (optional)
#' @param parallel Whether to run in parallel
#' @return A matrix of bootstrapped coefficients (B rows x k parameters)
#' @export

clustboot_fast <- function(data, formula, cluster, B = 1000, type = "pairs", seed = NULL, parallel = TRUE) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required.")
  library(data.table)
  library(parallel)

  if (!inherits(data, "data.table")) data <- as.data.table(data)
  if (!is.null(seed)) set.seed(seed)

  cluster_ids <- unique(data[[cluster]])
  G <- length(cluster_ids)

  run_one_boot <- function(b) {
    sampled_clusters <- sample(cluster_ids, G, replace = TRUE)
    sample_data <- data[get(cluster) %in% sampled_clusters]
    tryCatch({
      model <- lm(formula, data = sample_data)
      coef(model)
    }, error = function(e) rep(NA, length(coef(lm(formula, data = data)))))
  }

  if (parallel) {
    cl <- makeCluster(detectCores() - 1)
    clusterExport(cl, varlist = c("data", "cluster_ids", "cluster", "G", "formula"), envir = environment())
    result <- parLapply(cl, 1:B, run_one_boot)
    stopCluster(cl)
  } else {
    result <- lapply(1:B, run_one_boot)
  }

  boot_mat <- do.call(rbind, result)
  colnames(boot_mat) <- names(coef(lm(formula, data = data)))
  return(boot_mat)
}
