#' etable_latex
#'
#' Generate a simple LaTeX regression table from model output.
#'
#' @param models A list of models (or a single model) from lm(), rollreg_fast(), etc.
#' @param se Standard errors (optional). Can be a list or vector.
#' @param coef_names Optional vector of variable names to display.
#' @param digits Number of digits to round to.
#' @param caption Table caption for LaTeX output.
#' @param label Label for LaTeX referencing.
#' @return A character string of LaTeX table code.
#' @export

etable_latex <- function(models, se = NULL, coef_names = NULL, digits = 3, caption = "Regression Results", label = "tab:results") {
  if (!is.list(models)) models <- list(models)

  format_num <- function(x) formatC(x, digits = digits, format = "f")

  out <- "\\begin{table}[ht]\n\\centering\n"
  out <- paste0(out, "\\caption{", caption, "}\\label{", label, "}\n\\begin{tabular}{l", paste(rep("c", length(models)), collapse = ""), "}\n")
  out <- paste0(out, "\\toprule\n")

  # Extract variable names
  coefs_list <- lapply(models, function(m) if ("lm" %in% class(m)) coef(m) else m)
  all_names <- unique(unlist(lapply(coefs_list, names)))

  if (!is.null(coef_names)) {
    all_names <- coef_names
  }

  out <- paste0(out, " & ", paste0("Model ", seq_along(models), collapse = " & "), " \\\\\n\\midrule\n")

  for (var in all_names) {
    row <- var
    for (m in coefs_list) {
      est <- if (var %in% names(m)) format_num(m[[var]]) else ""
      row <- paste0(row, " & ", est)
    }
    row <- paste0(row, " \\\\\n")
    out <- paste0(out, row)
  }

  out <- paste0(out, "\\bottomrule\n\\end{tabular}\n\\end{table}\n")
  return(out)
}
