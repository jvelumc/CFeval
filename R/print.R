# pretty print, respecting output width, adding \n at the end
pp <- function(...) {
  txt <- paste0(c(...), collapse = "")
  cat(paste0(strwrap(txt), "\n"))
}


#' @export
print.cfscore <- function(x, ...) {
  if (!x$quiet) {
    assumptions(x)
  }

  numeric_metrics <- x$metrics[x$metrics != "oeplot"]

  # if we bootstrapped, make 1 table for each metric
  # if not, make 1 table for all
  if (x$bootstrap == TRUE) {
    for (metric in numeric_metrics) {
      cat("\n", metric, "\n\n", sep = "")

      # build result table
      tab <- data.frame(model = x$models)
      tab[[metric]] <- unlist(x$results[[metric]][x$models])
      tab$lower <- sapply(
        x$models,
        function(m)
          quantile(x$results_bootstrap[[metric]][[m]], probs = 0.025)
      )
      tab$upper <- sapply(
        x$models,
        function(m)
          quantile(x$results_bootstrap[[metric]][[m]], probs = 0.975)
      )
      print(tab, digits = 3, row.names = FALSE)
    }
  } else {
    cat("\n")
    tab <- data.frame(model = x$models)
    for (metric in numeric_metrics) {
      tab[[metric]] <- unlist(x$results[[metric]][x$models])
    }
    print(tab, digits = 3, row.names = FALSE)
  }

  if ("oeplot" %in% x$metrics) {
    plot(x)
  }
}

#' @export
plot.cfscore <- function(x, ...) {
  arg <- list(...)
  if (!("model" %in% names(arg))) {
    models <- x$models
  }
  else {
    models <- arg$models
  }
  for (m in models) {
    cat("model: ", m, "\n")
    x$results$oeplot[[m]]()
  }
}

assumptions <- function(x) {

  if ("confounders" %in% names(x)) {
    adjustment_text <- paste0("{", paste0(x$confounders, collapse = ", "), "}")
  } else {
    adjustment_text <- "given IP-weights"
  }

  pp("Estimation of the performance of the prediction model in a counterfactual
     (CF) dataset where everyone's treatment ", x$treatment_column,
     " was set to ", x$treatment_of_interest, ".")

  pp("The following assumptions must be satisfied for correct inference:")

  pp(" - Conditional exchangeability requires that ", adjustment_text,
     " are sufficient to adjust for confounding and selection bias between
      treatment and outcome.")

  pp("- Positivity (assess $weights for outliers)")

  pp("- Consistency")

  pp("- No interference")

  pp("- Correctly specified propensity formula")

}
