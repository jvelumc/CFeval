# pretty print, respecting output width, adding \n at the end
pp <- function(...) {
  txt <- paste0(c(...), collapse = "")
  cat(paste0(strwrap(txt), "\n"))
}


#' @export
print.CFscore <- function(x, ...) {
  assumptions(x)
  numeric_metrics <- x$metrics[x$metrics != "calplot"]

  if (x$bootstrap_iterations != 0) {
    for (metric in numeric_metrics) {
      cat("\n", metric, "\n\n", sep = "")
      tab <- data.frame(model = names(x$predictions))
      tab[[metric]] <- x$score[[metric]]
      tab$lower <- sapply(x$bootstrap$results[[metric]], function(x) x[[1]])
      tab$upper <- sapply(x$bootstrap$results[[metric]], function(x) x[[2]])
      print(tab, digits = 3, row.names = FALSE)
    }
  } else {
    cat("\n")
    tab <- data.frame(model = names(x$predictions))
    for (metric in numeric_metrics) {
      tab[[metric]] <- x$score[[metric]]
    }
    print(tab, digits = 3, row.names = FALSE)
  }
}

# if (!x$quiet) {
#   assumptions(x)
# }
#
# numeric_metrics <- x$metrics[x$metrics != "oeplot"]
#
# # if we bootstrapped, make 1 table for each metric
# # if not, make 1 table for all
# if (x$bootstrap == TRUE) {
#   for (metric in numeric_metrics) {
#     cat("\n", metric, "\n\n", sep = "")
#
#     # build result table
#     tab <- data.frame(model = x$models)
#     tab[[metric]] <- unlist(x$results[[metric]][x$models])
#     tab$lower <- sapply(
#       x$models,
#       function(m)
#         quantile(x$results_bootstrap[[metric]][[m]], probs = 0.025)
#     )
#     tab$upper <- sapply(
#       x$models,
#       function(m)
#         quantile(x$results_bootstrap[[metric]][[m]], probs = 0.975)
#     )
#     print(tab, digits = 3, row.names = FALSE)
#   }
# } else {
#   cat("\n")
#   tab <- data.frame(model = x$models)
#   for (metric in numeric_metrics) {
#     tab[[metric]] <- unlist(x$results[[metric]][x$models])
#   }
#   print(tab, digits = 3, row.names = FALSE)
# }
#
# if ("oeplot" %in% x$metrics) {
#   plot(x)
# }


#' @export
plot.CFscore <- function(x, ...) {
  # this plotting function should ideally be more customizable,
  # i.e. show/hide legend, colors, xlim, ylim, ....


  # if no bootstrap, draw all calibration plots in 1
  # if bootstrap, each model gets on calibration plot

  models <- names(x$predictions)

  if (x$bootstrap_iterations == 0) {
    plot(1, type = "n",
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Risk", ylab = "CF observed",
         main = "CF Calibration plot")
    graphics::abline(0, 1, col = "black")
    colors <- palette.colors(n = length(models) + 1, recycle = TRUE, alpha = 0.8)[-1]
    for (i in seq_along(models)) {
      lines(
        x = x$score$calplot[["pred", models[i]]],
        y = x$score$calplot[["obs", models[i]]],
        type = "o",
        col = colors[i]
      )
    }
    legend("topleft",
           legend = models,
           col    = colors,
           lty    = 1,
           lwd    = 1,
           pch    = 1,
           bty    = "n")
  } else {
    for (m in models) {

    }
  }



}


assumptions <- function(x) {

  if ("confounders" %in% names(x)) {
    adjustment_text <- paste0(
      "{", paste0(x$ipt$confounders, collapse = ", "), "}"
    )
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

  pp("- Correctly specified propensity formula. Estimated treatment model is ",
     print_model(x$ipt$model))

  if (x$outcome_type == "survival") {
    pp("- Censoring is accounted for with")
  }
}


print_model <- function(model) {
  link <- model$family$link
  lhs_var <- model$formula[[2]]

  lhs <- paste0(link, "(", lhs_var, ")")

  var_names <- names(model$coefficients)

  coef <- round(unname(model$coefficients), 2)

  rhs <- paste(coef, var_names, sep = "*", collapse = " + ")
  rhs <- gsub("*(Intercept)", "", rhs, fixed = TRUE)
  rhs <- gsub("+ -", "- ", rhs, fixed = TRUE)

  formula <- paste(lhs, rhs, sep = " = ")
  formula
}
