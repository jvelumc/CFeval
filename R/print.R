# pretty print, respecting output width, adding \n at the end
pp <- function(...) {
  txt <- paste0(c(...), collapse = "")
  cat(paste0(strwrap(txt), "\n"))
}

formatci <- function(ci) {
  paste0(format(ci, digits = 3), collapse = " - ")
}

#' @export
print.cfscore <- function(x, trt = NULL) {
  if (!x$quiet) {
    assumptions(x)
    # assumptions(x$treatments, x$confounding_set)
  }
  if (is.null(trt)) {
    trt <- x$treatments
  }
  results <- data.frame("metric" = c("Brier", "auc", "OEratio"))
  for (t in trt) {
    if (t != "observed") {
      col <- paste0("CF", t)
      results[[col]] <- format(unlist(x[[col]][1:3]), digits = 3)

      if ("bootstrap" %in% names(x)) {
        results[[paste0(col, ".95CI")]] <-
          sapply(x[["bootstrap"]][[col]], formatci)
      }

    } else {
      col <- "observed"
      results[[col]] <- c(x$auc, x$brier, x$OEratio)
    }
  }
  print(results)
}

#' @export
plot.cfscore <- function(x, trt = NULL) {
  if (is.null(trt)) {
    trt <- x$treatments
  }
  for (t in trt) {
    if (t != "observed") {
      x[[paste0("CF", t)]]$calibrationplot()
    }
    else {
      x$calibrationplot()
    }
  }
}

assumptions <- function(x) {
  t <- x$treatments
  n_t <- length(t)
  if (n_t >= 2) {
    t_formatted <- paste0(
      paste0(t[1:(n_t-1)], collapse = ", "), " and ", t[n_t]
    )
  } else {
    t_formatted <- t[[1]]
  }
  pp("Estimation of the performance of the prediction model in a counterfactual
     (CF) dataset where everyone's treatment was set to ",
     t_formatted, ".")
  pp("The following assumptions must be satisfied for correct inference:")

  if ("confounders" %in% names(x)) {
    adjustment_text <- paste0("{", paste0(x$confounders, collapse = ", "), "}")
  } else {
    adjustment_text <- "given IP-weights"
  }
  pp(" - Conditional exchangeability requires that ", adjustment_text,
     " are sufficient to adjust for confounding and selection bias between
      treatment and outcome.")
  pp("- Positivity (assess $weights for outliers)")
  pp("- Consistency")
  pp("- No interference")
  pp("- No measurement error")
  pp("- Correctly specified propensity formula")
}
