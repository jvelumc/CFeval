# pretty print, respecting output width, adding \n at the end
pp <- function(...) {
  txt <- paste0(c(...), collapse = "")
  cat(paste0(strwrap(txt), "\n"))
}

#' @export
print.cfscore <- function(x, quiet = FALSE) {
  assumptions(x$treatments, x$confounding_set)
}

#' @export
plot.cfscore <- function(x, trt = NULL) {
  if (is.null(trt)) {
    trt <- x$treatments
  }
  for (t in trt) {
    x[[paste0("CF", t)]]$calibration$plot()
  }
}

assumptions <- function(t, confounders) {
  n_t <- length(t)
  if (n_t >= 2) {
    t_formatted <- paste0(
      paste0(t[1:(n_t-1)], collapse = ", "), " and ", t[n_t]
    )
  } else {
    t_formatted <- t[[1]]
  }

  confounders_formatted <- paste0(confounders, collapse = ", ")
  pp("Estimation of the performance of the prediction model in a counterfactual
     (CF) dataset where everyone's treatment was set to ",
     t_formatted, ".")
  pp("The following assumptions must be satisfied for correct inference:")
  pp(" - Conditional exchangeability requires that {, ", confounders_formatted,
     "} is sufficient to adjust for confounding and selection bias between
      treatment and outcome.")
  pp("- Positivity")
  pp("- Consistency")
  pp("- No interference")
  pp("- No measurement error")
  pp("- Correctly specified propensity formula")
}
