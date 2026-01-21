#' Get the inverse probability of treatment weights
#'
#' @param data a data.frame
#' @param propensity_formula a formula with on the l.h.s. the treatment variable and
#' on the right hand side the confounders.
#'
#' @returns a numeric vector of the IP weights
#' @export
ipt_weights <- function(data, propensity_formula) {
  A <- all.vars(propensity_formula)[1]
  propensity_model <- stats::glm(propensity_formula, family = "binomial", data)
  prop_score <- stats::predict(propensity_model, type = "response")
  prob_trt <- ifelse(data[[A]] == 1, prop_score, 1 - prop_score)
  list(
    model = propensity_model,
    weights = 1 / prob_trt
  )
}

