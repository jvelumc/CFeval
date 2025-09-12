predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}


#' @export
CFscore_undertrt <- function(data, model, Y, A_column_name, ipw, trt) {
  cf <- predict_CF(model, data, A_column_name, trt) # counterfactuals as estimated by model
  trt_ids <- data[[A_column_name]] == trt # rows of patients with trt of interest

  calibration <- calibration_weighted(
    outcomes = Y,
    predictions = cf,
    treatments = data[[A_column_name]],
    treatment_of_interest = trt,
    weights = ipw
  )

  auc <- auc_weighted(
    outcomes = Y[trt_ids],
    predictions = cf[trt_ids],
    weights = ipw[trt_ids]
  )

  brier <- brier_weighted(
    outcomes = Y[trt_ids],
    predictions = cf[trt_ids],
    weights = ipw[trt_ids]
  )

  list("brier" = brier, "auc" = auc, "calibration" = calibration)
}


#' Assess counterfactual performance of a model capable of predictions under
#' interventions
#'
#' @param data A data.frame on which the model is to be validated.
#' @param model A glm (lm?) which can make predictions under interventions.
#' @param Y A character string indicating the name of the observed outcome
#'   column in data, or a numeric vector of the observed outcomes.
#' @param propensity_formula A formula used to estimate the inverse-probability
#'   weights for the validation data. Treatment variable should be on the left
#'   hand side, all confounders on the right hand side. It is possible that
#'   there is a different set of confounders in the validation dataset compared
#'   to the model-development dataset.
#' @param treatments A list of all treatments for which the counterfactual
#'   perormance measures should be evaluated.
#'
#' @returns A list of performance measures (Brier, Observed/Expected, AUC) of
#'   the model on observed data (naive) and counterfactual data, where each
#'   subject is assigned to both treatment options.
#' @export
#'
#' @examples
#' causal_model <- glm(
#'   Y ~ A + P,
#'   family = "binomial",
#'   data = df_dev,
#'   weights = ip_weights(df_dev, A ~ L)
#' )
#' CFscore(df_val, causal_model, "Y", A ~ L, treatments = list(0,1))
CFscore <- function(data, model, Y, propensity_formula, treatments) {

  if (is.character(Y)) {
    Y <- data[[Y]]
  }

  A <- all.vars(propensity_formula)[1]
  confounding_set <- all.vars(propensity_formula)[-1]

  ip <- ip_weights(data, propensity_formula)

  results <- lapply(
    X = treatments,
    FUN = function(x) {
      CFscore_undertrt(data, model, Y, A, ip, trt = x)
    }
  )
  names(results) <- lapply(
    X = treatments,
    FUN = function(x) paste0("CF", x)
  )

  results$treatments <- treatments
  results$confounders <- confounding_set

  class(results) <- "cfscore"
  results
}




