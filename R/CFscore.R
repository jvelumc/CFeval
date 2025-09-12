predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}


#' @export
CFscore_undertrt <- function(data, model, Y_column_name, A_column_name, ipw, trt) {
  cf <- predict_CF(model, data, A_column_name, trt) # counterfactuals as estimated by model
  outcomes <- data[[Y_column_name]] # outcomes
  trt_ids <- data[[A_column_name]] == trt # rows of patients with trt of interest

  calibration <- calibration_weighted(
    outcomes = data[[Y_column_name]],
    predictions = cf,
    treatments = data[[A_column_name]],
    treatment_of_interest = trt,
    weights = ipw
  )

  auc <- auc_weighted(
    outcomes = outcomes[trt_ids],
    predictions = cf[trt_ids],
    weights = ipw[trt_ids]
  )

  brier <- brier_weighted(
    outcomes = outcomes[trt_ids],
    predictions = cf[trt_ids],
    weights = ipw[trt_ids]
  )

  list("brier" = brier, "auc" = auc, "calibration" = calibration)
}

# pretty print, respecting output width, adding \n at the end
pp <- function(...) {
  txt <- paste0(c(...), collapse = "")
  cat(paste0(strwrap(txt), "\n"))
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

#' Assess counterfactual performance of a model capable of predictions under
#' interventions
#'
#' @param data A data.frame on which the model is to be validated.
#' @param model A glm (lm?) which can make predictions under interventions.
#' @param Y_column_name The observed outcome column of the data.
#' @param propensity_formula A formula used to estimate the inverse-probability
#'   weights for the validation data. Treatment variable should be on the left
#'   hand side, all confounders on the right hand side. It is possible that
#'   there is a different set of confounders in the validation dataset compared
#'   to the model-development dataset.
#' @param quiet_mode Set to TRUE to avoid printing all assumptions.
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
#' df_val <- build_data(2000)
#' CFscore(df_val, causal_model, "Y", A ~ L)
CFscore <- function(data, model, Y_column_name, propensity_formula, treatments,
                    quiet_mode = FALSE) {

  A <- all.vars(propensity_formula)[1]
  confounding_set <- all.vars(propensity_formula)[-1]

  ip <- ip_weights(data, propensity_formula)

  results <- lapply(
    X = treatments,
    FUN = function(x) {
      CFscore_undertrt(data, model, Y_column_name, A, ip, trt = x)
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


#' @export
print.cfscore <- function(x) {
  assumptions(x$treatments, x$confounding_set)
}

