predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}


#' @export
CFscore_undertrt <- function(data, model, Y_column_name, propensity_formula, trt) {
  A <- all.vars(propensity_formula)[1] # treatment variable
  cf <- predict_CF(model, data, A, trt) # counterfactuals as estimated by model
  ip <- ip_weights(data, propensity_formula) # ip weights
  outcomes <- data[[Y_column_name]] # outcomes
  trt_ids <- data[[A]] == trt # rows of patients with trt of interest

  calibration <- calibration_weighted(
    outcomes = data[[Y_column_name]],
    predictions = data$cf,
    treatments = data[[A]],
    treatment_of_interest = trt,
    weights = data$ip
  )

  auc <- auc_weighted(
    outcomes = outcomes[trt_ids],
    predictions = cf[trt_ids],
    weights = ip[trt_ids]
  )

  brier <- brier_weighted(
    outcomes = outcomes[trt_ids],
    predictions = cf[trt_ids],
    weights = ip[trt_ids]
  )

  list("brier" = brier, "auc" = auc, "calibration" = calibration)
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
CFscore <- function(data, model, Y_column_name, propensity_formula, treatments, quiet_mode = FALSE) {

  A <- all.vars(propensity_formula)[1]
  confounding_set <- all.vars(propensity_formula)[-1]

  for (trt in treatments) { # probably lapply

  }

  data$CF0 <- predict_CF(model, data, A, 0)
  data$CF1 <- predict_CF(model, data, A, 1)
  # prediction_under_observed_trt <- stats::predict(model, newdata = data, type = "response")

  propensity_model <- stats::glm(
    formula = propensity_formula,
    family = "binomial",
    data = data
  )

  prop_score <- stats::predict(propensity_model, type = "response")
  prob_trt <- ifelse(data[[A]] == 1, prop_score, 1 - prop_score)
  data$ipw <- 1 / prob_trt

  data0 <- data[data[[A]] == 0, ]
  data1 <- data[data[[A]] == 1, ]

  calibration0 <- calibration_weighted(
    outcomes = data[[Y_column_name]],
    predictions = data$CF0,
    treatments = data[[A]],
    treatment_of_interest = 0,
    weights = data$ipw
  )

  calibration1 <- calibration_weighted(
    outcomes = data[[Y_column_name]],
    predictions = data$CF1,
    treatments = data[[A]],
    treatment_of_interest = 1,
    weights = data$ipw
  )

  oe0 <- calibration0$OEratio
  oe1 <- calibration1$OEratio

  oe_observed <- mean(data[[Y_column_name]]) / mean(prediction_under_observed_trt)


  auc0 <- auc_weighted(
    outcomes = data0[[Y_column_name]],
    predictions = data0$CF0,
    weights = data0$ipw
  )
  auc1 <- auc_weighted(
    outcomes = data1[[Y_column_name]],
    predictions = data1$CF1,
    weights = data1$ipw
  )
  auc_observed <- auc_weighted(
    outcomes = data[[Y_column_name]],
    predictions = prediction_under_observed_trt,
    weights = rep(1, nrow(data))
  )

  brier0 <- brier_weighted(data0[[Y_column_name]], data0$CF0, data0$ipw)
  brier1 <- brier_weighted(data1[[Y_column_name]], data1$CF1, data1$ipw)
  brier_observed <- brier_weighted(data[[Y_column_name]],
                                   prediction_under_observed_trt,
                                   rep(1, nrow(data)))

  resultsDF <- data.frame(
    "Metric" = c("O/E ratio", "AUC", "Brier score"),
    "Naive" = c(oe_observed, auc_observed, brier_observed),
    "CF0" = c(oe0, auc0, brier0),
    "CF1" = c(oe1, auc1, brier1)
  )

  resultsList <- as.list(resultsDF)

  resultsList$plot0 <- calibration0$plot
  resultsList$plot1 <- calibration1$plot

  if (!quiet_mode) {
    print("Estimating the performance of the prediction model in a counterfactual (CF) dataset where everyone received treatment and a CF dataset where nobody received treatment.")

    print("The following assumptions must be satisfied for correct inference:")

    print(paste0("[1] Conditional exchangeability requires that {", paste0(confounding_set, collapse = ", "), "} is sufficient to adjust for confounding and selection bias between ", A, " and ", Y_column_name, "."))

    print("[2] Positivity (Assess IPW in the output with $weights)")

    print("[3] Consistency")

    print("[4] No interference")

    print("[5] No measurement error")

    print("[6] Correctly specified propensity formula")

    cat("\nresults:\n")

  }
  print(resultsDF)
  if (!quiet_mode) {
    cat("\nNaive performance is the model performance on the observed validation data.\n")
    cat("CF0/CF1 is the estimated model performance on a CF dataset where everyone was untreated/treated, respectively.\n")
  }

  return(resultsList)
}
