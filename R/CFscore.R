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
#' df_dev <- build_data(5000)
#' causal_model <- build_causal_model(df_dev)
#' df_val <- build_data(2000)
#' CFscore(df_val, causal_model, "Y", A ~ L)
CFscore <- function(data, model, Y_column_name, propensity_formula, quiet_mode = FALSE) {

  A <- all.vars(propensity_formula)[1]
  confounding_set <- all.vars(propensity_formula)[-1]

  predict_CF <- function(model, data, CF_treatment) {
    # predict outcome probabilities for all patients, setting their treatment
    # to CF_treatment
    data[[A]] <- CF_treatment
    stats::predict(model, newdata = data, type = "response")
  }

  data$CF0 <- predict_CF(model, data, 0)
  data$CF1 <- predict_CF(model, data, 1)
  prediction_under_observed_trt <- stats::predict(model, newdata = data, type = "response")

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

  oe0 <- stats::weighted.mean(data[data[[A]] == 0, Y_column_name], data[data[[A]] == 0, "ipw"]) /
    mean(data$CF0)
  oe1 <- stats::weighted.mean(data[data[[A]] == 1, Y_column_name], data[data[[A]] == 1, "ipw"]) /
    mean(data$CF1)
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

  results <- list(
    "Metric" = c("O/E ratio", "AUC", "Brier score"),
    "Naive" = c(oe_observed, auc_observed, brier_observed),
    "CF0" = c(oe0, auc0, brier0),
    "CF1" = c(oe1, auc1, brier1)
  )

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
  print(as.data.frame(results))
  if (!quiet_mode) {
    cat("\nNaive performance is the model performance on the observed validation data.\n")
    cat("CF0/CF1 is the estimated model performance on a CF dataset where everyone was untreated/treated, respectively.\n")
  }

  if (quiet_mode) {
    return(invisible(results))
  } else {
    # we already printed results in non-quiet mode, so return invisible
    return(invisible(results))
  }
}
