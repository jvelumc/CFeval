predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}

CFscore_undertrt <- function(data, cf, Y, A_column_name, ipw, trt,
                             metrics, sample = 1:nrow(data)) {
  lapply(
    X = metrics, # Name metrics s.t. result is also named
    FUN = function(metric) {
      lapply(
        X = cf,
        FUN = function(pred) {
          CFscore_undertrt_metric_pred(
            data[sample, ], pred[sample], Y[sample],
            A_column_name, ipw, trt, metric
          )
        }
      )
    }
  )
}

CFscore_undertrt_metric_pred <- function(data, cf, Y, A_column_name, ipw, trt,
                                         metric) {
  # rows of patients with trt of interest
  trt_ids <- data[[A_column_name]] == trt

  if (metric == "brier") {
    results <- brier_weighted(
      predictions = cf[trt_ids],
      outcomes = Y[trt_ids],
      weights = ipw[trt_ids]
    )
  }

  if (metric == "auc") {
    results <- auc_weighted(
      predictions = cf[trt_ids],
      outcomes = Y[trt_ids],
      weights = ipw[trt_ids]
    )
  }

  if (metric == "oe") {
    results <- calibration_weighted(
      outcomes = Y,
      predictions = cf,
      treatments = data[[A_column_name]],
      treatment_of_interest = trt,
      weights = ipw
    )
  }

  if (metric == "oeplot") {
    results <- calibration_plot_weighted(
      outcomes = Y,
      predictions = cf,
      treatments = data[[A_column_name]],
      treatment_of_interest = trt,
      weights = ipw
    )
  }

  return(results)
}

name_unnamed_list <- function(x) {
  # give names, if not named
  sapply(
    1:length(x),
    function(i)

      if (is.null(names(x)[i]) || names(x)[i] == "") {
        paste0("model.", i)
      } else {
        names(x[i])
      }
  )
}

make_list_if_not_list <- function(x) {
  if (!("list" %in% class(x)))
    x <- list(x)
  names(x) <- name_unnamed_list(x)
  x
}




CFscore <- function(validation_data, model, predictions, outcome_column,
                    propensity_formula, ipweights,
                    treatment_column, treatment_of_interest,
                    metrics = c("auc", "brier", "oe", "oeplot"),
                    null.model = TRUE, bootstrap = FALSE,
                    bootstrap_iterations = 200,
                    quiet = FALSE) {
  check_missing(validation_data)
  check_missing_xor(model, predictions)
  check_missing(outcome_column)
  check_missing_xor(propensity_formula, ipweights)
  check_missing_xor(propensity_formula, treatment_column)
  check_missing(treatment_of_interest)

  # extract treatment_column & ipweights from propensity formula if given
  if (missing(treatment_column)) {
    treatment_column <- all.vars(propensity_formula)[1]
  }
  if (!missing(propensity_formula)) {
    ipweights <- ip_weights(validation_data, propensity_formula)
  }

  # extract predictions from models
  if (!missing(model)) {
    model <- make_list_if_not_list(model)

    predictions <- lapply(model, function(mod) {
      predict_CF(
        model = mod,
        data = validation_data,
        A_column = treatment_column,
        CF_treatment = treatment_of_interest
      )
    })
  }
  predictions <- make_list_if_not_list(predictions)

  # get outcome vector
  if (is.character(outcome_column) == TRUE) {
    outcome_column <- validation_data[[outcome_column]]
  }

  correct_trt_id <- validation_data[[treatment_column]] == treatment_of_interest

  # fit a null model on counterfactual validation data
  if (null.model == TRUE) {
    null_model <- lm(
      outcome_column[correct_trt_id] ~ 1,
      weights = ipweights[correct_trt_id]
    )

    null_preds <- predict(null_model,
      newdata = validation_data,
      type = "response"
    )

    predictions <- c(list("null.model" = null_preds), predictions)
  }

  if (anyDuplicated(names(predictions))) {
    stop("Multiple models with name '",
         names(predictions)[duplicated(names(predictions))][1],
         "' found, please provide unique names (and don't use name ",
         "'null.model' if null.model = TRUE).")
  }

  # make metric named
  names(metrics) <- metrics

  cfscore <- list()

  cfscore$results <- CFscore_undertrt(
    data = validation_data,
    cf = predictions,
    Y = outcome_column,
    A_column_name = treatment_column,
    ipw = ipweights,
    trt = treatment_of_interest,
    metric = metrics
  )

  if (bootstrap == TRUE) {
    b <- run_bootstrap(
      data = validation_data,
      propensity_formula = propensity_formula,
      predictions = predictions,
      Y = outcome_column,
      A = treatment_column,
      treatment_of_interest = treatment_of_interest,
      metrics = metrics,
      iterations = bootstrap_iterations
    )
    cfscore$b <- b
  }

  cfscore$models <- names(predictions)
  cfscore$metrics <- metrics
  cfscore$ipweights <- ipweights
  cfscore$treatment_column <- treatment_column
  if (!missing(propensity_formula)) {
    cfscore$propensity <- propensity_formula
    cfscore$confounders <- all.vars(propensity_formula)[-1]
  }
  cfscore$quiet <- quiet

  class(cfscore) <- "cfscore"
  return(cfscore)
}


