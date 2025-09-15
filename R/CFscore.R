predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}


#' @export
CFscore_undertrt <- function(data, cf, Y, A_column_name, ipw, trt) {
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

# input: single element or list of length 1 or length(treatments)
# output: a list of length treatments, where each element is input, possibly repeated
make_x_as_list <- function(x, treatments) {
  n_t <- length(treatments)
  n_x <- ifelse("list" %in% class(x), length(x), 1) # do we have a list of x or just 1?

  if ("list" %in% class(x)) {
    if (length(x) == 1) {
      return(replicate(n_t, x[[1]], simplify = FALSE))
    } else {
      stopifnot(
        "Number of predictions/models is incompatible with number of treatments" =
          length(x) == n_t
      )
      return(x)
    }
  } else {
    return(replicate(n_t, x, simplify = FALSE))
  }
}

#' Assess counterfactual performance of a model capable of predictions under
#' interventions
#'
#' @param data A data.frame on which the model is to be validated.
#' @param model A glm (lm?) which can make predictions under interventions which
#'   has treatment as a predictor, or a list of glm's for each specified
#'   treatment in the treatments variable. Either model or predictions must be
#'   given.
#' @param predictions A numeric vector of predictions under intervention, or a
#'   list of numeric vectors of predictions under different interventions as
#'   specified by treatments variable. Either model or predictions must be given
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
CFscore <- function(data, model, predictions, Y, propensity_formula, treatments) {

  n_t <- length(treatments)
  if (!missing(model)) {
    n_models <- ifelse("list" %in% class(model), length(model), 1)
  }
  if (!missing(predictions)) {
    if (is.numeric(predictions)) {
      n_predictions <- 1
    } else if (is.list(predictions)) {
      stopifnot("Predictions should either be a numeric vector or a list of
                numeric vectors" = is.numeric(predictions[[1]]))
      n_predictions <- length(predictions)
    } else {
      stop("Predictions should either be a numeric vector or a list of
           numeric vectors.")
    }
  }

  stopifnot(
    "Either model or predictions must be specified, and not both" =
      xor(missing(model), missing(predictions)),
    "There should be 1 model or a model for each treatment value" =
      missing(model) || n_models %in% c(1, n_t),
    "There should be 1 prediction vector or a list of prediction vectors for each treatment value" =
      missing(predictions) || length(predictions) %in% c(1, n_t)
  )

  if (is.character(Y)) {
    Y <- data[[Y]]
  }

  A <- all.vars(propensity_formula)[1]
  confounding_set <- all.vars(propensity_formula)[-1]
  ip <- ip_weights(data, propensity_formula)

  if (!missing(model)) {
    model <- make_x_as_list(model, treatments)
    predictions <- lapply(
      1:length(model),
      function(i) predict_CF(model[[i]], data, A, treatments[[i]])
    )
  } else {
    predictions <- make_x_as_list(predictions, treatments)
  }

  results <- lapply(
    X = 1:length(treatments),
    FUN = function(i) {
      CFscore_undertrt(data, predictions[[i]], Y, A, ip, trt = treatments[[i]])
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




