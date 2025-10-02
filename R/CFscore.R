predict_CF <- function(model, data, A_column, CF_treatment) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  stats::predict(model, newdata = data, type = "response")
}

CFscore_undertrt <- function(data, cf, Y, A_column_name, ipw, trt, plot) {
  trt_ids <- data[[A_column_name]] == trt # rows of patients with trt of interest

  calibration <- calibration_weighted(
    outcomes = Y,
    predictions = cf,
    treatments = data[[A_column_name]],
    treatment_of_interest = trt,
    weights = ipw,
    plot = plot
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

  list("brier" = brier, "auc" = auc,
       "OEratio" = calibration$OEratio,
       "calibrationplot" = calibration$plot)
}

score_realized_trt <- function(pred, outcomes, plot) {
  calibration <- calibration(
    outcomes = outcomes,
    predictions = pred,
    plot = plot
  )
  auc <- auc_weighted(
    outcomes = outcomes,
    predictions = pred,
    weights = rep(1, length(outcomes))
  )
  brier <- brier_weighted(
    outcomes = outcomes,
    predictions = pred,
    weights = rep(1, length(outcomes))
  )
  list("brier" = brier, "auc" = auc,
       "OEratio" = calibration$OEratio,
       "calibrationplot" = calibration$plot)
}

#' Assess performance of predictions for realized treatment values.
#'
#' @param data A data.frame on which the model is to be validated
#' @param model A glm
#' @param predictions A numeric vector of predictions. Either this or data AND
#'   model must be given.
#' @param Y A numeric vector of observed outcomes, or a character string
#'   indicating the name of the observed outcomes in the data.
#' @param plot If set to TRUE, generate calibration plot
#' @returns  A list of performance measures (Brier, Observed/Expected, AUC) of
#'   the model on observed data
#' @export
#'
#' @examples
#' model <- glm(
#'   Y ~ A + P,
#'   family = "binomial",
#'   data = df_dev,
#' )
#' observed_score(data = df_val, model = model, Y = "Y")
observed_score <- function(data, model, predictions, Y, plot = TRUE) {
  stopifnot(
    "Either data and model, or predictions must be given" =
    xor(missing(predictions), ( missing(data) && missing(model) ) ),
    "Outcomes Y must be numeric or a column name. If the latter, data must
    be given" = is.numeric(Y) || !missing(data)
  )

  if (is.character(Y)) {
    Y <- data[[Y]]
  }

  if (missing(predictions)) {
    predictions <- stats::predict(model, newdata = data, type = "response")
  }
  results <- score_realized_trt(predictions, Y, plot)
  class(results) <- "cfscore"
  results$treatments <- "observed"
  results$quiet <- TRUE
  return(results)
}


# input: single element or list of length 1 or length(treatments) output: a list
# of length treatments, where each element is input, possibly repeated
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



CFscore <- function(validation_data, model, predictions, propensity_formula,
                    outcome_column, treatment_column, treatment_of_interest,
                    null.model = TRUE, bootstrap = FALSE, plot = TRUE,
                    quiet = FALSE) {
  # input checking

  if (!missing(model)) { # glm's dont have list class. Do other model objects?
    n_models <- ifelse("list" %in% class(model), length(model), 1)
  }

  #
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
#'   specified by treatments variable. Either model or predictions must be
#'   given.
#' @param Y A character string indicating the name of the observed outcome
#'   column in data, or a numeric vector of the observed outcomes.
#' @param propensity_formula A formula used to estimate the inverse-probability
#'   weights for the validation data. Treatment variable should be on the left
#'   hand side, all confounders on the right hand side. It is possible that
#'   there is a different set of confounders in the validation dataset compared
#'   to the model-development dataset. Either this or ipw must be given
#' @param ip The inverse probabilty weights for the validation data. Can be
#'   either string indicating the name of the ip column in the validation data,
#'   or a numeric vector of ip-weights.
#' @param A A character string indicating the name of the realized treatment
#'   column in data. Must be given if ipw's are specified. It is automatically
#'   inferred from propensity_formula if given.
#' @param treatments A treatment level or a list of all treatment levels for
#'   which the counterfactual perormance measures should be evaluated.
#' @param bootstrap If a 95% CI around performance estimates estimated by
#'   bootstrappingis desired, this can be achieved by setting this variable to a
#'   integer indicating the amount of iterations. Argument propensity_formula
#'   must then be given.
#' @param plot If set to TRUE, generate calibration plot
#' @param quiet If set to TRUE, don't print assumptions
#'
#' @returns A list of performance measures (Brier, Observed/Expected, AUC) of
#'   the model on counterfactual data, where each subject is assigned to the
#'   treatments as given.
#' @export
#'
#' @examples
#' causal_model <- glm(
#'   Y ~ A + P,
#'   family = "binomial",
#'   data = df_dev,
#'   weights = ip_weights(df_dev, A ~ L)
#' )
#' CFscore(data = df_val, model = causal_model, Y = "Y",
#'         propensity_formula = A ~ L, treatments = list(0,1))
# CFscore <- function(data, model, predictions, Y, propensity_formula,
#                     ip, A, treatments, bootstrap, plot = TRUE, quiet = FALSE) {
#
#   # check inputs and make consistent
#   n_t <- length(treatments)
#   if (!missing(model)) {
#     n_models <- ifelse("list" %in% class(model), length(model), 1)
#   }
#   if (!missing(predictions)) {
#     if (is.numeric(predictions)) {
#       n_models <- 1
#     } else if (is.list(predictions)) {
#       stopifnot("Predictions should either be a numeric vector or a list of
#                 numeric vectors" = is.numeric(predictions[[1]]))
#       n_models <- length(predictions)
#     } else {
#       stop("Predictions should either be a numeric vector or a list of
#            numeric vectors.")
#     }
#   }
#
#   if (is.character(Y)) {
#     Y <- data[[Y]]
#   }
#
#   stopifnot(
#     "Either model or predictions must be specified, and not both" =
#       xor(missing(model), missing(predictions)),
#     "There should be 1 model or a model for each treatment value" =
#       missing(model) || n_models %in% c(1, n_t),
#     "There should be 1 prediction vector or a list of prediction vectors for
#     each treatment value" =
#       missing(predictions) || n_models %in% c(1, n_t),
#     "Either propensity formula or ip must be specified, and not both" =
#       xor(missing(propensity_formula), missing(ip)),
#     "A must be given if propensity_formula is not specified" =
#       !missing(propensity_formula) || !missing(A),
#     "A must be column name (character) of treatment variable in validation
#     data" =
#       missing(A) || is.character(A),
#     "IP weights must be numeric and of same length as outcome" =
#       missing(ip) || ( is.numeric(ip) && length(ip) == length(Y) ),
#     "If we are bootstrapping, propensity formula must be given as this is used
#     in the bootstrap process" =
#       missing(bootstrap) || !missing(propensity_formula)
#   )
#
#   if (!missing(propensity_formula)) {
#     if (!missing(A)) {
#       stopifnot("A must be the l.h.s. of propensity formula",
#                 A == all.vars(propensity_formula)[1])
#     }
#     A <- all.vars(propensity_formula)[1]
#   }
#
#   if (!missing(model)) {
#     model <- make_x_as_list(model, treatments)
#     predictions <- lapply(
#       1:length(model),
#       function(i) predict_CF(model[[i]], data, A, treatments[[i]])
#     )
#   } else {
#     predictions <- make_x_as_list(predictions, treatments)
#   }
#
#   if (!missing(propensity_formula)) {
#     ip <- ip_weights(data, propensity_formula)
#   }
#
#
#   # peform actual analysis
#   results <- lapply(
#     X = 1:length(treatments),
#     FUN = function(i) {
#       CFscore_undertrt(data, predictions[[i]], Y, A, ip, trt = treatments[[i]],
#                        plot)
#     }
#   )
#   names(results) <- lapply(
#     X = treatments,
#     FUN = function(x) paste0("CF", x)
#   )
#
#   results$treatments <- treatments
#
#   # bootstrap
#   if (!missing(bootstrap)) {
#     b <- run_bootstrap(data, propensity_formula, predictions,
#                        Y, A, treatments, bootstrap)
#     results <- append(results, b)
#   }
#
#   results$weights <- ip
#
#   if (!missing(propensity_formula)) {
#     results$propensity <- propensity_formula
#     results$confounders <- all.vars(propensity_formula)[-1]
#   }
#   results$quiet <- quiet
#   class(results) <- "cfscore"
#   results
# }
#



