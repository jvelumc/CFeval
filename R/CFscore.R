library(survival)



# treatment formula mag A ~ 1 zijn
CFscore <- function(
    data, model, predictions, outcome_formula,
    treatment_formula, iptw_weights,
    treatment_of_interest,
    cens.model = "cox", ipcw_weights, time_horizon,
    metrics = c("auc", "brier", "oeratio"),
    bootstrap = FALSE, bootstrap_iterations = 200
) {

  # checking input (move to seperate function?)

  check_missing(data)
  check_missing_xor(model, predictions)
  check_missing(outcome_formula)
  check_missing(treatment_formula)
  check_missing(treatment_of_interest)

  # assert treatment is binary
  # assert non-surival outcome is binary
  # assert rhs(outcome_formula != 1) iff surv model AND!missing(iptw_weights)
  # handle formulas in general (lhs is 1 term, ...)

  if (bootstrap == TRUE)
    stopifnot("can't bootstrap if iptw are given" = !missing(iptw_weights))

  # done checking input

  cfscore <- list()
  class(cfscore) <- "CFscore"

  # get the observed outcome
  cfscore$outcome <- extract_lhs(data, outcome_formula)
  if (inherits(cfscore$outcome, "Surv")) {
    cfscore$outcome_type <- "survival"
    cfscore$time_horizon <- time_horizon
    cfscore$status_at_horizon <- ifelse(
      cfscore$outcome[, 1] < time_horizon,
      cfscore$outcome[, 2],
      F
    )
  } else {
    cfscore$outcome_type <- "binary"
  }

  # get the treatment
  cfscore$treatment_column <- treatment_formula[[2]]
  cfscore$observed_treatment <- extract_lhs(data, treatment_formula)
  cfscore$cf_treatment <- treatment_of_interest

  # get the CF predictions
  if (!missing(model)) {
    predictions <- predict_CF(
      model = model,
      data = data,
      A_column = cfscore$treatment_column,
      CF_treatment = cfscore$cf_treatment,
      cfscore$time_horizon
    )
  }
  cfscore$predictions <- predictions

  # get iptw
  cfscore$ipt$method = "weights manually specified"
  if (!missing(treatment_formula)) {
    cfscore$ipt$method <- "binomial glm"
    cfscore$ipt$propensity_formula <- treatment_formula
    iptw <- ipt_weights(data, treatment_formula)
    iptw_weights <- iptw$weights
    cfscore$ipt$model <- iptw$model
  }
  cfscore$ipt$weights <- iptw_weights

  # get ipcw
  if (cfscore$outcome_type == "survival") {
    cfscore$ipc$method <- "weights manually specified"
    if (missing(ipcw_weights)) {
      cfscore$ipc$method <- cens.model
      cfscore$ipc$cens.formula <- outcome_formula
      ipcw <- ipc_weights(data, outcome_formula, cens.model, time_horizon)
      ipcw_weights <- ipcw$weights
      cfscore$ipc$model <- ipcw$model
    }
    cfscore$ipc$weights <- ipcw_weights
  }

  # compute metrics
  if (cfscore$outcome_type == "survival") {
    if ("auc" %in% metrics) {
      cfscore$score$auc <- cf_auc(
        obs_outcome = cfscore$status_at_horizon,
        obs_trt = cfscore$observed_treatment,
        cf_pred = cfscore$predictions,
        cf_trt = cfscore$cf_treatment,
        ipw = cfscore$ipt$weights * cfscore$ipc$weights
      )
    }
    if ("brier" %in% metrics) {
      cfscore$score$brier <- cf_brier(
        obs_outcome = cfscore$status_at_horizon,
        obs_trt = cfscore$observed_treatment,
        cf_pred = cfscore$predictions,
        cf_trt = cfscore$cf_treatment,
        ipw = cfscore$ipt$weights * cfscore$ipc$weights
      )
    }
    if ("oeratio" %in% metrics) {
      cfscore$score$oeratio <- cf_oeratio(
        obs_outcome = cfscore$status_at_horizon,
        obs_trt = cfscore$observed_treatment,
        cf_pred = cfscore$predictions,
        cf_trt = cfscore$cf_treatment,
        ipw = cfscore$ipt$weights * cfscore$ipc$weights
      )
    }
  } else {
    for (m in metrics) {
      cfscore$score[[m]] <- cf_metric(
        m,
        obs_outcome = cfscore$outcome,
        obs_trt = cfscore$observed_treatment,
        cf_pred = cfscore$predictions,
        cf_trt = cfscore$cf_treatment,
        ipw = cfscore$ipt$weights
      )
    }
  }

  return(cfscore)
}


# riskRegression::Score()
# CFscore_undertrt <- function(data, cf, Y, A_column_name, ipw, trt,
#                              metrics, sample = 1:nrow(data)) {
#   lapply(
#     X = metrics, # Name metrics s.t. result is also named
#     FUN = function(metric) {
#       lapply(
#         X = cf,
#         FUN = function(pred) {
#           CFscore_undertrt_metric_pred(
#             data[sample, ], pred[sample], Y[sample],
#             A_column_name, ipw, trt, metric
#           )
#         }
#       )
#     }
#   )
# }
#
# CFscore_undertrt_metric_pred <- function(data, cf, Y, A_column_name, ipw, trt,
#                                          metric) {
#   # rows of patients with trt of interest
#   trt_ids <- data[[A_column_name]] == trt
#
#   if (metric == "brier") {
#     results <- brier_weighted(
#       predictions = cf[trt_ids],
#       outcomes = Y[trt_ids],
#       weights = ipw[trt_ids]
#     )
#   }
#
#   if (metric == "auc") {
#     results <- auc_weighted(
#       predictions = cf[trt_ids],
#       outcomes = Y[trt_ids],
#       weights = ipw[trt_ids]
#     )
#   }
#
#   if (metric == "oe") {
#     results <- calibration_weighted(
#       outcomes = Y,
#       predictions = cf,
#       treatments = data[[A_column_name]],
#       treatment_of_interest = trt,
#       weights = ipw
#     )
#   }
#
#   if (metric == "oeplot") {
#     results <- calibration_plot_weighted(
#       outcomes = Y,
#       predictions = cf,
#       treatments = data[[A_column_name]],
#       treatment_of_interest = trt,
#       weights = ipw
#     )
#   }
#
#   return(results)
# }
#
# name_unnamed_list <- function(x) {
#   # give names, if not named
#   sapply(
#     1:length(x),
#     function(i)
#
#       if (is.null(names(x)[i]) || names(x)[i] == "") {
#         paste0("model.", i)
#       } else {
#         names(x[i])
#       }
#   )
# }
#
# make_list_if_not_list <- function(x) {
#   if (!("list" %in% class(x)))
#     x <- list(x)
#   names(x) <- name_unnamed_list(x)
#   x
# }




#' Main CFscore function
#'
#' @param validation_data A data.frame on which the model is to be validated.
#' @param model A glm (lm?) to be validated, or a list of glm's to be validated.
#' @param predictions  A numeric vector of predictions, or a list of numeric
#'   vectors of predictions. Either model or predictions must be given, not
#'   both.
#' @param outcome_column A character string indicating the name of the observed
#'   outcome column in data, or a numeric vector of the observed outcomes.
#' @param propensity_formula A formula used to estimate the inverse-probability
#'   weights for the validation data. Treatment variable should be on the left
#'   hand side, all confounders on the right hand side. It is possible that
#'   there is a different set of confounders in the validation dataset compared
#'   to the model-development dataset. Either this or ipweights must be given
#' @param ipweights The inverse probabilty weights for the validation data. Can be
#'   either string indicating the name of the ip column in the validation data,
#'   or a numeric vector of ip-weights.
#' @param treatment_column A character string indicating the name of the realized treatment
#'   column in data. Must be given if ipw's are specified. It is automatically
#'   inferred from propensity_formula if given.
#' @param treatment_of_interest A treatment level  for which the counterfactual perormance measures should be evaluated.
#' @param metrics The metrics to be computed, options are c("auc", "brier",
#' "oe", "oeplot")
#' @param null.model If TRUE, fit a null model on the counterfactual validation data
#' @param bootstrap If TRUE, a 95% CI around performance metrics is estimated by
#'   bootstrapping
#' @param bootstrap_iterations the number of bootstrap iterations
#' @param quiet If set to TRUE, don't print assumptions
#'
#' @returns A list with Performance metrics
#' @export
#'
#' @examples
#' simulate_data <- function(n) {
#'   df <- data.frame(id = 1:n)
#'   df$L <- rnorm(n)
#'   df$A <- rbinom(n, 1, plogis(df$L))
#'   df$P <- rnorm(n)
#'   df$Y <- rbinom(n, 1, plogis(0.5 + df$L + 1.25 * df$P - 0.6*df$A))
#'   return(df)
#' }
#'
#' set.seed(123)
#' df_dev <- simulate_data(5000)
#' df_val <- simulate_data(4000)
#'
#' naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
#'
#' propensity_model <- glm(A ~ L, family = "binomial", df_dev)
#' prop_score <- predict(propensity_model, type = "response")
#' prob_trt <- ifelse(df_dev$A == 1, prop_score, 1 - prop_score)
#' ipw <- 1 / prob_trt
#'
#' causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
#'                    weights = ipw)
#'
#' CFscore(
#'   validation_data = df_val,
#'   model = list("naive model" = naive_model, "causal model" = causal_model),
#'   outcome_column = "Y",
#'   propensity_formula = A ~ L,
#'   treatment_of_interest = 0
#' )
# CFscore <- function(validation_data, model, predictions, outcome_column,
#                     propensity_formula, ipweights,
#                     treatment_column, treatment_of_interest,
#                     metrics = c("auc", "brier", "oe", "oeplot"),
#                     null.model = TRUE, bootstrap = FALSE,
#                     bootstrap_iterations = 200,
#                     quiet = FALSE) {
#   check_missing(validation_data)
#   check_missing_xor(model, predictions)
#   check_missing(outcome_column)
#   check_missing_xor(propensity_formula, ipweights)
#   check_missing_xor(propensity_formula, treatment_column)
#   check_missing(treatment_of_interest)
#
#   # extract treatment_column & ipweights from propensity formula if given
#   if (missing(treatment_column)) {
#     treatment_column <- all.vars(propensity_formula)[1]
#   }
#   if (!missing(propensity_formula)) {
#     ipweights <- ip_weights(validation_data, propensity_formula)
#   }
#
#   # extract predictions from models
#   if (!missing(model)) {
#     model <- make_list_if_not_list(model)
#
#     predictions <- lapply(model, function(mod) {
#       predict_CF(
#         model = mod,
#         data = validation_data,
#         A_column = treatment_column,
#         CF_treatment = treatment_of_interest
#       )
#     })
#   }
#   predictions <- make_list_if_not_list(predictions)
#
#   # get outcome vector
#   if (is.character(outcome_column) == TRUE) {
#     outcome_column <- validation_data[[outcome_column]]
#   }
#
#   correct_trt_id <- validation_data[[treatment_column]] == treatment_of_interest
#
#   # fit a null model on counterfactual validation data
#   if (null.model == TRUE) {
#     null_model <- lm(
#       outcome_column[correct_trt_id] ~ 1,
#       weights = ipweights[correct_trt_id]
#     )
#
#     null_preds <- predict(null_model,
#       newdata = validation_data,
#       type = "response"
#     )
#
#     predictions <- c(list("null.model" = null_preds), predictions)
#   }
#
#   if (anyDuplicated(names(predictions))) {
#     stop("Multiple models with name '",
#          names(predictions)[duplicated(names(predictions))][1],
#          "' found, please provide unique names (and don't use name ",
#          "'null.model' if null.model = TRUE).")
#   }
#
#   # make metric named
#   names(metrics) <- metrics
#
#   cfscore <- list()
#
#   cfscore$results <- CFscore_undertrt(
#     data = validation_data,
#     cf = predictions,
#     Y = outcome_column,
#     A_column_name = treatment_column,
#     ipw = ipweights,
#     trt = treatment_of_interest,
#     metric = metrics
#   )
#
#   if (bootstrap == TRUE) {
#     b <- run_bootstrap(
#       data = validation_data,
#       propensity_formula = propensity_formula,
#       predictions = predictions,
#       Y = outcome_column,
#       A = treatment_column,
#       treatment_of_interest = treatment_of_interest,
#       metrics = metrics,
#       iterations = bootstrap_iterations
#     )
#     cfscore$results_bootstrap <- b
#   }
#
#   cfscore$models <- names(predictions)
#   cfscore$metrics <- metrics
#   cfscore$ipweights <- ipweights
#   cfscore$treatment_column <- treatment_column
#   cfscore$treatment_of_interest <- treatment_of_interest
#   cfscore$bootstrap <- bootstrap
#   if (!missing(propensity_formula)) {
#     cfscore$propensity <- propensity_formula
#     cfscore$confounders <- all.vars(propensity_formula)[-1]
#   }
#   cfscore$quiet <- quiet
#
#   class(cfscore) <- "cfscore"
#   return(cfscore)
# }
#

