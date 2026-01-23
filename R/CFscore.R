library(survival)

CFscore <- function(object, data, outcome_formula, treatment_formula,
                    treatment_of_interest, metrics = c("auc", "brier", "oeratio"),
                    time_horizon, cens.model = "cox",
                    bootstrap = 0, iptw, ipcw) {

  check_missing(object)
  check_missing(data)
  check_missing(outcome_formula)
  check_missing(treatment_formula)
  check_missing(treatment_of_interest)

  # assert treatment is binary
  # assert non-surival outcome is binary
  # assert rhs(outcome_formula != 1) iff surv model AND!missing(iptw_weights)
  # handle formulas in general (lhs is 1 term, ...)
  # assert longest surv time is longer than time horizon, to avoid annoying weights

  if (bootstrap != 0)
    stopifnot("can't bootstrap if iptw are given" = missing(iptw))

  # more input checking, move to seperate function

  cfscore <- list()
  class(cfscore) <- "CFscore"

  # get the observed outcome
  cfscore$outcome <- extract_lhs(data, outcome_formula)
  if (inherits(cfscore$outcome, "Surv")) {
    cfscore$outcome_type <- "survival"
    cfscore$time_horizon <- time_horizon
    cfscore$status_at_horizon <- ifelse(
      test = cfscore$outcome[, 1] < time_horizon,
      yes = cfscore$outcome[, 2],
      no = FALSE
    )
  } else {
    cfscore$outcome_type <- "binary"
  }

  # get the treatment
  cfscore$treatment_column <- treatment_formula[[2]]
  cfscore$observed_treatment <- extract_lhs(data, treatment_formula)
  cfscore$treatment_of_interest <- treatment_of_interest

  # make a list of risk predictions
  object <- make_list_if_not_list(object)
  cfscore$predictions <- lapply(
    X = object,
    FUN = function(x) {
      if (is.numeric(x) && is.null(dim(x))) {
        x # user supplied risk predictions
      } else {
        predict_CF(
          x,
          data,
          cfscore$treatment_column,
          cfscore$treatment_of_interest,
          time_horizon = cfscore$time_horizon
        )
      }
    }
  )

  # get iptw
  cfscore$ipt$method = "weights manually specified"
  if (!missing(treatment_formula)) {
    cfscore$ipt$method <- "binomial glm"
    cfscore$ipt$propensity_formula <- treatment_formula
    ipt <- ipt_weights(data, treatment_formula)
    iptw <- ipt$weights
    cfscore$ipt$model <- ipt$model
  }
  cfscore$ipt$weights <- iptw

  # get ipcw
  if (cfscore$outcome_type == "survival") {
    cfscore$ipc$method <- "weights manually specified"
    if (missing(ipcw)) {
      cfscore$ipc$method <- cens.model
      cfscore$ipc$cens.formula <- outcome_formula
      ipc <- ipc_weights(data, outcome_formula, cens.model, time_horizon)
      ipcw <- ipc$weights
      cfscore$ipc$model <- ipc$model
    }
    cfscore$ipc$weights <- ipcw
  }

  cfscore$metrics <- metrics
  cfscore$score <- get_metrics(cfscore)


  if (bootstrap != 0) {
    cfscore$bootstrap_iterations <- bootstrap
    cfscore$bootstrap <- bootstrap(data, cfscore)
  }
  return(cfscore)

}

get_metrics <- function(cfscore) {
  score <- list()
  if (cfscore$outcome_type == "survival") {
    for (m in cfscore$metrics) {
      score[[m]] <- sapply(
        X = cfscore$predictions,
        FUN = function(x) {
          cf_metric(
            m,
            obs_outcome = cfscore$status_at_horizon,
            obs_trt = cfscore$observed_treatment,
            cf_pred = x,
            cf_trt = cfscore$treatment_of_interest,
            ipw = cfscore$ipt$weights * cfscore$ipc$weights
          )
        }
      )
    }
  } else {
    for (m in cfscore$metrics) {
      score[[m]] <- sapply(
        X = cfscore$predictions,
        FUN = function(x) {
          cf_metric(
            m,
            obs_outcome = cfscore$outcome,
            obs_trt = cfscore$observed_treatment,
            cf_pred = x,
            cf_trt = cfscore$treatment_of_interest,
            ipw = cfscore$ipt$weights
          )
        }
      )
    }
  }
  score
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


