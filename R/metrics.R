# Brier

cf_brier <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

  # indices of persons that follow the trt of interest
  pseudo_i <- obs_trt == cf_trt

  # cf brier score is brier score of weighted pseudopop
  1 / sum(ipw[pseudo_i]) *
    sum(
      ( cf_pred[pseudo_i] - obs_outcome[pseudo_i] )^2 * ipw[pseudo_i]
    )
}


# AUC

cf_auc <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

  # indices of persons that follow the trt of interest
  pseudo_i <- obs_trt == cf_trt

  obs_outcome <- obs_outcome[pseudo_i]
  obs_trt <- obs_trt[pseudo_i]
  cf_pred <- cf_pred[pseudo_i]
  cf_trt <- cf_trt[pseudo_i]
  ipw <- ipw[pseudo_i]

  stopifnot(
    "no controls (outcome = 0) in pseudopopulation" = 0 %in% obs_outcome,
    "no cases (outcome = 1) in pseudopopulation" = 1 %in% obs_outcome,
    "nonbinary outcome" = length(unique(obs_outcome)) == 2
  )

  cases_indices <- which(obs_outcome == 1)
  controls_indices <- which(obs_outcome == 0)

  cases_predictions <- cf_pred[cases_indices]
  controls_predictions <- cf_pred[controls_indices]

  cases_weights <- ipw[cases_indices]
  controls_weights <- ipw[controls_indices]

  numerator <- 0
  denominator <- 0

  for (i in cases_indices) {

    cf_pred_i <- cf_pred[i]
    ipw_i <- ipw[i]

    n_wins <- sum(
      ipw_i * controls_weights[cf_pred_i > controls_predictions]
    )

    n_ties <- sum(
      ipw_i * controls_weights[cf_pred_i == controls_predictions]
    )

    numerator <- numerator + n_wins + 0.5 * n_ties

    denominator <- denominator + sum(ipw_i * controls_weights)

  }
  return(numerator / denominator)
}


# OE
