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
  cf_pred <- cf_pred[pseudo_i]
  ipw <- ipw[pseudo_i]

  stopifnot(
    "no controls (outcome = 0) in pseudopopulation" = 0 %in% obs_outcome,
    "no cases (outcome = 1) in pseudopopulation" = 1 %in% obs_outcome,
    "nonbinary outcome" = length(unique(obs_outcome)) == 2
  )

  o <- order(cf_pred)
  obs_outcome <- obs_outcome[o]
  ipw <- ipw[o]

  # weighted ranks
  cum_ipw <- cumsum(ipw)
  rank_ipw <- cum_ipw - 0.5 * ipw

  W1 <- sum(ipw[obs_outcome == 1])
  W0 <- sum(ipw[obs_outcome == 0])

  auc <- (
    sum(ipw[obs_outcome == 1] * rank_ipw[obs_outcome == 1]) -
      0.5 * W1^2
  ) / (W1 * W0)

  auc
}

# calibration

# # oe ratio
cf_oeratio <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

  # indices of persons that follow the trt of interest
  pseudo_i <- obs_trt == cf_trt

  observed <- stats::weighted.mean(
    x = obs_outcome[pseudo_i],
    w = ipw[pseudo_i]
  )

  expected <- mean(cf_pred)

  return(observed/expected)
}

cf_oeratio_e_from_pp <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

  # indices of persons that follow the trt of interest
  pseudo_i <- obs_trt == cf_trt

  observed <- stats::weighted.mean(
    x = obs_outcome[pseudo_i],
    w = ipw[pseudo_i]
  )

  expected <- stats::weighted.mean(
    x = cf_pred[pseudo_i],
    w = ipw[pseudo_i]
  )

  return(observed/expected)
}
