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
  s <- cf_pred[o]
  y <- obs_outcome[o]
  w <- ipw[o]

  # totals
  W1 <- sum(w[y == 1])
  W0 <- sum(w[y == 0])

  # group by unique score values
  split_idx <- split(seq_along(s), s)

  cum_w0 <- 0
  num <- 0

  for (idx in split_idx) {
    w1_t <- sum(w[idx][y[idx] == 1])
    w0_t <- sum(w[idx][y[idx] == 0])

    num <- num + w1_t * cum_w0 + 0.5 * w1_t * w0_t
    cum_w0 <- cum_w0 + w0_t
  }

  auc <- num / (W1 * W0)
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
