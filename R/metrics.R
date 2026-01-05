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

cf_auc0  <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

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

cf_auc2 <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

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

  # group indices of ties
  g <- rle(s)$lengths
  end <- cumsum(g)
  start <- c(1, head(end + 1, -1))

  w1 <- w0 <- numeric(length(g))

  for (i in seq_along(g)) {
    idx <- start[i]:end[i]
    w1[i] <- sum(w[idx][y[idx] == 1])
    w0[i] <- sum(w[idx][y[idx] == 0])
  }

  cum_w0 <- cumsum(w0) - w0

  W1 <- sum(w1)
  W0 <- sum(w0)

  auc <- sum(w1 * cum_w0 + 0.5 * w1 * w0) / (W1 * W0)
  auc
}

cf_auc3 <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

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
  y <- obs_outcome[o]      # 0/1
  w <- ipw[o]

  # collapse by unique score using run-length encoding
  r <- rle(s)
  len <- r$lengths
  end <- cumsum(len)
  start <- end - len + 1

  # aggregate weights per tie block
  w1 <- w0 <- numeric(length(len))

  for (i in seq_along(len)) {
    idx <- start[i]:end[i]
    wi <- w[idx]
    yi <- y[idx]
    w1[i] <- sum(wi * yi)
    w0[i] <- sum(wi * (1 - yi))
  }

  # cumulative negatives strictly below each score
  cum_w0 <- cumsum(w0) - w0

  W1 <- sum(w1)
  W0 <- sum(w0)

  if (W1 == 0 || W0 == 0) return(NA_real_)

  # AUC numerator with proper tie handling
  num <- sum(w1 * cum_w0 + 0.5 * w1 * w0)

  num / (W1 * W0)
}

cf_auc4 <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw) {

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
  y <- obs_outcome[o]      # 0/1
  w <- ipw[o]

  # numeric group id per unique score
  g <- cumsum(c(TRUE, diff(s) != 0))

  # aggregate in C
  w1 <- rowsum(w * y, g, reorder = FALSE)[, 1]
  w0 <- rowsum(w * (1 - y), g, reorder = FALSE)[, 1]

  cum_w0 <- cumsum(w0) - w0

  W1 <- sum(w1)
  W0 <- sum(w0)

  if (W1 == 0 || W0 == 0) return(NA_real_)

  sum(w1 * cum_w0 + 0.5 * w1 * w0) / (W1 * W0)
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
