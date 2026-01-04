
build_data <- function(n, shift) {
  df <- data.frame(id = 1:n)
  df$L1 <- stats::rnorm(n)
  df$L2 <- stats::rbinom(n, 1, 0.5) - 0.5
  df$A <- stats::rbinom(n, 1, stats::plogis(shift + df$L1 + df$L2))
  df$P1 <- stats::rnorm(n)
  df$P2 <- stats::rbinom(n, 1, 0.5) - 0.5
  df$Y0 <- stats::rbinom(n, 1, stats::plogis(
    0.5 + df$L1 + df$L2 + 1.25 * df$P1 + df$P2
  ))
  df$Y1 <- stats::rbinom(n, 1, stats::plogis(
    0.5 + df$L1 + df$L2 + 1.25 * df$P1 + df$P2 - 0.6
  ))
  df$Y <- ifelse(df$A == 1, df$Y1, df$Y0)
  return(df)
}

set.seed(1)
df_dev <- build_data(200, shift = 1)
# causal model
df_dev$ipw <- ip_weights(df_dev, A ~ L1 + L2)

model <- suppressWarnings(
  glm(Y ~ A + P1 + P2, family = "binomial",
      data = df_dev, weights = ipw)
)



estimate <- function(seed, n) {
  set.seed(seed)
  print(seed)
  shift <- -1
  df_val <- build_data(n, shift = shift)
  # we know the ipw formula, so might as well fill that in
  df_val$ipw <- 1/ifelse(
    df_val$A == 0,
    1 - stats::plogis(shift + df_val$L1 + df_val$L2),
    stats::plogis(shift + df_val$L1 + df_val$L2)
  )

  # truth vs cf estimation
  truth <- list(
    auc = cf_auc(
      obs_outcome = df_val$Y0,
      obs_trt = rep(0, nrow(df_val)),
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = rep(1, nrow(df_val))
    ),
    brier = cf_brier(
      obs_outcome = df_val$Y0,
      obs_trt = rep(0, nrow(df_val)),
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = rep(1, nrow(df_val))
    ),
    oe = cf_oeratio(
      obs_outcome = df_val$Y0,
      obs_trt = rep(0, nrow(df_val)),
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = rep(1, nrow(df_val))
    ),
    oe_e_pp = cf_oeratio_e_from_pp(
      obs_outcome = df_val$Y0,
      obs_trt = rep(0, nrow(df_val)),
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = rep(1, nrow(df_val))
    ))

  cf_est <- list(
    auc = cf_auc(
      obs_outcome = df_val$Y,
      obs_trt = df_val$A,
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = df_val$ipw
    ),
    brier = cf_brier(
      obs_outcome = df_val$Y,
      obs_trt = df_val$A,
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = df_val$ipw
    ),
    oe = cf_oeratio(
      obs_outcome = df_val$Y,
      obs_trt = df_val$A,
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = df_val$ipw
    ),
    oe_e_pp = cf_oeratio_e_from_pp(
      obs_outcome = df_val$Y,
      obs_trt = df_val$A,
      cf_pred = predict_CF(model, df_val, "A", 0),
      cf_trt = 0,
      ipw = df_val$ipw
    ))

  r <- data.frame(
    truth = truth,
    cf_est = cf_est
  )
  r$delta_auc <- r$truth.auc - r$cf_est.auc
  r$delta_brier <- r$truth.brier - r$cf_est.brier
  r$delta_oe <- r$truth.oe - r$cf_est.oe
  r$delta_oe_e_pp <- r$truth.oe_e_pp - r$cf_est.oe_e_pp
  r
}

results <- lapply(1:100,
                  function(i) estimate(i, 5000000))
results <- do.call(rbind, results)


summary(results)
summary(results$delta_auc)
summary(results$delta_brier)
summary(results$delta_oe)
summary(results$delta_oe_e_pp)


#
