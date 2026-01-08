library(survival)


build_data <- function(n) {
  df <- data.frame(id = 1:n)
  df$L <- stats::rnorm(n)
  df$A <- stats::rbinom(n, 1, stats::plogis(df$L))
  df$P <- stats::rnorm(n)

  u <- runif(n)
  uc <- runif(n)

  lambda0 <- 0.05
  lambdac <- 0.001

  df$failuretime0 <-  -log(u) / (lambda0 * exp(0.25*df$L + 0.1*df$P))
  df$failuretime1 <-  -log(u) / (lambda0 * exp(0.25*df$L + 0.1*df$P - 0.4))

  # non informative censoring
  df$censortime <- -log(uc)/ (lambdac * exp(0.1))

  df$failuretime <- ifelse(df$A == 1, df$failuretime1, df$failuretime0)

  df$time0 <- pmin(df$censortime, df$failuretime0)
  df$time <- pmin(df$censortime, df$failuretime)

  df$status0 <- df$failuretime0 < df$censortime
  df$status <- df$failuretime < df$censortime
  return(df)
}

df_dev <- build_data(100000)

time_horizon <- 10
df_dev$status_at_horizon <- ifelse(df_dev$time > time_horizon, FALSE, df_dev$status)
df_dev$time_at_horizon <- pmin(df_dev$time, time_horizon)

df_dev$ipw <- ip_weights(df_dev, A ~ L)
df_dev$ipc <- ipc_weights(df_dev, Surv(time, status) ~ 1, type = "KM",
                          time_horizon = time_horizon)

coxph(Surv(time_at_horizon, status_at_horizon) ~ P + A, data = df_dev)
coxph(Surv(time_at_horizon, status_at_horizon) ~ P + A,
      data = df_dev[df_dev$ipc != 0,], weights = ipc)

model <- coxph(Surv(time, status) ~ P + A, data = df_dev, weights = ipw)


myPredictCox <- function(model, time_of_interest, newdata, A_column, trt_of_interest) {
  newdata$time <- time_of_interest
  newdata[[A_column]] <- trt_of_interest
  1 - predict(model, newdata = newdata, type = "survival")
}

mpc <- function(model, time, trt) {
  myPredictCox(model, time, df_dev, "A", trt)
}

df_dev$pred0 <- mpc(model, time_horizon, 0)

convert_surv_to_binary <- function(survtime, status, horizon) {
  ifelse(survtime <= horizon, status, FALSE)
}

summary(convert_surv_to_binary(df_dev$time, df_dev$status, time_horizon))

cf_brier(
  status_at_horizon,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw*df_dev$ipc
)

cf_oeratio(
  status_at_horizon,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw*df_dev$ipc
)

cf_auc(
  status_at_horizon,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw*df_dev$ipc
)

score <- riskRegression::Score(list(df_dev$pred0), Hist(failuretime0, status0) ~ 1,
                      data = df_dev, times = 10, null.model = F)
score$AUC$score$AUC
score$Brier$score$Brier
mean(df_dev$failuretime0 <= 10) / mean(df_dev$pred0)

# performance after time X:
