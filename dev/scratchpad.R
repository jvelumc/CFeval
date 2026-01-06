library(survival)

build_data <- function(n) {
  df <- data.frame(id = 1:n)
  df$L <- stats::rnorm(n)
  df$A <- stats::rbinom(n, 1, stats::plogis(df$L))
  df$P <- stats::rnorm(n)

  u <- runif(n)

  lambda0 <- 0.05

  df$time0 <-  -log(u) / (lambda0 * exp(0.25*df$L + 0.1*df$P))
  df$time1 <-  -log(u) / (lambda0 * exp(0.25*df$L + 0.1*df$P - 0.4))

  df$time <- ifelse(df$A == 1, df$time1, df$time0)
  df$status <- 1
  return(df)
}

df_dev <- build_data(1000000)

summary(df_dev$time0)
summary(df_dev$time1)


df_dev$ipw <- ip_weights(df_dev, A ~ L)

#coxph(Surv(time, status) ~ P + A, data = df_dev)
model <- coxph(Surv(time, status) ~ P + A, data = df_dev, weights = ipw)


myPredictCox <- function(model, time_of_interest, newdata, A_column, trt_of_interest) {
  newdata$time <- time_of_interest
  newdata[[A_column]] <- trt_of_interest
  1 - predict(model, newdata = newdata, type = "survival")
}

mpc <- function(model, time, trt) {
  myPredictCox(model, time, df_dev, "A", trt)
}

df_dev$pred0 <- mpc(model, 10, 0)


summary(df_dev$pred0)

outcome_at_time <- df_dev$time < 10 # and not censored

convert_time_to_binary <- function(survtime, status, horizon) {

}


cf_brier(
  outcome_at_time,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw
)

cf_oeratio(
  outcome_at_time,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw
)

cf_auc(
  outcome_at_time,
  df_dev$A,
  df_dev$pred0,
  0,
  df_dev$ipw
)

score <- riskRegression::Score(list(df_dev$pred0), Hist(time0, status) ~ 1,
                      data = df_dev, times = 10, null.model = F)
score$AUC$score$AUC
score$Brier$score$Brier

mean(df_dev$time0 < 10) / mean(df_dev$pred0)

# performance after time X:
