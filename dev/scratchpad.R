horizon <- 10
build_data <- function(n) {
  df <- data.frame(id = 1:n)
  df$L1 <- stats::rnorm(n)
  df$L2 <- stats::rbinom(n, 1, 0.5)
  df$P1 <- stats::rnorm(n)
  df$P2 <- stats::rbinom(n, 1, 0.5)
  df$A <- stats::rbinom(n, 1, plogis(df$L1 - 0.5*df$L2))

  u <- runif(n)
  uc <- runif(n)

  lambda0 <- 0.04
  lambdac <- 0.03

  df$failuretime <-  -log(u) / (lambda0 * exp(
    0.5*df$L1 + 0.3*df$L2 + 0.2*df$P1 + 0.4*df$P2 - 0.3*df$A))

  # non informative censoring
  df$censortime <- -log(uc)/ (lambdac * exp(0.1))

  df$time <- pmin(df$censortime, df$failuretime)

  df$status <- df$failuretime < df$censortime


  df$time_at_horizon <- pmin(df$time, horizon)
  df$status_at_horizon <- ifelse(df$time < horizon, df$status, F)

  df$status_at_horizon_uncensored <- df$failuretime < horizon
  return(df)
}
df_dev <- build_data(10000)
model <- coxph(Surv(time, status) ~ L1 + L2 + P1 + P2,data = df_dev)

cfscore <- CFscore(df_dev, model, outcome_formula = Surv(time, status) ~ 1,
        treatment_formula = A ~ L1 + L2, treatment_of_interest = 1,
        cens.model = "KM", time_horizon = 20)

cfscore$score
