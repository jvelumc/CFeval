set.seed(1)
horizon <- 24.999
adminstrative_censor <- 25
n <- 100000

data <- data.frame(
  L = rnorm(n, mean = 0),
  P = rnorm(n, mean = 0)
)

data$time <- simulate_time_to_event(n, 0.07, data$L + 0.5*data$P)
data$censortime <- simulate_time_to_event(n, 0.06, 0.5*data$L + 0.6*data$P)

summary(data$time)
summary(data$censortime)

data$status <- ifelse(
  data$time <= data$censortime,
  TRUE,
  FALSE
)
data$time <- pmin(data$time, data$censortime)

ipc_weights(data, Surv(time, status) ~ L + P,
            type = "cox", time_horizon = horizon)$model

# now artificially censor everybody at T = 25:
data$status <- ifelse(data$time > adminstrative_censor, F, data$status)
data$time <- pmin(data$time, adminstrative_censor)

ipc_weights(data, Surv(time, status) ~ L + P,
            type = "cox", time_horizon = horizon)$model





data$time_uncensored <- ifelse(data$A == 1, data$time1, data$time0)
data$status_uncensored <- 1



data$time <- ifelse(
  data$status == TRUE,
  data$time_uncensored,
  pmin(data$censortime, adminstrative_censor)
)

summary(data$status)
summary(data$time)
