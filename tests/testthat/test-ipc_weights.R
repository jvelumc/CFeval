test_that("ipc simple correct", {
  data <- data.frame(
    time = c(1,2,3),
    status = c(1,0,1)
  )

  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 0.5),
    c(1,1,1)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 1.5),
    c(1,1,1)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 2.5),
    c(1,0,2)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 3.5),
    c(1,0,2)
  )

  data <- data.frame(
    time = c(1,2,3),
    status = c(0,1,0)
  )

  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 0.5),
    c(1,1,1)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 1.5),
    c(0,1.5,1.5)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 2.5),
    c(0,1.5,1.5)
  )
  expect_equal(
    ipc_weights(data, Surv(time, status) ~ 1, type = "KM", time_horizon = 3.5),
    c(0,1.5,0)
  )
})

test_that("ipc-weighted population represents uncensored population,
          noninformative censoring" {
  build_data <- function(n) {
    df <- data.frame(id = 1:n)
    df$L1 <- stats::rnorm(n)
    df$L2 <- stats::rbinom(n, 1, 0.5)
    df$P1 <- stats::rnorm(n)
    df$P2 <- stats::rbinom(n, 1, 0.5)

    u <- runif(n)
    uc <- runif(n)

    lambda0 <- 0.04
    lambdac <- 0.03

    df$failuretime <-  -log(u) / (lambda0 * exp(
      0.5*df$L1 + 0.3*df$L2 + 0.2*df$P1 + 0.4*df$P2))

    # non informative censoring
    df$censortime <- -log(uc)/ (lambdac * exp(0.1))

    df$time <- pmin(df$censortime, df$failuretime)

    df$status <- df$failuretime < df$censortime

    horizon <- 20

    df$time_at_horizon <- pmin(df$time, horizon)
    df$status_at_horizon <- ifelse(df$time < horizon, df$status, F)
    return(df)
  }
  data <- build_data(10000)
  summary(data$failuretime)
  summary(data$censortime)

  ipc_weights(data, Surv(time, status) ~ 1, "KM", 20)

  coxph(Surv(time_at_horizon, status_at_horizon) ~ L1 + L2 + P1 + P2,
        data = data, )

  coxph(Surv(time, status) ~ L1 + L2 + P1 + P2,
        data = data)


  coxph(Surv(failuretime, rep(1, nrow(data))) ~ L1 + L2 + P1 + P2,
        data = data)


})

