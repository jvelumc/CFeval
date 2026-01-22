test_that("CFscore metrics equal to unobserved CF metrics, binary outcome", {

  set.seed(1)
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2+0.5*data$L))
  data$Y0 <- rbinom(n, 1, plogis(0.1 + 0.3*data$L + 0.4*data$P))
  data$Y1 <- rbinom(n, 1, plogis(0.1 + 0.3*data$L + 0.4*data$P - 0.4))
  data$Y <- ifelse(data$A == 1, data$Y1, data$Y0)

  model <- suppressWarnings(
    glm(
      Y ~ A + P,
      family = "binomial",
      data = data,
      weights = ipt_weights(data, A ~ L)$weights
    )
  )

  cfscore <- CFscore(
    data = data,
    model = model,
    outcome_formula = Y ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0
  )

  Y0_predicted <- predict_CF(model, data, "A", 0)
  score <- riskRegression::Score(
    list(Y0_predicted),
    formula = Y0 ~ 1,
    data = data,
    null.model = F
  )
  score$oe <- mean(data$Y0)/mean(Y0_predicted)

  expect_equal(cfscore$score$auc, score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(cfscore$score$brier, score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(cfscore$score$oeratio, score$oe, tolerance = 0.01)
})

test_that("CFscore metrics equal to unobserved CF metrics, surv, uncensored", {
  set.seed(1)
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2, 0.5*data$L))
  data$status <- 1 # no censoring, so status is always 1 at the end

  u <- runif(n)
  lambda0 <- 0.04
  data$time0 <- -log(u) / (lambda0 * exp(data$L + 0.5*data$P))
  data$time1 <- -log(u) / (lambda0 * exp(data$L + 0.5*data$P - 0.6))

  data$time <- ifelse(data$A == 1, data$time1, data$time0)

  summary(data$time0)
  summary(data$time1)

  model <- coxph(
    formula = Surv(time, status) ~ P + A,
    data = data,
    weights = ipt_weights(data, A ~ L)$weights
  )

  cfscore <- CFscore(
    data = data,
    model = model,
    outcome_formula = Surv(time, status) ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    time_horizon = 10
  )

  time0_predicted <- predict_CF(model, data, "A", 0, 10)
  score <- riskRegression::Score(
    list(time0_predicted),
    formula = Hist(time0, status) ~ 1,
    data = data,
    null.model = F,
    times = 10
  )
  score$oe <- mean(data$time0 <= 10)/mean(time0_predicted)

  expect_equal(cfscore$score$auc, score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(cfscore$score$brier, score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(cfscore$score$oeratio, score$oe, tolerance = 0.01)
})
