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
  cfscore$score

  Y0_predicted <- predict_CF(model, data, "A", 0)
  score <- riskRegression::Score(
    list(Y0_predicted),
    formula = Y0 ~ 1,
    data = data,
    null.model = F
  )
  score$oe <- mean(Y0_predicted)/mean(data$Y0)

  expect_equal(cfscore$score$auc, score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(cfscore$score$brier, score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(cfscore$score$oeratio, score$oe, tolerance = 0.01)
})


test_that("CFscore metrics equal to unobserved CF metrics, surv, uncensored", {
  set.seed(1)
})
