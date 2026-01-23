# input checks

test_that("supplying (list of) model or predictions equivalent", {
  set.seed(1)
  n <- 1000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.5+0.2*data$L))
  data$Y <- rbinom(n, 1, plogis(0.3*data$L + 0.6*data$P - 0.5*data$A))

  model1 <- glm(Y ~ P, family = "binomial", data = data)
  model2 <- glm(Y ~ A + P, family = "binomial", data = data)

  expect_equal(
    CFscore(
      data = data,
      object = model1,
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    ),
    CFscore(
      data = data,
      object = list(model1),
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    )
  )

  expect_equal(
    CFscore(
      data = data,
      object = list(model2),
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    ),
    CFscore(
      data = data,
      object = predict_CF(model2, data, "A", 0),
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    )
  )

  expect_equal(
    CFscore(
      data = data,
      object = list(aa = model2),
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    ),
    CFscore(
      data = data,
      object = list(aa = predict_CF(model2, data, "A", 0)),
      outcome_formula = Y ~ 1,
      treatment_formula = A ~ L,
      treatment_of_interest = 0
    )
  )
})


# TODO manual iptw/ipcw equivalent to equivalent models



# metrics

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
    object = model,
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

  expect_equal(unname(cfscore$score$auc), score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore$score$brier), score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore$score$oeratio), score$oe, tolerance = 0.01)
})

test_that("CFscore metrics equal to unobserved CF metrics, surv, uncensored", {
  set.seed(1)
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2 + 0.5*data$L))
  data$status <- 1 # no censoring, so status is always 1 at the end

  data$time0 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P)
  data$time1 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P - 0.6)
  data$time <- ifelse(data$A == 1, data$time1, data$time0)

  summary(data$time0)
  summary(data$time1)

  model <- coxph(
    formula = Surv(time, status) ~ P + A,
    data = data,
    weights = ipt_weights(data, A ~ L)$weights
  )

  horizon <- 10

  cfscore <- CFscore(
    data = data,
    object = model,
    outcome_formula = Surv(time, status) ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    time_horizon = horizon
  )

  time0_predicted <- predict_CF(model, data, "A", 0, horizon)
  score <- riskRegression::Score(
    list(time0_predicted),
    formula = Hist(time0, status) ~ 1,
    data = data,
    null.model = F,
    times = horizon
  )
  score$oe <- mean(data$time0 <= horizon)/mean(time0_predicted)

  expect_equal(unname(cfscore$score$auc), score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore$score$brier), score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore$score$oeratio), score$oe, tolerance = 0.01)
})

test_that("CFscore metrics equal to unobserved CF metrics, surv, censor at T", {
  set.seed(1)
  horizon <- 9.999
  adminstrative_censor <- 10
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2 + 0.5*data$L))

  data$time0 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P)
  data$time1 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P - 0.6)
  data$time_uncensored <- ifelse(data$A == 1, data$time1, data$time0)

  summary(data$time0)
  summary(data$time1)

  data$status <- ifelse(data$time_uncensored <= adminstrative_censor, TRUE, FALSE)
  data$time <- ifelse(data$status == TRUE, data$time_uncensored, adminstrative_censor)

  data$status_uncensored <- 1

  model <- coxph(
    formula = Surv(time, status) ~ P + A,
    data = data
  ) # naive model, i.e. not adjusting for confounding

  cfscore <- CFscore(
    data = data,
    object = model,
    outcome_formula = Surv(time, status) ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    time_horizon = horizon
  )

  expect_equal(
    cfscore$ipc$weights,
    rep(1, n)
  )

  time0_predicted <- predict_CF(model, data, "A", 0, horizon)
  score <- riskRegression::Score(
    list(time0_predicted),
    formula = Hist(time0, status_uncensored) ~ 1,
    data = data,
    null.model = F,
    times = horizon
  )
  score$oe <- mean(data$time0 <= horizon)/mean(time0_predicted)

  expect_equal(unname(cfscore$score$auc), score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore$score$brier), score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore$score$oeratio), score$oe, tolerance = 0.01)
})

test_that("CFscore metrics equal to unobserved CF metrics, surv, KM censor", {
  set.seed(1)
  horizon <- 10
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2 + 0.5*data$L))

  data$time0 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P)
  data$time1 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P - 0.6)
  data$censortime <- simulate_time_to_event(n, 0.04, 0)
  data$time_uncensored <- ifelse(data$A == 1, data$time1, data$time0)
  data$status_uncensored <- 1

  summary(data$time0)
  summary(data$time1)
  summary(data$censortime)

  data$status <- ifelse(data$time_uncensored <= data$censortime, TRUE, FALSE)
  data$time <- ifelse(data$status == TRUE,
                      data$time_uncensored,
                      data$censortime)

  model <- coxph(
    formula = Surv(time, status) ~ P + A,
    data = data
  )

  cfscore <- CFscore(
    data = data,
    object = model,
    outcome_formula = Surv(time, status) ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    time_horizon = horizon,
    cens.model = "KM"
  )

  time0_predicted <- predict_CF(model, data, "A", 0, horizon)
  score <- riskRegression::Score(
    list(time0_predicted),
    formula = Hist(time0, status_uncensored) ~ 1,
    data = data,
    null.model = F,
    times = horizon
  )
  score$oe <- mean(data$time0 <= horizon)/mean(time0_predicted)

  expect_equal(unname(cfscore$score$auc), score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore$score$brier), score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore$score$oeratio), score$oe, tolerance = 0.01)

  # also try treatment == 1 for good measure
  cfscore1 <- CFscore(
    data = data,
    object = model,
    outcome_formula = Surv(time, status) ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 1,
    time_horizon = horizon,
    cens.model = "KM"
  )

  time1_predicted <- predict_CF(model, data, "A", 1, horizon)
  score1 <- riskRegression::Score(
    list(time1_predicted),
    formula = Hist(time1, status_uncensored) ~ 1,
    data = data,
    null.model = F,
    times = horizon
  )
  score1$oe <- mean(data$time1 <= horizon)/mean(time1_predicted)

  expect_equal(unname(cfscore1$score$auc), score1$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore1$score$brier), score1$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore1$score$oeratio), score1$oe, tolerance = 0.01)
})

test_that("CFscore metrics equal to unobserved CF metrics, surv, cox censor", {
  set.seed(1)
  horizon <- 10
  n <- 100000
  data <- data.frame(
    L = rnorm(n, mean = 0),
    P = rnorm(n, mean = 0)
  )
  data$A <- rbinom(n, 1, plogis(0.2 + 0.5*data$L))

  data$time0 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P)
  data$time1 <- simulate_time_to_event(n, 0.04, data$L + 0.5*data$P - 0.6)
  data$censortime <- simulate_time_to_event(n, 0.04, 0.5*data$L + 0.6*data$P +
                                              0.2*data$A)
  data$time_uncensored <- ifelse(data$A == 1, data$time1, data$time0)
  data$status_uncensored <- 1

  summary(data$time0)
  summary(data$time1)
  summary(data$censortime)

  data$status <- ifelse(data$time_uncensored <= data$censortime, TRUE, FALSE)
  data$time <- ifelse(data$status == TRUE,
                      data$time_uncensored,
                      data$censortime)

  model <- coxph(
    formula = Surv(time, status) ~ P + A,
    data = data
  )

  cfscore <- CFscore(
    data = data,
    object = model,
    outcome_formula = Surv(time, status) ~ L + P + A,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    time_horizon = horizon,
    cens.model = "cox"
  )

  time0_predicted <- predict_CF(model, data, "A", 0, horizon)
  score <- riskRegression::Score(
    list(time0_predicted),
    formula = Hist(time0, status_uncensored) ~ 1,
    data = data,
    null.model = F,
    times = horizon
  )
  score$oe <- mean(data$time0 <= horizon)/mean(time0_predicted)

  expect_equal(unname(cfscore$score$auc), score$AUC$score$AUC, tolerance = 0.01)
  expect_equal(unname(cfscore$score$brier), score$Brier$score$Brier, tolerance = 0.01)
  expect_equal(unname(cfscore$score$oeratio), score$oe, tolerance = 0.01)
})

# minor bootstrap test
test_that("results are in between lower & upper bootstrap", {
  set.seed(1)
  n <- 1000
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
      data = data
    )
  )

  cfscore <- CFscore(
    data = data,
    object = model,
    outcome_formula = Y ~ 1,
    treatment_formula = A ~ L,
    treatment_of_interest = 0,
    bootstrap = 200
  )

  expect_true(
    cfscore$score$auc > cfscore$bootstrap$results$auc[[1]][1] &
      cfscore$score$auc < cfscore$bootstrap$results$auc[[1]][2]
  )
  expect_true(
    cfscore$score$brier > cfscore$bootstrap$results$brier[[1]][1] &
      cfscore$score$brier < cfscore$bootstrap$results$brier[[1]][2]
  )
  expect_true(
    cfscore$score$oeratio > cfscore$bootstrap$results$oeratio[[1]][1] &
      cfscore$score$oeratio < cfscore$bootstrap$results$oeratio[[1]][2]
  )
})

