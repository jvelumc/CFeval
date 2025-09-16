test_that("correct edge cases", {
  expect_equal(auc_weighted(c(1,1,0, 0), c(1,1,1,1), c(1,1,1,1)), 0.5)
  expect_equal(auc_weighted(c(1,1,0, 0), c(.9,.8,.7,.6), c(1,1,1,1)), 1)
  expect_equal(auc_weighted(c(1,1,0, 0), c(0,0,1,1), c(1,1,1,1)), 0)
})

test_that("1 class auc error", {
  expect_error(auc_weighted(c(1,1,1), c(1,0,1), c(1,1,1)))
  expect_error(auc_weighted(c(0,0,0), c(1,0,1), c(1,1,1)))
  expect_error(auc_weighted(c(1,1,1), c(0,0,0), c(1,1,1)))
  expect_error(auc_weighted(c(1,1,1), c(1,1,1), c(0,0,0)))
  expect_error(auc_weighted(c(0,1,2), c(1,1,1), c(0,0,0)))
})

test_that("correct auc", {
  model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
  pred <- predict(model, type = "response")
  w <- ip_weights(df_dev, A ~ L)

  expect_equal(auc_weighted(df_dev$Y, pred, rep(1, nrow(df_dev))),
               pROC::auc(df_dev$Y, pred, direction = "<",
                         levels = c(0,1))[[1]])
  expect_equal(auc_weighted(df_dev$Y, pred, w),
               WeightedROC::WeightedAUC(
                 WeightedROC::WeightedROC(guess = pred, label = df_dev$Y,
                                          weight = w))
  )
})

test_that("correct causal auc estimation", {
  df_dev$pt <- plogis(df_dev$L)
  df_dev$ipw <- 1 / ifelse(df_dev$A == 1, df_dev$pt, 1-df_dev$pt)
  suppressWarnings(
    causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                      weights = ipw)
  )
  pred_cf0 <- predict_CF(causal_model, df_dev, "A", 0)
  pred_cf1 <- predict_CF(causal_model, df_dev, "A", 1)

  expect_equal(
    auc_weighted(df_dev$Y, pred_cf0, weights = df_dev$ipw),
    auc_weighted(df_dev$Y0, pred_cf0, weights = rep(1, nrow(df_dev))),
    tolerance = 0.01
  )
  expect_equal(
    auc_weighted(df_dev$Y, pred_cf1, weights = df_dev$ipw),
    auc_weighted(df_dev$Y1, pred_cf1, weights = rep(1, nrow(df_dev))),
    tolerance = 0.01
  )
})

# TODO: wrong/weird input checks (e.g. list).
