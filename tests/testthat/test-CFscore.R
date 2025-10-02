test_that("n models assessed correctly", {
  mod <- glm(Y ~ A + P, family = "binomial", data = df_dev)
  pred <- predict(mod, type = "response")

  expect_equal(CFscore(
    df_val,
    model = mod,
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 1)

  expect_equal(CFscore(df_val,
    model = list(mod),
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 1)

  expect_equal(CFscore(df_val,
    model = list(mod, mod),
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 2)

  expect_error(CFscore(df_val,
    model = list(),
    outcome_column = "Y",
    treatment_column = "A"
  ), "Empty list of models")

  expect_equal(CFscore(df_val,
    predictions = pred,
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 1)

  expect_equal(CFscore(df_val,
    predictions = list(pred),
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 1)

  expect_equal(CFscore(df_val,
    predictions = list(pred, pred),
    outcome_column = "Y",
    treatment_column = "A"
  )$n_models, 2)

  expect_error(CFscore(df_val,
    predictions = list(),
    outcome_column = "Y",
    treatment_column = "A"
  ), "Empty prediction list")

  expect_error(CFscore(df_val,
    outcome_column = "Y",
    treatment_column = "A"
  ), "Either model or predictions")

  expect_error(
    CFscore(df_val,
      model = mod, predictions = pred,
      outcome_column = "Y",
      treatment_column = "A"
    ),
    "Either model or predictions"
  )
})

test_that("treatment/propensity/outcome input checks", {
  mod <- glm(Y ~ A + P, family = "binomial", data = df_dev)
  pred <- predict(mod, type = "response")

  expect_error(
    CFscore(df_val,
            model = mod,
            outcome_column = "Y",
            treatment_column = "Q"
    ), "treatment_column is not"
  )

  expect_error(
    CFscore(df_val,
            model = mod,
            outcome_column = "Q",
            treatment_column = "A"
    ), "outcome_column is not"
  )

  expect_error(
    CFscore(df_val,
            model = mod,
            outcome_column = c(1,2,3),
            treatment_column = "A"
    ), "Length of outcome_column"
  )

  expect_no_error(
    CFscore(df_val,
            model = mod,
            outcome_column = df_val$Y,
            treatment_column = "A"
    )
  )


})
