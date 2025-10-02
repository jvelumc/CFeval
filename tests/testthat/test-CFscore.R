test_that("n models assessed correctly", {
  mod <- glm(Y ~ A + P, family = "binomial", data = df_dev)
  expect_equal(CFscore(model = mod)$n_models, 1)
  expect_equal(CFscore(model = list(mod))$n_models, 1)
  expect_equal(CFscore(model = list(mod, mod))$n_models, 2)
  expect_equal(CFscore(model = list())$n_models, 0)
})

