test_that("IPW function works under binary point trt/binary outcome", {
  expect_equal(
    ipt_weights(df_dev, A ~ L)$weights,
    ipw::ipwpoint(A, family = "binomial", link = "logit",
                  denominator =  ~ L, data = df_dev)$ipw.weights
  )
})

