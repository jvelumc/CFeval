test_that("Binary outcome/point trt analytically correct", {

  # set-up

  build_data <- function(n) {
    df <- data.frame(id = 1:n)
    df$L <- stats::rbinom(n, 1, 0.5)
    df$A <- stats::rbinom(n, 1, stats::plogis(df$L))
    df$P <- stats::rnorm(n)
    df$Y0 <- stats::rbinom(n, 1, stats::plogis(0.5 + df$L + 1.25 * df$P))
    df$Y1 <- stats::rbinom(n, 1, stats::plogis(0.5 + df$L + 1.25 * df$P - 0.6))
    df$Y <- ifelse(df$A == 1, df$Y1, df$Y0)
    return(df)
  }

  set.seed(1)

  df_toy <- build_data(500)

  # artificially add some ties
  df_toy <- rbind(df_toy, df_toy[1:50,])

  df_toy$ipw <- ip_weights(df_toy, A ~ L)

  is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  # find smallest a such that x * a is a whole number
  smallest_a_st_xa_int <- function(x) {
    smallest_a_st_xa_int2 <- function(x) {

      for (a in 1:10000) {
        if (is_whole(x*a)) {
          return(a)
        }
      }
      stop("No smallest a <= 10000 found")
    }
    sapply(x, smallest_a_st_xa_int2)
  }

  # least common multiplier
  lcm <- function(x) {
    gcd <- function(a, b) {
      a <- abs(a)
      b <- abs(b)
      while (b != 0) {
        temp <- b
        b <- a %% b
        a <- temp
      }
      a
    }

    lcm2 <- function(a, b) {
      if (a == 0 || b == 0) return(0)
      abs(a * b) / gcd(a, b)
    }
    Reduce(lcm2, x)
  }

  # build an 'unweighted' pseudopop, by repeating each row a whole number
  # proportional to its ipw
  df_pseudo_exact <- df_toy[df_toy$A == 0, ]
  multiply_rows <- lcm(smallest_a_st_xa_int(unique(df_pseudo_exact$ipw)))
  # this needs to be rounded due to rounding errors in R.
  # i.e. 7.0000000001 becomes 7
  df_pseudo_exact$rep <- round(df_pseudo_exact$ipw * multiply_rows)
  df_pseudo_exact <- df_pseudo_exact[rep(
    seq_len(nrow(df_pseudo_exact)),
    times = round(df_pseudo_exact$rep)
  ),]


  # fit a CF model on original toy data
  model <- suppressWarnings(
    glm(Y ~ A + P, family = "binomial",
               data = df_toy, weights = ipw)
  )
  # predict CF outcomed under trt 0
  df_toy$pred <- predict_CF(model, df_toy, "A", 0)
  df_pseudo_exact$pred <- predict_CF(model, df_pseudo_exact, "A", 0)

  score <- riskRegression::Score(list(df_pseudo_exact$pred), Y ~ 1,
                                 data = df_pseudo_exact,
                                 null.model = F, se.fit = F)

  expect_equal(
    cf_brier(
      obs_outcome = df_toy$Y,
      obs_trt = df_toy$A,
      cf_pred = df_toy$pred,
      cf_trt = 0,
      ipw = df_toy$ipw
    ),
    score$Brier$score$Brier
  )

  expect_equal(
    cf_auc(
      obs_outcome = df_toy$Y,
      obs_trt = df_toy$A,
      cf_pred = df_toy$pred,
      cf_trt = 0,
      ipw = df_toy$ipw
    ),
    score$AUC$score$AUC
  )

  # expect_equal(
  #   cf_oeratio(
  #     obs_outcome = df_toy$Y,
  #     obs_trt = df_toy$A,
  #     cf_pred = df_toy$pred,
  #     cf_trt = 0,
  #     ipw = df_toy$ipw
  #   ),
  #   mean(df_pseudo_exact$Y)/mean(df_pseudo_exact$pred)
  # )

  expect_equal(
    cf_oeratio_e_from_pp(
      obs_outcome = df_toy$Y,
      obs_trt = df_toy$A,
      cf_pred = df_toy$pred,
      cf_trt = 0,
      ipw = df_toy$ipw
    ),
    mean(df_pseudo_exact$Y)/mean(df_pseudo_exact$pred)
  )

})


