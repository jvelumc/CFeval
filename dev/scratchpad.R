simulate_data <- function(n) {
  data <- data.frame(id = 1:n)
  data$L <- rnorm(n)
  data$A <- rbinom(n, 1, plogis(2*data$L))
  data$P1 <- rnorm(n)
  data$P2 <- rbinom(n,1, plogis(data$L))
  data$Y0 <- rbinom(n, 1, plogis(0.5 + 2*data$L + 1.25 * data$P1 + 0.8 * data$P2))
  data$Y1 <- rbinom(n, 1, plogis(0.5 + 2*data$L + 1.25 * data$P1 + 0.8 * data$P2 - 0.6))
  data$Y <- ifelse(data$A == 0, data$Y0, data$Y1)
  data
}

df_dev <- simulate_data(10000)

propensity_model <- glm(A ~ L, family = "binomial", data = df_dev)
propensity_score <- predict(propensity_model, type = "response")
iptw <- 1 / ifelse(df_dev$A == 1, propensity_score, 1 - propensity_score)

causal_model <- glm(Y ~ P1 + P2 + A, family = "binomial", data = df_dev, weights = iptw)
naive_model <- glm(Y ~ P1 + P2 + A, family = "binomial", data = df_dev)

naive_pred <- predict_CF(naive_model, df_dev, "A", 1)
causal_pred <- predict_CF(causal_model, df_dev, "A", 1)

naive_model$coefficients
coefficients(causal_model)

riskRegression::Score(list(naive_pred, causal_pred), Y1 ~ 1, df_dev)


