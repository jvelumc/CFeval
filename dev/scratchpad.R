n <- 10000

data <- data.frame(L = rnorm(n))
data$A <- rbinom(n, 1, plogis(data$L))
data$Y0 <- rbinom(n, 1, plogis(0.1 + 0.5*data$L))
data$Y1 <- rbinom(n, 1, plogis(0.1 + 0.5*data$L - 4*data$A))
data$Y <- ifelse(data$A == 1, data$Y1, data$Y0)

naive_perfect <- data$Y
causal_perfect <- data$Y0

CFscore(
  object = list("naive" = naive_perfect, "causal" = causal_perfect),
  data = data,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 0,
  metrics = c("oeratio", "oeratio_pp"),
  null.model = FALSE
)

