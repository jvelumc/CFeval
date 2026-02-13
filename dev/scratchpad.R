n <- 1000

data <- data.frame(L = rnorm(n), P = rnorm(n))
data$A <- rbinom(n, 1, plogis(data$L))
data$Y <- rbinom(n, 1, plogis(0.1 + 0.5*data$L + 0.7*data$P - 2*data$A))

random <- runif(n, 0, 1)
model <- glm(Y ~ A + P, data = data, family = "binomial")
naive_perfect <- data$Y

CFscore(
  object = list("ran" = random, "mod" = model, "per" = naive_perfect),
  data = data,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 0,
)

