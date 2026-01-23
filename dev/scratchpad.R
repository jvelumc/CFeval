set.seed(1)
n <- 10000

data <- data.frame(
  L = rnorm(n, mean = 0),
  P = rnorm(n, mean = 0)
)
data$A <- rbinom(n, 1, plogis(0.5+0.2*data$L))
data$Y <- rbinom(n, 1, plogis(0.3*data$L + 0.6*data$P - 0.5*data$A))

model1 <- glm(Y ~ A, family = "binomial", data = data)
model2 <- glm(Y ~ A + L, family = "binomial", data = data)
model3 <- coxph(Surv(L, Y) ~ P, data = data)

cfscore <- CFscore(
  data = data,
  model = model1,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 0
)

cfscore


score <- riskRegression::Score.list(list(model1, model2), formula = Y ~ 1, data = data)

score$AUC$score$AUC
