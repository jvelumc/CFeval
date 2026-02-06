set.seed(1)
n <- 5000
data <- data.frame(
  L1 = rnorm(n, mean = 0),
  L2 = rbinom(n, 1, 0.5),
  P1 = rnorm(n, mean = 0),
  P2 = rnorm(n, mean = 0)
)
data$A <- rbinom(n, 1, plogis(0.9*data$L1 + 0.4*data$L2))
data$Y0 <- rbinom(n, 1, plogis(0.1 + 0.9*data$L1 + 0.6*data$L2 + 0.4*data$P1 + 0.3*data$P2))
data$Y1 <- rbinom(n, 1, plogis(0.1 + 0.9*data$L2 + 0.6*data$L2 + 0.4*data$P1 + 0.3*data$P2 - 0.8))
data$Y <- ifelse(data$A == 1, data$Y1, data$Y0)

summary(data$A)

model0 <- suppressWarnings(
  glm(
    Y ~ A + P1,
    family = "binomial",
    data = data
  )
)

model1 <- suppressWarnings(
  glm(
    Y ~ A + P1 + P2,
    family = "binomial",
    data = data
  )
)
model2 <- suppressWarnings(
  glm(
    Y ~ A + P1 + P2,
    family = "binomial",
    data = data,
    weights = ipt_weights(data, A ~ L1 + L2)$weights
  )
)

print_model(model2)

cfscore <- CFscore(
  data = data,
  object = list("lol" = model0, "hi" = model1, "hello" = model2),
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L1 + L2,
  treatment_of_interest = 0,
  bootstrap = 100
)


cfscore
cfscore$ipt$weights |> summary()

cfscore_s <- CFscore(
  data = data,
  object = list(model0, model1, model2),
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L1 + L2,
  treatment_of_interest = 0,
  bootstrap = 200,
  stable_iptw = TRUE
)

cfscore_s$ipt$weights |> summary()
cfscore$score$auc
cfscore_s

plot(x = cfscore$bootstrap[[1]]$calplot[[1]],
     y = cfscore$bootstrap[[1]]$calplot[[2]],
     type = "l", xlab = "observed", ylab = "predicted",xlim = c(0,1), ylim = c(0,1))

for (i in 2:length(cfscore$bootstrap)) {
  lines(
    x = cfscore$bootstrap[[i]]$calplot[[1]],
    y = cfscore$bootstrap[[i]]$calplot[[2]],
    type = "l"
  )
}

graphics::abline(0, 1, col = "red")


lines(
  x = cfscore$score$calplot[[1]],
  y = cfscore$score$calplot[[2]],
  type = "o",
  col = "blue",
  lwd = 2
)

