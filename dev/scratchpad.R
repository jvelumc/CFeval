set.seed(1)
n <- 5000
data <- data.frame(
  L = rnorm(n, mean = 0),
  P = rnorm(n, mean = 0)
)
data$A <- rbinom(n, 1, plogis(0.2+0.5*data$L))
data$Y0 <- rbinom(n, 1, plogis(0.1 + 0.3*data$L + 0.4*data$P))
data$Y1 <- rbinom(n, 1, plogis(0.1 + 0.3*data$L + 0.4*data$P - 0.4))
data$Y <- ifelse(data$A == 1, data$Y1, data$Y0)

model <- suppressWarnings(
  glm(
    Y ~ A + P,
    family = "binomial",
    data = data
  )
)
model2 <- suppressWarnings(
  glm(
    Y ~ A + P + L,
    family = "binomial",
    data = data
  )
)

print_model(model)

cfscore <- CFscore(
  data = data,
  object = list(model, model2),
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 0,
  bootstrap = 10,
  metrics = c("oeratio", "calplot","auc")
)


cfscore |> plot()





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

