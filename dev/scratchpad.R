set.seed(1)
n <- 10000

data <- data.frame(
  L = rnorm(n, mean = 0),
  P = rnorm(n, mean = 0)
)
data$A <- rbinom(n, 1, plogis(0.5+0.2*data$L))
data$Y <- rbinom(n, 1, plogis(0.3*data$L + 0.6*data$P - 0.5*data$A))
data$status <- data$Y
data$time <- data$P


model1 <- glm(Y ~ A + L, family = "binomial", data = data)
model2 <- glm(Y ~ A + P, family = "binomial", data = data)
model3 <- coxph(Surv(time, status) ~ L, data = data)

cfscore <- CFscore(
  object = list(model1, model1, model2),
  data = data,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 1,
  # bootstrap = 20
)

CFscore(
  model3,
  data = data,
  outcome_formula = Surv(time,status) ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 1,
  time_horizon = 2
)

print(cfscore)

cfscore$bootstrap


lapply(cfscore$bootstrap_results, function(m))


# bootstrap iteration > metric > models
# models > metrics > bootstrap iterations

lapply(CFscore$predictions, function(x) print(x))


object1 <- pred
object2 <- model1
object3 <- list(pred)
object4 <- list(model1)
object5 <- list(model1, pred)


class(pred)
class(model1)

x <- object1
if ("glm" %in% class(object1)) {

}

if (is.numeric(x) && is.null(dim(x))) {
  # predictions
}








cfscore


score <- riskRegression::Score.list(list(model1, model2), formula = Y ~ 1, data = data)

score$AUC$score$AUC


