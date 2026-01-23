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

pred <- predict_CF(model2, data, "A", 1)


cfscore <- CFscore(
  object = pred,
  data = data,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 1
)




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


