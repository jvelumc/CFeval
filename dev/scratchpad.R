df_dev

model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
             weights = ip_weights(df_dev, A ~ L))


model

riskRegression::Score()

CFscore <- function(predictions, outcome_formula, data, trt, metrics) {


}




CFscore.binary <- function(obs_outcome, obs_trt, cf_pred, cf_trt, ipw, metrics) {

}
