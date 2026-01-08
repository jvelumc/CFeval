predict_CF <- function(model, data, A_column, CF_treatment,
                       time_column, time_horizon) {
  # predict outcome probabilities for all patients, setting their treatment
  # to CF_treatment
  data[[A_column]] <- CF_treatment
  if ("glm" %in% class(model)) {
    return(predict_glm(model, data))
  }
  if ("coxph" %in% class(model)) {
    return(predict_cox(model, data, time_column, time_horizon))
  }
  stop("model class", class(model), "not supported")
}


predict_glm <- function(model, data) {
  stats::predict(model, newdata = data, type = "response")
}


predict_cox <- function(model, data, time_column, time_horizon) {
  # data[[time_column]] <- time_horizon
  # 1 - predict(model, newdata = data, type = "survival")
  # sf <- survfit(model, newdata = data)
  # S_t <- summary(sf, times = time_horizon)$surv
  # 1 - S_t

  bh <- survival::basehaz(model, centered = FALSE)
  H0_horizon <- bh$hazard[max(which(bh$time <= time_horizon))]
  S0_horizon <- exp(-H0_horizon)

  lp <- predict(model, newdata = data, type = "lp")

  S_horizon <- S0_horizon^exp(lp)
  1-S_horizon
}


