ipc_weights <- function(data, formula, type, time_horizon) {

  mf <- model.frame(formula, data)
  y <- model.response(mf)

  time <- y[, "time"]
  status <- y[, "status"]

  y_flip <- survival::Surv(time, 1 - status)

  if (type == "KM") {
    fit <- survival::survfit(y_flip ~ 1)
  } else {
    mf_flip <- mf
    mf_flip[[1]] <- y_flip
    fit <- survival::survfit(formula, data = mf_flip)
  }

  p_not_censor <- stepfun(fit$time, c(1, fit$surv))


  # if censored before time horizon, weight is 0,
  # else, weight is 1/probability uncensored at event/time horizon
  w <- ifelse(
    status == 0 & time < time_horizon,
    0,
    1 / p_not_censor(pmin(time, time_horizon))
  )
  unname(w)
}
