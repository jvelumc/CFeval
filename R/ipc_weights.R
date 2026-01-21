ipc_weights <- function(data, formula, type, time_horizon) {


  if (type == "KM")
    stopifnot(rhs_is_one(formula))

  mf <- model.frame(formula, data)
  y <- model.response(mf)
  time <- y[, "time"]
  status <- y[, "status"]

  flipped_form <- update.formula(formula, Surv(time, status == 0) ~ .)

  p_uncensored <- switch(
    type,
    KM = {
      fit <- survival::survfit(flipped_form, data = data)
      p_not_censor <- stepfun(fit$time, c(1, fit$surv))
      list(
        model = fit,
        probability = p_not_censor(pmin(time, time_horizon))
      )
    },
    cox = {
      fit <- coxph(flipped_form, data = data)
      list(
        model = fit,
        probability = 1-predict_cox(fit, data, pmin(time_horizon, time))
      )
    },
    stop("cens.model ", type, " not implemented")
  )

  # if censored before time horizon, weight is 0,
  # else, weight is 1/probability uncensored at event/time horizon
  w <- ifelse(
    status == 0 & time < time_horizon,
    0,
    1 / p_uncensored$probability
  )

  list(
    model = p_uncensored$model,
    weights = unname(w)
  )
}
