ipc_weights <- function(data, formula, type, time_horizon) {
  # assert l.h.s. of formula is Surv object
  # make sure time and status are not variables in the formula on the rhs
  # the model.frame logic is probably brittle. Does plugging in
  # formula = Surv(time, 1 - status) ~ 1 work as it should?
  # probably infer type = KM/cox from formula; if rhs is 1, do KM, else cox

  # flip events (we are predicting censor events)
  df_flipped <- model.frame(formula, data)
  df_flipped$status <- 1 - df_flipped[[1]][, "status"]
  df_flipped$time <- df_flipped[[1]][, "time"]

  fit <- survival::survfit(formula, data = df_flipped)

  p_not_censor <- stepfun(fit$time, c(1, fit$surv))

  # if censored (flipped status = 1!) before time horizon, weight is 0,
  # else, weight is 1/probability uncensored at event/time horizon
  ifelse(
    df_flipped$status == 1 & df_flipped$time < time_horizon,
    0,
    1 / p_not_censor(pmin(df_flipped$time, time_horizon))
  )
}
