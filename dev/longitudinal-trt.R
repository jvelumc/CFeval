library(data.table)

# CFscire.long <- function(object, outcome_formula, treatment_formula)
# treatment_formula = list(A_0 ~ L0, A1 ~ L1 + A0, ...) ?

# simulate some longitudinal trt time to event data

n <- 50000
n_visits <- 5

gamma_0 <- -1
gamma_L <- 0.5

alpha_0 <- -2
alpha_A <- -0.5
alpha_L <- 0.5
alpha_U <- 0.5

simulate_longitudinal <- function(n, fix_trt = NULL) {
  U <- rnorm(n, 0, 0.1)

  A <- matrix(nrow = n, ncol = n_visits)
  L <- matrix(nrow = n, ncol = n_visits)

  time <- rep(NA, n)
  status <- rep(NA, n)

  simulate_A <- function(i, L, fix_trt) {
    if (is.null(fix_trt)) {
      return(rbinom(n, 1, plogis(gamma_0 + gamma_L * L[, i])))
    } else {
      return(rep(fix_trt, n))
    }
  }

  L[, 1] <- rnorm(n, U, 1)
  A[, 1] <- simulate_A(1, L, fix_trt)

  for (i in 2:n_visits) {
    L[, i] <- rnorm(n, 0.8 * L[, i - 1] - A[, i - 1] + 0.1 * i + U)
    A[, i] <- ifelse(A[, i - 1] == 1, rep(1, n), simulate_A(i, L, fix_trt))
  }

  for (i in 1:n_visits){
    new.t <- simulate_time_to_event(
      n = n,
      constant_baseline_haz = 1,
      LP = alpha_0 + alpha_A * A[, i] + alpha_L * L[, i] + alpha_U * U
    )
    time <- ifelse(is.na(time) & new.t < 1, i - 1 + new.t,time)
  }
  status <- ifelse(is.na(time), 0, 1)
  time <- ifelse(is.na(time), 5, time)

  colnames(A) <- paste0("A", 0:(n_visits - 1))
  colnames(L) <- paste0("L", 0:(n_visits - 1))

  data.table(id = 1:n, time, status, A, L, U)
}

df_dev <- simulate_longitudinal(n)
summary(df_dev$time)

df_cf0 <- simulate_longitudinal(n, 0)
df_cf1 <- simulate_longitudinal(n, 1)

make_long <- function(data) {
  long <- melt(data,
       measure.vars = patterns("^A", "^L"),
       variable.name = "visit",
       value.name = c("A", "L"),
       variable.factor = F)

  long[, visit := as.numeric(visit) - 1]

  long <- long[order(id, visit)]
  long[, `:=`(
    time_start = visit,
    time_end = pmin(time, visit + 1)
  )]
  long <- long[time_start < time, ]
  long[, status := fifelse(time_end == time, status, 0)]
  long[, time := NULL]

  long[, paste0("A_lag", 1:(n_visits-1)) := shift(A, n = 1:(n_visits-1), fill = 0), by = .(id)]
  long[, paste0("L_lag", 1:(n_visits-1)) := shift(L, n = 1:(n_visits-1), fill = 0), by = .(id)]
  long[, L0 := first(L), by = .(id)]

  return(long[])
}

df_dev_long <- make_long(df_dev)

# model dev ---------------------------------------------------------------

fit_model <- function(data_long) {
  data_long <- copy(data_long)
  ipt <- ipt_weights(data_long, A ~ L * A_lag1)
  data_long[, wt := ipt$weights]
  data_long[, ipw := cumprod(wt), by = id]

  coxph(
    Surv(time_start, time_end, status) ~ A + A_lag1 + A_lag2 + A_lag3 + A_lag4 + L0,
    data = data_long,
    weights = ipw
  )
}

cox.msm <- fit_model(df_dev_long)

cumhaz <- basehaz(cox.msm, centered = FALSE)$hazard
event.times <- basehaz(cox.msm, centered = FALSE)$time
cumhaz.fun <- stepfun(event.times, c(0, cumhaz))

L0 <- df_dev_long[visit == 0, L0]

# hazard rate term
hrt <- function(variable, value) {
  exp(coef(cox.msm)[variable]*value)
}

# # risk under never treated
risk0 <- function(t, L0) {
  1 - exp(-cumhaz.fun(t)*hrt("L0", L0))
}

# # risk under always treated
risk1 <- function(t, L0) {
  1 - exp(-(
    cumhaz.fun(min(t, 1))*
      hrt("L0", L0)*hrt("A", 1) +

      (t >= 1) *
      (cumhaz.fun(min(t, 2)) - cumhaz.fun(1))*
      hrt("L0", L0)*hrt("A", 1)*hrt("A_lag1", 1) +

      (t >= 2) *
      (cumhaz.fun(min(t, 3)) - cumhaz.fun(2))*
      hrt("L0", L0)*hrt("A", 1)*hrt("A_lag1", 1)*hrt("A_lag2", 1) +

      (t >= 3) *
      (cumhaz.fun(min(t, 4)) - cumhaz.fun(3))*
      hrt("L0", L0)*hrt("A", 1)*hrt("A_lag1", 1)*hrt("A_lag2", 1)*hrt("A_lag3", 1) +

      (t >= 4) *
      (cumhaz.fun(min(t, 5)) - cumhaz.fun(4))*
      hrt("L0", L0)*hrt("A", 1)*hrt("A_lag1", 1)*hrt("A_lag2", 1)*
      hrt("A_lag3", 1)*hrt("A_lag4", 1)
  ))
}

# 'observed' risks ----------------------------------------------------------

# CF observed risk under never treatment
iptw <- ipt_weights(df_dev_long, A ~ L * A_lag1)
iptw$model

df_dev_long[, ipw_visit := iptw$weights]
df_dev_long[, iptw := cumprod(ipw_visit), by = id]
df_dev_long[, in.dat.0 := A == 0]

1 - (survfit(Surv(time_start, time_end, status) ~ 1,
             data = df_dev_long[in.dat.0 == TRUE], weights = iptw) |>
       summary(times = 5))$surv

df_dev_long[, correct_treatment := all(as.integer(A == 0)), by = .(id)]
df_dev_long[, risk_untreated := risk0(5, L0)]
data_wide <- df_dev_long[, .SD[.N], by = id]


# this does not take into account patients that have trt (0, 0, 1,1,1) at all.
# but these should be taken into account, and censored after t = 2.
lapply(
  X = c("auc", "brier", "oeratio"),
  FUN = function(m) {
    data_wide[, cf_metric(metric = m,
                          obs_outcome = status,
                          obs_trt = correct_treatment,
                          cf_pred = risk_untreated,
                          cf_trt = 1,
                          ipw = iptw
    )]
  }
)


# -------------------------------------------------------------------------


#
# trt_of_interest <- c(0,0,0,0,0)
#
# df_dev_long[, trt_deviation :=
#      visit[match(TRUE, A != trt_of_interest[visit+1])],
#    by = id]
#
# df_dev_long[,
#   artificially_censored := fifelse(is.na(trt_deviation), FALSE, visit >= trt_deviation)
# ]

# df_wide_again <- df_dev_long[, .(
#   L0 = L0[1],
#   time = max(0, time_end[artificially_censored == FALSE]),
#   status = max(0, status[artificially_censored == FALSE]),
#   iptw = last(iptw[artificially_censored == FALSE])
# ), by = id]
#
# summary(df_wide_again$status)
# df_dev_long[in.dat.0 == TRUE]
#
# df_wide_again[, trt := fifelse(is.na(iptw), 0, 1)]
#
# km <- survfit(Surv(time, status) ~ 1, data = df_wide_again[trt == 1], weights = iptw)
#



# can this be done as weighted mean in the in.dat.0 pop?



# -------------------------------------------------------------------------









#
# df_collapsed <- df_dev_long[in.dat.0 == TRUE, .(
#   time = max(time_end),
#   status = max(status),
#   iptw = last(iptw),
#   L0 = L0[1],
#   risk_untreated = first(risk_untreated)
# ), by = id]
#
# 1 - (survfit(Surv(time, status) ~ 1, data = df_collapsed, weights = iptw) |>
#   summary(times = 5))$surv
#
#
#
#
# # define indicator that is 1 if treatment of interest is followed
# # means subject must follow treatment strategy of interest until survtime or
# # until prediction horizon
# df_dev_long[, correct_treatment := all(as.integer(A == 0)), by = .(id)] # works in this simple case
# data_wide <- df_dev_long[, .SD[.N], by = id]
#
#
# lapply(
#   X = c("auc", "brier", "oeratio"),
#   FUN = function(m) {
#     data_wide[, cf_metric(metric = m,
#                           obs_outcome = status,
#                           obs_trt = correct_treatment,
#                           cf_pred = risk_untreated,
#                           cf_trt = 1,
#                           ipw = ipw0
#     )]
#   }
# )
#
# df_cf0[, `:=`(one = 1, risk = risk0(5, L0))]
#
# lapply(
#   X = c("auc", "brier", "oeratio"),
#   FUN = function(m) {
#     df_cf0[, cf_metric(metric = m,
#                           obs_outcome = status,
#                           obs_trt = one,
#                           cf_pred = risk,
#                           cf_trt = 1,
#                           ipw = rep(1, n)
#     )]
#   }
# )
