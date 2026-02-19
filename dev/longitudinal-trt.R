library(data.table)

# CFscire.long <- function(object, outcome_formula, treatment_formula)
# treatment_formula = list(A_0 ~ L0, A1 ~ L1 + A0, ...) ?

# simulate some longitudinal trt time to event data

n <- 10000
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
    L[, i] <- rnorm(n, plogis(0.8 * L[, i - 1] - A[, i - 1] + 0.1 * i + U))
    A[, i] <- ifelse(A[, i - 1] == 1, 1, simulate_A(i, L, fix_trt))
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

  colnames(A) <- paste0("A", 1:n_visits - 1)
  colnames(L) <- paste0("L", 1:n_visits - 1)

  data.table(time, status, A, L, U)
}

df_dev <- simulate_longitudinal(n)
df_cf0 <- simulate_longitudinal(n, 0)
df_cf1 <- simulate_longitudinal(n, 1)



data_long <- reshape(
  data,
  varying = list(A = colnames(A), L = colnames(L)),
  v.names = c("A", "L"),
  timevar = "visit",
  times = 1:n_visits - 1,
  direction = "long"
)

data_long <- data_long[order(data_long$id, data_long$visit), ]
data_long$time_start <- data_long$visit
data_long$time_end <- pmin(data_long$time, data_long$visit + 1)
data_long <- data_long[data_long$time_start < data_long$time, ]
data_long$status <- ifelse(data_long$time_end == data_long$time, data_long$status, 0)
data_long <- data_long[, c("id", "visit", "time_start", "time_end", "status", "A", "L")]

setDT(data_long)

data_long[, paste0("A_lag", 1:(n_visits-1)) := shift(A, n = 1:(n_visits-1), fill = 0), by = .(id)]
data_long[, paste0("L_lag", 1:(n_visits-1)) := shift(L, n = 1:(n_visits-1), fill = 0), by = .(id)]
data_long[, L0 := first(L), by = .(id)]

# now we should form a counterfactual dataset to assess whether results are correct


# model dev ---------------------------------------------------------------

wt.mod <- glm(A ~ L, family="binomial", data=data_long[A_lag1==0,])
pred.wt <- predict(wt.mod, type = "response", newdata = data_long)
data_long[, wt := fifelse(A == 1, pred.wt, 1 - pred.wt)]
data_long[, wt := fifelse(A_lag1 == 1, 1, wt)]
data_long[, wt.cum := cumprod(wt), by = id]

data_long[, ipw := 1 / wt.cum]

cox.msm <- coxph(
  Surv(time_start, time_end, status) ~ A + A_lag1 + A_lag2 + A_lag3 + A_lag4 + L0,
  data = data_long,
  weights = ipw
)

cumhaz <- basehaz(cox.msm, centered = FALSE)$hazard
event.times <- basehaz(cox.msm, centered = FALSE)$time
cumhaz.fun <- stepfun(event.times, c(0, cumhaz))

L0 <- data_long[visit == 0, L0]

# hazard rate term
hrt <- function(variable, value) {
  exp(coef(cox.msm)[variable]*value)
}

# # risk under never treated
risk0 <- function(t) {
  1 - exp(-cumhaz.fun(t)*hrt("L0", L0))
}

# # risk under always treated
risk1 <- function(t) {
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

data_long[, `:=`(wt = NULL, wt.cum = NULL, ipw = NULL)]

# 'observed' risks ----------------------------------------------------------

# CF observed risk under never treatment

wt.mod <- glm(A ~ L, family = "binomial", data = data_long[A_lag1 == 0])
pred.wt0 <- predict(wt.mod, type = "response", newdata = data_long)
data_long[, wt0 := 1 - pred.wt0]
data_long[, ipw0 := 1/cumprod(wt0), by = id]
data_long[, in.dat.0 := A == 0]

# can this be done as weighted mean in the in.dat.0 pop?
km.0 <- survfit(Surv(time_start, time_end, status) ~ 1,
                data = data_long[in.dat.0 == TRUE], weights = ipw0)
step.risk0.obs=stepfun(km.0$time,c(1,km.0$surv))

risk0_obs <- 1 - step.risk0.obs(5)


# define indicator that is 1 if treatment of interest is followed
# means subject must follow treatment strategy of interest until survtime or
# until prediction horizon
data_long[, correct_treatment := all(as.integer(A == 0)), by = .(id)] # works in this simple case
data_wide <- data_long[, .SD[.N], by = id]
data_wide[, risk_untreated := risk0(5)]

brier_null <- data_wide[, cf_brier(
  obs_outcome = data_wide$status,
  obs_trt = correct_treatment,
  cf_pred = rep(risk0_obs, 10000),
  cf_trt = 1,
  ipw = ipw0
)]

(brier_null - 0.224619)/brier_null*100

lapply(
  X = c("auc", "brier", "oeratio"),
  FUN = function(m) {
    data_wide[, cf_metric(metric = m,
                          obs_outcome = data_wide$status,
                          obs_trt = correct_treatment,
                          cf_pred = risk_untreated,
                          cf_trt = 1,
                          ipw = ipw0
    )]
  }
)


# data_new <- data_long[, .(L0 = first(L)), by = id]


# make wide again (until time horizon of interest)

time_horizon <- 5






data_long[time_end <= time_horizon, .SD[.N], by = id]



test <- data.table(A = c(0, 0, 1, 0, 0, 1))


