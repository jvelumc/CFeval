# Counterfactual validation score

Counterfactual validation score

## Usage

``` r
CFscore(
  object,
  data,
  outcome_formula,
  treatment_formula,
  treatment_of_interest,
  metrics = c("auc", "brier", "oeratio", "oeratio_pp", "calplot"),
  time_horizon,
  cens.model = "cox",
  null.model = TRUE,
  stable_iptw = FALSE,
  bootstrap = 0,
  bootstrap_progress = TRUE,
  iptw,
  ipcw,
  quiet = FALSE
)
```

## Arguments

- object:

  One of the following three options to be validated:

  - a numeric vector, corresponding to risk predictions

  - a glm or coxph model

  - a (named) list, with one or more of the previous 2 options, for
    validating and comparing multiple models at once.

- data:

  A data.frame containing at least the observed outcome, assigned
  treatment, and necessary confounders for the validation of object.

- outcome_formula:

  A formula which identifies the outcome (left hand side). E.g. Y ~ 1
  for binary and Surv(time, status) ~ 1 for time-to-event outcomes. In
  right censored data, the right hand side of the formula is used to
  estimate the inverse probability of censoring weights (IPCW) model.
  Alternatively, the IPCW can also be specified themselves using the
  ipcw argument, in which case the right hand side of this formula is
  ignored.

- treatment_formula:

  A formula which identifies the treatment (left hand side). E.g. A ~ 1.
  The right hand side of the formula can be used to specify the
  confounders used to estimate the inverse probability of treatment
  weights (IPTW) model. E.g. A ~ L, where L is the confounder required
  to adjust for treatment. The IPTW can also be specified themselves
  using the iptw argument, in which case the right hand side of this
  formula is ignored.

- treatment_of_interest:

  A treatment level for which the counterfactual perormance measures
  should be evaluated.

- metrics:

  A character vector specifying which performance metrics to be
  computed. Options are c("auc", "brier", "oeratio", "oeratio_pp",
  "calplot"). See details.

- time_horizon:

  For time to event data, the prediction horizon of interest.

- cens.model:

  Model for estimating inverse probability of censoring weights (IPCW).
  Methods currently implemented are Kaplan-Meier ("KM") or Cox ("cox")
  both applied to the censored times.

- null.model:

  If TRUE fit a risk prediction model which ignores the covariates and
  predicts the same value for all subjects. The model is fitted using
  the data in which all subjects are counterfactually assigned the
  treatment of interest (using the IPTW, as estimated using the
  treatment_formula or as given by the iptw argument). For time-to-event
  outcomes, the subjects are also counterfactually uncensored (using the
  IPCW, as estimated using the outcome_formula, or as given by the ipcw
  argument).

- stable_iptw:

  if TRUE, estimate stabilized IPTW weights. See details.

- bootstrap:

  If this is an integer greater than 0, this indicates the number of
  bootstrap iterations, to compute 95% confidence intervals around the
  performance metrics.

- iptw:

  A numeric vector, containing the inverse probability of treatment
  weights. These are normally computed using the treatment_formula, but
  they can be specified directly via this argument. If specified via
  this argument, bootstrap is not possible.

- ipcw:

  A numeric vector, containing the inverse probability of censor
  weights. These are normally computed using the outcome_formula, but
  they can be specified directly via this argument. If specified via
  this argument, bootstrap is not possible.

- quiet:

  If set to TRUE, don't print assumptions.

## Value

A list with performance metrics.

## Details

auc is area under the (ROC) curve, estimated ...

Brier score is defined as 1 / sum(iptw) sum(predictions_i - outcome_i)^2

oeratio and oeratio_pp represent the observed/expected ratio, where
observed is the mean of the outcomes in the pseudopopulation. For
oeratio, the expected is the mean of the predictions in the original
observed population. For oeratio_pp, expected is the mean of the
predictions in the pseudopopulation. A perfect model will have
oeratio_pp equal to exactly one, but not necessarily oeratio equal to
one, if the size of the (weighted) pseudopopulation is not exactly equal
to the size of the original population.

## Examples

``` r
data <- data.frame(L = rnorm(1000), P = rnorm(1000))
data$A <- rbinom(1000, 1, plogis(L))
#> Error: object 'L' not found
data$Y <- rbinom(1000, 1, plogis(0.1 + 0.5*data$L + 0.7*data$P - 0.8*data$A))
#> Warning: NAs produced

random <- runif(1000, 0, 1)
model <- glm(Y ~ A + P, data = data, family = "binomial")
#> Error in eval(predvars, data, env): object 'A' not found
perfect <- data$Y

CFscore(
  object = list("ran" = random, "mod" = model, "per" = perfect),
  data = data,
  outcome_formula = Y ~ 1,
  treatment_formula = A ~ L,
  treatment_of_interest = 0,
)
#> Error in eval(predvars, data, env): object 'A' not found
```
