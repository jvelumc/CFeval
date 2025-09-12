#' Simulate some simple data, in which treatment A is a binary point treatment
#' and outcome Y is binary. Variable L acts as a confounder on treatment and
#' outcome. Variable P additionally predicts the outcome. Counterfactual
#' outcomes under both treatment options are generated (Y0 and Y1). The observed
#' outcome Y is consistent with the counterfactual outcome under the observed
#' treament. Intented to be used as a toy example together with build_causal_model()
#'
#' @param n Number of rows
#'
#' @returns A data.frame, where confounder L and prognostic variable P has
#'   normal distribution. Treatment A is a binary variable, with probability of
#'   the inverse logit of L. The counterfactual outcome Y0 is a binary variable
#'   with probability of the inverse logit of 0.5 + L + 1.25 * P - 0.6*A.
#'   Outcome Y is the observed outcome under the realized treatment A.
#' @export
#'
#' @examples
#' df_dev <- build_data(1000)
build_data <- function(n) {
  df <- data.frame(id = 1:n)
  df$L <- stats::rnorm(n)
  df$A <- stats::rbinom(n, 1, stats::plogis(df$L))
  df$P <- stats::rnorm(n)
  df$Y0 <- stats::rbinom(n, 1, stats::plogis(0.5 + df$L + 1.25 * df$P))
  df$Y1 <- stats::rbinom(n, 1, stats::plogis(0.5 + df$L + 1.25 * df$P - 0.6))
  df$Y <- ifelse(df$A == 1, df$Y1, df$Y0)
  return(df)
}

set.seed(123)
df_dev <- build_data(5000)
df_val <- build_data(4000)

usethis::use_data(df_dev, df_val, overwrite = TRUE)
