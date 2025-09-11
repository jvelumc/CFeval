brier_weighted <- function(outcomes, predictions, weights) {
  1 / sum(weights) * # of 1/n?
    sum(
      (predictions - outcomes)^2 * weights
    )
}
