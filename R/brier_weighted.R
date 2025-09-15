brier_weighted <- function(outcomes, predictions, weights) {
  1 / sum(weights) *
    sum(
      (predictions - outcomes)^2 * weights
    )
}
