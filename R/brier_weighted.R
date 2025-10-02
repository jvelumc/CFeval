brier_weighted <- function(predictions, outcomes, weights) {
  1 / sum(weights) *
    sum(
      (predictions - outcomes)^2 * weights
    )
}
