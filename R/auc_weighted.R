auc_weighted <- function(outcomes, predictions, weights) {

  cases_indices <- which(outcomes == 1)
  controls_indices <- which(outcomes == 0)

  cases_predictions <- predictions[cases_indices]
  controls_predictions <- predictions[controls_indices]

  cases_weights <- weights[cases_indices]
  controls_weights <- weights[controls_indices]

  numerator <- 0
  denominator <- 0

  for (i in cases_indices) {

    pred_i <- predictions[i]
    weight_i <- weights[i]

    n_wins <- sum(
      weight_i * controls_weights[pred_i > controls_predictions]
    )

    n_ties <- sum(
      weight_i * controls_weights[pred_i == controls_predictions]
    )

    numerator <- numerator + n_wins + 0.5 * n_ties

    denominator <- denominator + sum(weight_i * controls_weights)

  }
  return( numerator / denominator)
}
