calibration_weighted <- function(outcomes, predictions, treatments,
                                 treatment_of_interest, weights,
                                 plot = TRUE) {

  # Observed / expected ratio
  # We have expected for everyone (just the predicted probability)
  # but we only observe outcomes for those with correct treatment strategy,
  # so for the observed we have to use the weights

  expected <- mean(predictions)
  observed <- stats::weighted.mean(
    x = outcomes[treatments == treatment_of_interest],
    w = weights[treatments == treatment_of_interest]
  )

  oe_ratio <- observed/expected

  return(oe_ratio)
}
