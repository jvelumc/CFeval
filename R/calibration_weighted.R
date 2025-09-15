generate_calibration_plot <- function(mean_preds, mean_obs, title, ylab) {
  plot(mean_preds, mean_obs, type = "o", xlim = c(0,1), ylim = c(0,1),
       xlab = "Predicted risk", ylab = ylab,
       main = title)
  graphics::abline(0, 1, col = "red")
}

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

  calibration <- list(
    "OEratio" = oe_ratio
  )

  if (plot != FALSE) {
    n_breaks <- 8

    cal <- data.frame(A = treatments, Y = outcomes, ipw = weights, pred = predictions)
    cal <- cal[order(predictions), ]
    cal$group <- cut(cal$pred, breaks = 8, label = F)

    mean_preds <- tapply(cal$pred, cal$group, mean)
    cal_trt <- cal[cal$A == treatment_of_interest, ]
    mean_obs <- tapply(
      X = cal_trt,
      INDEX = cal_trt$group,
      FUN = function(x) stats::weighted.mean(x$Y, x$ipw)
    )

    calibration$plot <- function() {
      generate_calibration_plot(
        mean_preds = mean_preds,
        mean_obs = mean_obs,
        title = paste0("Calibration plot had everyone followed treatment ",
                       treatment_of_interest),
        ylab = "Counterfactual observed risk")
    }
  }
  return(calibration)
}

calibration <- function(outcomes, predictions, plot = T) {
  expected <- mean(predictions)
  observed <- mean(outcomes)

  oe_ratio <- observed/expected
  calibration <- list(
    "OEratio" = oe_ratio
  )

  if (plot != FALSE) {
    n_breaks <- 8
    cal <- data.frame(Y = outcomes, pred = predictions)
    cal <- cal[order(predictions), ]
    cal$group <- cut(cal$pred, breaks = 8, label = F)

    mean_preds <- tapply(cal$pred, cal$group, mean)
    mean_obs <- tapply(cal$Y, cal$group, mean)

    calibration$plot <- function() {
      generate_calibration_plot(
        mean_preds = mean_preds,
        mean_obs = mean_obs,
        title = paste0("Calibration plot for realized treatment values"),
        ylab = "Observed risk"
      )
    }
  }
  return(calibration)
}
