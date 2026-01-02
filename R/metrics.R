# Brier

cf_brier <- function(real_outc, real_trt, cf_pred, cf_trt, ipw) {

  # indices of persons that follow the trt of interest
  pseudo_i <- real_trt == cf_trt

  # cf brier score is brier score of weighted pseudopop
  1 / sum(ipw[pseudo_i]) *
    sum(
      ( cf_pred[pseudo_i] - real_outc[pseudo_i] )^2 * ipw[pseudo_i]
    )
}


# AUC

# OE
