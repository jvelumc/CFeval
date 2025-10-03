
bootstrap_iteration <- function(data, propensity_formula, predictions,
                                Y, A, treatment_of_interest, metrics) {
  bs_sample <- sample(nrow(data), size = nrow(data), replace = T)
  bs_ip <- ip_weights(data[bs_sample, ], propensity_formula)
  bs_results <- lapply(
    X = predictions,
    FUN = function(pred) {
      CFscore_undertrt(
        data = data[bs_sample, ],
        cf = pred[bs_sample],
        Y = Y[bs_sample],
        A_column_name = A,
        ipw = bs_ip,
        trt = treatment_of_interest,
        metrics = metrics
      )
    }
  )
  bs_results
}

lapply_progress <- function(x, FUN, task_description) {

  FUN2 <- function(x, i, n) {
    result <- FUN(x)
    cat("\r", task_description, ": ", i, "/", n, "                          ")
    return(result)
  }

  n <- length(x)
  result <- lapply(as.list(1:n), function(i) FUN2(x[[i]], i, n))
  cat("\r")
  return(result)
}


ci <- function(values, cover = 0.95) {
  lower <- (1-cover) / 2
  upper <- 1 - lower
  stats::quantile(values, probs = c(lower, upper))
}

get_bootstrapped_metric <- function(bootstrap_results, modelnumber, metric) {
  sapply(
    X = bootstrap_results,
    FUN = function(boot_iter) boot_iter[[modelnumber]][[metric]]
  )
}

run_bootstrap <- function(data, propensity_formula, predictions,
                          Y, A, treatment_of_interest, metrics, iterations) {
  b <- lapply_progress(
    as.list(1:iterations),
    function(x) {
      bootstrap_iteration(data, propensity_formula, predictions, Y, A,
                          treatment_of_interest, metrics)
    },
    "bootstrapping"
  )

  bootstrap_metrics <- lapply(
    X = 1:length(predictions),
    FUN = function(modelnumber) {
      bootstrapped_metrics_model <- lapply(
        X = metrics,
        FUN = function(metric) {
          get_bootstrapped_metric(b, modelnumber, metric)
        }
      )
      setNames(bootstrapped_metrics_model, metrics)
    }
  )
  setNames(object = bootstrap_metrics, nm = names(predictions))
}


