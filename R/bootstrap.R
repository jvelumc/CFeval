
bootstrap_iteration <- function(data, propensity_formula, predictions,
                                Y, A, treatments) {
  bs_sample <- sample(nrow(data), size = nrow(data), replace = T)
  bs_ip <- ip_weights(data[bs_sample, ], propensity_formula)
  bs_results <- lapply(
    X = 1:length(treatments),
    FUN = function(i) {
      CFscore_undertrt(
        data = data[bs_sample, ],
        cf = predictions[[i]][bs_sample],
        Y = Y[bs_sample],
        A_column_name = A,
        ipw = bs_ip,
        trt = treatments[[i]],
        plot = FALSE
      )
    }
  )
  names(bs_results) <- lapply(
    X = treatments,
    FUN = function(x) paste0("CF", x)
  )
  bs_results
}

extract_var <- function(bootstrap_results, trt, variable) {
  CFtrt <- paste0("CF", trt)
  sapply(
    as.list(1:5),
    function(x) {
      bootstrap_results[[x]][[CFtrt]][[variable]]
    }
  )
}

ci <- function(values, cover = 0.95) {
  lower <- (1-cover) / 2
  upper <- 1 - lower
  quantile(values, probs = c(lower, upper))
}

run_bootstrap <- function(data, propensity_formula, predictions,
                          Y, A, treatments, iterations) {
  b <- lapply(
    as.list(1:iterations),
    function(x) {
      bootstrap_iteration(data, propensity_formula, predictions, Y, A,
                          treatments)
    }
  )

  # overly complicated unnesting of results
  bootstrap_raw <- lapply(
    treatments,
    function(x) {
      lapply(
        list("brier" = "brier", "auc" = "auc", "OEratio" = "OEratio"),
        function(p) {
          extract_var(b, x, p)
        })
    }
  )
  bootstrap_summarized <- lapply(
    as.list(1:length(treatments)),
    function(x) {
      lapply(
        list("brier" = "brier", "auc" = "auc", "OEratio" = "OEratio"),
        function(p) {
          ci(bootstrap_raw[[x]][[p]], 0.95)
        }
      )
    }
  )

  names(bootstrap_raw) <- lapply(
    X = treatments,
    FUN = function(x) paste0("CF", x)
  )
  names(bootstrap_summarized) <- lapply(
    X = treatments,
    FUN = function(x) paste0("CF", x)
  )

  list("bootstrap" = bootstrap_summarized,
       "bootstrap.iterations" = bootstrap_raw)
}


