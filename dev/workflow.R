naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

b <- CFscore(data = df_val,
        model = causal_model,
        Y = "Y",
        propensity_formula = A ~ L,
        treatments = list(0,1))

extract_var <- function(trt, variable) {
  CFtrt <- paste0("CF", trt)
  sapply(
    as.list(1:5),
    function(x) {
      b[[x]][[CFtrt]][[variable]]
    }
  )
}
extract_var(0, "brier")
extract_var(1, "brier")

b[[1]][[1]]$brier


observed_score(data = df_val, model = causal_model, Y = df_val$Y)

