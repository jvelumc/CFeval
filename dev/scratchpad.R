naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

rrscore <- riskRegression::Score(list(naive_model, causal_model),
                                 formula = Y ~ 1,
                      data = df_val)

rrscore$AUC$score$model

naive_model <- lm(Y ~ A + P, data = df_dev)
class(naive_model)


cfs <- CFscore(df_val, list("naive" = naive_model, causal_model),
        outcome_column = "Y", propensity_formula = A ~ L,
        treatment_of_interest = 0)

CFscore(df_val, list("naive" = naive_model, "causal" = causal_model),
        outcome_column = "Y", propensity_formula = A ~ L,
        treatment_of_interest = 1)



unlist(cfs$results[[1]]) |> str()


split(
  unlist(cfs$results, use.names = T),
  f = c("brier", "auc", "oe", "oeplot")
) |> str()

b <- quote(causal_model)



