naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

rrscore <- riskRegression::Score(list(naive_model, causal_model),
                                 formula = Y ~ 1,
                      data = df_val)

rrscore$AUC$score$model

naive_model <- lm(Y ~ A + P, data = df_dev)
class(naive_model)


CFscore(df_val, list("naive" = naive_model, "causal" = causal_model),
        outcome_column = "Y", propensity_formula = A ~ L, metrics = "brier",
        treatment_of_interest = 0)

CFscore(df_val, list("naive" = naive_model, "causal" = causal_model),
        outcome_column = "Y", propensity_formula = A ~ L, metrics = "brier",
        treatment_of_interest = 1)





#
#
# b <- CFscore(data = df_val,
#              model = causal_model,
#              Y = "Y",
#              propensity_formula = A ~ L,
#              treatments = list(0,1),
#              bootstrap = 200)
#
# c <- CFscore(data = df_val,
#              model = causal_model,
#              Y = "Y",
#              propensity_formula = A ~ L,
#              treatments = list(0,1))
#
# summary(c$weights)
# b
#
#
# ci(0, "brier", cover = 0.95)
#
# extract_var(b, 0, "brier")
# extract_var(1, "brier")
#
# b[[1]][[1]]$brier
#
#
# observed_score(data = df_val, model = causal_model, Y = df_val$Y)
#
# lapply(list("a" = 1, "b" = 2), function(x) x^2)
#
#
#> causal  Brier 0.197 0.212
#> naive   Brier 0.201 0.218
