naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

b <- CFscore(data = df_val,
             model = causal_model,
             Y = "Y",
             propensity_formula = A ~ L,
             treatments = list(0,1),
             bootstrap = 200)

c <- CFscore(data = df_val,
             model = causal_model,
             Y = "Y",
             propensity_formula = A ~ L,
             treatments = list(0,1))

c
b


ci(0, "brier", cover = 0.95)

extract_var(b, 0, "brier")
extract_var(1, "brier")

b[[1]][[1]]$brier


observed_score(data = df_val, model = causal_model, Y = df_val$Y)

lapply(list("a" = 1, "b" = 2), function(x) x^2)



