naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

CFscore(data = df_val,
        model = causal_model,
        Y = "Y",
        propensity_formula = A ~ L,
        treatments = list(0,1),
        bootstrap = 50)


observed_score(data = df_val, model = causal_model, Y = df_val$Y)


bootstrap_sample <-
unique(sample(nrow(df_val), nrow(df_val), replace = T))
