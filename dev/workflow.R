naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
summary(naive_model)

causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))
summary(causal_model)

CFscore_undertrt(df_val, causal_model, "Y", A ~ L, 0)
