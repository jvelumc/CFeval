naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
summary(naive_model)

causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))
summary(causal_model)

CFscore(df_val, causal_model, "Y", A ~ L, list(0, 1))

trts <- list(1,2,3,4,5)
tn <- 5

trts[1:4]
