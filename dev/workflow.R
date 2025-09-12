naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

a <- CFscore(df_val, causal_model, "Y", A ~ L, list(0, 1))

a$CF0$calibration$plot()
a$CF1$calibration$plot()

plot(a, trt = list(0,1))
print(a)
