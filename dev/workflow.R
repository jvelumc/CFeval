naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

a <- CFscore(df_val, causal_model, "Y", A ~ L, list(0, 1))


print.cfscore(a)




a <- c(1,2,3)
class(a) <- "mijnclass"



print(a)
a
