naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev,
                    weights = ip_weights(df_dev, A ~ L))

a <- CFscore(data = df_val,
             model = list(causal_model, naive_model),
             Y = "Y",
             propensity_formula = A ~ L,
             treatments = list(1,2))



a

a$CF0$calibration$plot()
a$CF1$calibration$plot()

plot(a, trt = list(0,1))
print(a)



test <- function(a,b,c) {
  print("hallo")
}

test()


stopifnot(
  "test aa" = 1==1,
  "lol bb" = 1==2
)
