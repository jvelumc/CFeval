
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CFeval <!-- badges: start --> <!-- badges: end -->

Predictions under interventions are estimates of what a subject’s risk
would be if they were to follow a certain counterfactual treatment.
Assessing predictive performance for these predictions is challenging,
as only the outcome of the realized treatment can be observed.(Keogh,
van Geloven, DOI 10.1097/EDE.0000000000001713). This R package
facilitates assessing counterfactual performance of interventional
predictions.

## Installation

You can install the development version of CFeval from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::github("jvelumc/CFeval")
```

## Toy example

Simulate some example data for binary outcome Y and (point) treatment A,
confounded by a variable L. Variable P is a prognostic variable for only
the outcome. The treatment reduces the risk on a bad outcome (Y = 1) in
this simulated example. The R package contains a 5000 row df_dev and a
4000 row df_val, both simulated as described.

<figure>
<img src="man/figures/dag.png" alt="Figure 1. DAG for toy example" />
<figcaption aria-hidden="true">Figure 1. DAG for toy
example</figcaption>
</figure>

``` r
library(CFeval)
head(df_dev)
#>   id           L A          P Y0 Y1 Y
#> 1  1 -0.56047565 0  0.3500025  1  0 1
#> 2  2 -0.23017749 0  0.8144417  0  0 0
#> 3  3  1.55870831 0 -0.5166661  0  1 0
#> 4  4  0.07050839 1 -2.6922644  0  0 0
#> 5  5  0.12928774 1 -1.0969546  0  0 0
#> 6  6  1.71506499 1 -1.2554751  1  1 1
```

Fitting a logistic regression model on this data without accounting for
the confounder L results in a model where treatment apparently increases
the risk on the outcome

``` r
naive_model <- glm(Y ~ A + P, family = "binomial", data = df_dev)
summary(naive_model)
#> 
#> Call:
#> glm(formula = Y ~ A + P, family = "binomial", data = df_dev)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.11397    0.04514   2.525  0.01157 *  
#> A            0.20486    0.06394   3.204  0.00135 ** 
#> P            1.13624    0.03941  28.828  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 6897.8  on 4999  degrees of freedom
#> Residual deviance: 5746.9  on 4997  degrees of freedom
#> AIC: 5752.9
#> 
#> Number of Fisher Scoring iterations: 4
```

Fitting a model using IP-weighting to account for the confounder results
in a model where treatment decreases the risk on the outcome, which we
know to be true in our simulated data

``` r
causal_model <- glm(Y ~ A + P, family = "binomial", data = df_dev, 
                    weights = ip_weights(df_dev, A ~ L))
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
summary(causal_model)
#> 
#> Call:
#> glm(formula = Y ~ A + P, family = "binomial", data = df_dev, 
#>     weights = ip_weights(df_dev, A ~ L))
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.43502    0.03222   13.50   <2e-16 ***
#> A           -0.45611    0.04532  -10.06   <2e-16 ***
#> P            1.11034    0.02763   40.19   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 13788  on 4999  degrees of freedom
#> Residual deviance: 11517  on 4997  degrees of freedom
#> AIC: 11282
#> 
#> Number of Fisher Scoring iterations: 4
```

If either model is to be used to decide on treatment options A, we need
accurate estimates of the counterfactual risk on outcome under both
treatment options A = 1 and A = 0.

Validating a model capable of estimating counterfactual risks is
challenging. This package aims to guide the user in assessing how well
the predictions would match the validation data if all individuals had
followed the treatment under which predictions are made.

The main function CFscore() estimates these counterfactual performance
measures in a validation dataset, printing all assumptions required
along the way.

``` r
results_causal <- CFscore(
  data = df_val,
  model = causal_model, 
  Y = "Y", 
  propensity_formula = A ~ L, 
  treatments = list(0, 1)
)
results_causal
#>    metric       CF0       CF1
#> 1     AUC 0.7530103 0.7255930
#> 2   Brier 0.1972703 0.2117189
#> 3 OEratio 1.0028658 0.9905755
```

``` r
plot(results_causal)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

Compare that to the counterfactual performance of the naive model:

``` r
results_naive <- CFscore(
  data = df_val,
  model = naive_model,
  Y = "Y",
  propensity_formula = A ~ L,
  treatments = list(0,1)
)
results_naive
#>    metric       CF0       CF1
#> 1     AUC 0.7530103 0.7255930
#> 2   Brier 0.2014497 0.2182442
#> 3 OEratio 1.1261003 0.8718221
```

``` r
plot(results_naive)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

## Other options

Maybe we are only interested in counterfactual performance under level 1

``` r
CFscore(
  data = df_val,
  model = causal_model, 
  Y = "Y", 
  propensity_formula = A ~ L, 
  treatments = 1
)
#>    metric       CF1
#> 1     AUC 0.7255930
#> 2   Brier 0.2117189
#> 3 OEratio 0.9905755
```

Maybe we have a model for each treatment level

``` r
df_dev$ip <- ip_weights(df_dev, A ~ L)
model0 <- glm(Y ~ P, family = "binomial", data = df_dev[df_dev$A == 0, ], weights = ip)
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
model1 <- glm(Y ~ P, family = "binomial", data = df_dev[df_dev$A == 1, ], weights = ip)
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!

CFscore(
  data = df_val,
  model = list(model0, model1),
  Y = "Y",
  propensity_formula = A ~ L,
  treatments = list(0,1)
)
#>    metric       CF0       CF1
#> 1     AUC 0.7530103 0.7255930
#> 2   Brier 0.1974358 0.2113294
#> 3 OEratio 1.0030430 0.9900907
```

We can also give counterfactual predictions to CFscore, instead of
models

``` r
cf0 <- predict(model0, newdata = df_val, type = "response")
cf1 <- predict(model1, newdata = df_val, type = "response")

CFscore(
  data = df_val,
  predictions = list(cf0, cf1),
  Y = "Y",
  propensity_formula = A ~ L,
  treatments = list(0,1)
)
#>    metric       CF0       CF1
#> 1     AUC 0.7530103 0.7255930
#> 2   Brier 0.1974358 0.2113294
#> 3 OEratio 1.0030430 0.9900907
```

And we can also give it user-specified weights, instead of a propensity
formula.

``` r
prop_model <- glm(A ~ L, family = "binomial", data = df_val)
prop_score <- predict(prop_model, type = "response")
prob_trt <- ifelse(df_val$A == 1, prop_score, 1 - prop_score)
my_ip_weights <- 1 / prob_trt
  
CFscore(
  data = df_val,
  predictions = list(cf0, cf1),
  Y = "Y",
  ip = my_ip_weights,
  A = "A", #need to specify treatment var, which is normally inferred from propensity formula
  treatments = list(0,1)
)
#>    metric       CF0       CF1
#> 1     AUC 0.7530103 0.7255930
#> 2   Brier 0.1974358 0.2113294
#> 3 OEratio 1.0030430 0.9900907
```
