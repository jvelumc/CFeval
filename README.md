
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
#> Estimation of the performance of the prediction model in a
#>  counterfactual (CF) dataset where everyone's treatment was set to 0 and
#>  1.
#> The following assumptions must be satisfied for correct inference:
#> - Conditional exchangeability requires that {L} are sufficient to
#>  adjust for confounding and selection bias between treatment and
#>  outcome.
#> - Positivity (assess $weights for outliers)
#> - Consistency
#> - No interference
#> - No measurement error
#> - Correctly specified propensity formula
#>    metric   CF0   CF1
#> 1   Brier 0.197 0.212
#> 2     auc 0.753 0.726
#> 3 OEratio 1.003 0.991
```

Weights are exported:

``` r
summary(results_causal$weights)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.017   1.339   1.644   1.996   2.207  14.425
```

And calibration plots:

``` r
plot(results_causal)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

Compare that to the counterfactual performance of the naive model:

``` r
results_naive <- CFscore(
  data = df_val,
  model = naive_model,
  Y = "Y",
  propensity_formula = A ~ L,
  treatments = list(0,1),
  quiet = TRUE
)
results_naive
#>    metric   CF0   CF1
#> 1   Brier 0.201 0.218
#> 2     auc 0.753 0.726
#> 3 OEratio 1.126 0.872
```

``` r
plot(results_naive)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-8-2.png" width="100%" />

## Other options

Bootstrapping for 95% confidence intervals (95CI) - in this setting
(sample size 4000, binary outcome and treatment, 1 confounder) takes
about 30 seconds on my pc.

``` r
CFscore(
  data = df_val,
  model = causal_model, 
  Y = "Y", 
  propensity_formula = A ~ L, 
  treatments = list(0, 1),
  bootstrap = 200,
  quiet = TRUE
)
#>  bootstrapping :  1 / 200                            bootstrapping :  2 / 200                            bootstrapping :  3 / 200                            bootstrapping :  4 / 200                            bootstrapping :  5 / 200                            bootstrapping :  6 / 200                            bootstrapping :  7 / 200                            bootstrapping :  8 / 200                            bootstrapping :  9 / 200                            bootstrapping :  10 / 200                            bootstrapping :  11 / 200                            bootstrapping :  12 / 200                            bootstrapping :  13 / 200                            bootstrapping :  14 / 200                            bootstrapping :  15 / 200                            bootstrapping :  16 / 200                            bootstrapping :  17 / 200                            bootstrapping :  18 / 200                            bootstrapping :  19 / 200                            bootstrapping :  20 / 200                            bootstrapping :  21 / 200                            bootstrapping :  22 / 200                            bootstrapping :  23 / 200                            bootstrapping :  24 / 200                            bootstrapping :  25 / 200                            bootstrapping :  26 / 200                            bootstrapping :  27 / 200                            bootstrapping :  28 / 200                            bootstrapping :  29 / 200                            bootstrapping :  30 / 200                            bootstrapping :  31 / 200                            bootstrapping :  32 / 200                            bootstrapping :  33 / 200                            bootstrapping :  34 / 200                            bootstrapping :  35 / 200                            bootstrapping :  36 / 200                            bootstrapping :  37 / 200                            bootstrapping :  38 / 200                            bootstrapping :  39 / 200                            bootstrapping :  40 / 200                            bootstrapping :  41 / 200                            bootstrapping :  42 / 200                            bootstrapping :  43 / 200                            bootstrapping :  44 / 200                            bootstrapping :  45 / 200                            bootstrapping :  46 / 200                            bootstrapping :  47 / 200                            bootstrapping :  48 / 200                            bootstrapping :  49 / 200                            bootstrapping :  50 / 200                            bootstrapping :  51 / 200                            bootstrapping :  52 / 200                            bootstrapping :  53 / 200                            bootstrapping :  54 / 200                            bootstrapping :  55 / 200                            bootstrapping :  56 / 200                            bootstrapping :  57 / 200                            bootstrapping :  58 / 200                            bootstrapping :  59 / 200                            bootstrapping :  60 / 200                            bootstrapping :  61 / 200                            bootstrapping :  62 / 200                            bootstrapping :  63 / 200                            bootstrapping :  64 / 200                            bootstrapping :  65 / 200                            bootstrapping :  66 / 200                            bootstrapping :  67 / 200                            bootstrapping :  68 / 200                            bootstrapping :  69 / 200                            bootstrapping :  70 / 200                            bootstrapping :  71 / 200                            bootstrapping :  72 / 200                            bootstrapping :  73 / 200                            bootstrapping :  74 / 200                            bootstrapping :  75 / 200                            bootstrapping :  76 / 200                            bootstrapping :  77 / 200                            bootstrapping :  78 / 200                            bootstrapping :  79 / 200                            bootstrapping :  80 / 200                            bootstrapping :  81 / 200                            bootstrapping :  82 / 200                            bootstrapping :  83 / 200                            bootstrapping :  84 / 200                            bootstrapping :  85 / 200                            bootstrapping :  86 / 200                            bootstrapping :  87 / 200                            bootstrapping :  88 / 200                            bootstrapping :  89 / 200                            bootstrapping :  90 / 200                            bootstrapping :  91 / 200                            bootstrapping :  92 / 200                            bootstrapping :  93 / 200                            bootstrapping :  94 / 200                            bootstrapping :  95 / 200                            bootstrapping :  96 / 200                            bootstrapping :  97 / 200                            bootstrapping :  98 / 200                            bootstrapping :  99 / 200                            bootstrapping :  100 / 200                            bootstrapping :  101 / 200                            bootstrapping :  102 / 200                            bootstrapping :  103 / 200                            bootstrapping :  104 / 200                            bootstrapping :  105 / 200                            bootstrapping :  106 / 200                            bootstrapping :  107 / 200                            bootstrapping :  108 / 200                            bootstrapping :  109 / 200                            bootstrapping :  110 / 200                            bootstrapping :  111 / 200                            bootstrapping :  112 / 200                            bootstrapping :  113 / 200                            bootstrapping :  114 / 200                            bootstrapping :  115 / 200                            bootstrapping :  116 / 200                            bootstrapping :  117 / 200                            bootstrapping :  118 / 200                            bootstrapping :  119 / 200                            bootstrapping :  120 / 200                            bootstrapping :  121 / 200                            bootstrapping :  122 / 200                            bootstrapping :  123 / 200                            bootstrapping :  124 / 200                            bootstrapping :  125 / 200                            bootstrapping :  126 / 200                            bootstrapping :  127 / 200                            bootstrapping :  128 / 200                            bootstrapping :  129 / 200                            bootstrapping :  130 / 200                            bootstrapping :  131 / 200                            bootstrapping :  132 / 200                            bootstrapping :  133 / 200                            bootstrapping :  134 / 200                            bootstrapping :  135 / 200                            bootstrapping :  136 / 200                            bootstrapping :  137 / 200                            bootstrapping :  138 / 200                            bootstrapping :  139 / 200                            bootstrapping :  140 / 200                            bootstrapping :  141 / 200                            bootstrapping :  142 / 200                            bootstrapping :  143 / 200                            bootstrapping :  144 / 200                            bootstrapping :  145 / 200                            bootstrapping :  146 / 200                            bootstrapping :  147 / 200                            bootstrapping :  148 / 200                            bootstrapping :  149 / 200                            bootstrapping :  150 / 200                            bootstrapping :  151 / 200                            bootstrapping :  152 / 200                            bootstrapping :  153 / 200                            bootstrapping :  154 / 200                            bootstrapping :  155 / 200                            bootstrapping :  156 / 200                            bootstrapping :  157 / 200                            bootstrapping :  158 / 200                            bootstrapping :  159 / 200                            bootstrapping :  160 / 200                            bootstrapping :  161 / 200                            bootstrapping :  162 / 200                            bootstrapping :  163 / 200                            bootstrapping :  164 / 200                            bootstrapping :  165 / 200                            bootstrapping :  166 / 200                            bootstrapping :  167 / 200                            bootstrapping :  168 / 200                            bootstrapping :  169 / 200                            bootstrapping :  170 / 200                            bootstrapping :  171 / 200                            bootstrapping :  172 / 200                            bootstrapping :  173 / 200                            bootstrapping :  174 / 200                            bootstrapping :  175 / 200                            bootstrapping :  176 / 200                            bootstrapping :  177 / 200                            bootstrapping :  178 / 200                            bootstrapping :  179 / 200                            bootstrapping :  180 / 200                            bootstrapping :  181 / 200                            bootstrapping :  182 / 200                            bootstrapping :  183 / 200                            bootstrapping :  184 / 200                            bootstrapping :  185 / 200                            bootstrapping :  186 / 200                            bootstrapping :  187 / 200                            bootstrapping :  188 / 200                            bootstrapping :  189 / 200                            bootstrapping :  190 / 200                            bootstrapping :  191 / 200                            bootstrapping :  192 / 200                            bootstrapping :  193 / 200                            bootstrapping :  194 / 200                            bootstrapping :  195 / 200                            bootstrapping :  196 / 200                            bootstrapping :  197 / 200                            bootstrapping :  198 / 200                            bootstrapping :  199 / 200                            bootstrapping :  200 / 200                           
#>    metric   CF0      CF0.95CI   CF1      CF1.95CI
#> 1   Brier 0.197 0.194 - 0.203 0.212 0.208 - 0.214
#> 2     auc 0.753 0.736 - 0.768 0.726 0.718 - 0.735
#> 3 OEratio 1.003 0.977 - 1.022 0.991 0.988 - 1.014
```

Maybe we are only interested in counterfactual performance under level 1

``` r
CFscore(
  data = df_val,
  model = causal_model, 
  Y = "Y", 
  propensity_formula = A ~ L, 
  treatments = 1,
  quiet = TRUE
)
#>    metric   CF1
#> 1   Brier 0.212
#> 2     auc 0.726
#> 3 OEratio 0.991
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
  treatments = list(0,1),
  quiet = TRUE
)
#>    metric   CF0   CF1
#> 1   Brier 0.197 0.211
#> 2     auc 0.753 0.726
#> 3 OEratio 1.003 0.990
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
  treatments = list(0,1),
  quiet = TRUE
)
#>    metric   CF0   CF1
#> 1   Brier 0.197 0.211
#> 2     auc 0.753 0.726
#> 3 OEratio 1.003 0.990
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
  treatments = list(0,1),
  quiet = TRUE
)
#>    metric   CF0   CF1
#> 1   Brier 0.197 0.211
#> 2     auc 0.753 0.726
#> 3 OEratio 1.003 0.990
```

We can also assess performance measures on the observed data with the
realized treatment values:

``` r
predictions_naive <- predict(naive_model, type = "response", newdata = df_val)
observed_score(predictions = predictions_naive, Y = df_val$Y)
#>    metric  observed
#> 1   Brier 0.7467057
#> 2     auc 0.2034472
#> 3 OEratio 0.9976336
```
