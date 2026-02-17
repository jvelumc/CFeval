# Performance in observed dataset This function exists only to demonstrate the difference between 'normal' performance and counterfactual performance. It is not user friendly and should not be used.

Performance in observed dataset This function exists only to demonstrate
the difference between 'normal' performance and counterfactual
performance. It is not user friendly and should not be used.

## Usage

``` r
observed_score(
  object,
  data,
  outcome_formula,
  metrics = c("auc", "brier", "oeratio", "calplot")
)
```

## Value

Performance metrics in the observed dataset.
