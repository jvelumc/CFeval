# Get the inverse probability of treatment weights

Get the inverse probability of treatment weights

## Usage

``` r
ipt_weights(data, propensity_formula)
```

## Arguments

- data:

  a data.frame

- propensity_formula:

  a formula with on the l.h.s. the treatment variable and on the right
  hand side the confounders.

## Value

a numeric vector of the IP weights
