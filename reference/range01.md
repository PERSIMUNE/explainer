# Data scale to 0 and 1

Scale the data to the range of 0 to 1. It uses the Hampel filter to
adjust outliers, followed by min-max normalization.

## Usage

``` r
range01(x)
```

## Arguments

- x:

  Vector or array of numbers to be normalized

## Value

Normalized vector

## References

Pearson, R. K. (1999). “Data cleaning for dynamic modeling and control”.
European Control Conference, ETH Zurich, Switzerland.

## See also

[`eSHAP_plot()`](https://persimune.github.io/explainer/reference/eSHAP_plot.md)

## Examples

``` r
normalized_vector <- range01(seq(-10:1000))
```
