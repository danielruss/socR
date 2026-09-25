# Calculates the Shannon entropy for a Crosswalk

The more potential codes that a crosswalk will allow an intial code to
become, the higher the entropy. The entropy (S) is given by \$\$S =
-\Sigma \Sigma p log p\$\$ where p = 1/n and n is the number of
potential codes. a single code can map to, natural logs are used in the
calculation. The inner summation can be is the sum of n iteration of
1/n, so the equation can be simplified to \$\$S = -\Sigma log p\$\$

## Usage

``` r
xwalk_entropy(x)
```

## Arguments

- x:

  The crosswalk

## Value

the entropy
