# to_level

A utility function for converting occupational codes to higher levels in
the hierarchy.

## Usage

``` r
to_level(codingsystem, level)
```

## Arguments

- codingsystem:

  The coding system we are using

- level:

  The level in the coding system we want. Should be a column name in the
  codingsystem table.

## Value

a function that converts a vector of codes from a lower level to a the
level input.

## Examples

``` r
to_soc2010_2d <- to_level(soc2010_all, soc2d)
to_soc2010_2d(c("11-1011","15-1110"))
#> [1] "11-0000" "15-0000"
```
