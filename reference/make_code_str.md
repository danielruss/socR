# convert a list column of codes to vector of string for display

convert a list column of codes to vector of string for display

## Usage

``` r
make_code_str(x)
```

## Arguments

- x:

  codes column

## Value

a vector a string concatenating all the codes

## Examples

``` r
df <- tibble::tibble(soc2010_codes = list(c("11-1011","11-1021"),c("11-1000")))
df <- dplyr::mutate(df,code_str=make_code_str(soc2010_codes))
```
