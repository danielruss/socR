# Check if codes agree with reviewer

Compares if a vector of codes is in a vector of reviewer codes.

## Usage

``` r
codesAgree(codes, reviewer)
```

## Arguments

- codes:

  codes to compare

- reviewer:

  reviewer's code – "gold" standard

## Value

TRUE if the codes are in the reviewer otherwise FALSE

## Details

Particularly useful when combined with purrr::map_lgl

## Examples

``` r
x <- '11-1011'
y <- c('11-1011','11-1031')
codesAgree(x,c("11-1011","11-1021"))
#> [1] TRUE
codesAgree(y,c("11-1021","11-1031"))
#> [1] TRUE
codesAgree(x,c("13-1011","11-1021"))
#> [1] FALSE
codesAgree(y,c("13-1011","11-1021"))
#> [1] FALSE
```
