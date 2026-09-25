# Standardize US SOC 1980 codes

US SOC 1980 codes are often written in none stand form (e.g 4600 instead
of 46-47). This function attempt to standardize some of the ways SOC
1980 codes are written.

## Usage

``` r
standardize_soc1980_codes(codes)
```

## Arguments

- codes:

  vector of US SOC 1980 codes

## Value

standardized US SOC 1980 codes

## Details

the function trims leading and trailing zeros ("up to 2 trailing zero -
20 is a valid soc code)

## Examples

``` r
standardize_soc1980_codes(c("2000",'7600'))
#> [1] "20"    "75-76"
```
