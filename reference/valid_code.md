# Is valid code

check whether a code is valid

## Usage

``` r
valid_code(codeList)

is_valid_6digit_soc2010(code)

is_valid_soc1980(code)

is_most_detailed_soc1980(code)

is_valid_extended_soc1980(code)

is_most_detailed_extended_soc1980(code)
```

## Arguments

- codeList:

  a vector of valid codes

- code:

  codes to compare

## Value

valid_code returns a function. The functions (e.g. is_valid_soc2010)
take a code or a vector of codes and returns a logic vector representing
if the codes are valid.

## Details

valid_code is a functional that create a function that check if a vector
of codes is valid

is_valid_4digit_soc1980, is_valid_6digit_soc2010 and
is_valid_4digit_noc2011 were made using valid_code functional.

## See also

\[standardize_soc1980_codes()\]

## Examples

``` r
is_valid_toy <- valid_code(c("A","B","C"))
is_valid_toy(c("X","A","Z","B"))
#> [1] FALSE  TRUE FALSE  TRUE
```
