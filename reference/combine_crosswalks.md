# Combine two crosswalks to produce a new crosswalk

Takes two concordance tables (xw1 and xw2), where xw1 go from coding
system one to an intermediary coding system, and xw2 goes from the
intermediary coding system to coding system two. The goal is to make one
table that goes from coding system 1 to coding system 2.

## Usage

``` r
combine_crosswalks(xw1, xw2)
```

## Arguments

- xw1:

  \- crosswalk 1, either an xwalk object or a data.frame

- xw2:

  \- crosswalk 2, either an xwalk object or a data.frame

## Examples

``` r
# the noc_isco example has an extra column that confuses the parser,
# so I have to specify the parts or skip the last column.
noc_isco <- xwalk("https://danielruss.github.io/codingsystems/noc2011_isco2008.csv",
                   col_types = "cccc-")
isco_soc <- xwalk("https://danielruss.github.io/codingsystems/isco2008_soc2010.csv")
combine_crosswalks(noc_isco,isco_soc)
#> # Crosswalk: noc2011 <==> soc2010 
#>     noc2011 noc2011_title                                   soc2010 soc2010_title
#>    <chr>   <chr>                                           <chr>   <chr>        
#>  1 0011    Legislators                                     11-1011 Chief Execut…
#>  2 0011    Legislators                                     11-1031 Legislators  
#>  3 0012    Senior government managers and officials        11-1011 Chief Execut…
#>  4 0012    Senior government managers and officials        11-1021 General and …
#>  5 0012    Senior government managers and officials        11-9161 Emergency Ma…
#>  6 0013    Senior managers - financial, communications an… 11-1011 Chief Execut…
#>  7 0013    Senior managers - financial, communications an… 11-1021 General and …
#>  8 0014    Senior managers - health, education, social an… 11-1011 Chief Execut…
#>  9 0014    Senior managers - health, education, social an… 11-1021 General and …
#> 10 0014    Senior managers - health, education, social an… 11-2031 Public Relat…
#> # ℹ 2,655 more rows 
```
