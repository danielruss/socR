# Get the code Level

Gets the levels for a vector of codes from a codingsystem The type
returned depends on the data.

## Usage

``` r
level(data, codes)

# S3 method for class 'codingsystem'
level(data, codes)
```

## Arguments

- data:

  \- a codingsystem

- codes:

  \- a vector of codes to check

## Value

a vector of Levels

## Examples

``` r
level(soc1980_all,"99-99") # "division"
#>      99-99 
#> "division" 
level(soc2010_all,c("11-1011","11-2010")) # c(6,5)
#> 11-1011 11-2010 
#>       6       5 
```
