# Getting Started with socR

I use socR to handle almost everyday to handle common tasks that involve
occupational or industrial codes. The most common task I have involves
dealing with coding systems. This vignette is designed to show you how I
do common tasks with socR.

## Create a coding system

This is often the first thing you have to do. I save my coding system
data on github pages. It is public data, feel free to use it. If you
want to add a coding system to my github repository, let me know. As
long as there are no licensing issues, I’ll be happy to add it.

As an example, I will load the soc2000 system from
<https://danielruss.github.io/codingsystems/soc2000_all.csv>. I actually
use soc2010 in my work, but that comes with socR, as does a few other I
that use often.

``` r

library(socR)
#> 
#> Attaching package: 'socR'
#> The following object is masked from 'package:stats':
#> 
#>     filter

soc2000_all <- codingsystem("https://danielruss.github.io/codingsystems/soc2000_all.csv",name="soc2000")
soc2000_all
#> # Coding System: soc2000 
#>     code    title     Level Hierarchical_structure parent soc2d soc3d soc5d soc6d
#>    <chr>   <chr>     <int> <chr>                  <chr>  <chr> <chr> <chr> <chr>
#>  1 11-0000 Manageme…     2 Major Group            NA     11-0… NA    NA    NA   
#>  2 11-1000 Top Exec…     3 Minor Group            11-00… 11-0… 11-1… NA    NA   
#>  3 11-1010 Chief Ex…     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  4 11-1011 Chief Ex…     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  5 11-1020 General …     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  6 11-1021 General …     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  7 11-1030 Legislat…     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  8 11-1031 Legislat…     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  9 11-2000 Advertis…     3 Minor Group            11-00… 11-0… 11-2… NA    NA   
#> 10 11-2010 Advertis…     5 Broad Occupation       11-20… 11-0… 11-2… 11-2… NA   
#> # ℹ 1,379 more rows
```

A coding system is an S3 class that wraps a tibble. The coding system is
**required** to have a column name `code` and a column named `title`.
The other columns are optional, however, if you want to move up the code
hierarchy having the additional columns are useful. In this example,
soc2000 has `Level` which corresponds to the number of digits in the
code (not counting trailing zeros, e.g. 11-0000 is a 2-digit code
(Level=2) and 11-1010 is a 5-digit code Level=5). The `parent` column is
the immediate parent in the heirarchy of a coding system. The columns
`soc2d` through `soc6d` are the codes at the various levels. My
codingsystem use `NA` to mark cases that don’t exist (e.g. the soc6d for
11-0000). The codingsystem also has a name that is printed out for your
use.

Here is the soc2010 coding system that comes with socR. There is also a
soc2010_6d, which is deprecated and will be removed soon since you can
create it from by filtering soc2010_all.

``` r

soc2010_all
#> # Coding System: soc2010 
#>     code    title     Level Hierarchical_structure parent soc2d soc3d soc5d soc6d
#>    <chr>   <chr>     <dbl> <chr>                  <chr>  <chr> <chr> <chr> <chr>
#>  1 11-0000 Manageme…     2 Major Group            NA     11-0… NA    NA    NA   
#>  2 11-1000 Top Exec…     3 Minor Group            11-00… 11-0… 11-1… NA    NA   
#>  3 11-1010 Chief Ex…     5 Broad Group            11-10… 11-0… 11-1… 11-1… NA   
#>  4 11-1011 Chief Ex…     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  5 11-1020 General …     5 Broad Group            11-10… 11-0… 11-1… 11-1… NA   
#>  6 11-1021 General …     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  7 11-1030 Legislat…     5 Broad Group            11-10… 11-0… 11-1… 11-1… NA   
#>  8 11-1031 Legislat…     6 Detailed Occupation    11-10… 11-0… 11-1… 11-1… 11-1…
#>  9 11-2000 Advertis…     3 Minor Group            11-00… 11-0… 11-2… NA    NA   
#> 10 11-2010 Advertis…     5 Broad Group            11-20… 11-0… 11-2… 11-2… NA   
#> # ℹ 1,415 more rows
```

## Changing to higher level codes

Given a vector of soc codes, you may want to convert them to 2-digit
socs. In order to do this we use a function factory method to create the
appropriate function.

``` r

## create a function to convert a vector of codes to a the 2-digit level
## notice we are uses the column name that contains the 2-digit socs for
## each code
to_2d <- to_level(soc2000_all,soc2d)

to_2d(c("11-1021","11-1031"))
#> [1] "11-0000" "11-0000"

## lets do it for a tibble...
my_data <- tibble::tibble(resp_id=c("A13254","A33122"),soc2000=c("11-1021","11-1031")) |>
  dplyr::mutate(soc2000_2d=to_2d(soc2000))
my_data
#> # A tibble: 2 × 3
#>   resp_id soc2000 soc2000_2d
#>   <chr>   <chr>   <chr>     
#> 1 A13254  11-1021 11-0000   
#> 2 A33122  11-1031 11-0000
```

## Checking for invalid codes

Sometimes you want to check if your data has invalid codes. socR has a
few ways of checking codes. If you have a coding system, you can create
a function using a provided factory method `valid_code` which takes
either a coding system or a vector of codes. This is why the data had to
have a column named `code`, the codingsystem knows which column is the
code column and can create a list of all the valid codes for you. If you
want, you could replace the codingsystem object with a vector of valid
codes

``` r

is_valid_soc2000 <- valid_code(soc2000_all)
is_valid_soc2000( c("11-0000","11","11-1021","11-1030") )
#> [1]  TRUE FALSE  TRUE  TRUE
```

## Filtering a coding system

Sometime you are not interested in the entire coding system, but only
the codes at a particular level. Since a codingsystem is a thin wrapper
around a tibble, you can use some of the `dplyr` verbs (select and
filter – I can add others if needed). Now you see why I named the
variable `soc2000_all`. *If you get odd errors when you filter, you may
be using the wrong filter function*. The stats package, which is loaded
by default, has a filter method.

``` r

soc2000_5d <- soc2000_all |> dplyr::filter(Level == 5,name="soc2000_5d")
soc2000_5d
#> # Coding System: soc2000_5d 
#>     code    title     Level Hierarchical_structure parent soc2d soc3d soc5d soc6d
#>    <chr>   <chr>     <int> <chr>                  <chr>  <chr> <chr> <chr> <chr>
#>  1 11-1010 Chief Ex…     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  2 11-1020 General …     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  3 11-1030 Legislat…     5 Broad Occupation       11-10… 11-0… 11-1… 11-1… NA   
#>  4 11-2010 Advertis…     5 Broad Occupation       11-20… 11-0… 11-2… 11-2… NA   
#>  5 11-2020 Marketin…     5 Broad Occupation       11-20… 11-0… 11-2… 11-2… NA   
#>  6 11-2030 Public R…     5 Broad Occupation       11-20… 11-0… 11-2… 11-2… NA   
#>  7 11-3010 Administ…     5 Broad Occupation       11-30… 11-0… 11-3… 11-3… NA   
#>  8 11-3020 Computer…     5 Broad Occupation       11-30… 11-0… 11-3… 11-3… NA   
#>  9 11-3030 Financia…     5 Broad Occupation       11-30… 11-0… 11-3… 11-3… NA   
#> 10 11-3040 Human Re…     5 Broad Occupation       11-30… 11-0… 11-3… 11-3… NA   
#> # ℹ 439 more rows

## you can check for valid 5-digit soc codes
is_valid_5digit_soc2010 <- valid_code(soc2000_5d)

is_valid_5digit_soc2010( c("11-0000","11","11-1021","11-1030") )
#> [1] FALSE FALSE FALSE  TRUE
```

If you need a dplyr verb that I don’t support, if you ask I might be
able to add it.  
Otherwise, the work around is to get the tibble from the codingsystem
which is the `table` entry of the S3 codingsystem object. Since you now
have a tibble, you can continue working with it as any other tibble, or
convert it back to a codingsystem using the `as_codingsystem` function.
You will need to give the codingsystem a name, or it will default to
something useless like coding system.

``` r

soc2000_3d <- soc2000_all$table |> dplyr::filter(Level == 3) |>
  as_codingsystem(name="soc2000_3d")
soc2000_3d
#> # Coding System: soc2000_3d 
#>     code    title     Level Hierarchical_structure parent soc2d soc3d soc5d soc6d
#>    <chr>   <chr>     <int> <chr>                  <chr>  <chr> <chr> <chr> <chr>
#>  1 11-1000 Top Exec…     3 Minor Group            11-00… 11-0… 11-1… NA    NA   
#>  2 11-2000 Advertis…     3 Minor Group            11-00… 11-0… 11-2… NA    NA   
#>  3 11-3000 Operatio…     3 Minor Group            11-00… 11-0… 11-3… NA    NA   
#>  4 11-9000 Other Ma…     3 Minor Group            11-00… 11-0… 11-9… NA    NA   
#>  5 13-1000 Business…     3 Minor Group            13-00… 13-0… 13-1… NA    NA   
#>  6 13-2000 Financia…     3 Minor Group            13-00… 13-0… 13-2… NA    NA   
#>  7 15-1000 Computer…     3 Minor Group            15-00… 15-0… 15-1… NA    NA   
#>  8 15-2000 Mathemat…     3 Minor Group            15-00… 15-0… 15-2… NA    NA   
#>  9 17-1000 Architec…     3 Minor Group            17-00… 17-0… 17-1… NA    NA   
#> 10 17-2000 Engineers     3 Minor Group            17-00… 17-0… 17-2… NA    NA   
#> # ℹ 86 more rows
```
