# Return the First or Last Parts of an Object

Returns the first or last parts of a vector, matrix, array, table, data
frame or function. Since [`head()`](https://rdrr.io/r/utils/head.html)
and [`tail()`](https://rdrr.io/r/utils/head.html) are generic functions,
they have been extended to other classes, including
`"`[`ts`](https://rdrr.io/r/stats/ts.html)`"` from stats.

## Usage

``` r
# S3 method for class 'codingsystem'
tail(x, ...)
```

## Arguments

- x:

  an object

- ...:

  arguments to be passed to or from other methods.

## Value

An object (usually) like `x` but generally smaller. Hence, for
[`array`](https://rdrr.io/r/base/array.html)s, the result corresponds to
`x[.., drop=FALSE]`. For [`ftable`](https://rdrr.io/r/stats/ftable.html)
objects `x`, a transformed `format(x)`.

## Details

For vector/array based objects,
[`head()`](https://rdrr.io/r/utils/head.html)
([`tail()`](https://rdrr.io/r/utils/head.html)) returns a subset of the
same dimensionality as `x`, usually of the same class. For historical
reasons, by default they select the first (last) 6 indices in the first
dimension ("rows") or along the length of a non-dimensioned vector, and
the full extent (all indices) in any remaining dimensions.
[`head.matrix()`](https://rdrr.io/r/utils/head.html) and
[`tail.matrix()`](https://rdrr.io/r/utils/head.html) are exported.

The default and array(/matrix) methods for
[`head()`](https://rdrr.io/r/utils/head.html) and
[`tail()`](https://rdrr.io/r/utils/head.html) are quite general. They
will work as is for any class which has a
[`dim()`](https://rdrr.io/r/base/dim.html) method, a
[`length()`](https://rdrr.io/r/base/length.html) method (only required
if [`dim()`](https://rdrr.io/r/base/dim.html) returns `NULL`), and a `[`
method (that accepts the `drop` argument and can subset in all
dimensions in the dimensioned case).

For functions, the lines of the deparsed function are returned as
character strings.

When `x` is an array(/matrix) of dimensionality two and more,
[`tail()`](https://rdrr.io/r/utils/head.html) will add dimnames similar
to how they would appear in a full printing of `x` for all dimensions
`k` where `n[k]` is specified and non-missing and `dimnames(x)[[k]]` (or
`dimnames(x)` itself) is `NULL`. Specifically, the form of the added
dimnames will vary for different dimensions as follows:

- `k=1` (rows): :

  `"[n,]"` (right justified with whitespace padding)

- `k=2` (columns): :

  `"[,n]"` (with *no* whitespace padding)

- `k>2` (higher dims): :

  `"n"`, i.e., the indices as *character* values

Setting `keepnums = FALSE` suppresses this behaviour.

As [`data.frame`](https://rdrr.io/r/base/data.frame.html) subsetting
(‘indexing’) keeps
[`attributes`](https://rdrr.io/r/base/attributes.html), so do the
[`head()`](https://rdrr.io/r/utils/head.html) and
[`tail()`](https://rdrr.io/r/utils/head.html) methods for data frames.

The auxiliary function `.checkHT(d, n)` is useful in `head(x, n)` or
`tail(x, n)` methods, checking validity of `d <- dim(x)` and `n`.

## Note

For array inputs the output of `tail` when `keepnums` is `TRUE`, any
dimnames vectors added for dimensions `>2` are the original numeric
indices in that dimension *as character vectors*. This means that, e.g.,
for 3-dimensional array `arr`, `tail(arr, c(2,2,-1))[ , , 2]` and
`tail(arr, c(2,2,-1))[ , , "2"]` may both be valid but have completely
different meanings.

## Author

Patrick Burns, improved and corrected by R-Core. Negative argument added
by Vincent Goulet. Multi-dimension support added by Gabriel Becker.

## Examples

``` r
head(letters)
#> [1] "a" "b" "c" "d" "e" "f"
head(letters, n = -6L)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t"

head(freeny.x, n = 10L)
#>       lag quarterly revenue price index income level market potential
#>  [1,]               8.79636     4.70997      5.82110          12.9699
#>  [2,]               8.79236     4.70217      5.82558          12.9733
#>  [3,]               8.79137     4.68944      5.83112          12.9774
#>  [4,]               8.81486     4.68558      5.84046          12.9806
#>  [5,]               8.81301     4.64019      5.85036          12.9831
#>  [6,]               8.90751     4.62553      5.86464          12.9854
#>  [7,]               8.93673     4.61991      5.87769          12.9900
#>  [8,]               8.96161     4.61654      5.89763          12.9943
#>  [9,]               8.96044     4.61407      5.92574          12.9992
#> [10,]               9.00868     4.60766      5.94232          13.0033
head(freeny.y)
#>         Qtr1    Qtr2    Qtr3    Qtr4
#> 1962         8.79236 8.79137 8.81486
#> 1963 8.81301 8.90751 8.93673        

head(gait) # 3d array
#> , , Variable = Hip Angle
#> 
#>        Subject
#> Time    boy1 boy2 boy3 boy4 boy5 boy6 boy7 boy8 boy9 boy10 boy11 boy12 boy13
#>   0.025   37   47   46   37   20   57   46   46   46    35    38    35    34
#>   0.075   36   46   44   36   18   48   38   46   42    34    37    31    31
#>   0.125   33   42   39   27   11   44   33   43   37    29    33    29    27
#>   0.175   29   34   34   20    8   35   25   40   34    28    29    26    23
#>   0.225   23   27   33   15    7   31   18   36   31    19    26    22    19
#>   0.275   18   21   27   15    5   27   15   30   25    15    20    19    15
#>        Subject
#> Time    boy14 boy15 boy16 boy17 boy18 boy19 boy20 boy21 boy22 boy23 boy24 boy25
#>   0.025    43    43    40    51    52    36    35    46    43    55    39    37
#>   0.075    41    37    41    49    46    33    37    38    41    51    38    34
#>   0.125    36    35    36    45    41    28    33    30    37    47    31    30
#>   0.175    31    28    32    39    35    22    27    23    30    41    27    27
#>   0.225    26    26    27    31    31    18    22    17    24    35    21    26
#>   0.275    20    21    20    23    24    13    14    13    16    30    14    19
#>        Subject
#> Time    boy26 boy27 boy28 boy29 boy30 boy31 boy32 boy33 boy34 boy35 boy36 boy37
#>   0.025    36    36    42    38    46    54    52    32    46    46    48    44
#>   0.075    33    33    40    34    47    48    44    28    41    44    42    41
#>   0.125    28    30    40    30    44    44    44    26    38    40    42    38
#>   0.175    22    28    34    23    37    37    33    22    31    35    35    32
#>   0.225    18    21    23    17    29    30    28    19    25    31    30    24
#>   0.275    13    15    15    12    23    27    27    13    20    25    23    18
#>        Subject
#> Time    boy38 boy39
#>   0.025    55    48
#>   0.075    56    50
#>   0.125    51    47
#>   0.175    46    42
#>   0.225    41    37
#>   0.275    36    29
#> 
#> , , Variable = Knee Angle
#> 
#>        Subject
#> Time    boy1 boy2 boy3 boy4 boy5 boy6 boy7 boy8 boy9 boy10 boy11 boy12 boy13
#>   0.025   10   16   18    5    2   15   13   14   15     9    13     7     9
#>   0.075   15   25   27   14    6   17   16   17   20    22    24     8    14
#>   0.125   18   28   32   16    6   23   22   18   23    25    27    11    16
#>   0.175   18   25   32   17    6   23   17   19   26    21    23    12    15
#>   0.225   15   18   28   10    5   20   12   19   25    10    18     8    15
#>   0.275   14   12   23    8    6   19    9   15   21     9    13     6    12
#>        Subject
#> Time    boy14 boy15 boy16 boy17 boy18 boy19 boy20 boy21 boy22 boy23 boy24 boy25
#>   0.025    15     6    11    24    16    16     7    21    11    12     8    11
#>   0.075    20    11    19    32    20    20    13    24    14    17    12    20
#>   0.125    22    20    30    35    21    22    14    25    14    20    14    22
#>   0.175    22    18    28    33    20    21    17    21    11    20    13    21
#>   0.225    21    13    25    29    18    20    14    16     8    18    12    21
#>   0.275    19     9    17    24    14    20     8     9     5    12     9    17
#>        Subject
#> Time    boy26 boy27 boy28 boy29 boy30 boy31 boy32 boy33 boy34 boy35 boy36 boy37
#>   0.025    16    19    13    11    17    20    18     9     8     9    13    19
#>   0.075    20    26    23    15    25    20    18    12    10    18    18    23
#>   0.125    22    28    30    19    30    22    25    16    17    19    27    26
#>   0.175    21    28    28    20    30    16    23    15    16    19    26    25
#>   0.225    20    24    19    18    27    10    18    14    12    19    25    21
#>   0.275    20    18    10    17    22    10    19    11    10    15    18    18
#>        Subject
#> Time    boy38 boy39
#>   0.025    16    14
#>   0.075    23    25
#>   0.125    28    32
#>   0.175    28    34
#>   0.225    25    30
#>   0.275    21    20
#> 
head(gait, c(6L, 2L))
#> , , Variable = Hip Angle
#> 
#>        Subject
#> Time    boy1 boy2
#>   0.025   37   47
#>   0.075   36   46
#>   0.125   33   42
#>   0.175   29   34
#>   0.225   23   27
#>   0.275   18   21
#> 
#> , , Variable = Knee Angle
#> 
#>        Subject
#> Time    boy1 boy2
#>   0.025   10   16
#>   0.075   15   25
#>   0.125   18   28
#>   0.175   18   25
#>   0.225   15   18
#>   0.275   14   12
#> 
head(gait, c(6L, 2L, -1L))
#> , , Variable = Hip Angle
#> 
#>        Subject
#> Time    boy1 boy2
#>   0.025   37   47
#>   0.075   36   46
#>   0.125   33   42
#>   0.175   29   34
#>   0.225   23   27
#>   0.275   18   21
#> 

tail(letters)
#> [1] "u" "v" "w" "x" "y" "z"
tail(letters, n = -6L)
#>  [1] "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"
#> [20] "z"

tail(freeny.x)
#>       lag quarterly revenue price index income level market potential
#> [34,]               9.69405     4.30909      6.17369          13.1459
#> [35,]               9.69958     4.30909      6.16135          13.1520
#> [36,]               9.68683     4.30552      6.18231          13.1593
#> [37,]               9.71774     4.29627      6.18768          13.1579
#> [38,]               9.74924     4.27839      6.19377          13.1625
#> [39,]               9.77536     4.27789      6.20030          13.1664
## the bottom-right "corner" :
tail(freeny.x, n = c(4, 2))
#>       income level market potential
#> [36,]      6.18231          13.1593
#> [37,]      6.18768          13.1579
#> [38,]      6.19377          13.1625
#> [39,]      6.20030          13.1664
tail(freeny.y)
#>         Qtr1    Qtr2    Qtr3    Qtr4
#> 1970                 9.69958 9.68683
#> 1971 9.71774 9.74924 9.77536 9.79424

tail(gait)
#> , , Variable = Hip Angle
#> 
#>        Subject
#> Time    boy1 boy2 boy3 boy4 boy5 boy6 boy7 boy8 boy9 boy10 boy11 boy12 boy13
#>   0.725   31   34   34   23   27   32   31   35   29    39    36    35    27
#>   0.775   38   42   43   31   36   40   37   44   39    45    44    41    34
#>   0.825   43   47   51   33   34   49   46   49   47    48    47    44    40
#>   0.875   44   48   53   35   33   55   45   49   51    48    45    45    41
#>   0.925   40   45   50   34   32   56   46   47   52    43    42    40    39
#>   0.975   35   43   49   32   28   55   41   43   47    39    40    39    36
#>        Subject
#> Time    boy14 boy15 boy16 boy17 boy18 boy19 boy20 boy21 boy22 boy23 boy24 boy25
#>   0.725    37    35    38    33    32    22    34    36    33    41    37    31
#>   0.775    41    43    49    44    42    32    35    48    42    52    47    40
#>   0.825    44    49    57    51    46    39    40    55    48    57    53    43
#>   0.875    44    50    59    55    48    41    43    57    48    61    53    43
#>   0.925    41    45    54    56    49    38    43    56    48    63    49    38
#>   0.975    37    46    46    51    46    34    42    50    46    58    44    38
#>        Subject
#> Time    boy26 boy27 boy28 boy29 boy30 boy31 boy32 boy33 boy34 boy35 boy36 boy37
#>   0.725    22    26    43    22    39    50    43    30    34    40    37    36
#>   0.775    32    37    51    32    48    56    52    36    45    48    45    44
#>   0.825    39    44    57    38    52    61    58    39    53    53    52    49
#>   0.875    41    47    58    41    48    59    59    36    57    53    53    46
#>   0.925    38    44    54    41    43    57    57    30    55    50    52    38
#>   0.975    34    37    46    40    42    58    52    29    43    47    46    35
#>        Subject
#> Time    boy38 boy39
#>   0.725    31    51
#>   0.775    43    59
#>   0.825    52    63
#>   0.875    56    64
#>   0.925    59    61
#>   0.975    59    55
#> 
#> , , Variable = Knee Angle
#> 
#>        Subject
#> Time    boy1 boy2 boy3 boy4 boy5 boy6 boy7 boy8 boy9 boy10 boy11 boy12 boy13
#>   0.725   70   70   79   67   71   65   71   79   65    77    75    72    71
#>   0.775   66   66   77   61   66   66   68   76   67    70    69    66    70
#>   0.825   57   55   67   43   44   61   65   61   61    55    50    51    62
#>   0.875   40   39   46   18   23   45   40   35   47    36    25    30    45
#>   0.925   22   23   22    3    5   21   17   13   26    17    13     8    26
#>   0.975   11   16   14    0    4    9    3    5   10    16     6     5    12
#>        Subject
#> Time    boy14 boy15 boy16 boy17 boy18 boy19 boy20 boy21 boy22 boy23 boy24 boy25
#>   0.725    73    71    79    75    73    74    71    80    72    69    76    73
#>   0.775    68    65    77    72    72    71    65    80    73    69    71    66
#>   0.825    53    48    67    60    61    60    58    68    64    63    55    49
#>   0.875    32    25    48    43    43    41    39    48    46    44    26    24
#>   0.925    16     5    23    26    22    23    20    27    28    28     8     6
#>   0.975    10     8    13    16    11    15    10    16    13     8    12     9
#>        Subject
#> Time    boy26 boy27 boy28 boy29 boy30 boy31 boy32 boy33 boy34 boy35 boy36 boy37
#>   0.725    74    78    81    71    75    72    77    71    76    80    77    82
#>   0.775    71    80    75    70    68    63    69    67    77    75    75    76
#>   0.825    60    74    61    56    46    47    60    54    71    59    64    59
#>   0.875    41    57    42    36    21    23    38    30    54    34    44    31
#>   0.925    23    35    25    19     6     9    17     9    26    10    23     7
#>   0.975    15    18    16    11     8    19    14     6     3     9    16     7
#>        Subject
#> Time    boy38 boy39
#>   0.725    69    82
#>   0.775    71    76
#>   0.825    62    65
#>   0.875    45    46
#>   0.925    28    25
#>   0.975    20    15
#> 
tail(gait, c(6L, 2L))
#> , , Variable = Hip Angle
#> 
#>        Subject
#> Time    boy38 boy39
#>   0.725    31    51
#>   0.775    43    59
#>   0.825    52    63
#>   0.875    56    64
#>   0.925    59    61
#>   0.975    59    55
#> 
#> , , Variable = Knee Angle
#> 
#>        Subject
#> Time    boy38 boy39
#>   0.725    69    82
#>   0.775    71    76
#>   0.825    62    65
#>   0.875    45    46
#>   0.925    28    25
#>   0.975    20    15
#> 
tail(gait, c(6L, 2L, -1L))
#> , , Variable = Knee Angle
#> 
#>        Subject
#> Time    boy38 boy39
#>   0.725    69    82
#>   0.775    71    76
#>   0.825    62    65
#>   0.875    45    46
#>   0.925    28    25
#>   0.975    20    15
#> 

## gait without dimnames --> keepnums showing original row/col numbers
a3 <- gait ; dimnames(a3) <- NULL
tail(a3, c(6, 2, -1))# keepnums = TRUE is default here!
#> , , 2
#> 
#>       [,38] [,39]
#> [15,]    69    82
#> [16,]    71    76
#> [17,]    62    65
#> [18,]    45    46
#> [19,]    28    25
#> [20,]    20    15
#> 
tail(a3, c(6, 2, -1),  keepnums = FALSE)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]   69   82
#> [2,]   71   76
#> [3,]   62   65
#> [4,]   45   46
#> [5,]   28   25
#> [6,]   20   15
#> 

## data frame w/ a (non-standard) attribute:
treeS <- structure(trees, foo = "bar")
(n <- nrow(treeS))
#> [1] 31
stopifnot(exprs = { # attribute is kept
    identical(htS <- head(treeS), treeS[1:6, ])
    identical(attr(htS, "foo") , "bar")
    identical(tlS <- tail(treeS), treeS[(n-5):n, ])
    ## BUT if I use "useAttrib(.)", this is *not* ok, when n is of length 2:
    ## --- because [i,j]-indexing of data frames *also* drops "other" attributes ..
    identical(tail(treeS, 3:2), treeS[(n-2):n, 2:3] )
})

tail(library) # last lines of function
#>                                    
#> 373         return(y)              
#> 374     }                          
#> 375     if (logical.return)        
#> 376         TRUE                   
#> 377     else invisible(.packages())
#> 378 }                              

head(stats::ftable(Titanic))
#>                                                
#>                           "Survived" "No" "Yes"
#>  "Class" "Sex"    "Age"                        
#>  "1st"   "Male"   "Child"               0     5
#>                   "Adult"             118    57
#>          "Female" "Child"               0     1
#>                   "Adult"               4   140
#>  "2nd"   "Male"   "Child"               0    11
#>                   "Adult"             154    14

## 1d-array (with named dim) :
a1 <- array(1:7, 7); names(dim(a1)) <- "O2"
stopifnot(exprs = {
  identical( tail(a1, 10), a1)
  identical( head(a1, 10), a1)
  identical( head(a1, 1), a1 [1 , drop=FALSE] ) # was a1[1] in R <= 3.6.x
  identical( tail(a1, 2), a1[6:7])
  identical( tail(a1, 1), a1 [7 , drop=FALSE] ) # was a1[7] in R <= 3.6.x
})
```
