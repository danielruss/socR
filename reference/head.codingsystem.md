# Return the First or Last Parts of an Object

Returns the first or last parts of a vector, matrix, table, data frame
or function. Since [`head()`](https://rdrr.io/r/utils/head.html) and
[`tail()`](https://rdrr.io/r/utils/head.html) are generic functions,
they may also have been extended to other classes.

## Usage

``` r
# S3 method for class 'codingsystem'
head(x, ...)
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

head(iris3)
#> , , Setosa
#> 
#>      Sepal L. Sepal W. Petal L. Petal W.
#> [1,]      5.1      3.5      1.4      0.2
#> [2,]      4.9      3.0      1.4      0.2
#> [3,]      4.7      3.2      1.3      0.2
#> [4,]      4.6      3.1      1.5      0.2
#> [5,]      5.0      3.6      1.4      0.2
#> [6,]      5.4      3.9      1.7      0.4
#> 
#> , , Versicolor
#> 
#>      Sepal L. Sepal W. Petal L. Petal W.
#> [1,]      7.0      3.2      4.7      1.4
#> [2,]      6.4      3.2      4.5      1.5
#> [3,]      6.9      3.1      4.9      1.5
#> [4,]      5.5      2.3      4.0      1.3
#> [5,]      6.5      2.8      4.6      1.5
#> [6,]      5.7      2.8      4.5      1.3
#> 
#> , , Virginica
#> 
#>      Sepal L. Sepal W. Petal L. Petal W.
#> [1,]      6.3      3.3      6.0      2.5
#> [2,]      5.8      2.7      5.1      1.9
#> [3,]      7.1      3.0      5.9      2.1
#> [4,]      6.3      2.9      5.6      1.8
#> [5,]      6.5      3.0      5.8      2.2
#> [6,]      7.6      3.0      6.6      2.1
#> 
head(iris3, c(6L, 2L))
#> , , Setosa
#> 
#>      Sepal L. Sepal W.
#> [1,]      5.1      3.5
#> [2,]      4.9      3.0
#> [3,]      4.7      3.2
#> [4,]      4.6      3.1
#> [5,]      5.0      3.6
#> [6,]      5.4      3.9
#> 
#> , , Versicolor
#> 
#>      Sepal L. Sepal W.
#> [1,]      7.0      3.2
#> [2,]      6.4      3.2
#> [3,]      6.9      3.1
#> [4,]      5.5      2.3
#> [5,]      6.5      2.8
#> [6,]      5.7      2.8
#> 
#> , , Virginica
#> 
#>      Sepal L. Sepal W.
#> [1,]      6.3      3.3
#> [2,]      5.8      2.7
#> [3,]      7.1      3.0
#> [4,]      6.3      2.9
#> [5,]      6.5      3.0
#> [6,]      7.6      3.0
#> 
head(iris3, c(6L, -1L, 2L))
#> , , Setosa
#> 
#>      Sepal L. Sepal W. Petal L.
#> [1,]      5.1      3.5      1.4
#> [2,]      4.9      3.0      1.4
#> [3,]      4.7      3.2      1.3
#> [4,]      4.6      3.1      1.5
#> [5,]      5.0      3.6      1.4
#> [6,]      5.4      3.9      1.7
#> 
#> , , Versicolor
#> 
#>      Sepal L. Sepal W. Petal L.
#> [1,]      7.0      3.2      4.7
#> [2,]      6.4      3.2      4.5
#> [3,]      6.9      3.1      4.9
#> [4,]      5.5      2.3      4.0
#> [5,]      6.5      2.8      4.6
#> [6,]      5.7      2.8      4.5
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

tail(iris3)
#> , , Setosa
#> 
#>       Sepal L. Sepal W. Petal L. Petal W.
#> [45,]      5.1      3.8      1.9      0.4
#> [46,]      4.8      3.0      1.4      0.3
#> [47,]      5.1      3.8      1.6      0.2
#> [48,]      4.6      3.2      1.4      0.2
#> [49,]      5.3      3.7      1.5      0.2
#> [50,]      5.0      3.3      1.4      0.2
#> 
#> , , Versicolor
#> 
#>       Sepal L. Sepal W. Petal L. Petal W.
#> [45,]      5.6      2.7      4.2      1.3
#> [46,]      5.7      3.0      4.2      1.2
#> [47,]      5.7      2.9      4.2      1.3
#> [48,]      6.2      2.9      4.3      1.3
#> [49,]      5.1      2.5      3.0      1.1
#> [50,]      5.7      2.8      4.1      1.3
#> 
#> , , Virginica
#> 
#>       Sepal L. Sepal W. Petal L. Petal W.
#> [45,]      6.7      3.3      5.7      2.5
#> [46,]      6.7      3.0      5.2      2.3
#> [47,]      6.3      2.5      5.0      1.9
#> [48,]      6.5      3.0      5.2      2.0
#> [49,]      6.2      3.4      5.4      2.3
#> [50,]      5.9      3.0      5.1      1.8
#> 
tail(iris3, c(6L, 2L))
#> , , Setosa
#> 
#>       Petal L. Petal W.
#> [45,]      1.9      0.4
#> [46,]      1.4      0.3
#> [47,]      1.6      0.2
#> [48,]      1.4      0.2
#> [49,]      1.5      0.2
#> [50,]      1.4      0.2
#> 
#> , , Versicolor
#> 
#>       Petal L. Petal W.
#> [45,]      4.2      1.3
#> [46,]      4.2      1.2
#> [47,]      4.2      1.3
#> [48,]      4.3      1.3
#> [49,]      3.0      1.1
#> [50,]      4.1      1.3
#> 
#> , , Virginica
#> 
#>       Petal L. Petal W.
#> [45,]      5.7      2.5
#> [46,]      5.2      2.3
#> [47,]      5.0      1.9
#> [48,]      5.2      2.0
#> [49,]      5.4      2.3
#> [50,]      5.1      1.8
#> 
tail(iris3, c(6L, -1L, 2L))
#> , , Versicolor
#> 
#>       Sepal W. Petal L. Petal W.
#> [45,]      2.7      4.2      1.3
#> [46,]      3.0      4.2      1.2
#> [47,]      2.9      4.2      1.3
#> [48,]      2.9      4.3      1.3
#> [49,]      2.5      3.0      1.1
#> [50,]      2.8      4.1      1.3
#> 
#> , , Virginica
#> 
#>       Sepal W. Petal L. Petal W.
#> [45,]      3.3      5.7      2.5
#> [46,]      3.0      5.2      2.3
#> [47,]      2.5      5.0      1.9
#> [48,]      3.0      5.2      2.0
#> [49,]      3.4      5.4      2.3
#> [50,]      3.0      5.1      1.8
#> 

## iris with dimnames stripped
a3d <- iris3 ; dimnames(a3d) <- NULL
tail(a3d, c(6, -1, 2)) # keepnums = TRUE is default here!
#> , , 2
#> 
#>       [,2] [,3] [,4]
#> [45,]  2.7  4.2  1.3
#> [46,]  3.0  4.2  1.2
#> [47,]  2.9  4.2  1.3
#> [48,]  2.9  4.3  1.3
#> [49,]  2.5  3.0  1.1
#> [50,]  2.8  4.1  1.3
#> 
#> , , 3
#> 
#>       [,2] [,3] [,4]
#> [45,]  3.3  5.7  2.5
#> [46,]  3.0  5.2  2.3
#> [47,]  2.5  5.0  1.9
#> [48,]  3.0  5.2  2.0
#> [49,]  3.4  5.4  2.3
#> [50,]  3.0  5.1  1.8
#> 
tail(a3d, c(6, -1, 2), keepnums = FALSE)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  2.7  4.2  1.3
#> [2,]  3.0  4.2  1.2
#> [3,]  2.9  4.2  1.3
#> [4,]  2.9  4.3  1.3
#> [5,]  2.5  3.0  1.1
#> [6,]  2.8  4.1  1.3
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]  3.3  5.7  2.5
#> [2,]  3.0  5.2  2.3
#> [3,]  2.5  5.0  1.9
#> [4,]  3.0  5.2  2.0
#> [5,]  3.4  5.4  2.3
#> [6,]  3.0  5.1  1.8
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
