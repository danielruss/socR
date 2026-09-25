# Dimensions of an Object

Retrieve or set the dimension of an object.

## Usage

``` r
# S3 method for class 'codingsystem'
dim(x)
```

## Arguments

- x:

  an R object, for example a matrix, array or data frame.

## Value

For an array (and hence in particular, for a matrix) `dim` retrieves the
`dim` attribute of the object. It is `NULL` or a vector of mode
[`integer`](https://rdrr.io/r/base/integer.html).

The replacement method changes the `"dim"` attribute (provided the new
value is compatible) and removes any `"dimnames"` *and* `"names"`
attributes.

## Details

The functions `dim` and `dim<-` are [internal
generic](https://rdrr.io/r/base/InternalMethods.html)
[primitive](https://rdrr.io/r/base/Primitive.html) functions.

`dim` has a method for
[`data.frame`](https://rdrr.io/r/base/data.frame.html)s, which returns
the lengths of the `row.names` attribute of `x` and of `x` (as the
numbers of rows and columns respectively).

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`ncol`](https://rdrr.io/r/base/nrow.html),
[`nrow`](https://rdrr.io/r/base/nrow.html) and
[`dimnames`](https://rdrr.io/r/base/dimnames.html).

## Examples

``` r
x <- 1:12 ; dim(x) <- c(3,4)
x
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    7   10
#> [2,]    2    5    8   11
#> [3,]    3    6    9   12

# simple versions of nrow and ncol could be defined as follows
nrow0 <- function(x) dim(x)[1]
ncol0 <- function(x) dim(x)[2]
```
