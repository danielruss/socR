# Convert a column to a specified type

Convert a column in `x$table` to a base R type and return the updated
coding system.

## Usage

``` r
convert_column_type(x, col, type)

# S3 method for class 'codingsystem'
convert_column_type(x, col, type)
```

## Arguments

- x:

  A `codingsystem` object.

- col:

  An unquoted column name in `x$table` to convert. A quoted column name
  is also accepted. To supply a name stored in a variable, use `!!`, for
  example `!!column_name`.

- type:

  A character string specifying the target type: one of `"integer"`,
  `"character"`, `"double"`, or `"logical"`. Unambiguous abbreviations
  are accepted.

## Value

A `codingsystem` object with the specified column in `table` converted
to the requested type.

## Examples

``` r
cs <- codingsystem(data.frame(code = "1", title = "Example", Level = "2"))
convert_column_type(cs, Level, "integer")
#> # Coding System:  
#>  Example
#> 2 
```
