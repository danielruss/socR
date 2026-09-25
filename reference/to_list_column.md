# Create a list column from multiple columns

This function replaces a set of input columns that you pass in with a
list column containing the values of input column on a row-by-row basis.

## Usage

``` r
to_list_column(df, colname, ...)
```

## Arguments

- df:

  the data frame you are modifying

- colname:

  the name of the new column

- ...:

  the columns you are combining into a list column

## Value

The original data frame with a new list column \`colname\` replacing the
columns given

## Examples

``` r
df <- tibble::tibble(a_1=1:3,a_2=2:4,a_3=3:5,b=4:6) |> to_list_column(a,a_1,a_2,a_3)
```
