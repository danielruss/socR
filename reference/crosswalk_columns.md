# Crosswalk multiple columns in a tibble/data frame

If you have a data frame of data with multiple columns that need to be
crosswalked, use this in a pipe.

## Usage

``` r
crosswalk_columns(.data, xwalk, new_column_name, ..., unnest_results = TRUE)
```

## Arguments

- .data:

  job data

- xwalk:

  crosswalk going from code system 1 to coding system 2

- new_column_name:

  the column name for the results if the results are unnested, the
  results will be colname_1, colname_2 ... colname_n, otherwise the
  results are a list column with name new_column_name.

- ...:

  Columns that need to be crosswalked

- unnest_results:

  default=TRUE, should the results be separated into individual columns
  or else as a single list column

## Value

a crosswalked tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
a <- tibble::tibble(id=c("job-1","job-2"),soc2010_1=c("11-1011","11-2011"),
     soc2010_2=c("11-1021",NA))
xw <- socR::xwalk("https://danielruss.github.io/codingsystems/soc2010_soc2018.csv")
a |> crosswalk_columns(xw,soc2018_xw,soc2010_1,soc2010_2)
a |> crosswalk_columns(xw,soc2018_xw,soc2010_1,soc2010_2,unnest_results=FALSE)
} # }
```
