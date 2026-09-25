# Using dplyr verbs with crosswalks

These methods allow you to work with the underlying data within a
crosswalk as if was a tibble.

## Usage

``` r
# S3 method for class 'xwalk'
filter(.data, ..., .by = NULL, .preserve = FALSE)

# S3 method for class 'xwalk'
arrange(.data, ..., .by_group = FALSE)

# S3 method for class 'xwalk'
as_tibble(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
)
```

## Arguments

- .data:

  The crosswalk

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the variables in `.data`. If multiple expressions are included, they
  are combined with the `&` operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

- x:

  A data frame, list, matrix, or other object that could reasonably be
  coerced to a tibble.

- .rows:

  The number of rows, useful to create a 0-column tibble or just as an
  additional check.

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence,

  - `"unique"`: Make sure names are unique and not empty,

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`,

  - `"universal"`: Make the names `unique` and syntactic

  - `"unique_quiet"`: Same as `"unique"`, but "quiet"

  - `"universal_quiet"`: Same as `"universal"`, but "quiet"

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base R).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

  This argument is passed on as `repair` to
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).
  See there for more details on these terms and the strategies used to
  enforce them.

- rownames:

  How to treat existing row names of a data frame or matrix:

  - `NULL`: remove row names. This is the default.

  - `NA`: keep row names.

  - A string: the name of a new column. Existing rownames are
    transferred into this column and the `row.names` attribute is
    deleted. No name repair is applied to the new column name, even if
    `x` already contains a column of that name. Use
    `as_tibble(rownames_to_column(...))` to safeguard against this case.

  Read more in
  [rownames](https://tibble.tidyverse.org/reference/rownames.html).
