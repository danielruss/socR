# Use Coding system with dplyr

These methods allow you to use the codingsystem like a tibble. When
using select, make sure you keep the code/title or else you can break
the functionality of the codingsystem.

## Usage

``` r
# S3 method for class 'codingsystem'
select(.data, ...)

# S3 method for class 'codingsystem'
filter(.data, ..., .by = NULL, .preserve = FALSE, name = NULL)

# S3 method for class 'codingsystem'
mutate(.data, ...)

# S3 method for class 'codingsystem'
arrange(.data, ..., .by_group = FALSE)

# S3 method for class 'codingsystem'
as_tibble(x, ..., .rows = NULL, .name_repair = NULL, rownames = NULL)

# S3 method for class 'codingsystem'
count(x, ..., wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- .data:

  the coding system

- ...:

  parts of the coding system

- .by:

  passed to dplyr::filter

- .preserve:

  passed to dplyr::filter

- name:

  name for the filtered coding system

- .by_group:

  passed to dplyr::arrange

- x:

  the coding system

- .rows:

  passed to dplyer::as_tibble

- .name_repair:

  passed to dplyer::as_tibble

- rownames:

  passed to dplyer::as_tibble

## Value

a new codingsystem
