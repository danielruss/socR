# xwalk class constructor

takes a data frame (the crosswalk) and which columns are the codes and
titles and create an xwalk object that can perform crosswalks...

## Usage

``` r
xwalk(
  dta,
  codes1,
  titles1,
  codes2,
  titles2,
  col_types = ifelse(grepl("\\.xlsx?$", dta), "text", "c"),
  ...
)
```

## Arguments

- dta:

  the data frame of the crosswalk, or the filename/URL of a csv
  crosswalk file or the filename of an excel file.

- codes1:

  Codes for the (Default) input coding system for crosswalking

- titles1:

  Titles for the (Default) input coding system.

- codes2:

  Codes for the (Default) output coding system for crosswalking

- titles2:

  Titles for the (Default) output coding system.

- col_types:

  set the default col_type parameter for read_csv/read_excel

- ...:

  additional parameters passed to read_csv
