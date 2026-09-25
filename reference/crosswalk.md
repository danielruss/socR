# Use the concordance table (crosswalk) to convert from one coding system to another.

Use the concordance table (crosswalk) to convert from one coding system
to another.

## Usage

``` r
crosswalk(codes, xwalk, invert = FALSE, unlist = FALSE)
```

## Arguments

- codes:

  the vector of codes that will be crosswalked

- xwalk:

  the concordance table.

- invert:

  by default the crosswalk goes from codes1 to codes2 setting invert to
  TRUE make the crosswalk go from codes2 to codes1

- unlist:

  instead of returning a list, return an unamed vector use it when
  crosswalking a dataframe column with mutate

## Value

an unnamed list of codes in the resulting coding system
