# Detailed SOC 1980 classification system

The US SOC 1980 classification system can have higher level (major or
minor codes) codes without any children. This data contains all the most
detailed codes regardless of the code level.

## Usage

``` r
soc1980_detailed
```

## Format

- soc1980_code:

  the soc 1980 code

- title:

  a short definition of the code

- Level:

  the level of the soc 1980 code

- parent:

  the parent of the soc 1980 code, note: at the division level, the
  parent is 0000

- division:

  for any soc 1980 code, what is the division code

- major:

  for any soc 1980 code, what is the major code. Is NA for division
  codes.

- minor:

  for any soc 1980 code, what is the minor code. Is NA for division and
  major codes.

- unit:

  for any soc 1980 code, what is the unit code. Is NA for non-unit
  codes.

## Source

<https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv>
