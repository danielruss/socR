# Extended SOC 1980 complete classification system

The US SOC 1980 classification system can have higher level (major or
minor codes) codes without any children. We extended the SOC 1980
classification system to require all major codes (2-digit code) to have
at least 1 minor code (3-digit code ), and every minor codes to have at
least 1 unit code (4-digit code). The most detailed code is now always a
unit code.

## Usage

``` r
soc1980_extended
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

<https://danielruss.github.io/codingsystems/soc_1980_extended.csv>
