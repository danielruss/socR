# Complete SOC 2010 classification system

The complete US SOC 2010 classification system. This data contains all
the codes regardless of the code level.

## Usage

``` r
soc2010_all
```

## Format

- code:

  the soc 2010 code

- title:

  a short definition of the code

- Level:

  The number of significant digits in the code

- Hierarchical_structure:

  The name of the level

- parent:

  the parent of the soc code, note: 2 digit soc code dont have parents

- soc2d:

  for any soc code, what is the 2-digit code

- soc3d:

  for any soc code, what is the 3-digit code. Is NA for 2-digit codes.

- soc5d:

  for any soc code, what is the 5-digit code. Is NA for 2- and 3-digit
  codes.

- soc6d:

  for any soc code, what is the 6-digit code. Is NA for 2-, 3-, and
  5-digit codes.

## Source

<https://danielruss.github.io/codingsystems/soc2010_all.csv>
