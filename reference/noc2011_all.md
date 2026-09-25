# noc2011 4 digit classification system

Canadian 4 digit National Occupational Classification (NOC) 2011

## Usage

``` r
noc2011_all
```

## Format

- code:

  a 1-4-digit code formated like '0011', be careful must be a string not
  an integer

- title:

  a short definition of the code

- Level:

  Unofficial name for the level in the hierarchy (number of digits) for
  the code, 1, 2, 3, or 4

- Hierarchical_structure:

  Official name for the level in the hierarchy

- noc1d:

  the 1-digit noc code associated with the code

- noc2d:

  the 2-digit noc code associated with the code, is NA for 1-digit codes

- noc3d:

  the 3-digit noc code associated with the code, is NA for 1- or 2-digit
  codes

- noc4d:

  the 4-digit noc code associated with the code, is NA for 1-, 2-, or
  3-digit codes

## Source

<https://danielruss.github.io/codingsystems/noc_2011_4d.csv>

<https://www.statcan.gc.ca/eng/subjects/standard/noc/2011/index>
