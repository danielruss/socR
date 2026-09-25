# constructor create a coding system S3 class

constructor create a coding system S3 class

## Usage

``` r
codingsystem(codes, titles, ..., name = "")
```

## Arguments

- codes:

  vector of codes, a dataframe containing the columns "code" (with
  codes) and "title" (with titles), or a url/file path of a csv file
  containing the codes and titles with header row containing at least
  "code" and title. Other columns may be present.

- titles:

  vector of title

- ...:

  additional parameters passed into rio::import

- name:

  coding system name

## Value

the codingsystem object

## Examples

``` r

url <- "https://danielruss.github.io/codingsystems/naics2022_all.csv"
naic2022 <- codingsystem(url,name = "naics2022",
   colClasses=c(rep("character",2),"integer",rep("character",5)))
```
