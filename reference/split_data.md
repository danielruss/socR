# Split data

A simple deterministic mechanism for splitting data into training,
development, and test data based on the MD5 hash of a unused string
parameters.

## Usage

``` r
split_data(x, pTrain = 0.9, pDev = 0.09, pTest = 0.01)
```

## Arguments

- x:

  unused string data used to split the data

- pTrain:

  approximate percent of the training split

- pDev:

  approximate percent of the development split

- pTest:

  approximate percent of the test split

## Value

a vector of factors (Train,Dev,Test) denoting the data split

## Examples

``` r
split_data(rownames(mtcars))
#>  [1] train train train train train train train train train train train train
#> [13] train train train train train train train train train train train train
#> [25] test  train train train test  dev   train train
#> Levels: train dev test
```
