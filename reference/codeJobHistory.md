# Auto-code job results with SOCcer using via the soccer API

This codes one job at a time. In order to code multiple jobs, you can
create a tibble (data frame) and use pmap_dfr to produce a results
tibble similar to the web-based version of SOCcer
(https://soccer.nci.nih.gov)

## Usage

``` r
codeJobHistory(title, task = "", industry = "", ..., n = 10)
```

## Arguments

- title:

  The job title

- task:

  tasks performed on the job

- industry:

  industry (SIC 1987 code)

- ...:

  (not used)

- n:

  the number of soc codes to return (default)

## Value

a tibble consisting of the title/task/industry and the top n SOCcer
results and scores

## Details

Please use the web-based version for handling large jobs.

## Examples

``` r
if (FALSE) { # \dontrun{
soccer_results <- codeJobHistory("epidemiologist")
jobs <- tibble::tibble(title=c("chemist","farmer","data scientist"),
                       task=rep("",3),industry=rep("",3))
soccer_results_3 <- purrr::pmap_dfr(jobs,codeJobHistory,n=20)
} # }
```
