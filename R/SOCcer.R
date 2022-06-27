SOCcerURL = "https://sitf-raft3imjaq-uc.a.run.app/soccer/code"

#' Auto-code job results with SOCcer using via the soccer API
#'
#' This codes one job at a time.  In order to code multiple jobs,
#' you can create a tibble (data frame) and use pmap_dfr to produce
#' a results tibble similar to the web-based version of SOCcer (https://soccer.nci.nih.gov)
#'
#' Please use the web-based version for handling large jobs.
#'
#' @param title The job title
#' @param task  tasks performed on the job
#' @param industry industry (SIC 1987 code)
#' @param ... (not used)
#' @param n   the number of soc codes to return (default)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return a tibble consisting of the title/task/industry and the top n SOCcer results and scores
#' @export
#'
#' @examples
#' \dontrun{
#' soccer_results <- codeJobHistory("epidemiologist")
#' jobs <- tibble::tibble(title=c("chemist","farmer","data scientist"),
#'                        task=rep("",3),industry=rep("",3))
#' soccer_results_3 <- purrr::pmap_dfr(jobs,codeJobHistory,n=20)
#'}
codeJobHistory <- function(title,task="",industry="",...,n=10){

  if (nchar(title)<1) stop("SOCcer requires a job title")
  if (n<1) n=1
  dta=tibble::tibble(title=title,task=task,industry=industry)


  url=paste0(SOCcerURL,"?title=",utils::URLencode(title) )
  url=ifelse( (nchar(task)>1) ,yes=paste0(url,"&task=",utils::URLencode(task) ),no=url)
  url=ifelse(nchar(industry)>1, paste0(url,"&industry=",utils::URLencode(industry)),url)
  url=paste0(url,"&n=",n)

  raw <- httr::GET(url)
  httr::warn_for_status(raw)
  httr::content(raw)
  res <- httr::content(raw,as = "parsed",type="application/json") %>%
   purrr::map_dfr(~.x) %>% dplyr::mutate(rank=dplyr::row_number()) %>%
   dplyr::rename(soc2010=.data$code) %>%
   tidyr::pivot_wider(id_cols = c(.data$soc2010,.data$score),values_from = c(.data$soc2010,.data$score),names_from = .data$rank)
  cols <- colnames(res)
  cols <- cols[order(as.integer(stringr::str_extract(cols,"\\d+$")))]
  res <- res[,cols]
  cbind(dta,res)
}

