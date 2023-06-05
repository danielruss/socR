#' load_socassign_db
#'
#' load the data from a SOCAssign SQLite database
#'
#' @param fname the SOCAssign db file
#' @param addSrc should I add the file name as an src column
#'
#' @return tibble with coder results
#' @export
#'
#' @importFrom rlang .data
load_socassign_db <- function(fname,addSrc=FALSE){
  message("loading ",fname)
  con <- DBI::dbConnect(RSQLite::SQLite(), fname)
  a1 <- DBI::dbReadTable(con,"ASSIGNMENTS_TABLE") %>% tibble::as_tibble() |> purrr::set_names("ROW","coder_1","coder_2","coder_3","FLAG","COMMENT")
  r1 <- DBI::dbReadTable(con,"RESULTS_TABLE") %>% tibble::as_tibble()
  ## having problems with capitalization of the headers. SOC2010_n and SCORE_n
  ## so convert them all to lower case...
  h1 <- DBI::dbReadTable(con,"HEAD_TABLE") |> dplyr::mutate(value=gsub("^(SCORE|SOC)","\\L\\1",.data$value,perl = T))
  DBI::dbDisconnect(con)
  rm(con)

  h1 <- h1 %>% dplyr::mutate(column=.data$column-1)
  x2 <- r1 %>% dplyr::left_join(h1,by="column") %>% purrr::set_names("row","column","value","title") %>%
    dplyr::select("row","value","title") %>%
    tidyr::pivot_wider(values_from = .data$value,names_from = .data$title) %>%
    dplyr::select(-.data$row) %>% dplyr::mutate(Id=as.integer(.data$Id))

    x2 <- x2 |> dplyr::left_join(a1,by=c("Id"="ROW"))
  if (addSrc){
    x2 <-  dplyr::mutate(x2,src=basename(fname))
  }
  return(x2)
}
