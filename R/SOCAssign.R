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
  a1 <- DBI::dbReadTable(con,"ASSIGNMENTS_TABLE") %>% tibble::as_tibble()
  r1 <- DBI::dbReadTable(con,"RESULTS_TABLE") %>% tibble::as_tibble()
  h1 <- DBI::dbReadTable(con,"HEAD_TABLE")
  DBI::dbDisconnect(con)
  rm(con)

  h1 <- h1 %>% dplyr::mutate(column=.data$column-1)
  x1 <- a1 %>% dplyr::select(.data$ROW,.data$CODE1:.data$CODE3) %>% tidyr::pivot_longer(.data$CODE1:.data$CODE3) %>%
    dplyr::filter(nchar(.data$value)>0) %>%  dplyr::rename(Id=.data$ROW)
  x2 <- r1 %>% dplyr::left_join(h1,by="column") %>% purrr::set_names("row","column","value","title") %>%
    dplyr::select(.data$row,.data$value,.data$title) %>%
    tidyr::pivot_wider(values_from = .data$value,names_from = .data$title) %>%
    dplyr::select(-.data$row) %>% dplyr::mutate(Id=as.integer(.data$Id)) %>%
    tidyr::pivot_longer(.data$JobTitle:.data$Score_10)
  x2 <-  dplyr::bind_rows(x1,x2)

  x2 <- tidyr::pivot_wider(x2,names_from = .data$name,values_from = .data$value) %>%
    dplyr::select(.data$Id, .data$JobTitle,.data$SIC,.data$JobTask,.data$CODE1:.data$CODE3)
  if (addSrc){
    x2 <-  dplyr::mutate(x2,src=basename(fname))
  }
  return(x2)
}
