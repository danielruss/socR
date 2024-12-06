
#' Check if codes agree with reviewer
#'
#' Compares if a vector of codes is in a vector of reviewer
#' codes.
#'
#' Particularly useful when combined with purrr::map_lgl
#'
#' @param codes codes to compare
#' @param reviewer reviewer's code -- "gold" standard
#'
#' @return TRUE if the codes are in the reviewer otherwise FALSE
#' @export
#'
#' @examples
#' x <- '11-1011'
#' y <- c('11-1011','11-1031')
#' codesAgree(x,c("11-1011","11-1021"))
#' codesAgree(y,c("11-1021","11-1031"))
#' codesAgree(x,c("13-1011","11-1021"))
#' codesAgree(y,c("13-1011","11-1021"))
codesAgree <- function(codes, reviewer){
  return( any(codes %in% reviewer) )
}


#' Create a list column from multiple columns
#'
#' @description
#' This function replaces a set of input columns that you pass in with a list column
#' containing the values of input column on a row-by-row basis.
#'
#' @param df the data frame you are modifying
#' @param colname the name of the new column
#' @param ... the columns you are combining into a list column
#'
#' @return The original data frame with a new list column `colname` replacing
#' the columns given
#' @export
#'
#' @examples
#' df <- tibble::tibble(a_1=1:3,a_2=2:4,a_3=3:5,b=4:6) |> to_list_column(a,a_1,a_2,a_3)
to_list_column <- function(df,colname,...){
  cols=rlang::enquos(...)
  col_values <- dplyr::select(df, !!!cols)
  df <- df |> dplyr::mutate({{colname}} := purrr::pmap(col_values,\(...) {r=unique(c(...));r[!is.na(r)]}) )
  df |> dplyr::select(-c(...))
}
