
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
