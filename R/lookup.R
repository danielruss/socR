#' Look up code
#'
#' @param x list of codes to lookup
#' @param valid_codes the set of legal codes, if an invalid codes is pass the function will return NA/
#' @param titles the set of title matching the codes.
#'
#' @return a vector of titles for the codes
#' @export
#'
#' @examples
#' x1 <- c("11-2031","Fred","11-3031")
#' lookup_code(x1,soc2010_6digit$soc_code,soc2010_6digit$title)
lookup_code<-function(x,valid_codes,titles){
  titles[match(x,valid_codes)]
}
