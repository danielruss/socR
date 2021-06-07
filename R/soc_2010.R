#' is a soc 2010 code valid?
#'
#' checks to see if a soc 2010 6 digit code is valid
#' @param code a character vector of soc 2010 codes
#' @return a boolean vector
is_valid_6d_soc2010 <- function(code){
  code %in% socR::soc2010_6digit$soc_code
}

#' is a noc 2011 code valid?
#'
#' checks to see if a noc 2011 4 digit code is valid
#' @param code a character vector of noc 2011 codes
#' @return a boolean vector
is_valid_4d_noc2011 <- function(code){
  code %in% socR::noc2011_4digit$noc_code
}
