valid_code <- function(codeList){
  funct<-function(code){
    if (is.list(code)){
      # the user gave us a list of codes...
      x<-purrr::map(code,~.x %in% codeList)
    } else{
      # the user gave us a vector of codes
      x<-code %in% codeList
    }
    x
  }
  funct
}

#' Is a code valid 6-digit soc 2010 code?
#'
#' @param code  a character vector of soc 2010 codes
#'
#' @return a boolean vector
#' @export
is_valid_6digit_soc2010 <- valid_code(soc2010_6digit$soc_code)

#' Is a code valid 4-digit noc 2011 code?
#'
#' checks to see if a noc 2011 4 digit code is valid
#' @param code a character vector of noc 2011 codes
#' @return a boolean vector
#' @export
is_valid_4digit_noc2011<- valid_code(noc2011_4digit$noc_code)

