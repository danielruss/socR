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



#' Standardize US SOC 1980 codes
#'
#' US SOC 1980 codes are often written in none stand form (e.g 4600 instead of 46-47).  This function
#' attempt to standardize some of the ways SOC 1980 codes are written.
#'
#' the function trims leading and trailing zeros ("up to 2 trailing zero - 20 is a valid soc code)
#'
#' @param codes vector of US SOC 1980 codes
#'
#' @return standardized US SOC 1980 codes
#'
#' @export
#'
#' @examples
#' standardize_soc1980_codes(c("2000",'7600'))
#'
standardize_soc1980_codes <- function(codes){
  codes <- stringr::str_remove_all(codes,"^0+|00?$")

  code_map <-c(rep('12-13',2),rep('46-47',2),rep('73-74',2),rep('75-76',2),rep("162-3",2),rep("434-5",2),
               rep("525-6",2),rep("646-7",2),rep("681-2",2),rep("731-2",2),rep("746-7",2),rep('751-2',2),
               rep('766-7',2))
  names(code_map) <- c(12,13,46,47,73,74,75,76,162,163,434,435,525,526,647,647,681,682,731,732,746,747,751,752,766,767)

  dplyr::recode(codes,!!!code_map)
}
