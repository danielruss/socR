#' Is valid code
#'
#' check whether a code is valid
#'
#' valid_code is a functional that create a function that check
#' if a vector of codes is valid
#'
#' is_valid_4digit_soc1980, is_valid_6digit_soc2010 and is_valid_4digit_noc2011 were made using
#' valid_code functional.
#'
#' @param codeList a vector of valid codes
#' @param code a vector of codes to check against the vector of valid codes
#'
#' @return valid_code returns a function. The functions (e.g. is_valid_soc2010)
#' return a logic vector representing if the codes are valid.
#' @export
#'
#' @seealso [standardize_soc1980_codes()]
#'
#' @examples
#' is_valid_toy <- valid_code(c("A","B","C"))
#' is_valid_toy(c("X","A","Z","B"))
valid_code <- function(codeList){
  function(code){
    if (is.list(code)){
      # the user gave us a list of codes...
      x<-purrr::map(code,~.x %in% codeList)
    } else{
      # the user gave us a vector of codes
      x<-code %in% codeList
    }
    x
  }
}

#' @rdname valid_code
#' @export
is_valid_6digit_soc2010 <- valid_code(socR::soc2010_6digit$soc_code)

#' @rdname valid_code
#' @export
is_valid_4digit_noc2011<- valid_code(socR::noc2011_4digit$noc_code)

#' @rdname valid_code
#' @export
is_valid_soc1980<- valid_code(soc1980_all$soc1980_code)

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




