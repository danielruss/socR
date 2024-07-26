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
  if (is.codingsystem(codeList)) {
    codeList=codeList$table$code
  }

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
is_valid_6digit_soc2010 <- valid_code(socR::soc2010_6digit$code)

#' @rdname valid_code
#' @export
is_valid_4digit_noc2011<- valid_code(socR::noc2011_4digit$code)

#' @rdname valid_code
#' @export
is_valid_soc1980<- valid_code(socR::soc1980_all$code)

#' @rdname valid_code
#' @export
is_most_detailed_soc1980<- valid_code(socR::soc1980_detailed$code)

#' @rdname valid_code
#' @export
is_valid_extended_soc1980<- valid_code(socR::soc1980_extended$soc1980_code)

#' @rdname valid_code
#' @export
is_most_detailed_extended_soc1980<- valid_code(socR::soc1980_extended$unit[!is.na(socR::soc1980_extended$unit)])

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
  ## convert 9XYY to 99 unless you have 91YY
  codes <- stringr::str_replace(codes,"^9[^1]\\d+","99")
  ## convert XX00 to XX or XXX0 to XXX
  codes <- stringr::str_replace(codes,"(\\d\\d)00?","\\1")

  code_map <-c(rep('12-13',2),rep('46-47',2),rep('73-74',2),rep('75-76',2),rep("162-3",2),rep("434-5",2),
               rep("525-6",2),rep("646-7",2),rep("681-2",2),rep("731-2",2),rep("746-7",2),rep('751-2',2),
               rep('766-7',2))
  names(code_map) <- c(12,13,46,47,73,74,75,76,162,163,434,435,525,526,647,647,681,682,731,732,746,747,751,752,766,767)

  dplyr::recode(codes,!!!code_map)
}



#' Extended SOC 1980 codes
#'
#' Takes valid 1980 standardized codes (the ones in the book) and extends
#' them so that unit codes are always the most detailed (even if it is exactly
#' the same as the parent code.)
#'
#' @param codes The codes we are extending
#'
#' @return extended codes.
#' @export
#'
extend_standard_soc1980_codes <- function(codes){
  invalid_soc_codes=codes[ !is_valid_soc1980(codes) ]
  if (length(invalid_soc_codes)>0 ) {
    stop('all codes must be standardized soc codes: \ninvalid codes: ',invalid_soc_codes)
  }
  code_level = codes %>% purrr::map_chr(~socR::soc1980_extended %>% dplyr::filter(soc1980_code==.x) %>%
                                          dplyr::pull(Level) )
  xtd_codes = purrr::map2(codes,code_level,
                   function(x,y){
                     socR::soc1980_extended %>%
                       dplyr::filter(!!rlang::sym(y)==x, !is.na(.data$unit)) %>%
                       dplyr::pull(.data$unit) %>% as.character
                   })
  ncodes = purrr::map_int(xtd_codes,length)
  xtd_code1 = purrr::map_chr(xtd_codes,dplyr::first)

  dplyr::if_else(ncodes>1,codes,xtd_code1)
}

