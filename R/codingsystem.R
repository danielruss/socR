#' constructor create a coding system S3 class
#'
#' @param codes vector of codes or a
#' @param titles vector of title
#' @param name coding system name
#'
#' @return the codingsystem object
#' @export
#'
#' @examples
#'
#' codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' codingsystem(soc2010_all,"US SOC 2010")
codingsystem <- function(codes,titles,name=""){
    obj=list()
    if (is.data.frame(codes) && all(c("code","title") %in% colnames(codes)) ){
      obj$table <- codes
    }else{
      obj$table <- tibble::tibble(code=codes,title=titles)
    }
    obj$name=name
    attr(obj, "class") <- "codingsystem"
    obj
}

#' checks if an object is a coding system
#'
#' @description
#' Is this object a coding system
#'
#' @param x object to test
#' @examples
#' x <- codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' is.codingsystem(x)
#' @export
is.codingsystem <- function(x) inherits(x,"codingsystem")

#' Check if a set of codes are valid for a coding system
#'
#' @param code vector of codes to check
#' @param system  the coding system
#'
#' @return boolean vector corresponding to whether the codes are in the coding system
#' @export
#'
#' @examples
#' soc2010 <- codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' is_valid( c("11-2031","Fred","11-3031"),soc2010 )
is_valid <- function(code,system){
  if (!is.codingsystem(system)) stop("system is not a codingsystem")
  code %in% system$table$code
}


#' Returns the user assigned name of the coding system
#'
#' @param system coding system
#'
#' @return  the name of the coding system (may be blank)
#' @export
#'
#' @examples
#' soc2010 <- codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' name(soc2010)
name <- function(system){
  system$name
}


#' Look up code
#'
#' @param x list of codes to lookup
#' @param system the coding system
#'
#' @return a vector of titles for the codes
#' @export
#'
#' @examples
#' soc2010 <- codingsystem(soc2010_6digit$soc_code,soc2010_6digit$title,"US SOC 2010 6 digit")
#' lookup_code( c("11-2031","Fred","11-3031") , soc2010)
lookup_code<-function(x,system){
  system$table$title[match(x,system$table$code)]
}
