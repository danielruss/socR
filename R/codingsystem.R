#' Check if a value is a url by looking
#' for the http(s)://
#'.Works with vectors...
#'
#' @param x String to check
#'
#' @return logical vector TRUE if the x is a url False otherwise
#' @export
#'
is_url <- function(x){
  grepl("^(http|https)://", x)
}

#' constructor create a coding system S3 class
#'
#' @param codes vector of codes or a dataframe containing the columns "code" (with codes) and "title" (with titles)
#' @param titles vector of title
#' @param name coding system name
#'
#' @return the codingsystem object
#' @export
#'
codingsystem <- function(codes,titles,name=""){
    obj=list()

    if ( length(codes)==1 && is_url(codes)){
      codes = readr::read_csv(codes,show_col_types = FALSE)
    }

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
#'
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
name <- function(system){
  system$name
}

#' Load a coding system from a url or a path
#'
#' @param url a url or a path to the coding system data
#' @param name the name for the codingsystem
#'
#' @return a codingsystem with data from the usl
#' @export
#'
load_codingsystem<-function(url,name){
  tbl <- rio::import(url,setclass = "tbl")
  codingsystem(tbl,name=name)
}

#' Look up code
#'
#' @param x list of codes to lookup
#' @param system the coding system
#'
#' @return a vector of titles for the codes
#' @export
#'
lookup_code<-function(x,system){
  stopifnot(is.codingsystem(system))
  system$table$title[match(x,system$table$code)]
}

#' Use Coding system with dplyr
#'
#' @param .data  the coding system
#' @param x  the coding system
#' @param ...  parts of the coding system
#' @param .by passed to dplyr::filter
#' @param .preserve passed to dplyr::filter
#' @param .rows passed to dplyer::as_tibble
#' @param .name_repair passed to dplyer::as_tibble
#' @param rownames passed to dplyer::as_tibble
#'
#' @return a tibble
#' @importFrom dplyr select
#' @rdname codingsystem_dplyr
#' @export
#'
select.codingsystem <- function(.data,...){
  data <- .data$table
  # Apply dplyr::select() to the data
  # note: this does not return a coding system...
  dplyr::select(data, ...)
}

#' @rdname codingsystem_dplyr
#' @importFrom dplyr filter
#' @export
filter.codingsystem <- function(.data,...,.by=NULL,.preserve=FALSE){
  data <- .data$table
  # Apsply dplyr::select() to the data
  # note: this does not return a coding system...
  dplyr::filter(data, ...,.by=.by,.preserve=.preserve)
}

#' @rdname codingsystem_dplyr
#' @importFrom dplyr as_tibble
#' @export
as_tibble.codingsystem <- function(x,...,.rows=NULL,.name_repair=NULL,rownames=NULL){
  x$table
}

#' formats a codingsystem
#'
#' @param x - the codingsystem
#' @param ... not currently used
#'
#' @return a formatted character vector
#' @export
#'
format.codingsystem <- function(x,...){
  grey58 = crayon::make_style(rgb(.58,.58,.58))
  table_str <- format(x$table,...)[-1]
  table_str <- paste( table_str[grepl("^[^#]",table_str)], collapse="\n" )
  paste(grey58("# \U2139 Coding System: ", x$name), "\n", table_str)
}

#' Get a list of codes from a coding system
#'
#' @param .codingsystem either a codingsystem or a tibble that has a a column
#' named "code".
#'
#' @return a vector of codes
#' @export
#'
get_codes <-function(.codingsystem){
  x <- c()
  if (is.codingsystem(.codingsystem)){
    x<-.codingsystem$table$code
  } else if(is.data.frame(.codingsystem) && "code" %in% colnames(.codingsystem)){
    x<-.codingsystem$code
  }
  unique(x)
}

#' prints a codingsystem
#'
#' @param x - the codingsystem
#' @param ... parameter for format, not currently used
#'
#' @export
#'
print.codingsystem <- function(x,...){
  cat(format(x,...), "\n")
  invisible(x)
}
