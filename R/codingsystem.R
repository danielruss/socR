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
#' @param codes vector of codes, a dataframe containing the columns "code"
#' (with codes) and "title" (with titles), or a url/file path of a csv file
#' containing the codes and titles with header row containing at least "code"
#' and title.  Other columns may be present.
#' @param titles vector of title
#' @param name coding system name
#' @param ... additional parameters passed into read_csv
#'
#' @return the codingsystem object
#' @export
#'
codingsystem <- function(codes,titles,...,name=""){
    obj=list()

    if ( length(codes)==1 && (is_url(codes) || file.exists(codes)) ){
      codes <- rio::import(codes,setclass="tbl")
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
#' @description
#' These methods allow you to use the codingsystem like a tibble.
#' When using select, make sure you keep the code/title or else you
#' can break the functionality of the codingsystem.
#'
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
#'
#' @return a new codingsystem
#' @importFrom dplyr select
#' @rdname codingsystem_dplyr
#' @export
#'
select.codingsystem <- function(.data,...){
  data <- .data$table
  as_codingsystem(dplyr::select(data, ...),name=.data$name)
}

#' @rdname codingsystem_dplyr
#' @param name name for the filtered coding system
#' @importFrom dplyr filter
#' @export
filter.codingsystem <- function (.data, ..., name=NULL, .by = NULL, .preserve = FALSE) {
  data <- .data$table
  name <- ifelse(is.null(name),trimws(paste0("filtered ",.data$name)),name)
  dplyr::filter(data, ..., .by = .by, .preserve = .preserve) |> as_codingsystem(name)
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
  table_str <- format(x$table,...)[-1]
  table_str <- paste( table_str[grepl("^[^#]",table_str)], collapse="\n" )
  paste(pillar::style_subtle(paste0("# Coding System: ", x$name)), "\n", table_str)
}

#' @inherit utils::head
#' @export
head.codingsystem <- function(x,...){
  as_codingsystem(head(x$table,...),name=x$name)
}

#' @inherit utils::tail
#' @export
tail.codingsystem <- function(x,...){
  as_codingsystem(tail(x$table,...),name=x$name)
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

#' Create a coding system from a data frame
#'
#' @param x the data frame containing columns "code" and "title"
#' @param name coding system name
#' @param ... additional parameters
#'
#' @return a codingsystem object.
#' @export
#'
as_codingsystem <- function(x, name="", ...) {
  UseMethod("as_codingsystem")
}

#' @rdname as_codingsystem
#' @export
as_codingsystem.data.frame <- function(x,name="",...){
  codingsystem(x,name=name)
}

#' @rdname as_codingsystem
#' @export
as_codingsystem.codingsystem <- function(x,name="",...){
  x
}

#' to_level
#'
#' @description
#' A utility function for converting occupational codes to higher levels
#' in the hierarchy.
#'
#' @param codingsystem The coding system we are using
#' @param level The level in the coding system we want.  Should be a column name
#'  in the codingsystem table.
#'
#' @return a function that converts a vector of codes from a lower level
#'  to a the level input.
#' @export
#'
#' @examples
#' to_soc2010_2d <- to_level(soc2010_all, soc2d)
#' to_soc2010_2d(c("11-1011","15-1110"))
#'
to_level <- function(codingsystem, level) {
  if (is.data.frame(codingsystem)){
    codingsystem <- as_codingsystem(codingsystem)
  }
  col = rlang::enquo(level)
  col_name = rlang::quo_name(col)

  if (!is.codingsystem(codingsystem)){
    stop("Please provide a codingsystem object or a data frame containing columns 'code' and 'title' and  '",col_name,"'")
  }

  if (!rlang::has_name(codingsystem$table, col_name)) {
    message(col_name, " is not a level in ", codingsystem$name)
    return( invisible() )
  }

  function(codes) {
    map_vec = dplyr::pull(codingsystem$table, {{col}}, name = code)
    return(unname(map_vec[codes]))
  }
}
