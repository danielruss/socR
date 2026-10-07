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
#' @param ... additional parameters passed into rio::import
#'
#' @return the codingsystem object
#'
#' @examples
#'
#' url <- "https://danielruss.github.io/codingsystems/naics2022_all.csv"
#' naic2022 <- codingsystem(url,name = "naics2022",
#'    colClasses=c(rep("character",2),"integer",rep("character",5)))
#'
#' @export
#'
codingsystem <- function(codes,titles,...,name=""){
    obj=list()

    if ( length(codes)==1 && (is_url(codes) || file.exists(codes)) ){
      codes <- rio::import(codes,setclass="tbl",...)
      #if ("Level" %in% names(codes)) {
      #  codes <- codes |> dplyr::mutate(Level = as.integer(Level))
      #}
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


#' Find sibling codes within a hierarchical coding system
#'
#' Given a code from a hierarchical coding system (e.g. NOC or SOC), returns
#' all other codes that share the same immediate parent. If the target code
#' has no siblings at its own level (i.e. it is an "only child"), the
#' function falls back to returning first cousins -- codes at the same
#' level that share the same grandparent instead.
#'
#' @param target_code A character string giving the code to find siblings
#'   for. Must be a valid code within \code{system}.
#' @param system A \code{codingsystem} object (as validated by
#'   \code{is.codingsystem}) containing a \code{table} element with, at
#'   minimum, \code{code} and \code{parent} columns.
#'
#' @return A character vector of sibling (or, failing that, first-cousin)
#'   codes. Returns \code{character(0)} if \code{target_code} has no parent,
#'   or if it has no parent and no grandparent from which cousins could be
#'   derived.
#'
#' @details
#' The search proceeds in two steps:
#' \enumerate{
#'   \item \strong{Siblings}: codes sharing \code{target_code}'s immediate
#'     parent (excluding \code{target_code} itself).
#'   \item \strong{Cousins}: if no siblings are found, codes sharing a
#'     parent with \code{target_code}'s parent (i.e. sharing a grandparent),
#'     excluding the parent itself. Because these are children of the
#'     parent's own siblings, they are automatically at the same
#'     hierarchical level as \code{target_code}.
#' }
#' The function does not climb beyond the grandparent level; if no
#' siblings or cousins are found there, it returns \code{character(0)}.
#'
#' @examples
#' \dontrun{
#' siblings("0013", noc2011_all)  # same-parent siblings
#' siblings("0311", noc2011_all)  # falls back to cousins, since 0311
#'                                 # is an only child under its parent
#' }
#'
#' @export
siblings <- function(target_code,system){
  stopifnot(is.codingsystem(system))
  stopifnot(is_valid(target_code,system))

  ## get the target code's level and parent
  tbl <- system$table
  target_code_rows <- tbl |> dplyr::filter(code==target_code)
  parent_code <- as.character(target_code_rows$parent)

  # no parent.. no siblings...
  if (!is_valid(parent_code,system)){
    return(character(0))
  }

  # siblings have the same parent....
  sibs <- tbl[tbl$parent==parent_code & !is.na(tbl$parent) & tbl$code != target_code,]$code
  if (length(sibs) > 0) return(sibs)

  ## there are no siblings.. look for first-cousins - same grandparent
  parent_code_row <- tbl |> dplyr::filter(code==parent_code)
  grandparent_code <- as.character(parent_code_row$parent)
  if (!is_valid(grandparent_code,system)){
    return(character(0))
  }
  parent_sibs <- tbl[tbl$parent == grandparent_code & !is.na(tbl$parent) & tbl$code != parent_code, ]$code
  cousins <- tbl[tbl$parent %in% parent_sibs,]$code

  cousins
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
filter.codingsystem <- function (.data, ...,.by = NULL, .preserve = FALSE, name=NULL) {
  dots = rlang::enquos(...)
  name= name %||% trimws(paste0("filtered ", .data$name))
  data <- .data$table
  dplyr::filter(data, !!!dots, .by = .by, .preserve = .preserve) |> as_codingsystem(name)
}

#' @rdname codingsystem_dplyr
#' @param name name for the filtered coding system
#' @importFrom dplyr mutate
#' @export
mutate.codingsystem <- function (.data, ...) {
  data <- .data$table
  print(head(data))
  dplyr::mutate(data, ...) |> as_codingsystem(.data$name)
}

#' @rdname codingsystem_dplyr
#' @importFrom dplyr mutate
#' @param .by_group passed to dplyr::arrange
#' @export
arrange.codingsystem <- function (.data, ..., .by_group = FALSE) {
  dplyr::arrange(.data$table, ..., .by_group = .by_group) |> as_codingsystem()
}

#' @rdname codingsystem_dplyr
#' @importFrom dplyr as_tibble
#' @export
as_tibble.codingsystem <- function(x,...,.rows=NULL,.name_repair=NULL,rownames=NULL){
  x$table
}

#' @importFrom dplyr count
#' @rdname codingsystem_dplyr
#' @export
count.codingsystem <- function(x,...,wt = NULL, sort = FALSE, name = NULL){
  dplyr::count(x$table,..., wt = NULL, sort = FALSE, name = NULL)
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

#' @inherit base::dim
#' @export
dim.codingsystem <- function(x){
  dim(x$table)
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

#' @export
#' @importFrom dplyr pull
pull.codingsystem <- function(.data,var=-1,name=NULL,...){
  dplyr::pull(.data$table,!!rlang::enquo(var),!!rlang::enquo(name),...)
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


#' Get the code Level
#'
#' Gets the levels for a vector of codes from a codingsystem
#' The type returned depends on the data.
#'
#' @param data - a codingsystem
#' @param codes - a vector of codes to check
#'
#' @returns a vector of Levels
#' @export
#'
#' @examples
#' level(soc1980_all,"99-99") # "division"
#' level(soc2010_all,c("11-1011","11-2010")) # c(6,5)
level <- function(data,codes) {
  UseMethod("level")
}
#' @rdname level
#' @export
level.codingsystem <- function(data,codes){
  if (!inherits(data,"codingsystem")) stop("Expected a 'codingsystem' object")
  map <- data$table |> pull(.data$Level,.data$code)
  map[codes]
}

#' Convert a column to a specified type
#'
#' Convert a column in \code{x} to a base R type and return the
#' updated coding system.
#'
#' @param x A \code{codingsystem} object.
#' @param col An unquoted column name in \code{x} to convert.
#'   A quoted column name is also accepted. 
#' @param type A character string specifying the target type: one of
#'   \code{"integer"}, \code{"character"}, \code{"double"}, or
#'   \code{"logical"}. Unambiguous abbreviations are accepted.
#'
#' @return A \code{codingsystem} object with the specified column
#'   converted to the requested type.
#'
#' @examples
#' cs <- codingsystem("https://danielruss.github.io/codingsystems/isco1988_all.csv",colClasses="character") 
#' convert_column_type(cs, Level, "integer")
#' @export
convert_column_type <- function(x,col,type) UseMethod("convert_column_type")

#' @rdname convert_column_type
#' @export
convert_column_type.codingsystem <- function(x,col,type){
  col <- rlang::as_name(rlang::ensym(col))
  type <- match.arg(type, c("integer","character","double","logical"))

  x$table[[col]] <- switch(type,
    integer = as.integer(x$table[[col]]),
    character = as.character(x$table[[col]]),
    double = as.numeric(x$table[[col]]),
    logical = as.logical(x$table[[col]])
  )

  x
}
