#' Combine two crosswalks to produce a new crosswalk
#'
#' Takes two concordance tables (xw1 and xw2), where xw1 go from coding system one
#' to an intermediary coding system, and xw2 goes from the intermediary coding system
#' to coding system two.  The goal is to make one table that goes from coding system 1 to
#' coding system 2.
#'
#'
#' @param xw1 - crosswalk table 1
#' @param xw2 - crosswalk table 2
#' @param xw1_from - the column in the xw1 table for the starting coding system
#' @param xw1_to - the column in the xw1 table for the intermediary coding system
#' @param xw2_from - the column in the xw2 table for the intermediary coding system
#' @param xw2_to - the column in the xw2 table for the final coding system
#' @param add_xwalk_string_column - boolean indicating whether a string equivalent should be added to the table
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' noc_isco <- readr::read_csv("https://danielruss.github.io/codingsystems/noc2011_isco2008.csv")
#' isco_soc <- readr::read_csv("https://danielruss.github.io/codingsystems/isco2008_soc2010.csv")
#' combine_crosswalks(noc_isco,isco_soc,noc2011_code,isco2008_code,isco2008_code,soc2010_code)
#'
combine_crosswalks<-function(xw1,xw2,xw1_from,xw1_to,xw2_from,xw2_to,add_xwalk_string_column=TRUE){
  from_start <- rlang::enquo(xw1_from)
  to_intermediate <- rlang::enquo(xw1_to)
  from_intermediate <- rlang::enquo(xw2_from)
  to_final <- rlang::enquo(xw2_to)

  from_codes <- dplyr::pull(xw1,rlang::ensym(xw1_from)) %>% unique()

  intermediate <-
    crosswalk(codes = from_codes,xwalk = xw1,from_column = !!from_start, to_column = !!to_intermediate,add_xwalk_string_column)
  intermediate_codes <- dplyr::pull(intermediate,!!from_intermediate)
  final <-
    crosswalk(codes=intermediate_codes, xwalk = xw2, from_column = !!from_intermediate,to_column = !!to_final,add_xwalk_string_column=TRUE)


  dplyr::bind_cols(intermediate[,1],final)
}

#' make a combined crosswalk
#'
#' Takes two crosswalks and combines them note that this takes tibbles
#' not crosswalks.  This will change in a future release.
#'
#' @param xw1 - the first crosswalk
#' @param xw2 - the second crosswalk
#' @param xw1_from - the column you want to crosswalk from in xw1
#' @param xw1_to - the column of the intermediate codes in xw1
#' @param xw2_from - the column of the intermediate codes in xw2
#' @param xw2_to - the column of code you want to code to in xw2
#' @export
make_combined_crosswalk <- function(xw1,xw2,xw1_from,xw1_to,xw2_from,xw2_to){
  start_code <- rlang::enquo(xw1_from)
  final_code <- rlang::enquo(xw2_to)
  xw <- combine_crosswalks(xw1,xw2,!!start_code,!!rlang::enquo(xw1_to),!!rlang::enquo(xw2_from),!!final_code)
  xw %>% dplyr::select(!!start_code,!!final_code) %>% tidyr::unnest(cols=!!final_code)
}

#' xwalk class constructor
#'
#' takes a data frame (the crosswalk) and which columns are the codes and titles
#' and create an xwalk object that can perform crosswalks...
#' @param dta the data frame of the crosswalk.
#' @param codes1 Codes for the (Default) input coding system for crosswalking can be overridden if the xwalk is bidirectional
#' @param titles1 Titles for the (Default) input coding system.
#' @param codes2 Codes for the (Default) output coding system for crosswalking can be overridden if the xwalk is bidirectional
#' @param titles2 Titles for the (Default) output coding system.
#' @param bidirectional Can we use this crosswalk in both directions? if true, you can supply the column names in the crosswalk function
#' to code in the reverse direction.
#' @export
xwalk <- function(dta,codes1,titles1,codes2,titles2,bidirectional=FALSE){
  if (missing(dta)) stop("xwalk require the crosswalk table (dta)")
  if (missing(codes1) || missing(codes2)) stop("xwalk needs to know which columns are the codes")
  c1_q <- rlang::ensym(codes1)
  c2_q <- rlang::ensym(codes2)

  obj <- list(bidirectional=bidirectional)
  obj$codes1 <-  rlang::as_string(c1_q)
  obj$codes2 <-  rlang::as_string(c2_q)


  if (!missing(titles1)){
    t1_q <- rlang::ensym(titles1)
    obj$title1  <-  rlang::as_string(t1_q)
  }
  if (!missing(titles2)){
    t2_q <- rlang::ensym(titles2)
    obj$title2  <-  rlang::as_string(t2_q)
  }

  obj$data <- dta %>% dplyr::select(obj$codes1,obj$title1,obj$codes2,obj$title2)
  attr(obj, "class") <- "xwalk"
  obj
}

#' checks if an object is a crosswalk
#'
#' @description
#' Is this object a crosswalk
#'
#' @param x
#' A crosswalk of class xwalk
#' @usage
#' is.xwalk(x)
#' @export
is.xwalk <- function(x) inherits(x,"xwalk")

#' checks if a crosswalk is bidirectional
#'
#' @description
#' Is a crosswalk bidirectional?
#'
#'
#' @usage
#' bidirectional(x)
#'
#' @param x A crosswalk of class xwalk
#' @export
bidirectional <- function(x) {
  if (!is.xwalk(x)) stop("x is not a crosswalk")
  x$bidirectional
}

#' @export
dim.xwalk <- function(x) dim(x$data)

#' get the codes for a crosswalk,
#'
#' @description
#' returns the all the codes in column code_column
#' @param x
#' A crosswalk of class xwalk
#' @param code_column
#' The column names for the desired codes.
#' @export
codes <- function(x,code_column){
  stopifnot(is.xwalk(x))
  cc <- rlang::as_string(rlang::ensym(code_column))
  if (! cc %in% names(x$data)){
    stop("\n\tcould not find column ",cc,"\n\tavailable columns: ",paste(names(x$data),collapse = ", ") )
  }
  x$data[[ cc ]]
}


#' looks up all corresponding codes from a crosswalk
#'
#'  @description
#'
#' Takes a vector of codes and concordance table (crosswalk) and converts from one
#' coding systems to the next.
#'
#' @param codes - the vector of codes that will be crosswalked
#' @param xwalk - the concordance table.
#' @param from_column - the column in the xwalk table for the codes of the starting coding system, if xwalk is of
#' class xwalk, and is unidirectional then this parameter is not needed.  If the xwalk if bidirectional,
#' then the parameter is optional. The default is xwalk$codes1.  If xwalk is not of class xwalk, then this parameter
#' is required
#' @param to_column - the column in the xwalk table for the codes of the results coding system, if xwalk is of
#' class xwalk, and is unidirectional then this parameter is not needed.  If the xwalk if bidirectional,
#' then the parameter is optional. The default is xwalk$codes2.  If xwalk is not of class xwalk, then this parameter
#' is required
#' @param add_xwalk_string_column - boolean indicating whether a string equivalent should be added to the table
#' @param unnest - boolean indicating that the data should have multiple lines if there are multiple resulting codes.
#' This is not recommended, because the one input code could be repeated multiple times.
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
#'
crosswalk<-function(codes,xwalk,from_column,to_column,add_xwalk_string_column=FALSE,unnest=FALSE){
  dta <- xwalk
  if (is.xwalk(xwalk)){
    dta <- xwalk$data
  }

  ## if you give me a crosswalk, and dont give me column names,
  ## assume that from codes1 -> codes2
  if (missing(from_column) && is.xwalk(xwalk) ){
    from_column_str <- rlang::sym( xwalk$codes1 )
    to_column_str <- rlang::sym( xwalk$codes2 )
  }else{
    ## quote the parameters -- they are not variables ...
    from_column_str <- rlang::enquo(from_column)
    to_column_str <- rlang::enquo(to_column)
  }

  xw_codes <-codes %>% purrr::map( ~dplyr::filter(dta,!!from_column_str %in% .) %>% dplyr::pull(!!to_column_str) %>% unique() %>% sort())
  tbl <- tibble::tibble( !!from_column_str := codes, !!to_column_str := xw_codes )


  if (add_xwalk_string_column){
    str_col <- rlang::sym( paste0( rlang::as_name( from_column_str ),"_str" ) )
    tbl <- tbl %>% dplyr::mutate(!!str_col := purrr::map_chr(!!from_column_str,~paste(.,collapse = " | "))  )
    str_col <- rlang::sym( paste0( rlang::as_name( to_column_str ),"_str" ) )
    tbl <- tbl %>% dplyr::mutate(!!str_col := purrr::map_chr(!!to_column_str,~paste(.,collapse = " | "))  )
  }

  if (unnest) tbl<-tidyr::unnest(tbl,to_column_str)
  invisible(tbl)
}
