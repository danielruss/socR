#' xwalk class constructor
#'
#' takes a data frame (the crosswalk) and which columns are the codes and titles
#' and create an xwalk object that can perform crosswalks...
#' @param dta the data frame of the crosswalk, or the filename/URL of a csv crosswalk file or the filename of an excel file.
#' @param codes1 Codes for the (Default) input coding system for crosswalking can be overridden if the xwalk is bidirectional
#' @param titles1 Titles for the (Default) input coding system.
#' @param codes2 Codes for the (Default) output coding system for crosswalking can be overridden if the xwalk is bidirectional
#' @param titles2 Titles for the (Default) output coding system.
#' @param bidirectional Can we use this crosswalk in both directions? if true, you can supply the column names in the crosswalk function
#' to code in the reverse direction.
#' @param ... parameters passed to read_csv
#' @export
xwalk <- function(dta,codes1,titles1,codes2,titles2,bidirectional=FALSE,...){
  if (missing(dta)) stop("xwalk requires either the crosswalk (dta)")

  if (typeof(dta)=="character"){
    dta <- switch(
      tools::file_ext(dta),
      "csv" = readr::read_csv(dta,col_types = 'c',...),
      "tsv" = readr::read_tsv(dta,col_types = 'c',...),
      "xls" = readxl::read_excel(dta,...),
      "xlsx"= readxl::read_excel(dta,...),
      stop("xwalk can only handle csv, tsv, or excel files by the file name")
      )
    dta
  }

  # if you dont tell me which columns are the
  # codes, use the default, which means you better
  # only have 4 columns.
  use_colnames = missing(codes1) || missing(codes2)
  if ( use_colnames && ncol(dta)!=4 )
    stop("xwalk needs to know which columns are the codes and cannot figure it out, please provide additional arguments code1 and code2")

  obj <- list(bidirectional=bidirectional)

  if ( use_colnames ){
    obj$codes1 <-  colnames(dta)[1]
    obj$titles1 <-  colnames(dta)[2]
    obj$codes2 <-  colnames(dta)[3]
    obj$titles2 <-  colnames(dta)[4]
  }else{
    codes1 <- rlang::enquo(codes1)
    obj$codes1 <- ifelse(rlang::quo_is_call(codes1),rlang::as_name(rlang::eval_tidy(codes1)),rlang::as_name(codes1))
    codes2 <- rlang::enquo(codes2)
    obj$codes2 <- ifelse(rlang::quo_is_call(codes2),rlang::as_name(rlang::eval_tidy(codes2)),rlang::as_name(codes2))
    obj$titles1 <- ""
    obj$titles2 <- ""
    titles1 <- rlang::enquo(titles1)
    if (!rlang::quo_is_missing(titles1)){
      obj$titles1 <- ifelse(rlang::quo_is_call(titles1),rlang::as_name(rlang::eval_tidy(titles1)),rlang::as_name(titles1))
    }
    titles2 <- rlang::enquo(titles2)
    if (!rlang::quo_is_missing(titles2)){
      obj$titles2 <- ifelse(rlang::quo_is_call(titles2),rlang::as_name(rlang::eval_tidy(titles2)),rlang::as_name(titles2))
    }
  }

  obj$data <- dta %>% dplyr::select(intersect(colnames(dta),c(obj$codes1,obj$titles1,obj$codes2,obj$titles2)))
  attr(obj, "class") <- "xwalk"
  obj
}

#' @export
#' @importFrom utils head
head.xwalk<-function(x,...){
  utils::head(x$data)
}

#' Combine two crosswalks to produce a new crosswalk
#'
#' Takes two concordance tables (xw1 and xw2), where xw1 go from coding system one
#' to an intermediary coding system, and xw2 goes from the intermediary coding system
#' to coding system two.  The goal is to make one table that goes from coding system 1 to
#' coding system 2.
#'
#'
#' @param xw1 - crosswalk 1, either an xwalk object or a data.frame
#' @param xw2 - crosswalk 2,  either an xwalk object or a data.frame
#' @param xw1_from - (ignored if xw1 is an xwalk object) the column in the xw1 table for the starting coding system
#' @param xw1_to - (ignored if xw1 is an xwalk object) the column in the xw1 table for the intermediary coding system
#' @param xw2_from - (ignored if xw1 is an xwalk object) the column in the xw2 table for the intermediary coding system
#' @param xw2_to - (ignored if xw1 is an xwalk object) the column in the xw2 table for the final coding system
#' @param flatten - should the function unnest the data frame into many 1 code -1 code rows, or leave row 1 code to many codes.
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # the noc_isco example has an extra column that confuses the parser, so I have to specify the parts.
#' noc_isco <- xwalk("https://danielruss.github.io/codingsystems/noc2011_isco2008.csv",
#'                   codes1 ="isco2008_code",titles1 = "isco2008_title",
#'                   codes2 = "noc2011_code",titles2="noc2011_title")
#' isco_soc <- xwalk("https://danielruss.github.io/codingsystems/isco2008_soc2010.csv")
#' combine_crosswalks(noc_isco,isco_soc,noc2011_code,isco2008_code,isco2008_code,soc2010_code)
#'
combine_crosswalks<-function(xw1,xw2,xw1_from,xw1_to,xw2_from,xw2_to,flatten=TRUE){

  ## the user should really use xwalks instead of data.frames
  ## create the crosswalks..
  if (is.data.frame(xw1)) xw1 <- xwalk(xw1,codes1=xw1_from,codes2=xw1_to)
  if (is.data.frame(xw2)) xw2 <- xwalk(xw2,codes1=xw2_from,codes2=xw2_to)

  if (!is.xwalk(xw1) || !is.xwalk(xw2)) {
    if (!is.xwalk(xw1)) stop("Problem making a xwalk object out of xw1")
    stop("Problem making a xwalk object out of xw2")
  }


  from_codes <- dplyr::pull(xw1$data,xw1$codes1) %>% unique()
  intermediate_codes <- crosswalk(codes=from_codes,xwalk = xw1)
  final_codes <- crosswalk(codes=intermediate_codes,xwalk = xw2)
  tbl <- tibble::tibble( !!as.name(xw1$codes1):=from_codes,
                         !!as.name(xw1$codes2):=intermediate_codes,
                         !!as.name(paste0(xw1$codes2,"_str")):=make_code_str(!!as.name(xw1$codes2)),
                         !!as.name(xw2$codes2):=final_codes,
                         !!as.name(paste0(xw2$codes2,"_str")):=make_code_str(!!as.name(xw2$codes2))
  )
  new_xw <- xwalk(tbl,codes1=xw1$codes1,codes2 = xw2$codes2)
  if (flatten){
    print(xw1$titles)
    new_xw$titles1 = xw1$titles1
    new_xw$titles2 = xw2$titles2
    code_map1 <- dplyr::pull(xw1$data,xw1$titles1,xw1$codes1)
    code_map2 <- dplyr::pull(xw2$data,xw2$titles2,xw2$codes2)
    new_xw$data <- new_xw$data %>%
      dplyr::mutate(!!as.name(xw1$titles1) := dplyr::recode(!!as.name(new_xw$codes1),!!!code_map1) ) %>%
      tidyr::unnest(!!as.name(xw2$codes2)) %>%
      dplyr::mutate(!!as.name(xw2$titles2) := dplyr::recode(!!as.name(new_xw$codes2),!!!code_map2) ) %>%
      dplyr::select(new_xw$codes1,new_xw$titles1,new_xw$codes2,new_xw$titles2)
  }

  new_xw
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

#' @export
print.xwalk <- function(x,...) {
#  print(c("codes1: ",x$codes1," titles1: ",x$titles1,"==>","codes2: ",x$codes2," titles2: ",x$titles2))
  print(x$data,...)
}

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


#' Use the concordance table (crosswalk) to convert from one
#' coding system to another.
#'
#' @param codes the vector of codes that will be crosswalked
#' @param xwalk the concordance table.
#' @param from_column the column in the xwalk table for the codes of the starting coding system, if xwalk is of
#' class xwalk, and is unidirectional then this parameter is not needed.  If the xwalk if bidirectional,
#' then the parameter is optional. The default is xwalk$codes1.  If xwalk is not of class xwalk, then this parameter
#' is required
#' @param to_column to_column the column in the xwalk table for the codes of the results coding system, if xwalk is of
#' class xwalk, and is unidirectional then this parameter is not needed.  If the xwalk if bidirectional,
#' then the parameter is optional. The default is xwalk$codes2.  If xwalk is not of class xwalk, then this parameter
#' is required
#'
#' @return an unnamed list of codes in the resulting coding system
#' @export
#'
crosswalk <- function(codes,xwalk,from_column,to_column){
  dta <- xwalk
  if (is.xwalk(xwalk)){
    dta <- xwalk$data
  }
  ## if you give me a crosswalk, and dont give me column names,
  ## assume that from codes1 -> codes2
  if (missing(from_column) && is.xwalk(xwalk) ){
    from_column_sym <- rlang::sym( xwalk$codes1 )
    to_column_sym <- rlang::sym( xwalk$codes2 )
  }else{
    ## quote the parameters -- they are not variables ...
    from_column_sym <- rlang::enquo(from_column)
    to_column_sym <- rlang::enquo(to_column)
  }

  codes %>% purrr::map( ~dplyr::filter(dta,!!from_column_sym %in% .)  %>% dplyr::pull(!!to_column_sym)  %>% unique() %>% sort() )
}


#' convert a list column of codes to vector of string for display
#'
#' @param x codes column
#'
#' @return a vector a string concatenating all the codes
#' @export
#'
#' @examples
#' df <- tibble::tibble(soc2010_codes = list(c("11-1011","11-1021"),c("11-1000")))
#' df <- dplyr::mutate(df,code_str=make_code_str(soc2010_codes))
make_code_str <- function(x){
  purrr::map_chr(x,paste0,collapse = " | ")
}

#' convert a column from a list to a pipe-delimeted string for
#' easy viewing..
#'
#' @param x a xwalk object
#' @param col the column that you want to make a string
#' @param newcol the new name of the column default= col_str
#'
#' @return a vector containing the string of concatenated occupational codes for each row
#'
#' @export
#' @importFrom magrittr %>%
#'
add_code_str <- function(x,col,newcol){
  if(!is.xwalk(x) || missing(col)) return;
  col_name <- rlang::ensym(col)
  newcol_name <- ifelse(missing(newcol),rlang::sym(paste0(rlang::as_string(col_name),"_str")))
  x$data <- x$data %>% dplyr::mutate(!!newcol_name:=purrr::map_chr(!!col_name,paste0,collapse = " | "))
  x
}


#'
#' @title Calculates the Shannon entropy for a Crosswalk
#' @param x The crosswalk
#' @description
#' The more potential codes that a crosswalk will allow an intial code to become, the higher
#' the entropy.  The entropy (H) is given by \deqn{H = \Sigma p log p} where p = 1/n and n is the number of potential codes
#' a single code can map to, natural logs are used in the calculation.
#'
#' @return the entropy
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
xwalk_entropy <- function(x){
  p<-0
  x$data %>% dplyr::group_by(!!as.name(x$codes1)) %>%
    dplyr::summarize(p=1/dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::summarize(entropy=sum(-p*log(p))) %>% tibble::deframe()
}

multi_hot_encoder<-function(all_codes){
  all_the_codes <- all_codes %>% unique %>% unname %>% tibble::enframe(name="index", value="code")

  transform <- function(codes){
    res <- logical(nrow(codes))
    res <- all_the_codes %>% dplyr::filter(.data$code %in% codes) %>%
      dplyr::pull(.data$index) %>%
      purrr::reduce(function(old,indx){ old[indx]=1;return(old) }, .init=res)
    res
  }
  list(transform=transform)
}
