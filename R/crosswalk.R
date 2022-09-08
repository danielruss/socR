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
#' @param col_types set the default col_type parameter for read_csv/read_excel
#' @param ... additional parameters passed to read_csv
#' @export
xwalk <- function(dta,codes1,titles1,codes2,titles2,bidirectional=FALSE,col_types=ifelse(grepl("\\.xlsx?$",dta),"text","c"),...){
  if (missing(dta)) stop("xwalk requires either the crosswalk (dta)")

  if (typeof(dta)=="character"){
    dta <- switch(
      tools::file_ext(dta),
      "csv" = readr::read_csv(dta,col_types = col_types,...),
      "tsv" = readr::read_tsv(dta,col_types = col_types,...),
      "xls" = readxl::read_excel(dta,col_types = col_types,...),
      "xlsx"= readxl::read_excel(dta,col_types = col_types,...),
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
  utils::head(x$data,...)
}

#' @export
#' @importFrom utils tail
tail.xwalk<-function(x,...){
  utils::tail(x$data,...)
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
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # the noc_isco example has an extra column that confuses the parser,
#' # so I have to specify the parts or skip the last column.
#' noc_isco <- xwalk("https://danielruss.github.io/codingsystems/noc2011_isco2008.csv",
#'                    col_types = "cccc-")
#' isco_soc <- xwalk("https://danielruss.github.io/codingsystems/isco2008_soc2010.csv")
#' combine_crosswalks(noc_isco,isco_soc)
#'
combine_crosswalks<-function(xw1,xw2){


  if (!is.xwalk(xw1)) stop("crosswalk1 is not an xwalk object. ")
  if (!is.xwalk(xw2)) stop("crosswalk2 is not an xwalk object. ")

  if (xw1$codes2 != xw2$codes1){
    stop("The two xwalk objects don't map between a common coding system: ",xw1$codes2," != ",xw2$codes1)
  }

  dta = xw1$data

  xw_data <- dta %>%
    # crosswalk the codes
    dplyr::mutate(!!as.name(xw2$codes2):=crosswalk(!!as.name(xw2$codes1),xwalk = xw2)) %>%
    tidyr::unnest(xw2$codes2) %>%
    dplyr::distinct(!!as.name(xw1$codes1),!!as.name(xw1$titles1),!!as.name(xw2$codes2)) %>%
    dplyr::left_join( dplyr::distinct(xw2$data[,c(xw2$codes2,xw2$titles2)]),by=xw2$codes2) %>%
    dplyr::arrange(!!as.name(xw1$codes1),!!as.name(xw2$codes2))

  return(xwalk(xw_data,codes1=xw1$codes1,titles1 = xw1$titles1,codes2 = xw2$codes2,titles2 = xw2$titles2))
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
  if ( (is.data.frame(xwalk) && (missing(from_column)|| missing(to_column)) ) && !is.xwalk(xwalk) ) {
    stop("crosswalk needs either a crosswalk object, or a dataframe along with the from and from columns")
  }

  xw_data = xwalk
  if (socR::is.xwalk(xwalk)){
    xw_data <- xwalk$data
  }

  if (missing(from_column)){
    from_column_sym <- rlang::sym( xwalk$codes1 )
    from_column_str <- xwalk$codes1
    to_column_sym   <- rlang::sym( xwalk$codes2 )
    to_column_str   <- xwalk$codes2
  }else{
    ## quote the parameters -- they are not variables ...
    from_column_sym <- rlang::enquo(from_column)
    from_column_str <- rlang::quo_name(from_column_sym)
    to_column_sym <- rlang::enquo(to_column)
    to_column_str <- rlang::quo_name(to_column_sym)
  }

  xw <- xw_data %>% dplyr::group_by({{from_column_sym}}) %>% dplyr::summarise({{to_column_sym}} :=list({{to_column_sym}}))
  tibble::enframe(codes,name=NULL,value=from_column_str) %>%
    dplyr::left_join(xw,by=from_column_str) %>%
    dplyr::mutate({{to_column_sym}} := purrr::modify_if({{to_column_sym}},is.null,~character(0)))%>%
    dplyr::pull({{to_column_sym}} )
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


#'
#' @title Calculates the Shannon entropy for a Crosswalk
#' @param x The crosswalk
#' @description
#' The more potential codes that a crosswalk will allow an intial code to become, the higher
#' the entropy.  The entropy (S) is given by \deqn{S = -\Sigma \Sigma p log p} where p = 1/n and n is the number of potential codes.
#' a single code can map to, natural logs are used in the calculation. The inner summation
#' can be is the sum of n iteration of 1/n, so the equation can be simplified
#' to \deqn{S = -\Sigma log p}
#'
#' @return the entropy
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
xwalk_entropy <- function(x) {
  x$data %>% dplyr::group_by(!!as.name(x$codes1)) %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::summarise(entropy=sum(log(.data$n))) %>%
    dplyr::pull(.data$entropy)
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
