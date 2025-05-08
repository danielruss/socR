#' xwalk class constructor
#'
#' takes a data frame (the crosswalk) and which columns are the codes and titles
#' and create an xwalk object that can perform crosswalks...
#' @param dta the data frame of the crosswalk, or the filename/URL of a csv crosswalk file or the filename of an excel file.
#' @param codes1 Codes for the (Default) input coding system for crosswalking
#' @param titles1 Titles for the (Default) input coding system.
#' @param codes2 Codes for the (Default) output coding system for crosswalking
#' @param titles2 Titles for the (Default) output coding system.
#' @param col_types set the default col_type parameter for read_csv/read_excel
#' @param ... additional parameters passed to read_csv
#' @export
xwalk <- function(dta,codes1,titles1,codes2,titles2,col_types=ifelse(grepl("\\.xlsx?$",dta),"text","c"),...){
  if (missing(dta)) stop("xwalk requires the crosswalk data or a path/URL to the data")

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

  obj <- list()

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

  xw_map <- dplyr::pull(obj$data,obj$codes2,obj$codes1)
  obj$map <- purrr::reduce2(xw_map,names(xw_map),function(acc,value,name){acc[[name]]=c(acc[[name]],value); acc},.init=list() )
  xw_map <- dplyr::pull(obj$data,obj$codes1,obj$codes2)
  obj$inverse_map <- purrr::reduce2(xw_map,names(xw_map),function(acc,value,name){acc[[name]]=c(acc[[name]],value); acc},.init=list() )

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


#' @export
dim.xwalk <- function(x) dim(x$data)

#' @export
print.xwalk <- function(x,...) {
  cat(format(x,...), "\n")
  invisible(x)
}

#' @export
format.xwalk <- function(x,...){
  table_str <- format(x$data,...)[-1]
  table_str <- paste( table_str[grepl("^[^#]",table_str)], collapse="\n" )
  paste(pillar::style_subtle(paste0("# Crosswalk: ", x$codes1, " <==> ",x$codes2)), "\n", table_str)
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
  if (missing("code_column")){
    message(paste0("columns: ",paste(x$codes1,x$codes2,sep= ", ")))
    return(invisible())
  }

  cc <- rlang::as_string(rlang::ensym(code_column))
  if (! cc %in% names(x$data)){
    stop("\n\tcould not find column ",cc,"\n\tavailable columns: ",paste(x$codes1,x$codes2,sep= ", ") )
  }
  x$data[[ cc ]]
}


#' Use the concordance table (crosswalk) to convert from one
#' coding system to another.
#'
#' @param codes the vector of codes that will be crosswalked
#' @param xwalk the concordance table.
#' @param invert by default the crosswalk goes from codes1 to
#' codes2 setting invert to TRUE make the crosswalk go from codes2
#' to codes1
#'
#' @return an unnamed list of codes in the resulting coding system
#' @export
#'
crosswalk <- function(codes,xwalk,invert=FALSE){
  if (!is.xwalk(xwalk)){
    stop("crosswalk needs either a crosswalk object")
  }
  ## if you set invert to true, use the inverse map, otherwise
  ## use the map
  map_to_use = xwalk$map
  if (invert) map_to_use = xwalk$inverse_map

  map_to_use[codes]
}

#' Crosswalk multiple columns in a tibble/data frame
#'
#' @description
#'
#' If you have a data frame of data with multiple columns that need to be
#' crosswalked, use this in a pipe.
#'
#' @param .data job data
#' @param xwalk crosswalk going from code system 1 to coding system 2
#' @param new_column_name the column name for the results if the results
#'  are unnested, the results will be colname_1, colname_2 ... colname_n,
#'  otherwise the results are a list column with name new_column_name.
#' @param ... Columns that need to be crosswalked
#' @param unnest_results default=TRUE, should the results be separated into
#' individual columns or else as a single list column
#'
#' @return a crosswalked tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' a <- tibble::tibble(id=c("job-1","job-2"),soc2010_1=c("11-1011","11-2011"),
#'      soc2010_2=c("11-1021",NA))
#' xw <- socR::xwalk("https://danielruss.github.io/codingsystems/soc2010_soc2018.csv")
#' a |> crosswalk_columns(xw,soc2018_xw,soc2010_1,soc2010_2)
#' a |> crosswalk_columns(xw,soc2018_xw,soc2010_1,soc2010_2,unnest_results=FALSE)
#' }
crosswalk_columns <- function(.data, xwalk, new_column_name, ... ,unnest_results=TRUE) {
  # Defuse the dots and capture the column names
  vars <- rlang::enquos(...)

  ## the important bit here is across(c(!!!vars))
  res <- .data |> dplyr::mutate({{new_column_name}} := purrr::pmap(dplyr::across(c(!!!vars)), \(...){
    x=c(...)
    x=x[!is.na(x)]
    unname( unlist( crosswalk(x,xwalk) ))
  }))

  if (unnest_results){
    res <- res |> tidyr::unnest_wider({{new_column_name}},names_sep = "_")
  }
  res
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


#' @inherit dplyr::filter
#' @export
filter.xwalk <- function (.data, ..., .by = NULL, .preserve = FALSE) {
  data <- .data$data
  dplyr::filter(data, ..., .by = .by, .preserve = .preserve) |> xwalk()
}

#' @importFrom dplyr arrange
#' @inherit dplyr::arrange
#' @export
arrange.xwalk <- function (.data, ..., .by_group = FALSE) {
  dplyr::arrange(.data$data, ..., .by_group = .by_group) |> xwalk()
}
