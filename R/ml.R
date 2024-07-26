#' Split data
#'
#' A simple deterministic mechanism for splitting data into
#' training, development, and test data based on the MD5
#' hash of a unused string parameters.
#'
#' @param x unused string data used to split the data
#' @param pTrain approximate percent of the training split
#' @param pDev approximate percent of the development split
#' @param pTest approximate percent of the test split
#'
#' @return a vector of factors (Train,Dev,Test) denoting the data split
#' @export
#' @examples
#' split_data(rownames(mtcars))
#' @importFrom magrittr %>%
split_data <- function(x,pTrain=.90,pDev=0.09,pTest=0.01){
  zz<- purrr::map_chr(x,digest::digest,algo = "md5") %>%
    purrr::map_chr(substr,start=27,stop=32) %>%
    purrr::map_int(strtoi,base=16)
  zz <- zz %% 1000
  p <- floor( 1000*c(pTrain,pDev,pTest)/(pTrain+pDev+pTest) )
  factor(dplyr::if_else(zz<p[1],"train",dplyr::if_else(zz<(p[1]+p[2]),"dev","test")),levels = c("train","dev","test"))
}


#' creates a multihot encoder from a list of labels
#'
#' @param allLabels The complete set of labels
#'
#' @return a function that preforms multihot encoding
#' @export
createMultiHotEncoder <- function (allLabels){
  function(labels){
    z <- integer(length = length(allLabels))
    z[match(labels,allLabels)]<-1
    z
  }
}

