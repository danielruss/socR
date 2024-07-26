#' noc2011 4 digit classification system
#'
#' Canadian 4 digit National Occupational Classification (NOC) 2011
#'
#' A codingsystem object with the noc2011 4-digit codes
#'
#' @source \url{https://danielruss.github.io/codingsystems/noc2011_all.csv}
#' @source \url{https://www.statcan.gc.ca/eng/subjects/standard/noc/2011/index}
"noc2011_4digit"


#' soc2010 6 digit classification system
#'
#' Downloaded by Daniel Russ
#'
#' A codingsystem object with the soc2010 6-digit codes
#'
#' The 6-digit soc2010 codes.
#' @format a coding system object
#' \describe{
#'   \item{code}{the 6-digit soc2010 code}
#'   \item{title}{the title of the 6-digit soc2010 code}
#' }
#' @source \url{https://danielruss.github.io/codingsystems/soc2010_all.csv}
#' @source \url{https://www.bls.gov/soc/2010/2010_major_groups.htm}
"soc2010_6digit"


#' noc2011 4 digit classification system
#'
#' Canadian  National Occupational Classification (NOC) 2011
#'
#' The complete Canadian NOC 2011 system
#'
#' @source \url{https://danielruss.github.io/codingsystems/noc011_all.csv}
#' @source \url{https://www.statcan.gc.ca/eng/subjects/standard/noc/2011/index}
"noc2011_all"

#' SOC 1980 complete classification system
#'
#' US SOC 1980 classification system
#'
#' The complete US SOC 1980 system
#'
#' @format a coding system that consists of
#' \describe{
#'  \item{code}{The soc 1980 code}
#'  \item{title}{The title of the code}
#' }
#' @source \url{https://danielruss.github.io/codingsystems/soc1980_all.csv}
"soc1980_all"

#' Extended SOC 1980 complete classification system
#'
#' The US SOC 1980 classification system can have higher level (major or minor codes)
#' codes without any children.  We extended the SOC 1980 classification system to
#' require all major codes (2-digit code) to have at least 1 minor code (3-digit code ),
#' and every minor codes to have at least 1 unit code (4-digit code).  The most
#' detailed code is now always a unit code.
#'
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc_1980_extended.csv}
"soc1980_extended"

#' Detailed SOC 1980  classification system
#'
#' The US SOC 1980 classification system can have higher level (major or minor codes)
#' codes without any children. This data contains all the most detailed codes regardless
#' of the code level.
#'
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv}
"soc1980_detailed"

#' Complete SOC 2010  classification system
#'
#' The complete US SOC 2010 classification system. This data contains all the codes
#'  regardless of the code level.
#'
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc2010_all.csv}
"soc2010_all"

#' Complete SOC 2018  classification system
#'
#' The complete US SOC 2018 classification system. This data contains all the codes
#'  regardless of the code level.
#'
#'
#'
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc2018_all.csv}
"soc2018_all"
