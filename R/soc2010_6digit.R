#' soc2010 6 digit classification system
#'
#' Downloaded by Daniel Russ
#'
#' @format
#' \describe{
#' \item{soc_code}{a 6 digit code formated like '11-1011'}
#' \item{title}{a short definition of the code}
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc_2010_6digit.csv}
#' @source \url{https://www.bls.gov/soc/2010/2010_major_groups.htm}
"soc2010_6digit"

#' noc2011 4 digit classification system
#'
#' Canadian 4 digit National Occupational Classification (NOC) 2011
#'
#' @format
#' \describe{
#' \item{noc_code}{a 4 digit code formated like '0011', be careful must be a string not an integer}
#' \item{title}{a short definition of the code}
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/noc_2011_4d.csv}
#' @source \url{https://www.statcan.gc.ca/eng/subjects/standard/noc/2011/index}
"noc2011_4digit"

#' SOC 1980 complete classification system
#'
#' US SOC 1980 classification system
#'
#' @format
#' \describe{
#' \item{soc1980_code}{the n-digit soc 1980}
#' \item{title}{a short definition of the code}
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc1980.csv}
"soc1980_all"

#' Extended SOC 1980 complete classification system
#'
#' The US SOC 1980 classification system can have higher level (major or minor codes)
#' codes without any children.  We extended the SOC 1980 classification system to
#' require all major codes (2-digit code) to have at least 1 minor code (3-digit code ),
#' and every minor codes to have at least 1 unit code (4-digit code).  The most
#' detailed code is now always a unit code.
#'
#' @format
#' \describe{
#' \item{soc1980_code}{the soc 1980 code}
#' \item{title}{a short definition of the code}
#' \item{Level}{the level of the soc 1980 code}
#' \item{parent}{the parent of the soc 1980 code, note: at the division level, the parent is 0000}
#' \item{division}{for any soc 1980 code, what is the division code}
#' \item{major}{for any soc 1980 code, what is the major code.  Is NA for division codes.}
#' \item{minor}{for any soc 1980 code, what is the minor code. Is NA for division and major codes.}
#' \item{unit}{for any soc 1980 code, what is the unit code.  Is NA for non-unit codes. }
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc_1980_extended.csv}
"soc1980_extended"

#' Detailed SOC 1980  classification system
#'
#' The US SOC 1980 classification system can have higher level (major or minor codes)
#' codes without any children. This data contains all the most detailed codes regardless
#' of the code level.
#'
#' @format
#' \describe{
#' \item{soc1980_code}{the soc 1980 code}
#' \item{title}{a short definition of the code}
#' \item{Level}{the level of the soc 1980 code}
#' \item{parent}{the parent of the soc 1980 code, note: at the division level, the parent is 0000}
#' \item{division}{for any soc 1980 code, what is the division code}
#' \item{major}{for any soc 1980 code, what is the major code.  Is NA for division codes.}
#' \item{minor}{for any soc 1980 code, what is the minor code. Is NA for division and major codes.}
#' \item{unit}{for any soc 1980 code, what is the unit code.  Is NA for non-unit codes. }
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv}
"soc1980_detailed"

#' Complete SOC 2010  classification system
#'
#' The complete US SOC 2010 classification system. This data contains all the codes
#'  regardless of the code level.
#'
#' @format
#' \describe{
#' \item{code}{the soc 2010 code}
#' \item{title}{a short definition of the code}
#' \item{Level}{The number of significant digits in the code}
#' \item{Hierarchical_structure}{The name of the level}
#' \item{parent}{the parent of the soc code, note: 2 digit soc code dont have parents}
#' \item{soc2d}{for any soc code, what is the 2-digit code}
#' \item{soc3d}{for any soc code, what is the 3-digit code. Is NA for 2-digit codes.}
#' \item{soc5d}{for any soc code, what is the 5-digit code. Is NA for 2- and 3-digit codes.}
#' \item{soc6d}{for any soc code, what is the 6-digit code. Is NA for 2-, 3-, and 5-digit codes. }
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc2010_all.csv}
"soc2010_all"

#' Complete SOC 2018  classification system
#'
#' The complete US SOC 2018 classification system. This data contains all the codes
#'  regardless of the code level.
#'
#' @format
#' \describe{
#' \item{code}{the soc 2018 code}
#' \item{title}{a short definition of the code}
#' \item{Level}{The number of significant digits in the code}
#' \item{Hierarchical_structure}{The name of the level}
#' \item{parent}{the parent of the soc code, note: 2 digit soc code dont have parents}
#' \item{soc2d}{for any soc code, what is the 2-digit code}
#' \item{soc3d}{for any soc code, what is the 3-digit code. Is NA for 2-digit codes.}
#' \item{soc5d}{for any soc code, what is the 5-digit code. Is NA for 2- and 3-digit codes.}
#' \item{soc6d}{for any soc code, what is the 6-digit code. Is NA for 2-, 3-, and 5-digit codes. }
#' }
#'
#' @source \url{https://danielruss.github.io/codingsystems/soc2018_all.csv}
"soc2018_all"
