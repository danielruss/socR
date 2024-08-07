% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{noc2011_all}
\alias{noc2011_all}
\title{noc2011 4 digit classification system}
\format{
\describe{
\item{code}{a 1-4-digit code formated like '0011', be careful must be a string not an integer}
\item{title}{a short definition of the code}
\item{Level}{Unofficial name for the level in the hierarchy (number of digits) for the code, 1, 2, 3, or 4}
\item{Hierarchical_structure}{Official name for the level in the hierarchy}
\item{noc1d}{the 1-digit noc code associated with the code}
\item{noc2d}{the 2-digit noc code associated with the code, is NA for 1-digit codes}
\item{noc3d}{the 3-digit noc code associated with the code, is NA for 1- or 2-digit codes}
\item{noc4d}{the 4-digit noc code associated with the code, is NA for 1-, 2-, or 3-digit codes}
}
}
\source{
\url{https://danielruss.github.io/codingsystems/noc_2011_4d.csv}

\url{https://www.statcan.gc.ca/eng/subjects/standard/noc/2011/index}
}
\usage{
noc2011_all
}
\description{
Canadian 4 digit National Occupational Classification (NOC) 2011
}
\keyword{datasets}
