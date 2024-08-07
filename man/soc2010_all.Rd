% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{soc2010_all}
\alias{soc2010_all}
\title{Complete SOC 2010  classification system}
\format{
\describe{
\item{code}{the soc 2010 code}
\item{title}{a short definition of the code}
\item{Level}{The number of significant digits in the code}
\item{Hierarchical_structure}{The name of the level}
\item{parent}{the parent of the soc code, note: 2 digit soc code dont have parents}
\item{soc2d}{for any soc code, what is the 2-digit code}
\item{soc3d}{for any soc code, what is the 3-digit code. Is NA for 2-digit codes.}
\item{soc5d}{for any soc code, what is the 5-digit code. Is NA for 2- and 3-digit codes.}
\item{soc6d}{for any soc code, what is the 6-digit code. Is NA for 2-, 3-, and 5-digit codes. }
}
}
\source{
\url{https://danielruss.github.io/codingsystems/soc2010_all.csv}
}
\usage{
soc2010_all
}
\description{
The complete US SOC 2010 classification system. This data contains all the codes
 regardless of the code level.
}
\keyword{datasets}
