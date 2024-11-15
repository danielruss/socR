% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codingsystem.R
\name{codingsystem}
\alias{codingsystem}
\title{constructor create a coding system S3 class}
\usage{
codingsystem(codes, titles, ..., name = "")
}
\arguments{
\item{codes}{vector of codes, a dataframe containing the columns "code"
(with codes) and "title" (with titles), or a url/file path of a csv file
containing the codes and titles with header row containing at least "code"
and title.  Other columns may be present.}

\item{titles}{vector of title}

\item{...}{additional parameters passed into read_csv}

\item{name}{coding system name}
}
\value{
the codingsystem object
}
\description{
constructor create a coding system S3 class
}
