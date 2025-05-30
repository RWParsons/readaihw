% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-lists.R
\name{get_datasets}
\alias{get_datasets}
\title{Show available datasets.}
\usage{
get_datasets(tidy_data = TRUE)
}
\arguments{
\item{tidy_data}{A logical. Whether or not to tidy the data (rename
columns and remove columns which only have one unique value).}
}
\value{
data
}
\description{
Show available datasets.
}
\examples{
\dontshow{if (interactive() && curl::has_internet()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
get_datasets()
\dontshow{\}) # examplesIf}
}
