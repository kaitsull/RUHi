% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ruMake.R
\name{ruMake}
\alias{ruMake}
\title{Create an mFISH object}
\usage{
ruMake(df, metadata = NULL)
}
\arguments{
\item{df}{Dataframe from \code{ruRead} or \code{ruCombine}.}

\item{metadata}{Dataframe containing metadata}
}
\value{
An mFISH object.
}
\description{
Create an mFISH object from a data table created by the function \code{ruRead} and/or \code{ruCombine}. Run \code{ruFilter} next!
}
\author{
Kaitlin E Sullivan
}
