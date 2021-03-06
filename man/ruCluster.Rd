% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ruCluster.R
\name{ruCluster}
\alias{ruCluster}
\title{Cluster an mFISH object}
\usage{
ruCluster(mFISH, k, dmetric = "euclidean", p = 2)
}
\arguments{
\item{mFISH}{An mFISH object}

\item{k}{Number of clusters}

\item{dmetric}{Metric for creating distance matrix (see \code{dist()})}

\item{p}{Power of the Minkowski distance (see \code{dist()})}
}
\value{
A clustered mFISH object.
}
\description{
Cluster an mFISH object
}
\author{
Kaitlin Sullivan
}
