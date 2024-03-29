% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{manage_missing}
\alias{manage_missing}
\title{Manages missing values}
\usage{
manage_missing(df, y)
}
\arguments{
\item{df}{A data source, that is one of major R formats: data.table, data.frame,
matrix and so on.}

\item{y}{A string which indicates a target column name.}
}
\value{
A dataframe with removed and imputed missing values.
}
\description{
Manages missing values
}
