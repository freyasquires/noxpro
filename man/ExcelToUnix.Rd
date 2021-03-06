% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExcelToUnix.R
\name{ExcelToUnix}
\alias{ExcelToUnix}
\title{Create unix timestamp from excel time}
\usage{
ExcelToUnix(df)
}
\arguments{
\item{df}{Data frame of raw AQD NOx output}
}
\value{
Dataframe df with column containing a unix timestamp. Column name is "UNIX.TS"
}
\description{
Some functions require unix timestamp. This creates a unix timestamp column from excel time in the raw
AQD NOx output file.
}
