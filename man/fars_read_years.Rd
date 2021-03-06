% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{----3-----
fars_read_years: Reads month and year from accident files}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector or list of years in numeric or integer format.}
}
\value{
Returns a list of tibbles with the same number of rows
        as the data in "accident_<year>.csv.bz2" files and two columns - month &
        year. Returns NULL and a warning if the file does not exist.
}
\description{
The function accepts a vector of years and returns a list of data
             frames with month and year columns based on data in "accident_<year>.csv.bz2
             files.
}

