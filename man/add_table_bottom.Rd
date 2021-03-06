% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_table_bottom.R
\name{add_table_bottom}
\alias{add_table_bottom}
\title{Adds bottom aggregation row to table}
\usage{
add_table_bottom(ftable, ffun = sum, fname = NA)
}
\arguments{
\item{ftable}{data.frame or data.table}

\item{ffun}{aggregation function such as sum, mean, etc.
Default value is sum.}

\item{fname}{named vector c(a='b'), where a is a column name
in ftable and b the assigned cell name of the aggregation.
E.g. fname = c(Object = 'Result'). Default value is NA.}
}
\value{
data.frame or data.table
}
\description{
Adds bottom aggregation row to table
}
\examples{
\dontrun{add_table_bottom(dt)}
}
