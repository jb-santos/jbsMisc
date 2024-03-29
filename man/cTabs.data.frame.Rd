% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctab.R
\name{ctabs.data.frame}
\alias{ctabs.data.frame}
\title{ctabs.data.frame}
\usage{
\method{ctabs}{data.frame}(design, yvar, xvars, ...)
}
\arguments{
\item{design}{Survey design object or data frame}

\item{yvar}{String denoting name of row variable (in quotes)}

\item{xvars}{String (or vector of strings) denoting name of column variable (in quotes)}

\item{...}{Other arguments (not currently implemented)}
}
\value{
A list of class \code{ctabs} with dataframes:
\describe{
\item{\code{$Summary}}{The summary crosstab of every column variable
against the row variable}
\item{\code{$Total}}{The marginal frequencies and proportions of the row variable}
\item{\code{$(name)}}{A grouped dataframe of the crosstab of the respective
column variable against the row variable}
}
}
\description{
ctabs.data.frame
}
\examples{
data("ab")
table_vote1 <- ctabs(ab, yvar = "dvote3", xvars = c("region", "female", "age"))
table_vote1

}
\author{
John Santos
}
