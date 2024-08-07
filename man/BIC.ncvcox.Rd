% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIC.ncvcox.R
\name{BIC.ncvcox}
\alias{BIC.ncvcox}
\title{BIC for Non-convex penalized Cox proportional hazards model}
\usage{
\method{BIC}{ncvcox}(object, data, group, offset, type = c("trad", "mod"), ...)
}
\arguments{
\item{object}{a ncvcox object.}

\item{data}{a data frame containing the variables in the model, which
should be the same as the data used to fit the model.}

\item{group}{a factor specifying the group of each sample.}

\item{offset}{a numeric vector specifying the offset for each sample.}

\item{type}{a character string specifying the type of BIC to compute.
The default is "trad", corresponding to Cn=1. The other option is
"mod", corresponding to Cn=log(log(d)).}

\item{...}{Unused.}
}
\value{
the BIC of the model.
}
\description{
BIC for Non-convex penalized Cox proportional hazards model
}
