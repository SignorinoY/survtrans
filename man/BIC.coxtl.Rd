% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIC.coxtl.R
\name{BIC.coxtl}
\alias{BIC.coxtl}
\title{BIC of Transfer Learning for Cox proportional hazards model}
\usage{
\method{BIC}{coxtl}(object, data, group, type = c("trad", "mod"), ...)
}
\arguments{
\item{object}{a coxtl object.}

\item{data}{a data frame containing the variables in the model, which
should be the same as the data used to fit the model.}

\item{group}{a factor specifying the group of each sample, which
should be the same as the group used to fit the model.}

\item{type}{a character string specifying the type of BIC to compute.
The default is "trad", corresponding to Cn=1. The other option is
"mod", corresponding to Cn=log(log(d)).}

\item{...}{Unused.}
}
\value{
the BIC of the model.
}
\description{
BIC of Transfer Learning for Cox proportional hazards model
}
