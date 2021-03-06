% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_cox.R
\name{bc_inv}
\alias{bc_inv}
\title{Box-Cox transform inverse}
\usage{
bc_inv(z, alpha, eps = 1e-05)
}
\arguments{
\item{z}{numeric, the transformed value}

\item{alpha}{numeric, the power of the box-cox transform}

\item{eps}{numeric, a guard around alpha = 0}
}
\value{
A vector of original quantities
}
\description{
Find the original value corresponding to a box-cox transform
}
\examples{
invy <- with(Cars93, bc(MPG.city, alpha = -1))
mpgc <- bc_inv(invy, alpha = -1)
range(mpgc - Cars93$MPG.city)
}
