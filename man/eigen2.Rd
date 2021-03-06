% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eigen2.R
\name{eigen2}
\alias{eigen2}
\title{Generalized eigenvalue problem}
\usage{
eigen2(B, W)
}
\arguments{
\item{B, W}{Similarly sized symmetric matrices with W positive definite.}
}
\value{
A list with components \code{values} and \code{vectors} as for \code{\link[base]{eigen}}
}
\description{
Solves the generalized eigenvalue problem (B - lambda*W)*alpha = 0,
where B and W are symmetric matrices of the same size, W is positive
definite, lambda is a scalar and alpha and 0 are vectors.
}
\details{
If W is not specified, W = I is assumed.
}
\examples{
X <- as.matrix(subset(iris, select = -Species))
W <- crossprod(resid(aov(X ~ Species, iris)))
B <- crossprod(resid(aov(X ~ 1,       iris))) - W
n <- nrow(iris)
p <- length(levels(iris$Species))
(ev <- eigen2(B/(p - 1), W/(n - p)))  ## hand-made discriminant analysis
DF <- X \%*\% ev$vectors[, 1:2]
with(iris, {
     plot(DF, col = Species, pch = 20,
          xlab = expression(DF[1]), ylab = expression(DF[2]))
     legend("topleft", levels(Species), pch = 20, col = 1:3)
})
}
