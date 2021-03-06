% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{rescale_100}
\alias{rescale_100}
\title{Rescale 1-10 integer to 0-100 numeric.}
\usage{
rescale_100(var)
}
\arguments{
\item{var}{An \code{integer} vector.}
}
\description{
Takes vectors representing 10-point likert scales and transforms them to
100-point scales (\code{numeric}). Formula: \code{(x-1)*(100/9)}
}
\examples{
x <- rescale_100(c(1L, 10L))
identical(x, c(0, 100))
}
\author{
Kristian D. Olsen
}
\seealso{
\code{\link{rescale_10}} for the complement.
}

