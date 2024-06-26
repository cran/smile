% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{single_tapmat}
\alias{single_tapmat}
\title{Matern (tapered) covariance function (scalar - generic)}
\usage{
single_tapmat(d, sigsq, phi, nu, theta)
}
\arguments{
\item{d}{a scalar representing the distance on which it is desired to
evaluate the covariance function.}

\item{sigsq}{the \eqn{\sigma^2} parameter from the Matern covariance
function.}

\item{phi}{the \eqn{\phi} parameter from the Matern covariance function,
controls the range of the spatial dependence.}

\item{nu}{the smoothness parameter \eqn{\nu} from the Matern covariance
function, controls the differentiability of the process.}

\item{theta}{the \eqn{\theta} tapper range.}
}
\value{
a scalar representing the (tapered matern) covariance between two
observations \code{d} apart of each other.
}
\description{
Computing the Matern covariance function for a single distance
measure, adapted from \code{geoR} using Wendland-1 as a tapper.
}
\seealso{
\code{\link{single_matern}}, \code{\link{single_matern5}}
\code{\link{single_exp}}, \code{\link{mat_cov}}
}
\keyword{internal}
