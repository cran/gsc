\name{gsc}
\alias{gsc}
\alias{print.gsc}
\alias{gsc_solve}
\title{generalised shape constraints}
\description{A gsc object represents generalised shape constraints. The gsc function creates a gsc object. The gsc_solve function transforms a given series to satisfy such constraints. Note that in future, these functions may be changed.}
\usage{
gsc (s0=NA, s1=NA, s2=NA, s3=NA, knots=numeric () )
\method{print}{gsc}(x, \dots)
gsc_solve (m, x, y, order=4, reweight=TRUE, p=0.98)
}
\arguments{
\item{m}{A gsc object.}
\item{s0}{Constraint string for 0th derivative.}
\item{s1}{Constraint string for 1th derivative.}
\item{s2}{Constraint string for 2th derivative.}
\item{s3}{Constraint string for 3th derivative.}
\item{knots}{Internal knots (in general, elements of x), that partition a function's domain.}
\item{x}{For gsc_solve, a vector of x values, which must be regularly spaced.}
\item{y}{A vector of y values.}
\item{order}{Degree of differencing, for roughness penalty.}
\item{reweight}{Scale roughness penalty parameter, heuristicly.}
\item{p}{Roughness penalty parameter, where 0 <= p < 1.}
\item{\dots}{.}
}

