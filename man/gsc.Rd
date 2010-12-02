\name{gsc}
\alias{gsc}
\alias{smoothc}
\alias{fit.gsc}
\title{generalised shape constraints}
\description{Functions for constrained smoothing.}
\usage{
gsc (k=numeric (), s0=NA, s1=NA, s2=NA, s3=NA)
smoothc (g, x, y, ns=50, degree=2, smoothness=0.67,
	smooth.in=TRUE, smooth.out=FALSE)
\method{fit}{gsc}(g, x, y, \dots)
}
\arguments{
\item{k}{Internal knots, that partition a function's domain.}
\item{s0}{Constraint vector for 0th derivative.}
\item{s1}{Constraint vector for 1th derivative.}
\item{s2}{Constraint vector for 2th derivative.}
\item{s3}{Constraint vector for 3th derivative.}
\item{g}{A gsc object.}
\item{x}{The x values.}
\item{y}{The y values.}
\item{ns}{Number of points for the smooth series.}
\item{degree}{Degree of local polynomials.}
\item{smoothness}{Bandwidth parameter, as proportion of x range.}
\item{smooth.in}{Smooth raw series, before transformation.}
\item{smooth.out}{Smooth constrained series, after transformation.}
\item{\dots}{Ignored.}
}


