\name{PBreg}
\alias{PBreg}
\alias{print.PBreg}
\title{Passing-Bablok regression}
\description{
  Implementation of the Passing-Bablok's procedure for assessing of
  the equality of measurements by two different analytical methods.
}
\usage{
  PBreg(x, y=NULL, conf.level=0.05, wh.meth=1:2)
  \method{print}{PBreg}(x,...)
}
\arguments{
  \item{x}{a \code{\link{Meth}} object, alternatively a numeric vector of
    measurements by method A, or a data frame of exactly two columns, first
    column with measurements by method A, second column with measurements
    by method B. }
  \item{y}{a numeric vector of measurements by method B - must be of
    the same length as \code{x}. If not provided, \code{x} must be the
    \code{\link{Meth}} object or a data frame of exactly 2 columns.}
  \item{conf.level}{confidence level for calculation of confidence
    boundaries - 0.05 is the default. }
  \item{wh.meth}{Which of the methods from the \code{Meth} object are used in
    the regression.}
  \item{\ldots}{other parameters, currently ignored.}
}
\details{
  This is an implementation of the original Passing-Bablok
  procedure of fitting unbiased linear regression line to data in the
  method comparison studies. It calcualtes the unbiased slope and intercept,
  along with their confidence intervals. However, the tests for
  linearity is not yet fully implemented.

  It doesn't matter which results are assigned to "Method A" and
  "Method B", however the "Method A" results will be plotted on the
  x-axis by the \code{plot} method.
  }
\note{
  Please note that this method can become very computationally
  intensive for larger numbers of observations. One can expect a reasonable
  computation times for datasets with fewer than 100 observations.
  }
\value{
  \code{PBreg} returns an object of class \code{"PBreg"}, for which
  the \code{print}, \code{predict} and \code{plot} methods are defined.

  An object of class \code{"PBreg"} is a list composed of the following
  elements:

  \item{coefficients}{a matrix of 3 columns and 2 rows, containing the
    estimates of the intercept and slope, along with their confidence
    boundaries.}
  \item{residuals}{defined as in the \code{"lm"} class, as the response
    minus the fitted value.}
  \item{fitted.values}{the fitted values.}
  \item{model}{the model data frame used.}
  \item{n}{a vector of two values: the number of observations read, and
    the number of observations used.}
  \item{S}{A vector of all slope estimates.}
  \item{I}{A vector of all intercept estimates.}
  \item{adj}{A vector of fit parameters, where \emph{Ss} is the number of
    estimated slopes (\code{length(S)}), \emph{K} is the offset for slopes <(-1),
    \emph{M1} and \emph{M2} are the locations of confidence boundaries
    in \code{S}, and  \emph{l} and \emph{L} are the numbers of points above and
    below the fitted line, used in cusum calculation.}
  \item{cusum}{A vector of cumulative sums of residuals sorted by the D-rank.}
  \item{Di}{A vector of D-ranks.}
}
\references{Passing, H. and Bablok, W. (1983), A New Biometrical Procedure
  for Testing the Equality of Measurements from Two Different Analytical
  Methods. \emph{Journal of Clinical Chemistry and Clinical Biochemistry},
  Vol 21, 709--720}
\author{Michal J. Figurski \email{mfigrs@gmail.com}}
\seealso{
  \code{\link{plot.PBreg}, \link{predict.PBreg}, \link{Deming}}.
}
\examples{
  ## Model data frame generation
  a <- data.frame(x=seq(1, 30)+rnorm(mean=0, sd=1, n=30),
                  y=seq(1, 30)*rnorm(mean=1, sd=0.4, n=30))

  ## Call to PBreg
  x <- PBreg(a)
  print(x)

  par(mfrow=c(2,2))
  plot(x, s=1:4)

  ## A real data example
  data(milk)
  milk <- Meth(milk)
  summary(milk)
  PBmilk <- PBreg(milk)
  par(mfrow=c(2,2))
  plot(PBmilk, s=1:4)
}
