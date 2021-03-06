\name{TDI}
\alias{TDI}
\title{Compute Lin's Total deviation index}
\description{
  This index calculates a value such that a certain fraction of difference
  between methods will be numerically smaller than this.
  }
\usage{
TDI( y1, y2, p = 0.05, boot = 1000, alpha = 0.05 )
}
\arguments{
  \item{y1}{Measurements by one method.}
  \item{y2}{Measurements by the other method}
  \item{p}{The fraction of items with differences numerically exceeding the TDI}
  \item{boot}{If numerical, this is the number of bootstraps. If \code{FALSE}
  no confidence interval for the TDI is produced.}
  \item{alpha}{1 - confidende degree.}
}
\details{
  If \code{boot==FALSE} a single number, the TDI is returned.
  If \code{boot} is a number, the median and the 1-\code{alpha}/2 central interval
  based on \code{boot} resamples are returned too, in a named vector of length
  4.
}
\value{
  A list with 3 components. The names of the list are preceeded by the
  criterion percentage, i.e. the percentage of the population that the TDI is
  devised to catch.
  \item{TDI}{The numerically computed value for the TDI. If \code{boot} is
             numeric, a vector of median and a bootstrap c.i. is appended.}
  \item{TDI}{The approximate value of the TDI}
  \item{Limits of Agreement}{Limits of agreement}
}
\references{LI Lin:
Total deviation index for measuring
individual agreement with applications in
laboratory performance and bioequivalence,
Statistics in Medicine, 19, 255-270 (2000)}
\author{Bendix Carstensen, bxc@steno.dk}
\note{
The TDI is a measure which esentially is a number K such that the interval [-K,K]
contains the limits of agreement.
}
\seealso{\code{\link{BA.plot}},\code{\link{corr.measures}}}
\examples{
data(plvol)
pw <- to.wide(plvol)
with(pw,TDI(Hurley,Nadler))
}
\keyword{models}
\keyword{regression}
