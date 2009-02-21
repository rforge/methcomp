\name{DA.reg}
\alias{DA.reg}
\title{Make a regresion of differeneces on averages}
\description{
  For each pair of methods in \code{data}, a regression of the differences on the
  averages between methods is made and a linear relationship between methods
  with prediction standard deviations is derived.
  }
\usage{
  DA.reg(data)
  }
\arguments{
  \item{data}{A \code{\link{Meth}} object. May also be a data frame with columns
              \code{meth}, \code{item} and \code{y}.}
  }
\details{
  If the input object contains replicate measurements these are taken as separate
  items in the order they appear in the dataset.
  }
\value{A three-dimensional array, with dimensions \code{From}, \code{To} (both
       with levels equal to the methods in \code{data}) and an unnamed dimension
       with levels \code{"alpha"}, \code{"beta"}, \code{"sd.pred"},
       \code{"beta=1"} and \code{"s.d.=K"}. Conversting from method \eqn{j} to
       method \eqn{k} using \deqn{y_{k|l}=\alpha+\beta y_l} with prediction
       standard deviation \eqn{\sigma}, just requires the entries
       \code{[l,k,c("alpha","beta","sd.pred"]}. The two last entries ae p-values
       for the hypothese the \eqn{\beta=1} and that the standard error is
       constant over the range.
  }
\references{
  B Carstensen: Limits of agreement: How to use the regression of differences on
  averages. Technical Report 08.6, Department of Biostatistics, University of
  Copenhagen,
  \url{http://www.pubhealth.ku.dk/bs/publikationer/Research_report_08-6.pdf},
  2008.
  }
\author{Bendix Carstensen, Steno Daibetes Center, \code{bxc$steno.dk}}
\examples{
data( milk )
DA.reg( milk )
round( ftable( DA.reg( milk ) ), 3 )
data( sbp )
round( ftable( DA.reg( sbp ) ), 3 )
}
\keyword{models}
\keyword{regression}
