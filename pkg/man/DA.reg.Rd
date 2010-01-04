\name{DA.reg}
\alias{DA.reg}
\title{Make a regresion of differeneces on averages}
\description{
  For each pair of methods in \code{data}, a regression of the differences on the
  averages between methods is made and a linear relationship between methods
  with prediction standard deviations is derived.
  }
\usage{
  DA.reg(data,
    Transform = NULL,
    trans.tol = 1e-6)
  }
\arguments{
  \item{data}{A \code{\link{Meth}} object. May also be a data frame with columns
              \code{meth}, \code{item} and \code{y}.}
  \item{Transform}{A character string, or a list of two functions, each other's
                   inverse. The measurements are transformed by this before
                   analysis. Possibilities are: "exp", "log", "logit",
                   "pctlogit" (transforms percentages by the logit), "sqrt",
                   "sq" (square), "cll" (complementary log-minus-log), "ll"
                   (log-minus-log). For further details see
                   \code{\link{choose.trans}}.}
  \item{trans.tol}{The tolerance used to check whether the supplied
                   transformation and its inverse combine to the identity.
                   Only used if \code{Transform} is a list of two functions.}
  }
\details{
  If the input object contains replicate measurements these are taken as separate
  items in the order they appear in the dataset.
  }
\value{A \code{\link{MethComp}} object, i.e. a list with three components,
       \code{Conv}, \code{VarComp}, and \code{data}. \code{Conv} is a
       three-dimensional array, with dimensions \code{To}, \code{From} (both
       with levels equal to the methods in \code{data}) and an unnamed dimension
       with levels \code{"alpha"}, \code{"beta"}, \code{"sd.pred"},
       \code{"beta=1"} and \code{"s.d.=K"}. Conversting from method \eqn{l} to
       method \eqn{k} using \deqn{y_{k|l}=\alpha+\beta y_l} with prediction
       standard deviation \eqn{\sigma}, just requires the entries
       \code{[k,l,c("alpha","beta","sd.pred"]}. The two last entries are p-values
       for the hypotheses: 1) \eqn{\beta=1} and 2) standard errors are
       constant over the range. The latter is derived by regressiin the absoulte
       values of the residuals on the averages.

       The \code{VarComp} element of the list is \code{NULL}, and only present
       for compatibility with the print method for \code{MethComp} objects.
       
       The \code{data} element is the input datframe. The mesurements in\code{y}
       are left un-transformed.
  }
\references{
  B Carstensen: Limits of agreement: How to use the regression of differences on
  averages. Technical Report 08.6, Department of Biostatistics, University of
  Copenhagen,
  \url{http://www.pubhealth.ku.dk/bs/publikationer/Research_report_08-6.pdf},
  2008.
  }
\author{Bendix Carstensen, Steno Diabetes Center, \code{bxc$steno.dk}}
\examples{
data( milk )
DA.reg( milk )
data( sbp )
print( DA.reg( sbp ), digits=3 )
}
\keyword{models}
\keyword{regression}
