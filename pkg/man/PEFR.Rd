\name{PEFR}
\alias{PEFR}
\docType{data}
\title{PEFR measurements with wright peak flow and mini wright peak
  flow meter.}
\description{Measurement of PEFR with wright peak flow and mini wright
  peak flow meter on 17 individuals. (PEFR=Peak Expiratory Flow Rate).
}
\usage{data(PEFR)}
\format{
  A data frame with 68 observations on the following 3 variables.
  \describe{
    \item{\code{meth}}{a factor with levels
          \code{Wright} and \code{Mini},
            representing measurements by a Wright peak flow
	    meter and a mini Wright meter respectively, in random order.}
    \item{\code{item}}{Numeric vector, the person ID.}
    \item{\code{y}}{Numeric vector, the measurements, i.e. PEFR for the
      two measurements with a Wright peak flow meter and a mini Wright
      meter respectively. This is numbers between 165 and 656 l/min}
    \item{repl}{Numeric vector, replicate number. Replicates are
      exchangeable within item.}
  }
}
\source{J. M. Bland and D. G. Altman (1986) Statistical Methods for Assessing
   Agreement Between Two Methods of Clinical Measurement, Lancet. 1986
   Feb 8;1(8476):307-10. 
}
\examples{
data(PEFR)
PEFR <- Meth(PEFR)
summary(PEFR)
plot(PEFR)
plot(perm.repl(PEFR))
}
\keyword{datasets}
