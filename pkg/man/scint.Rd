\name{scint}
\alias{scint}
\docType{data}
\title{Relative renal function by Scintigraphy}
\description{
  Measurements of the relative kidney function (=renal function) for 111
  patients. The percentage of the total renal function present in the left
  kidney is determined by one reference method, \code{DMSA} (static) and by
  one of two dynamic methods, \code{DTPA} or \code{EC}.
  }
\usage{data(scint)}
\format{
  A data frame with 222 observations on the following 5 variables:
  \describe{
    \item{\code{meth}}{Measurement method, a factor with levels
                       \code{DMSA}, \code{DTPA}, \code{EC}.}
    \item{\code{item}}{Patient identification.}
    \item{\code{y}}{Percentage of total kidney function in the left kidney.}
    \item{\code{age}}{Age of the patient.}
    \item{\code{sex}}{Sex of the patient, a factor with levels \code{F}, \code{M}.}
    }
  }
\source{
  F. C. Domingues, G. Y. Fujikawa, H. Decker, G. Alonso, J. C. Pereira, P. S. Duarte:
  Comparison of Relative Renal Function Measured with Either
  99mTc-DTPA or 99mTc-EC Dynamic Scintigraphies with that
  Measured with 99mTc-DMSA Static Scintigraphy.
  International Braz J Urol Vol. 32 (4): 405-409, 2006
  }
\examples{
  data(scint)
  str(scint)
  # Make a Bland-Altman plot for each of the possible comparisons:
  par(mfrow=c(1,2),mgp=c(3,1,0)/1.6,mar=c(3,3,1,3))
  BA.plot(scint,comp.levels=c(1,2),ymax=15,digits=1,cex=2)
  BA.plot(scint,comp.levels=c(1,3),ymax=15,digits=1,cex=2)
  }
\keyword{datasets}
