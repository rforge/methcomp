\name{plvol}
\alias{plvol}
\docType{data}
\title{Measurements of plasma volume measured by two different methods.}
\description{
 For each subject (\code{item}) the plasma volume is expressed as a percentage
 of the expected value for normal individuals. Two alternative sets of normal
 values are used, named Nadler and Hurley respectively.
}
\usage{data(plvol)}
\format{
  A data frame with 198 observations on the following 3 variables.
  \describe{
    \item{\code{meth}}{a factor with levels \code{Hurley} and \code{Nadler}}
    \item{\code{item}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\source{
 The datset is adapted from table 2 in: JM Bland and DG Altman: Measuring
 agreement in method comparison studies. Statistical Methods in Medical
 Research, 8:136-160, 1999. Originally supplied to Bland \& Altman by C Dore,
 see: Cotes PM, Dore CJ, Liu Yin JA, Lewis SM, Messinezy M, Pearson TC, Reid C.
 Determination of serum immunoreactive erythropoietin in the investigation of
 erythrocytosis. New England Journal of Medicine 1986; 315: 283-87.
}
\examples{
data(plvol)
str(plvol)
plot( y[meth=="Nadler"]~y[meth=="Hurley"],data=plvol,
      xlab="Plasma volume (Hurley) (pct)",
      ylab="Plasma volume (Nadler) (pct)" )
abline(0,1)
par( mar=c(4,4,1,4) )
BA.plot(plvol)
}
\keyword{datasets}
