\name{ox}
\alias{ox}
\docType{data}
\title{Measurement of oxygen saturation in blood}
\description{
61 children had their blood oxygen content measured at the Children's
Hospital in Melbourne, either with a chemical method analysing gases in
the blood (\code{CO}) or by a pulse oximeter measuring transcutaneously
(\code{pulse}). Replicates are linked between methods; i.e. replicate 1
for each of the two methods are done at the same time. However,
replicate measurements were taken in quick succession so the pairs of
measurements are exchangeable within person.
}
\usage{
data(ox)
}
\format{
  A data frame with 354 observations on the following 4 variables.
  \describe{
    \item{\code{meth}}{Measurement methods, factor with levels \code{CO}, \code{pulse}}
    \item{\code{item}}{Id for the child}
    \item{\code{repl}}{Replicate of measurements. There were 3 measurements for most
    children, 4 had only 2 replicates with each method, one only 1}
    \item{\code{y}}{Oxygen saturation in percent.}
  }
}
%\source{
%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(ox)
str(ox)
ox <- Meth(ox)
with( ox, table(table(item)) )
# The effect of basing LoA on means over replicates:
par( mfrow=c(1,2), mar=c(4,4,1,4) )
BA.plot( ox, ymax=20 )
BA.plot( ox, ymax=20, mean.repl=TRUE )
}
\keyword{datasets}
