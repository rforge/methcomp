\name{abconv}
\alias{abconv}
\title{
  Derive linear conversion coefficients from a set of indeterminate coefficients
  }
\description{
  If a method comparison model is defined as $y_{mi} = \alpha_m + \beta_m \mu_i$,
  $m=1,2$ the coefficients of the linear conversion form method 1 to 2
  are computed as well as the point where the linear conversion function
  intersects the identity line. The function is designed to work on
  numerical vectors of posterior samples from BUGS output.
}
\usage{
 abconv( a1, b1 = 1:4, a2 = NULL, b2 = NULL,
         col.names = c("alpha.2.1", "beta.2.1", "id.2.1") )
}
\arguments{
  \item{a1}{Numerical vector of intercepts for first method.
            Alternatively a dataframe where the vectors are selected from.}
  \item{b1}{Numerical vector of slopes for first method. If \code{a1} is a
            dataframe, this is assumed to be a numerical vector of length 4
            pointing to the columns of \code{a1} with the intercepts and
	    slopes.}
  \item{a2}{Numerical vector of intercepts for second method.}
  \item{b2}{Numerical vector of slopes for second method.}
  \item{col.names}{Names for the resulting three vectors.}
}
\value{
  A dataframe with three columns: intercept and slope for the conversion
  from method 1 to method 2, and the value where the conversion is the
  identity.
}
\references{
  B Carstensen: Comparing and predicting between several methods
  of measurement, Biostatistics, 5, pp 399-413, 2004
}
\author{Bendix Carstensen, Steno Diabetes Center,
        \url{http://www.biostat.ku.dk/~bxc}}
\seealso{\code{\link{BA.plot}}, \code{\link{MCmcmc}}}
\examples{
abconv( 0.3, 0.9, 0.8, 0.8 )
}
\keyword{design}
