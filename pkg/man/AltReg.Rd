\name{AltReg}
\alias{AltReg}
\title{Estimate in a method comparison model with replicates}
\description{
Estimates in the general model for method comparison studies with
replicate measurements by each method, allowing for a linear
relationship between methods, using the method of alternating
regressions. 
}
\usage{
  AltReg( data,
        linked = FALSE,
           IxR = linked,
           MxI = TRUE,
        varMxI = FALSE,
           eps = 0.001,
       maxiter = 50,
       int.loc = 0,
         trace = FALSE,
        sd.lim = 0.01 )
}
\arguments{
  \item{data}{Data frame with the data in the usual \code{Meth} format,
    i.e. it must have columns \code{meth}, \code{item}, \code{repl} and \code{y}}
  \item{linked}{Logical. Are the replicates linked across methods? If
    true, a random \code{item} by \code{repl} is included in the model.}
  \item{IxR}{Logical, alias for linked.}
  \item{MxI}{Logical, should the method by item effect (matrix effect)
    be in the model?} 
  \item{varMxI}{Logical, should the method by item effect have method-specific
    variances. Ignored if only two methods are compared. See details.}
  \item{eps}{Convergence criterion, the test is the max of the relative
    change since last iteration in both mean and variance parameters.}
  \item{maxiter}{Maximal number of iterations.}
  \item{int.loc}{Scalar. The location where the intercept is evaluated
    when returning the linear conversion paramaters between methods.}
  \item{trace}{Should a trace of the iterations be printed? If
    \code{TRUE} iteration number, convergence criterion and current
    estimates of means and sds are printed.}
  \item{sd.lim}{Estimated standard deviations below \code{sd.lim} are
    disregarded in the evaluation of convergence. See details.} 
  }
\value{
  A matrix with one row per method compared.
  There are columns for intercept and slope for each of the methods,
  as well as columns for each of the three variance components.

  Suppose methods are labelled \code{m1},  \code{m2} and  \code{m3}.
  Prediction of a measurement \code{y1} by method \code{m1} from an
  observation \code{y2} by method \code{m2} is obtained as
  \code{y1= A + B y2} where \code{A} and \code{B} are from the row
  labelled \code{m1}, columns labelled \code{a m1} and labelled
  \code{b m1}, respectivlely.
  }
\details{When fitting a model with both IxR and MxI interactions it may become
  very unstable to have different variances of the MxI random effects for each
  method, and hence the default option is to have a constant MxI variance
  across methods. On the other hand it may be grossly inadequate to assume
  these variances to be identical.
  
  If only two methods are compared, it is not possible to separate different
  variances of the MxI effect, and hence the \code{varMxI} is ignored in this
  case.

  The model fitted is formulated as:
  \deqn{y_{mir} = \alpha_m + \beta_m(\mu_i+a_{ir} + c_{mi}) +
    e_{mir}}{y_mir = alpha_m + beta_m*(mu_i+a_{ir}+c_mi) + e_mir}
  and the relevant parameters to report are the estimates sds of
  \eqn{a_{ir}}{a_{ir}} and \eqn{c_{mi}}{c_{mi}} multiplied with the corresonidng
  \eqn{\beta_m}{beta_m}. Therefore, different values of the variances for MxI
  and IxR are reported also when \code{varMxI==FALSE}. Note that
  \code{varMxI==FALSE} is the default and that this is the opposite of the
  default in \code{\link{BA.est}}.
  }
\references{B Carstensen: Comparing and predicting between several methods of
  measurement. Biostatistics (2004), 5, 3, pp. 399--413.
  }
\author{
  Bendix Carstensen, \email{bxc@steno.dk}
  }
\seealso{
\code{\link{BA.est}}
\code{\link{Meth.sim}}
  }
\examples{
dfr <- Meth.sim( Ni = 30,
                 Nm = 3,
               beta = c(0.9,0.8,1.1),
          sigma.mi  = c(4,5,8),
          sigma.ir  = 3,
          sigma.mir = c(5,4,3),
             m.thin = 1,
             i.thin = 1 )
levels(dfr$meth) <- paste( "m",1:3,sep="" )
str(dfr)
summary(dfr)
plot(dfr,var.names=TRUE)
# AltReg( dfr, linked=TRUE, trace=TRUE )
# AltReg( dfr, linked=TRUE, varMxI=TRUE, trace=TRUE )
data( sbp )
# AltReg( dfr, linked=TRUE, varMxI=TRUE, trace=TRUE )
}
\keyword{models}
\keyword{regression}
