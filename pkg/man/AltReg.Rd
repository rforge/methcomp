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
         trace = FALSE,
        sd.lim = 0.01,
     Transform = NULL,
     trans.tol = 1e-6 )
}
\arguments{
  \item{data}{Data frame with the data in long format,
    (or a \code{\link{Meth}} object)
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
  \item{trace}{Should a trace of the iterations be printed? If
    \code{TRUE} iteration number, convergence criterion and current
    estimates of means and sds are printed.}
  \item{sd.lim}{Estimated standard deviations below \code{sd.lim} are
    disregarded in the evaluation of convergence. See details.} 
  \item{Transform}{Transformation of data before analysis.
                   See \code{\link{choose.trans}}.}
  \item{trans.tol}{The tolerance used to check whether the supplied
                   transformation and its inverse combine to the identity.}
  }
\value{
  An object of class \code{c("MethComp","AltReg")}, which is a list with three
  elements:
  \item{Conv}{A 3-way array with the 2 first dimensions named "To:" and "From:",
              with methods as levels. The third dimension  is classifed by
              the linear parameters "alpha", "beta", and "sd.pred".}
  \item{VarComp}{A matrix with methods as rows and variance components as
                 columns. Entries are the estimated standrd deviations.}
  \item{data}{The orginal data used for the analysis. Needed for plotting
              purposes.}
  Moreover, if a transformation was applied before analysis, an attribute
  "Transform" is present; a list with two elements \code{trans} and \code{inv},
  both of which are functions, the first the transform, the last the inverse.
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
\code{\link{MethComp}}
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
