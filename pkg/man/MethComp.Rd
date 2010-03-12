\name{MethComp}
\Rdversion{1.1}
\alias{MethComp}
\alias{print.MethComp}
\alias{plot.MethComp}
\alias{lines.MethComp}
\alias{points.MethComp}
\title{
Summarize conversion equations and prediction intervals between methods.
}
\description{Takes the results from \code{\link{BA.est}}, \code{\link{AltReg}}
             or \code{\link{MCmcmc}} and returns a \code{MethComp} object,
             suitable for displaying the relationship between methods in print
             pr graphic form.}
\usage{
MethComp(obj)
\method{print}{MethComp}(x, digits=3, ... )
\method{plot}{MethComp}(x,
                   wh.cmp = 1:2,
                  pl.type = "convert",
                    axlim = range(x$data$y,na.rm=TRUE),
                   diflim = axlim-mean(axlim),
                   points = FALSE,
                     grid = TRUE,
                   N.grid = 10,
                 col.grid = grey(0.9),
                col.lines = "black",
               col.points = "black",
                      eqn = tolower(substr(pl.type,1,1))=="c" &
                            is.null(attr(x,"Transform")),
                  col.eqn = col.lines,
                 font.eqn = 2,
                   digits = 1,
%               Transform = FALSE,
                      ... )
\method{lines}{MethComp}(x,
                   wh.cmp = getOption("MethComp.wh.cmp"),
                  pl.type = getOption("MethComp.pl.type"),
%               Transform = getOption("MethComp.transform"),
                col.lines = "black",
                      lwd = c(3,1),
                      ... )
\method{points}{MethComp}(x,
                   wh.cmp = getOption("MethComp.wh.cmp"),
                  pl.type = getOption("MethComp.pl.type"),
 %              Transform = getOption("MethComp.transform"),
               col.points = "black",
                      ... )
}
\arguments{
  \item{obj}{A \code{MethComp} or \code{\link{MCmcmc}} object.}
  \item{x}{A \code{MethComp} object.}
  \item{digits}{How many digits should be used when displaying conversion
                equations and variance components?}
  \item{wh.cmp}{Numeric of length 2. Which two methods should be plotted.}
  \item{pl.type}{Character. If "conv" it will be a plot of two methods against
                 each other, otherwise it will be a plot of the 2nd minus the
                 1st versus the average; a Bland-Altman type plot.}
  \item{axlim}{The extent of the axes of the measurements.}
  \item{diflim}{The extent of the axis of the differences.}
  \item{points}{Logical. Should the points be included in the plot.}
  \item{grid}{Logical. Should there be a grid?}
  \item{N.grid}{Numeric. How many gridlines? If a vector of length>1, it will
                be taken as the position of the gridlines.}
  \item{col.grid}{Color of the gridlines.}
  \item{col.lines}{Color of the conversion lines.}
  \item{lwd}{Numerical vector of length 2. Width of the conversion line and the
             prediction limits respectively.}
  \item{col.points}{Color of the points.}
  \item{eqn}{Logical. Should the conversion equation be printed on the plot.}
  \item{col.eqn}{Color of the conversion formula}
  \item{font.eqn}{font for the conversion formula}
  \item{...}{Further arguments.}
}
\details{
Using \code{MethComp} on the results from \code{\link{BA.est}} or
\code{\link{AltReg}} is not necessary, as these two functions already return
objetcs of class \code{MethComp}.

\code{plot.MethComp} plots the conversion function with prediction
limits; always using the original scale of measurements. It also
sets the options \code{"MethComp.wh.cmp"} indicating which two methods are
plotted and \code{"MethComp.pl.type"} indicating whether a plot of methods
against each other or a Bland-Altman type plot of differences versus averages.
By default the conversion lines are plotted.

\code{lines.MethComp} and \code{points.MethComp} adds conversion lines with
prediction limits and points to a plot.
}
\value{
\code{MethComp} returns a \code{MethComp} object, which is a list with three
elements, \code{Conv}, a three-way array giving the linear conversion equations
between methods, \code{VarComp}, a two-way array classified by methods and
variance components and \code{data}, a copy of the original \code{\link{Meth}}
object supplied --- see the description under \code{\link{BA.est}}.

A \code{MethComp} object has an attribute \code{Transform},
which is either NULL, or a named list with elements \code{trans} and \code{inv},
both of which are functions. The first is the transformation applied
to measurements before analysis; the results are all given on the transformed
scale. The second is the inverse transformation; this is only used when plotting
the resulting relationship between methods.

The methods \code{print}, \code{plot}, \code{lines} and \code{points}
return nothing. 
}
\author{
Bendix Carstensen, Steno Diabetes Center, \email{bxc@steno.dk}.
}
\seealso{
\code{\link{BA.est}}
\code{\link{AltReg}}
\code{\link{MCmcmc}}
}
\examples{
data( ox )
BA.ox <- BA.est( ox, linked=TRUE )
print( BA.ox )
AR.ox <- AltReg( ox, linked=TRUE  )
print( AR.ox )
plot( AR.ox )
}
\keyword{design}
