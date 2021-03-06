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
\description{Takes the results from \code{\link{BA.est}},
             \code{\link{DA.reg}}, \code{\link{AltReg}} or
	     \code{\link{MCmcmc}} and returns a \code{MethComp} object,
             suitable for displaying the relationship between methods in print
             pr graphic form.}
\usage{
MethComp(obj)
\method{print}{MethComp}(x, digits=3, ... )
\method{plot}{MethComp}(x,
      wh.comp = 1:2,
      pl.type = "conv",
     dif.type = "lin",
      sd.type = "const",
        axlim = range(x$data$y,na.rm=TRUE),
       diflim = axlim-mean(axlim),
       points = FALSE,
    repl.conn = FALSE,
     col.conn = "gray",
     lwd.conn = 1,
         grid = TRUE,
       h.grid = TRUE,
     col.grid = grey(0.9),
          lwd = c(3,1,1),
    col.lines = "black",
   col.points = "black",
   pch.points = 16,
   cex.points = 1,
          eqn = is.null(attr(x,"Transform")),
      col.eqn = col.lines,
     font.eqn = 2,
       digits = 2,
         mult = FALSE,
        alpha = NULL,
          ... )
\method{lines}{MethComp}(x,
                   wh.comp = getOption("MethComp.wh.comp"),
                   pl.type = getOption("MethComp.pl.type"),
                  dif.type = getOption("MethComp.dif.type"),
                   sd.type = getOption("MethComp.sd.type"),
                 col.lines = "black",
                       lwd = c(3,1,1),
                    digits = 3,
                      mult = FALSE,
                     alpha = NULL,
                       ... )
\method{points}{MethComp}(x,
                    wh.comp = getOption("MethComp.wh.comp"),
                    pl.type = getOption("MethComp.pl.type"),
                 col.points = "black",
                 pch.points = 16,
                 cex.points = 1,
                  repl.conn = FALSE,
                   col.conn = "gray",
                   lwd.conn = 1,
                       mult = FALSE,
                        ... )
}
\arguments{
  \item{obj}{A \code{MethComp} or \code{\link{MCmcmc}} object.}
  \item{x}{A \code{MethComp} object.}
  \item{wh.comp}{Numeric or character of length 2. Which two methods should be plotted.}
  \item{pl.type}{Character. If "conv" it will be a plot of two methods against
                 each other, otherwise it will be a plot of the 1st minus the
                 2nd versus the average; a Bland-Altman type plot.}
  \item{dif.type}{Character. If "lin" (the default) a linear
    relationship between methods is allowed. Otherwise a constant
    difference is assumed and LoA can be indicated on the plot.}
  \item{sd.type}{Should the estimated dependence of the SD (from
    \code{\link{DA.reg}} be used when plotting prediction limits?}
  \item{axlim}{The extent of the axes of the measurements.}
  \item{diflim}{The extent of the axis of the differences.}
  \item{points}{Logical. Should the points be included in the plot.}
  \item{repl.conn}{Logical. Should replcate measurements be connected;
    this assumes linked replicates.}
  \item{col.conn}{Color of the lines connecting replicates.}
  \item{lwd.conn}{Width of the connection lines.}
  \item{grid}{Should there be a grid? If numeric, how many gridlines? If
    a vector of length>1, it will be taken as the position of the
    vertical gridlines.}
  \item{h.grid}{Position of the horizontal gridlines. Ignored if \code{pl.type="conv"}.}
  \item{col.grid}{Color of the gridlines.}
  \item{col.lines}{Color of the conversion lines.}
  \item{lwd}{Numerical vector of length 3. Width of the conversion line and the
    prediction limits.}
  \item{pch.points}{Plot character for points.}
  \item{cex.points}{Character expansion for points.}
  \item{col.points}{Color of the points.}
  \item{eqn}{Logical. Should the conversion equation be printed on the plot.}
  \item{col.eqn}{Color of the conversion formula}
  \item{font.eqn}{font for the conversion formula}
  \item{digits}{The number of digits after the decimal point in the
    conversion formulae.}
  \item{mult}{Logical. Should ratios be plotted on a log-scale instead
  of differences on a linear scale? See description of the argument for \code{\link{BA.plot}}.}
  \item{alpha}{1 minus the confidence level for the prediction
    interval. If not given, the prediction interval is constructed as
    plus/minus twice the SD.}
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
prediction limits and points to a plot.}
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
\dontrun{
AR.ox <- AltReg( ox, linked=TRUE  )
print( AR.ox )
plot( AR.ox ) }
}
\keyword{design}
