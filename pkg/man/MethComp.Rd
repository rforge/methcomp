\name{MethComp}
\Rdversion{1.1}
\alias{MethComp}
\alias{print.MethComp}
\alias{plot.MethComp}
\alias{lines.MethComp}
\alias{points.MethComp}
\title{
Summarize the conversion equations and prediciton intervals between methods.
}
\description{Takes the results from \code{\link{BA.est}}, \code{\link{AltReg}}
             or \code{\link{MCmcmc}} and returns \code{MethComp} object,
             suitable for displaying the relationship between methods.
}
\usage{
MethComp(obj)
\method{print}{MethComp}(x, digits=4, ... )
\method{plot}{MethComp}(x, wh.cmp = 1:2,
                          pl.type = "conv",
                            axlim = range(x$data$y,na.rm=TRUE),
                           points = FALSE,
                             grid = TRUE,
                           N.grid = 10,
                         col.grid = grey(0.9),
                        col.lines = "black",
                                ... )
\method{lines}{MethComp}(x, wh.cmp = getOption("MethComp.wh.cmp"),
                           pl.type = getOption("MethComp.pl.type"),
                             axlim = par("usr")[1:2],
                         col.lines = "black",
                               ... )
\method{points}{MethComp}(x, wh.cmp = getOption("MethComp.wh.cmp"),
                            pl.type = getOption("MethComp.pl.type"),
                                ... )
}
\arguments{
  \item{obj}{A \code{MethComp} or \code{\link{MCmcmc}} object.}
  \item{x}{A \code{MethComp} object.}
  \item{digits}{How many digits should be used when displaying conversion
                equations and variance componenets.}
  \item{wh.cmp}{Numeric. Which methods should be plotted.}
  \item{pl.type}{Character. If "conv" it will be a plot of two methods against
                 each other, otherwise it will be a plot of the 2nd minus the
                 1st versus the average; a Bland-Altman type plot.}
  \item{axlim}{The extent of the axes.}
  \item{points}{Logical. Should the points be included in the plot.}
  \item{grid}{Logical. Should there be a grid?}
  \item{N.grid}{Numeric. How many gridlines? If a vector of length>1, it will
                be taken as the position of the gridlines.}
  \item{col.grid}{Color of the gridlines.}
  \item{col.lines}{Color of the conversion lines.}
  \item{...}{Further arguments.}
}
\details{
\code{plot.MethComp} plots the conversion function with prediction limits. It also
sets the options \code{"MethComp.wh.cmp"} indicating which two methods are
plotted and \code{"MethComp.pl.type"} indicating whether a plot of methods
against each other or a Bland-Altman type plot of differences versus averages.

\code{lines.MethComp} and \code{points.MethComp} adds conversion lines with
prediction limits and points to a plot.
}
\value{
\code{MethComp} returns a \code{MethComp} object, which is a list with two
elements, \code{Conv}, a three-way array giving the linear conversion equations
between methods and \code{VarComp}, a two-way array classified by methods and
variance components.

The methods return nothing.
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
BA.ox <- BA.est( ox, linked=T )
print( BA.ox )
}
\keyword{design}
