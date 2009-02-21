\name{Meth}
\alias{Meth}
\alias{summary.Meth}
\alias{plot.Meth}
\alias{subset.Meth}
\alias{sample.Meth}
\alias{transform.Meth}
\title{Create a Meth object representing a method comparison study}
\description{
  Creates a dataframe with columns \code{meth}, \code{item}, (\code{repl}) and
  \code{y}.
  }
\usage{Meth( meth,
             item,
             repl,
                y,
              ...,
             print = FALSE)
\method{summary}{Meth}( object, ... )
\method{plot}{Meth}( x, y = NULL,
                   col.LA = "blue",
                 cex.name = 2,
                var.range,
               diff.range,
                var.names = FALSE,
                      ... )
\method{subset}{Meth}(x, ... )
\method{sample}{Meth}(x, size, ... )
\method{transform}{Meth}(`_data`, ... )
}
\arguments{
  \item{meth}{Vector of methods, numeric, character of factor.
              May also be a dataframe. If this has columns, \code{meth},
              \code{item}, (\code{repl}) and \code{y}, these are used.}
  \item{item}{Vector of items. If \code{meth} is a dataframe, \code{item} is
              taken as the columns of the \code{meth} dataframe to use as vectors
              of \code{meth}, \code{item}, (\code{repl}) and \code{y}.}
  \item{repl}{Vector of replicate numbers.}
  \item{y}{Vector of measurements. For the \code{plot} method the argument is
           either a vector indices or names of methods to plot.}
  \item{print}{Logical: Should a summary result be printed?}
  \item{object}{A \code{Meth} object.}
  \item{x}{A \code{Meth} object.}
  \item{col.LA}{What color should be used for the limits of agreement.}
  \item{cex.name}{Character expansion factor for plotting method names }
  \item{var.range}{The range of the axes in the scatter plot and
                   the x-axis in the Bland-Altman plot be?}
  \item{diff.range}{The range of yaxis in the Bland-Altman plot. Defaults to
                    a range as the x-axis, but centered around 0.}
  \item{var.names}{If logical: should the individual panels be labelled with the
                   variable names?. If character, then the values of the character
                   will be used to label the methods.}
  \item{size}{The number or fraction (if \code{size}<1) of items to sample.}
  \item{_data}{A \code{Meth} object.}
  \item{...}{Ignored by the \code{Meth} and the \code{summary} functions.
             In the \code{plot} function, parameters passed on the panel
             function plotting methods against each other, as well as those
             plotting differences against means.}
 }
\details{
  In order to perform analyses of method comparisons it is convenient to have a
  dataframe with classifying factors , \code{meth}, \code{item}, and possibly
  \code{repl} and the response variable \code{y}. This function creates such a
  dataframe, and gives it a class, \code{Meth}, for which there is a number of
  methods: \code{tab} - tabulation, \code{plot} - plotting and a couple of
  analysis methods (not fixed yet).
  }
\value{ The \code{Meth} function returns a \code{Meth} object which is a
  dataframe with columns \code{meth}, \code{item}, (\code{repl}) and \code{y}.
  \code{summary.Meth} returns a table classified by method and no. of
  replicate measurements, extended with columns of the total number of items,
  total number of observations and the range of the measurements.
  The \code{sample} returns a subset of the \code{Meth} object with complete
  interformtion a sample of the items.
  }
\author{Bendix Carstensen, \email{bxc@steno.dk} }
\examples{
data(fat)
# Different ways of selecting columns and generating replicate numbers
Sub1 <- Meth(fat,c(2,1,3,4),print=TRUE)
Sub2 <- Meth(fat,c(2,1,NA,4),print=TRUE)
Sub3 <- Meth(fat,c(2,1,4),print=TRUE)
summary( Sub3 )
plot( Sub3 )
# More than two methods
data( sbp )
plot( Meth( sbp ) )
# Creating non-unique replicate numbers per (meth,item) creates a warning:
data( hba1c )
hb1  <- with(        hba1c,              Meth( dev, item, d.ana-d.samp, y, print=TRUE ) )
hb2  <- with( subset(hba1c,type=="Cap"), Meth( dev, item, d.ana-d.samp, y, print=TRUE ) )
summary( hb1 )
summary( hb2 )
  }
\keyword{manip}