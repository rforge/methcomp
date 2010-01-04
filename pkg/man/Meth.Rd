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
\usage{
Meth( data=NULL,
      meth="meth", item="item", repl=NULL, y="y",
      print=!is.null(data), keep.vars=!is.null(data) )
\method{summary}{Meth}( object, ... )
\method{plot}{Meth}(x, y = NULL,
          col.LoA = "blue", col.pt = "black", cex.name = 2,
        var.range,
       diff.range,
        var.names = FALSE,
                      ... )
\method{subset}{Meth}(x, ... )
\method{sample}{Meth}( x,
                     how = "random",
                       N = if( how=="items" ) nlevels( x$item ) else nrow(x),
                     ... )
\method{transform}{Meth}(`_data`, ... )
}
\arguments{
  \item{data}{A dataframe.}
  \item{meth}{Vector of methods, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{item}{Vector of items, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{repl}{Vector of replicate numbers, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{y}{Vector or dataframe of measurements.
           If \code{y} is a dataframe or a list it is assumed (and checked)
           that the list elements are numerical vectors of the same length as
           the \code{meth}, \code{item} and \code{repl} vectors. The columns
           are assunmed to represent measurements by differnet methods.
           For the \code{plot} method the argument is
           either a vector of indices or names of methods to plot.}
  \item{print}{Logical: Should a summary result be printed?}
  \item{keep.vars}{Logical. Should the remaining variables from the dataframe
                   \code{data} be transferred to the \code{Meth} object.}
  \item{object}{A \code{Meth} object.}
  \item{x}{A \code{Meth} object.}
  \item{col.LoA}{What color should be used for the limits of agreement.}
  \item{col.pt}{What color should be used for the points.}
  \item{cex.name}{Character expansion factor for plotting method names }
  \item{var.range}{The range of the axes in the scatter plot and
                   the x-axis in the Bland-Altman plot be?}
  \item{diff.range}{The range of yaxis in the Bland-Altman plot. Defaults to
                    a range as the x-axis, but centered around 0.}
  \item{var.names}{If logical: should the individual panels be labelled with the
                   variable names?. If character, then the values of the character
                   will be used to label the methods.}
  \item{how}{What sampling strategy should be used. Charater strion, one of
             \code{"random"}, \code{"linked"} or \code{"item"}. Only the first
             letter is significant. See details for explanation.}
  \item{N}{How many observations should be sampled?}
  \item{_data}{A \code{Meth} object.}
  \item{...}{Ignored by the \code{Meth} and the \code{summary} and \code{sample}
             functions. In the \code{plot} function, parameters passed on the
             panel function plotting methods against each other, as well as those
             plotting differences against means.}
 }
\details{
  In order to perform analyses of method comparisons it is convenient to have a
  dataframe with classifying factors , \code{meth}, \code{item}, and possibly
  \code{repl} and the response variable \code{y}. This function creates such a
  dataframe, and gives it a class, \code{Meth}, for which there is a number of
  methods: \code{summary} - tabulation, \code{plot} - plotting and a couple of
  analysis methods.
  
  \code{sample.Meth} samples a \code{\link{Meth}} object with replacement.
  If \code{how=="random"}, a random sample of the rows are sampled, the existing
  values of \code{meth}, \code{item} and \code{y} are kept but new replicate
  numbers are generated.
  If \code{how=="linked"}, a random sample of the linked observations (i.e.
  observations with identical \code{item} and \code{repl} values) are sampled
  with replacement.
  }
\value{ The \code{Meth} function returns a \code{Meth} object which is a
  dataframe with columns \code{meth}, \code{item}, (\code{repl}) and \code{y}.
  \code{summary.Meht} returns a table classified by method and no. of
  replicate measurements, extended with columns of the total number of items,
  total number of observations and the range of the measurements.
  The \code{subset.Meth} returns a subset of the \code{Meth} object with
  complete informtion on a subset of the items.
  }
\author{Bendix Carstensen, \email{bxc@steno.dk} }
\examples{
data(fat)
# Different ways of selecting columns and generating replicate numbers
Sub1 <- Meth(fat,meth=2,item=1,repl=3,y=4,print=TRUE)
Sub2 <- Meth(fat,2,1,3,4,print=TRUE)
Sub3 <- Meth(fat,meth="Obs",item="Id",repl="Rep",y="Sub",print=TRUE)
summary( Sub3 )
plot( Sub3 )
# More than two methods
data( sbp )
plot( Meth( sbp ) )
# Creating non-unique replicate numbers per (meth,item) creates a warning:
data( hba1c )
hb1  <- with( hba1c,
              Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
hb2  <- with( subset(hba1c,type=="Cap"),
              Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
  }
\keyword{manip}