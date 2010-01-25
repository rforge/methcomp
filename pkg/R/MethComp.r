MethComp <-
function( obj )
{
if( inherits( obj, "MethComp" ) )
  {
  Conv    <- obj$Conv
  VarComp <- obj$VarComp
  dfr     <- data
  }
else
if( inherits( obj, "MCmcmc" ) )
  {
  dfr <- attr( obj, "data" )
  obj <- summary( obj )
  ca  <- obj$conv.array
  # Store the array in a different layout [This is crazy]
  names( dimnames( ca ) )[1:2] <- names( dimnames( ca ) )[2:1]
  Conv <- ca
  for( i in 1:3 ) Conv[,,i] <- t(ca[,,i])
  VarComp <- obj$VarComp[,-4,1]
  names(dimnames(VarComp)) <- c("Method","s.d.")
  }
else stop( "Input object (argument) must have class 'MethComp' or 'MCmcmc'.\n",
           "It has class ", class( obj ) )
res <- list( Conv = Conv,
          VarComp = VarComp,
             data = dfr )
class( res ) <- "MethComp"
attr( res, "Transform" ) <- attr( obj, "Transform" )
return( res )
}

################################################################################
## print method for MethComp
################################################################################
print.MethComp <-
function( x, digits=3, ... )
{
if( !is.null( trans <- attr(x,"Transform") ) )
  cat( "\nNote: Response transformed by: ",
       paste( body(trans$trans) ), "\n\n" )
cat("\n Conversion between methods:\n")
print( round( ftable( x$Conv ), digits ) )
# Account for the results from DA.reg where variances are not estimated
if( !is.null( x$VarComp ) )
  {
  cat("\n Variance components (sd):\n")
  print( round( x$VarComp, digits ) )
  }
}

################################################################################
## plot, lines and points for MethComp
################################################################################
plot.MethComp <-
function( x,
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
          eqn = tolower(substr(pl.type,1,1))=="c" & is.null(attr(x,"Transform")),
      col.eqn = col.lines,
     font.eqn = 2,
       digits = 1,
          ... )
{
# All method names
Mn <- dimnames( x[["Conv"]] )[[1]]
# Those two plotted here in the right order
Mn <- Mn[wh.cmp[1:2]]

if( tolower(substr(pl.type,1,1)) == "c" )
  # Conversion plot
  {
  plot( NA, xlim=axlim, ylim=axlim, type="n",
            xlab=Mn[1], ylab=Mn[2], ... )
  # Grid?
  if( is.logical( grid ) ) if( grid )
    grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim, n=N.grid )
  abline( h=grid, v=grid, col=col.grid )
  }
else
  # Bland-Altman type plot
  {
  plot( NA, xlim=axlim, ylim=diflim, type="n",
            xlab=paste( "(", Mn[1], "+",
                             Mn[2], ") / 2" ),
            ylab=paste( Mn[2], "-", Mn[1] ), ... )
  # Grid?
  if( is.logical( grid ) )
    if( grid )
      {
       grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim, n=N.grid )
      hgrid <- pretty( axlim-mean(axlim),
                       n = if( length(N.grid)>1 ) length(N.grid)
                           else N.grid )
      abline( h=hgrid, v=grid, col=col.grid )
      }
  }
box()

if( eqn )
  {
  A <- x[["Conv"]][Mn[2],Mn[1],"alpha"]
  B <- x[["Conv"]][Mn[2],Mn[1], "beta"]
  S <- x[["Conv"]][Mn[2],Mn[1],   "sd"]
  y.x <- paste( Mn[2], "=\n",
                formatC( A, format="f", digits=digits ), "+",
                formatC( B, format="f", digits=digits ),
                Mn[1], "\n  (",
                formatC( S, format="f", digits=digits ), ")" )
  x.y <- paste( Mn[1], "=\n",
                formatC( -A/B, format="f", digits=digits ), "+",
                formatC(  1/B, format="f", digits=digits ),
                Mn[2], "\n  (",
                formatC(  S/B, format="f", digits=digits ), ")" )
  # Heights and widths of the equations
  wul <- strwidth ( y.x, font=2 )
  hul <- strheight( y.x, font=2 )
  wlr <- strwidth ( x.y, font=2 )
  hlr <- strheight( x.y, font=2 )
  if( is.numeric(grid) )
    {
    rect( par("usr")[1], par("usr")[4],
          par("usr")[1]+wul+0.2*hul, par("usr")[4]-1.2*hul,
          border=col.grid, col="white" )
    rect( par("usr")[2], par("usr")[3],
          par("usr")[2]-wlr-0.2*hlr, par("usr")[3]+1.2*hlr,
          border=col.grid, col="white" )
    }
  text( par("usr")[1]+0.1*hul    , par("usr")[4]-0.1*hul, y.x,
        adj=c(0,1), font=font.eqn, col=col.eqn )
  text( par("usr")[2]-0.1*hlr-wlr, par("usr")[3]+0.1*hlr, x.y,
        adj=c(0,0), font=font.eqn, col=col.eqn )
  }

options( MethComp.pl.type = pl.type,
          MethComp.wh.cmp = wh.cmp )

if( points ) points.MethComp( x, col.points= col.points, ... )
              lines.MethComp( x, col.lines = col.lines, ... )
box()
}

################################################################################
## lines.MethComp
################################################################################
lines.MethComp <-
function( x,
       wh.cmp = getOption("MethComp.wh.cmp"),
      pl.type = getOption("MethComp.pl.type"),
    col.lines = "black",
          lwd = c(3,1),
          ... )
{
Mn <- dimnames( x[[1]] )[[1]]

# Define the transformation
if( is.null( attr( x, "Transform" ) ) )
  trf <- itr <- function( x ) x
else {
  trf <- attr( x, "Transform" )$trans
  itr <- attr( x, "Transform" )$inv
     }

# The slope and the sd, used to plot the lines
A <- x$Conv[wh.cmp[2],wh.cmp[1],"alpha"]
B <- x$Conv[wh.cmp[2],wh.cmp[1], "beta"]
S <- x$Conv[wh.cmp[2],wh.cmp[1],   "sd"]

# Define the method 1 points to use, making sure that the points span also the
# range of the BA-type plots:
axlim <- par("usr")[1:2]
# m1 is on the original scale, so is axlim;
# but A, B and S are for transformed measurements
# Expand well beyond the limits to accommodate the differnce-plot too
  m1 <- seq( axlim[1]-diff(axlim), axlim[2]+diff(axlim),, 500 )
trm1 <- trf( m1 )
trm2 <- cbind( A+B*trm1, S ) %*% rbind( c(1, 1, 1),
                                        c(0,-2, 2) )
  m2 <- itr( trm2 )

if( tolower(substr(pl.type,1,1)) == "c" )
     matlines( m1, m2,
               lwd=lwd[c(1,2,2)], lty=1, col=col.lines )
else matlines( (m1+m2)/2, m2-m1,
               lwd=lwd[c(1,2,2)], lty=1, col=col.lines )
}

################################################################################
## points.MethComp
################################################################################
points.MethComp <-
function( x,
       wh.cmp = getOption("MethComp.wh.cmp"),
      pl.type = getOption("MethComp.pl.type"),
   col.points = "black",
          ... )
{
Mn <- dimnames( x[[1]] )[[1]]
wide <- to.wide( x$data )
if( is.function( col.points ) )
   col.points <- col.points(nlevels(wide$item))[wide$item]
if( tolower(substr(pl.type,1,1)) == "c" )
  # Conversion plot
  points( wide[,Mn[wh.cmp[1]]], wide[,Mn[wh.cmp[2]]],
          col = col.points, ... )
else
  # Bland-Altman type plot
  points( (wide[,Mn[wh.cmp[1]]]+wide[,Mn[wh.cmp[2]]])/2,
           wide[,Mn[wh.cmp[2]]]-wide[,Mn[wh.cmp[1]]],
           col = col.points, ... )
}

################################################################################
## choose.trans
################################################################################
choose.trans <-
function( tr )
# Function to allow a character argument to choose a transformation and the
# required inverse.
{
if( is.character(tr) )
  {
  ltr <- switch( tr,
                 log = list( trans = log,
                               inv = exp ),
                 exp = list( trans = exp,
                               inv = log ),
                sqrt = list( trans = sqrt,
                               inv = function(x) x^2 ),
                  sq = list( trans = function(x) x^2,
                               inv = sqrt ),
               logit = list( trans = function(p) log(p/(1-p)),
                               inv = function(x) 1/(1+exp(-x)) ),
            pctlogit = list( trans = function(p) log(p/(100-p)),
                               inv = function(x) 100/(1+exp(-x)) ),
                 cll = list( trans = function(p) log(-log(1-p)),
                               inv = function(x) 1-exp(-exp(x)) ),
                  ll = list( trans = function(p) log(-log(p)),
                               inv = function(x) exp(-exp(x)) ),
                  NULL )
  if( is.null(ltr) ) cat('Transformation "', paste("\b",tr,sep=""),
                         '\b" not known --- none applied.\n')
  }
else
if( is.list(tr) )
  {
  if( is.function(tr[[1]]) & is.function(tr[[2]]) )
    {
    ltr <- tr
    names( ltr ) <- c("trans","inv")
    }
  else stop( "Argument to 'choose.trans' must be character or\n",
             "a list of two functions: the transformtion and its inverse." )
  }
else ltr <- NULL
invisible( ltr )
}

################################################################################
## check.trans
################################################################################
check.trans <-
function( trans, y, trans.tol=10e-6 )
{
if( any( abs( dif <- y - trans$inv(trans$trans(y)) ) > trans.tol ) )
  stop( "The transformation and its inverse seem not to agree:\n",
        "y - inv(trans(y)) has range ",
        paste( range(dif), collapse=" to " ),
        "\nyou may want to to change the current trans.tol=", trans.tol )
}
