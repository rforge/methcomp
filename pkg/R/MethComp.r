MethComp <-
function( obj )
{
if( inherits( obj, "MethComp" ) )
  {
  Conv    <- obj$Conv
  VarComp <- obj$VarComp
  }
else
if( inherits( obj, "MCmcmc" ) )
  {
  obj <- summary( obj )
  ca <- obj$conv.array
  names( dimnames( ca ) )[1:2] <- names( dimnames( ca ) )[2:1]
  Conv <- ca
  for( i in 1:3 ) Conv[,,i] <- t(ca[,,i])
  VarComp <- obj$VarComp[,-4,1]
  names(dimnames(VarComp)) <- c("Method","s.d.")
  }
else stop( "Input object (argument) must have class 'MethComp' or 'MCmcmc'.\n",
           "It has class ", class( obj ) )
res <- list( Conv = Conv,
          VarComp = VarComp )
class( res ) <- "MethComp"
attr( res, "Transform" ) <- attr( obj, "Transform" )
return( res )
}

################################################################################
## print for MethComp
################################################################################
print.MethComp <-
function( x, digits=4, ... )
{
if( !is.null( trans <- attr(x,"Transform") ) )
  cat( "\nNote: Response transformed by: ",
       paste( body(trans$trans) ), "\n\n" )
print( round( ftable( x$Conv ), digits ) )
cat("\n")
print( round( x$VarComp, digits ) )
}

################################################################################
## plot, lines and points for MethComp
################################################################################
plot.MethComp <-
function( x,
       wh.cmp = 1:2,
      pl.type = "conv",
        axlim = range(x$data$y,na.rm=TRUE),
       points = FALSE,
         grid = TRUE,
       N.grid = 10,
     col.grid = grey(0.9),
    col.lines = "black",
          ... )
{
Mn <- dimnames( x[[1]] )[[1]]
if( pl.type == "conv" ) # Conversion plot
  {
  plot( NA, xlim=axlim, ylim=axlim, type="n",
            xlab=Mn[wh.cmp[1]],
            ylab=Mn[wh.cmp[2]] )
  # Grid?
  if( is.logical( grid ) ) if( grid )
    grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim, n=N.grid )
  abline( h=grid, v=grid, col=col.grid )
  }
else # Bland-Altman type plot
  {
  plot( NA, xlim=axlim, ylim=axlim-mean(axlim), type="n",
            xlab=paste( "(", Mn[wh.cmp[1]], "+",
                             Mn[wh.cmp[2]], ") / 2" ),
            ylab=paste( Mn[wh.cmp[2]], "-", Mn[wh.cmp[1]] ) )
  # Grid?
  if( is.logical( grid ) ) if( grid )
                             {
           grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim, n=N.grid )
          hgrid <- pretty( axlim-mean(axlim),
                           n = if( length(N.grid)>1 ) length(N.grid)
                               else N.grid )
                             }
  abline( h=hgrid, v=grid, col=col.grid )
  }
box()
options( MethComp.pl.type = pl.type,
         MethComp.wh.cmp = wh.cmp )

              lines.MethComp( x, col.lines = col.lines, ... )
if( points ) points.MethComp( x, ... )
}

################################################################################
## lines.MethComp
################################################################################
lines.MethComp <-
function( x,
       wh.cmp = getOption("MethComp.wh.cmp"),
      pl.type = getOption("MethComp.pl.type"),
        axlim = par("usr")[1:2],
    col.lines = "black",
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
# Define the points to plot
  m1 <- seq(axlim[1],axlim[2],,100)
trm1 <- trf( m1 )
trm2 <- cbind( x$Conv[wh.cmp[2],wh.cmp[1],  "alpha"] +
               x$Conv[wh.cmp[2],wh.cmp[1],   "beta"] * trm1,
               x$Conv[wh.cmp[2],wh.cmp[1],"sd.pred"] ) %*% rbind( c(1, 1, 1),
                                                                c(0,-2, 2) )
  m2 <- itr( trm2 )

if( pl.type == "conv" )
     matlines( m1, m2,
               lwd=c(3,1,1), lty=1, col=col.lines )
else matlines( (m1+m2)/2, m2-m1,
               lwd=c(3,1,1), lty=1, col=col.lines )
}

################################################################################
## points.MethComp
################################################################################
points.MethComp <-
function( x,
       wh.cmp = getOption("MethComp.wh.cmp"),
      pl.type = getOption("MethComp.pl.type"),
          ... )
{
Mn <- dimnames( x[[1]] )[[1]]
wide <- to.wide( x$data )
if( pl.type == "conv" ) # Conversion plot
  points( wide[,Mn[wh.cmp[1]]], wide[,Mn[wh.cmp[2]]], ... )
else # Bland-Altman type plot
  points( (wide[,Mn[wh.cmp[1]]]+wide[,Mn[wh.cmp[2]]])/2,
           wide[,Mn[wh.cmp[2]]]-wide[,Mn[wh.cmp[1]]], ... )
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
                 log = list( trans = log, inv = exp ),
                 exp = list( trans = exp, inv = log ),
                sqrt = list( trans = sqrt, inv = function(x) x^2 ),
                  sq = list( trans = function(x) x^2, inv = sqrt ),
               logit = list( trans = function(p) log(p/(1-p)),
                               inv = function(x) 1/(1+exp(-x)) ),
            pctlogit = list( trans = function(p) log(p/(100-p)),
                               inv = function(x) 100/(1+exp(-x)) ),
                 cll = list( trans = function(p) log(-log(1-p)),
                               inv = function(x) 1-exp(-exp(x)) ),
                  ll = list( trans = function(p) log(-log(p)),
                               inv = function(x) exp(-exp(x)) ),
                  NULL )
  if( is.null(ltr) ) cat('Transformation "', paste("\b",trans,sep=""),
                         '\b" not known --- none applied.\n')
  }
else
if( is.list(tr) )
  {
  if( is.function(tr$trans) & is.function(tr$inv) ) ltr <- tr
  else stop( "Argument to 'choose.trans' must be character or a list of two functions" )
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
if( any( abs( y - trans$inv(trans$trans(y)) ) > trans.tol ) )
  stop( "The transformation and its inverse seems not to agree:\n",
        "y - inv(trans(y)) has range ",
        paste( range(difference), collapse=" to " ),
        "\nyou may want to to change the current trans.tol=", trans.tol )
}