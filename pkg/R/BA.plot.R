BA.plot <-
function( y1, y2,
      meth.names = NULL,
       mean.repl = FALSE,
       conn.repl = !mean.repl,
        lwd.conn = 1,
        col.conn = "black",
     comp.levels = 2:1,
             ... )
{
  if( is.data.frame( y1 ) )
    {
    # If in the long form, convert to 2-column matrix
    if( inherits(y1,"Meth") )
      {
      # Select the methods to compare and subset the Meth object
      if( is.numeric(comp.levels) ) comp.levels <- levels(y1$meth)[comp.levels]
      y1 <- y1[y1$meth %in% comp.levels,]
      # Are there replicates in the subset?
      repl <- has.repl( y1 )
      # Make a dataframe of the means if required
      if( repl & mean.repl )
        {
        yy <- as.data.frame( as.table(
                     tapply( y1[,"y"],
                             list(y1[,"item"],y1[,"meth"]),
                             mean ) ) )
        names( yy ) <- c("item","meth","y")
        }
      else yy <- y1
      # Make a wide dataset
      yy <- to.wide( yy, warn=FALSE )
      yy <- yy[complete.cases(yy),]
      n1 <- comp.levels[1]
      n2 <- comp.levels[2]
      if( nrow(yy)==0 ) stop( "No items have measurements by both method '",
                              n1, "' and '", n2, "'." )
      y1 <- yy[,n1]
      y2 <- yy[,n2]
      BlandAltman( y1, y2, x.name=n1, y.name=n2, ... )
    # Connecting replicates
      if( repl & conn.repl )
        {
        mm <- yy[,c(n1,n2)]
        mm[,1] <- ave( yy[,n1], yy$item )
        mm[,2] <- ave( yy[,n2], yy$item )
        segments( (mm[,1]+mm[,2])/2,
                   mm[,1]-mm[,2],
                  (yy[,n1]+yy[,n2])/2,
                   yy[,n1]-yy[,n2],
                   col=col.conn, lwd=lwd.conn )
        }
      }
    else
    # If a two-column matrix
      {
      if( dim(y1)[2]==2 )
        {
        meth.names <- if( is.null( meth.names ) ) names( y1 )
                      else meth.names
        y1 <- y1[,1]
        y2 <- y1[,2]
        n1 <- meth.names[1]
        n2 <- meth.names[2]
        BlandAltman( y1, y2, x.name=n1, y.name=n2, ... )
        }
      }
    }
  else
  # If two vectors are supplied
    {
    if( is.null( meth.names ) )
      {
      n1 <- deparse( substitute( y1 ) )
      n2 <- deparse( substitute( y2 ) )
      }
    else
      {
      n1 <- meth.names[1]
      n2 <- meth.names[2]
      }
    BlandAltman( y1, y2, x.name=n1, y.name=n2, ... )
    }
}
