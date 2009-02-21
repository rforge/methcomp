Meth <-
function( meth, item, repl, y, ..., print=FALSE )
{
if( is.data.frame( meth ) )
  {
  # If no column numbers are given, use names
  if( missing( item ) )
    {
    # Chek that the relevant columns are there
    rq.nam <- c("meth","item","y")
    if( sum( !is.na( wh <- match( rq.nam, names( meth ) ) ) ) < 3 )
             stop( "\nMM:The supplied dataframe misses column(s) named ",
                   paste(rq.nam[is.na(wh)],collapse=" "), ".\n" )
    # Then contruct based on names
    Meth <- factor(meth$meth)
    Item <- factor(meth$item)
    Repl <- if( "repl" %in% names(meth) )
            factor(meth$repl) else factor(rep(1,length(Meth)))
    Y    <-        meth$y
    wh <- c("meth","item",if( "repl" %in% names(meth) ) "repl" else " ","y")
    }
  else
    {
    # Construct based on column numbers alone.
    Meth <- factor(meth[,item[1]])
    Item <- factor(meth[,item[2]])
    Repl <- if( !is.na(item[3]) & length(item)>3 )
            factor(meth[,item[3]]) else factor(rep(1,length(Meth)))
    Y    <-        meth[,item[4-(length(item)==3)]]
    # wh <- names( meth )[item]
    }
  }
else
  {
  if( missing(meth) |
      missing(item) |
      missing(y) )
      stop( "\nIf the first arument is not a data frame, \n",
            "All arguments 'meth', 'item', and 'y' must be given.\n" )
  llg <- c(length(meth),length(item),if(!missing(repl)) length(repl),length(y))
  if( min(llg)!=max(llg) )
      stop( "\nIf the first arument is not a data frame, \n",
             "All arguments 'meth', 'item', ('repl') and 'y' must hve the same length.\n" )
                         Meth <- factor( meth )
                         Item <- factor( item )
  if( !missing( repl ) ) Repl <- factor( repl )
                         Y    <-            y
  }
res <- data.frame( meth=Meth, item=Item, repl=Repl, y=Y )
# Remove missing y-values (and possible factor levels only in these)
if( any(is.na(res$y) ) ) res <- Meth( res[!is.na(res$y),] )

# Check if there actually are replicates even if not indicated
made.repl <- FALSE
if( any( table( Meth, Item )> 1 ) )
  if( length(table(Repl))==1 )
    if( names(table(Repl))=="1" )
      {
      res <- make.repl( res )
      made.repl <- TRUE
      }
class( res ) <- c("Meth","data.frame")
if( print )
  {
  cat( "\nA Meth object is created:\n\n" )
  str( res )
  cat( "\n" )
  print( summary.Meth( res ) )
  }
if( made.repl )
  cat( "\nNOTE: Replicate numbers generated in the order of the data\n" )
if( max( with( res, table( meth, item, repl ) ) ) > 1 )
  cat( "\nWARNING: You have chosen a replicate variable which is not unique\n",
       "        within each (meth,item).\n" )

invisible( res )
}

# Utilities needed to preserve the Meth attribute
subset.Meth <-
function( x, ... )
{
y <- base:::subset.data.frame(x, ...)
class( y ) <- c("Meth", "data.frame")
# In order to reduce the factor levels to the ones actually present, use Meth()
return( Meth(y) )
}
  
sample.Meth <-
function( x, size, ... )
{
if( !inherits( x, "Meth" ) ) stop( "\n1st argument must be a Meth object.\n" )
if( size < 1 ) size <- round( size * nlevels( x$item ) )
wh.i <- sample( levels(x$item), size, replace=FALSE )
return( Meth( x[!is.na( match( x$item, wh.i ) ),]) )
}

transform.Meth <-
function( `_data`, ...)
{
    save.at <- attributes(`_data`)
    y <- base:::transform.data.frame(`_data`, ...)
    save.at[["names"]] <- attr(y, "names")
    attributes(y) <- save.at
    y
}
