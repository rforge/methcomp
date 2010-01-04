Meth <-
function( data=NULL,
          meth="meth", item="item", repl=NULL, y="y",
          print=!is.null(data), keep.vars=!is.null(data) )
{
dfr <- deparse(substitute(data))
was.dfr <- is.data.frame( data )
if( was.dfr ) dfr.nam <- names( data )

# Select the correct columns from a supplied data frame
if( was.dfr )
  {
  
  # Method
  if( is.character(meth) )
    meth <- match( meth, dfr.nam )
  if( is.numeric(meth) & length(meth)==1 )
    {
    taken <- meth.col <- meth
    meth <- data[,meth]
    }
  if( is.na(meth)[1] ) stop( "\nmeth not properly specified.")

  # Item
  if( is.character(item) )
    item <- match( item, dfr.nam )
  if( is.numeric(item) & length(item)==1 )
    {
    taken <- c(taken,item.col<-item)
    item <- data[,item]
    }
  if( is.na(item)[1] ) stop( "\nitem not properly specified.")
  Ni <- length( item )
  rows <- 1:Ni

  # Replicate
  if( is.null(repl) & "repl" %in% dfr.nam )
    repl <- "repl"
  if( is.character(repl) )
    repl <- match( repl, dfr.nam )
  if( is.numeric(repl) & length(repl)==1 )
    {
    taken <- c(taken,repl.col<-repl)
    repl <- data[,repl]
    }
  else repl <- rep(1,Ni)

  # Measurements
  # If we have supplied data in the wide form as indices of a dataframe:
  if( is.character(y) )
    y <- match( y, dfr.nam )
  if( is.numeric(y) & length(y)<length(item) )
    {
    taken <- c(taken,y.col<-y)
    y <- data[,y]
    }
  if( is.na(y)[1] ) stop( "\ny not properly specified.")

  # End of the data.frame case
  }

# The following also covers the situation where y is supplied as a dataframe
if( length(y) > 1 &
    length(y) < length(item) &
    is.list(y) )
  {
  mnam <- if( !is.null(names(y)) ) names(y)
        else paste( "Method", 1:length(y), sep="" )
  Nm <- length( mnam )
  meth <- rep( mnam, each=Ni )
  item <- rep( item, Nm )
  repl <- rep( repl, Nm )
     y <- unlist(y)
  rows <- rep( rows, Nm )
  }

# The resulting dataframe
res <- data.frame( meth = factor(meth),
                   item = factor(item),
                   repl = factor(repl),
                      y = y )
if( was.dfr & is.logical(keep.vars) )
  {
  if( keep.vars )
    {
    res <- data.frame( res, data[rows,-taken] )
    names(res)[-(1:4)] <- dfr.nam[-taken]
    }
  }
if( was.dfr & is.character(keep.vars) )
  keep.vars <- match( keep.vars, names(data) )
if( was.dfr & is.numeric(keep.vars) )
  {
  res <- data.frame( res, data[rows,keep.vars] )
  names(res)[-(1:4)] <- dfr.nam[keep.vars]
  }

# Remove missing y-values (and possible factor levels only in these)
if( any(is.na(res$y)) ) res <- Meth( res[!is.na(res$y),], keep.vars=TRUE, print=print )

# Check if there actually are replicates even if not indicated
made.repl <- FALSE
if( any( table(res$meth,res$item)> 1 ) )
  if( length(table(res$repl))==1 )
    if( names(table(res$repl))=="1" )
      {
      res <- make.repl( res )
      made.repl <- TRUE
      }
class( res ) <- c("Meth","data.frame")

if( print & was.dfr )
cat( "The following variables from the dataframe\n\"",
     dfr, "\" are used as the Meth variables:",
     paste( "\nmeth:", dfr.nam[meth.col],
            "\nitem:", dfr.nam[item.col], if( exists("repl.col") )
            "\nrepl:",                    if( exists("repl.col") )
                       dfr.nam[repl.col],
            "\n   y:", dfr.nam[   y.col], "\n" ), sep="" )
            
if( print )  print( summary.Meth( res ) )

if( made.repl )
  cat( "\nNOTE: Replication numbers generated in the order of the data\n" )
if( max( with( res, table( meth, item, repl ) ) ) > 1 )
  cat( "\nWARNING: You have chosen a replicate variable which is not unique\n",
       "        within each (n,item).\n" )
       
invisible( res )
}

# Utilities needed to preserve the Meth attribute
subset.Meth <-
function( x, ... )
{
y <- base:::subset.data.frame(x, ...)
# In order to reduce the factor levels to the ones actually present, use Meth()
return( Meth(y,keep.vars=TRUE,print=FALSE) )
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
