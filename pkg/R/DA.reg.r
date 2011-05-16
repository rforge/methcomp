DA.reg <-
function( data,
     Transform = NULL,    # Transformation to be applied to y
     trans.tol = 1e-6 )
{
# This function makes regression of differences on averages for all pairs
# of methods and makes ad-hoc test for slope=1 and constant variance

# Check that the supplied data is actually a Meth object
dfr <- data <- Meth( data, print=FALSE )

# Transform the response if required
Transform <- choose.trans( Transform )
if( !is.null(Transform) )
  {
  check.trans( Transform, data$y, trans.tol=trans.tol )
  data$y <- Transform$trans( data$y )
  }

# Names and number of methods
Mn <-  levels( data$meth )
Nm <- nlevels( data$meth )

# Array to hold the conversion parameters
sd.int <- paste( " sd.|A=", gsub(" ","",formatC(median(dfr$y),digits=3)),sep="")
dnam <- list( "To:" = Mn,
            "From:" = Mn,
                      c("alpha","beta","sd.pred"," beta=1",
                        sd.int," slope(sd)"," sd.=K") )
conv <- array( NA, dim=sapply(dnam,length), dimnames=dnam )

# Fill in the array
for( i in 1:Nm ) conv[i,i,] <- c(0,1,rep(NA,5))
for( i in 1:(Nm-1) ) for( j in (i+1):Nm )
   {
   sb <- data[data$meth %in% Mn[c(i,j)],]
   cf <- da.reg1( sb )
   conv[j,i,] <- c(  -cf[1]   /(1+cf[2]/2),
                   (1-cf[2]/2)/(1+cf[2]/2),
                      cf[3]   /(1+cf[2]/2),cf[4:7])
   conv[i,j,] <- c(  +cf[1]   /(1-cf[2]/2),
                   (1+cf[2]/2)/(1-cf[2]/2),
                      cf[3]   /(1-cf[2]/2),cf[4:7])
   }

# Collect the results
res <- list( Conv = conv,
          VarComp = NULL,
             data = dfr )

class( res ) <- "MethComp"
attr( res, "Transform" ) <- Transform
res
}

da.reg1 <-
function( data )
{
data <- subset( data, select=c("meth","item","repl","y") )
Mn <- levels( data$meth )
wd <- to.wide( data, warn=FALSE )
wd <- wd[complete.cases(wd),]
if( nrow(wd)==0 ) return( rep(NA,5) )
else
{
D <-  wd[,Mn[1]]-wd[,Mn[2]]
A <- (wd[,Mn[1]]+wd[,Mn[2]])/2
m0 <- lm( D ~ A )
ms <- lm( abs(residuals(m0)) ~ A )
res <- c(coef(m0),
         summary(m0)$sigma,
         summary(m0)$coef[2,4],
         rbind(c(1,median(data$y)),c(0,1)) %*%
         summary(ms)$coef[1:2,1]*sqrt(pi/2),
         summary(ms)$coef[2,4] )
return(res)
}
}
