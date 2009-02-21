DA.reg <-
function( data )
{
# Makes regression of differences on averages for all pairs of methods

# Names and number of methods
Mn <-  levels( data$meth )
Nm <- nlevels( data$meth )

# Array to hold the conversion parameters
dnam <- list( "From:" = Mn,
                "To:" = Mn,
                c("alpha","beta","sd.pred","beta=1","s.d.=K") )
conv <- array( NA, dim=sapply(dnam,length), dimnames=dnam )

# Fill in the array
for( i in 1:Nm ) conv[i,i,] <- c(0,1,NA,NA,NA)
for( i in 1:(Nm-1) ) for( j in (i+1):Nm )
   {
   sb <- data[data$meth %in% Mn[c(i,j)],]
# Why does this not work:
#   sb <- subset( data, meth %in% Mn[c(i,j)] )
# - it produces the error:
# Error in inherits(x, "factor") : object "i" not found
   cf <- da.reg1( sb )
   conv[i,j,] <- c(  -cf[1]   /(1+cf[2]/2),
                   (1-cf[2]/2)/(1+cf[2]/2),
                      cf[3]   /(1+cf[2]/2),cf[4],cf[5])
   conv[j,i,] <- c(  +cf[1]   /(1-cf[2]/2),
                   (1+cf[2]/2)/(1-cf[2]/2),
                      cf[3]   /(1-cf[2]/2),cf[4],cf[5])
   }
   
return( conv )
}

da.reg1 <-
function( data )
{
data$meth <- factor( data$meth )
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
         summary(ms)$coef[2,4])
return(res)
}
}