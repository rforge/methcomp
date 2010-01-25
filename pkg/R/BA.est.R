BA.est <-
function( data,
        linked = TRUE,  # Fit a model with replicate by item interaction
           IxR = has.repl(data), # Fit a model with replicate by item interaction
           MxI = has.repl(data), # To fit the model with a method by item interaction
        varMxI = TRUE,  # Should method by item have method-specific variance?
        IxR.pr = FALSE, # Should the IxR varation be included with the prediction var?
          bias = TRUE,  # Should we estimate a bias between the methods?
         alpha = 0.05,
     Transform = NULL,
     trans.tol = 1e-6
        )
{
# Check that data has item, method and repl
rq.nam <- c("meth","item","repl","y")
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) < 3 ) stop(
"\nThe supplied dataframe misses columns named ", rq.nam[is.na(wh)], ".\n" )
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) == 3 ) stop(
"\nThe supplied dataframe misses the column named ", rq.nam[is.na(wh)], ".\n" )

# Only complete cases
dfr <- Meth( data[,c("meth","item","repl","y")], print=FALSE )

# Exchangeability:
if( !missing(linked) ) IxR <- linked
# Should we use 2 or some t-quantile ( df = no. units minus no. param. )
cl.fact <- ifelse( missing(alpha),
                   2,
                   qt( 1-alpha/2,
                       nrow(data) - length(table(dfr$meth))
                                  - length(table(dfr$item)) - 1 ) )
# Transform the response if required
Transform <- choose.trans( Transform )
if( !is.null(Transform) )
  {
  check.trans( Transform, dfr$y, trans.tol=trans.tol )
  dfr$y <- Transform$trans( dfr$y )
  }
# Fit the relevant model
model.fit <- VC.est( data = dfr,
                      IxR = IxR,
                      MxI = MxI,
                   varMxI = varMxI,
                     bias = bias )
Nm   <- length( model.fit$Bias )
Mnam <-  names( model.fit$Bias )

# Tease out the elements necessary to compute limits of agreement &c.
Bias <- model.fit$Bias
Vcmp <- model.fit$VarComp
omega <- Vcmp[,"IxR"]
tau   <- Vcmp[,"MxI"]
sigma <- Vcmp[,"res"]
 
# The limits of agreement
LoA <- matrix( NA, Nm*(Nm+1)/2, 4 )
colnames( LoA ) <- c("Mean","Lower","Upper", "SD")
rownames( LoA ) <- 1:nrow(LoA)
row.no <- 0
for( i in 1:Nm ) for( j in 1:i )
{
  row.no <- row.no + 1
  rownames( LoA )[row.no] <- paste( Mnam[i], "-", Mnam[j], " " )
  LoA[row.no,1] <- Bias[i] - Bias[j]
  pred.var <- sigma[i]^2 + sigma[j]^2
  if( i!=j & MxI    ) pred.var <- pred.var + tau[i]^2 + tau[j]^2
  if( i==j & IxR.pr ) pred.var <- pred.var + 2*omega[i]^2
  LoA[row.no,4] <- sqrt( pred.var )
  LoA[row.no,2] <- LoA[row.no,1] - cl.fact*LoA[row.no,4]
  LoA[row.no,3] <- LoA[row.no,1] + cl.fact*LoA[row.no,4]
}
diags <- cumsum(1:Nm)
RC <- cbind( LoA[diags,4], cl.fact*LoA[diags,4] )
colnames( RC ) <- c("SD","Coef.")
if( !missing(alpha) ) colnames( RC )[2] <- paste( "Coef.(alpha=", alpha, ")", sep="" )
rownames( RC ) <- Mnam

dnam <- list( "To:" = Mnam,
            "From:" = Mnam,
                      c("alpha","beta","sd","  LoA: lower", "upper") )
Conv <- array( NA, dim=sapply( dnam, length ), dimnames=dnam )
Conv[,,2] <- 1
Conv[,,1] <- outer( Bias, Bias, "-" )
# Derive the prediction errors;
# For the same method it is replications errors,
# plus variation betrween replicates if required
for( i in 1:Nm ) for( j in 1:Nm )
   {
   Conv[i,j,3] <- sqrt(sum(Vcmp[c(i,j),c(if(i!=j)          "MxI",
                                         if(i==j & IxR.pr) "IxR",
                                                           "res")]^2))
   Conv[i,j,4:5] <- Conv[i,j,1]+c(-1,1)*cl.fact*Conv[i,j,3]
   }
# Return data on the original scale
res <- list( Conv = Conv,
          VarComp = Vcmp,
              LoA = LoA[-diags,,drop=FALSE],
          RepCoef = RC,
             data = data )
class( res ) <- c("MethComp","BA.est")
attr( res, "Transform" ) <- Transform
attr( res, "Repeatability" ) <- if( IxR.pr ) "Replication included"
                                else         "Replication excluded"
res
}

bias.BA.est <-
function( obj, ref=1, ... )
{
if( is.character( ref ) ) ref <- match(ref,dimnames(obj$Conv)[[1]])
if( is.na(ref) ) stop( "Wrong reference levels given, the methods are:\n  ",
                       paste( dimnames(obj$Conv)[[1]], collapse=", " ) )
if( inherits(obj,"BA.est") ) return( obj$Conv[,1,1]-obj$Conv[ref,1,1] )
else stop( "'bias' is only meaningful for objects of class BA.est" )
}
