BA.est <-
function( data,
        linked = TRUE,  # Fit a model with replicate by item interaction
           IxR = linked,# Fit a model with replicate by item interaction 
           MxI = TRUE,  # To fit the model with a method by item interaction
        varMxI = TRUE,  # Should method by item have method-specific variance?
          bias = TRUE,  # Should we estimate a bias between the methods?
         alpha = 0.05
      # , plot = TRUE  # To be used for calling some kind of plotting function,
                       # possibly a variant of plot.Meth or generalized version
                       # of BA.plot.
        )
{
# Check that data has item, method and repl
rq.nam <- c("meth","item","repl","y")
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) < 3 ) stop(
"\nThe supplied dataframe misses columns named ", rq.nam[is.na(wh)], ".\n" )
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) == 3 ) stop(
"\nThe supplied dataframe misses the column named ", rq.nam[is.na(wh)], ".\n" )

# Should we use 2 or some t-quantile ( df = no. units minus no. param. )
cl.fact <- ifelse( missing(alpha),
                   2,
                   qt( 1-alpha/2,
                       nrow(data) - length(table(data$meth))
                                  - length(table(data$item)) - 1 ) )
# Fit the relevant model
model.fit <- VC.est( data = data,
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
  if( i!=j & MxI ) pred.var <- pred.var + tau[i]^2 + tau[j]^2
  if( i==j & IxR ) pred.var <- pred.var + 2*omega[i]^2
  LoA[row.no,4] <- sqrt( pred.var )
  LoA[row.no,2] <- LoA[row.no,1] - cl.fact*LoA[row.no,4]
  LoA[row.no,3] <- LoA[row.no,1] + cl.fact*LoA[row.no,4]
}
diags <- cumsum(1:Nm)
RC <- cbind( LoA[diags,4], cl.fact*LoA[diags,4] )
colnames( RC ) <- c("SD","Coef.")
if( !missing(alpha) ) colnames( RC )[2] <- paste( "Coef.(alpha=", alpha, ")", sep="" )
rownames( RC ) <- Mnam
list( Bias = Bias,
   VarComp = Vcmp,
       LoA = LoA[-diags,,drop=FALSE],
   RepCoef = RC )
}
