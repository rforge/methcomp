PBreg <-
function(x, y=NULL, conf.level=0.05, wh.meth=1:2)
{
    meths <- c("x","y")
    if (is.null(y))
      {
      if ( inherits(x,"Meth") )
         {
         meths <- levels(x$meth)[wh.meth]
         a <- to.wide(x)[,meths]
         }
      else a = x
      }
    else            a = data.frame(x=x, y=y)
    names(a) = c("x","y")
    nread = nrow(a)
    a     = a[complete.cases(a$x,a$y),]
    n     = nrow(a)
    ch    = choose(n,2)
    nn    = combn(n,2)
    S     = NULL 
    for (i1 in 1:ch)
        {
        data  = a[nn[,i1],]
        slope = (data$y[2]-data$y[1])/(data$x[2]-data$x[1])
        S     = c(S,slope)
        }
    S    = S[order(S)]
    ch   = length(S)
    K    = length(S[S<(-1)])
    if ((ch %% 2)==0)  slo = (S[(ch)/2 + K] + S[(ch)/2 + K + 1])/2
    else               slo = S[(ch+1)/2 + K]
    M1   = round((ch-qnorm(1-conf.level/2) * sqrt((n * (n-1) * (2*n+5))/18))/2,0)
    M2   = ch-M1+1
    CIs  = c(S[M1+K], S[M2+K])
    int  = median(a$y - slo*a$x, na.rm=T)
    CIi  = c(median(a$y - CIs[2]*a$x, na.rm=T), median(a$y - CIs[1]*a$x, na.rm=T))
    fit  = slo*a$x+int
    res  = a$y - fit
    l    = length(res[res>0])
    L    = length(res[res<0])
    ri   = ifelse(res>0, sqrt(L/l), ifelse(res<0, -sqrt(l/L),0))
    Di   = (a$y + 1/slo*a$x - int)/sqrt(1+1/(slo^2))
    csum = cumsum(ri[order(Di)])
    m    = matrix(c(int, CIi, slo, CIs), nrow=2, byrow=T)
    rownames(m) = c("Intercept","Slope")
    colnames(m) = c("Estimate", "5%CI", "95%CI")
    invisible(structure(list(coefficients = m,
                                residuals = res,
                            fitted.values = fit,
                                    model = a,
                                        n = c(nread=nread,
                                              nused=n),
                                        S = S,
                                      adj = c(Ss=ch,
                                               K=K,
                                              M1=M1,
                                              M2=M2,
                                               l=l,
                                               L=L),
                                    cusum = csum,
                                       Di = Di,
                                    meths = meths ),
                        class="PBreg"))
}

print.PBreg <-
function(x,...)
{
    cat("\nPassing-Bablok linear regression of", x$meths[2], "on", x$meths[1], "\n\n")
    cat(paste("Observations read: ", x$n[1], ", used: ", x$n[2],"\n", sep=""))
    cat(paste("Slopes calculated: ", x$adj[1], ", offset: ", x$adj[2],"\n\n",sep=""))
    print(x$coefficients)
    cat("\nUnadjusted summary of slopes:\n")
    print(summary(x$S))
    cat("\nSummary of residuals:\n")
    print(summary(x$residuals))
    cat("\nTest for linearity:")
    # !!! Not working very well !!! 
    cat(if(any((x$cusum<1.36*sqrt(x$adj[5]+x$adj[6]))==FALSE)) " (failed)\n" else " (passed)\n")
    cat("Linearity test not fully implemented in this version.\n")
}
    
plot.PBreg <-
function(x, pch=21, bg="#2200aa33", xlim=c(0, max(x$model)), ylim=c(0, max(x$model)),
    xlab=x$meths[1], ylab=x$meths[2], subtype=1,...)
{
    int     = x$coefficients[1]
    slo     = x$coefficients[2]
    if (any(subtype==1)) {
        prec    = 30
        m       = max(x$model)
        cis     = data.frame(x=seq(-0.1*m, 1.1*m, length=50), 
                             y=int + seq(-0.1*m, 1.1*m, length=50)*slo)
        cis$hi  = cis$lo = cis$y
        a       = seq(x$coefficients[3],x$coefficients[5], length=prec)
        b       = seq(x$coefficients[4],x$coefficients[6], length=prec)
        for (i1 in 1:50) {
            for (i2 in 1:prec) {
                for (i3 in 1:prec) {
                    point = a[i2]+cis$x[i1]*b[i3]
                    if (cis$lo[i1]>point) cis$lo[i1]=point
                    if (cis$hi[i1]<point) cis$hi[i1]=point
            }   }   }
        plot(x$model, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n", ...)
        px = c(-.1*m, cis$x, 1.1*m, 1.1*m, rev(cis$x), -.1*m)
        py = c(-.1*m, cis$hi, 1.1*m, 1.1*m, rev(cis$lo), -.1*m)
        polygon(px,py,col="#ccaaff50", border=NA)
        abline(0,1, lwd=0.5, lty=2)
        points(x$model, pch=pch, bg=bg)
        abline(int, slo, lwd=2, col="blue")
        }
    if (any(subtype==2)) {
        ranked = x$residuals[order(x$Di)]
        ylim   = c(-max(abs(ranked)),max(abs(ranked)))
        plot(ranked, ylab="Residuals", ylim=ylim, pch=pch, bg=bg)
        abline(0,0, col="#99999955", lwd=1.5)
        }
    if (any(subtype==3)) {
        ylim   = c(-max(abs(x$cusum)),max(abs(x$cusum)))
        plot(x$cusum, ylab="Cusum", type="l", ylim=ylim, lwd=2)
        abline(0,0, col="#99999955", lwd=1.5)
        }
    if (any(subtype==4)) {
        S = x$S[x$S> slo-3.5*IQR(x$S) & x$S< slo+3.5*IQR(x$S)]
        h = hist(S, xlab="Individual slopes (range: 7 x IQR)", col="gray", main="",...)
        d = density(S)
        d$y = d$y*(max(h$counts)/max(d$y))
        lines(d, lwd=2, col="#8866aaa0")
        }
}
