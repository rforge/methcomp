PBreg <- function(x, y=NULL, conf.level=0.05, wh.meth=1:2) {
    meths  = c("Method A","Method B")
    if (is.null(y)) {
        if (inherits(x, "Meth"))  {
            meths    = rev(levels(x$meth)[wh.meth])
            a        = to.wide(x)[,meths]
            names(a) = c("x","y")
            }
        else {
            a        = x
            meths    = colnames(a)
            names(a) = c("x","y")
            }
        }
    else    a        = data.frame(x=x, y=y)
	if (any(a<0)) warning("Passing-Bablok regression requires positive numbers.")
    nread = nrow(a)
    a     = a[complete.cases(a$x,a$y),]
    n     = nrow(a)
    ch    = choose(n,2)
    nn    = combn(n,2)
    S     = I = vector(mode="numeric", length=ch)
    for (i1 in 1:ch)
        {
        data  = a[nn[,i1],]
        slope = (data$y[2]-data$y[1])/(data$x[2]-data$x[1])
        int   = data$y[2] - slope * data$x[2]
        S[i1] = slope
        I[i1] = int
        }
    S    = S[order(S)]
    I    = I[order(S)]
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
    colnames(m) = c("Estimate", "2.5%CI", "97.5%CI")
    invisible(structure(list(coefficients = m,
                                residuals = res,
                            fitted.values = fit,
                                    model = a,
                                        n = c(nread=nread,
                                              nused=n),
                                        S = S,
                                        I = I,
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

predict.PBreg <- function(object, newdata = object$model$x, interval="none", level=0.95,...) {
    S    = object$S
    ch   = object$adj["Ss"]
    x    = newdata
    N    = length(x)
    n    = nrow(object$model)
    K    = object$adj["K"]
    M1   = round((ch-qnorm(1-(1-level)/2) * sqrt((n * (n-1) * (2*n+5))/18))/2,0)
    M2   = ch-M1+1
    S    = S[(M1+K):(M2+K)]
    I    = NULL
    for (i1 in 1:length(S)) {
        I    = c(I, median(object$model$y - S[i1] * object$model$x))
    }

    y.ci = data.frame(fit=rep(NA,N), lwr=rep(NA, N), upr=rep(NA, N))
    for (i1 in 1:N) {
        y    = x[i1] * S + I
        y.ci[i1,] = quantile(y, c(0.5,0,1), na.rm=T)
    }

    if (interval=="confidence") { return(y.ci) }
    else { return(as.vector(y.ci$fit)) }
}

print.PBreg <- function(x,...) {
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
    cat("Linearity test not fully implemented in this version.\n\n")
}

plot.PBreg <- function(x, pch=21, bg="#2200aa33", xlim=c(0, max(x$model, na.rm=T)), ylim=c(0, max(x$model, na.rm=T)),
    xlab=x$meths[1], ylab=x$meths[2], subtype=1, colors = list(CI="#ccaaff50", fit="blue",
    ident = "black", ref="#99999955", bars="gray", dens="#8866aaa0", ref2=c("#1222bb99","#bb221299") ), ...)
    {
    ints    = c(x$coefficients[3],x$coefficients[1],x$coefficients[5])
    slos    = c(x$coefficients[4],x$coefficients[2],x$coefficients[6])
    if (any(subtype==1)) {
        mx      = max(xlim)
        my      = max(ylim)
        xs      = seq(-0.1*mx, 1.1*mx, length=70)
        cis     = cbind(x=xs, predict(x, newdata=xs, interval="confidence"))
        plot(x$model, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n", ...)
        px = c(-.1*mx, cis$x, 1.1*mx, 1.1*mx, rev(cis$x), -.1*mx)
        py = c(-.1*my, cis$upr, 1.1*my, 1.1*my, rev(cis$lwr), -.1*my)
        polygon(px,py,col=colors[["CI"]], border=NA)
        abline(0,1, lwd=0.5, lty=2, col=colors[["ident"]])
        points(x$model, pch=pch, bg=bg)
        abline(ints[2], slos[2], lwd=2, col=colors[["fit"]])

        text(mx*0.10,my*0.94, "Intercept =", adj=c(1,0), cex=0.8)
        text(mx*0.12,my*0.94, paste(formatC(ints[2], digits=4, format="g"), " [", formatC(ints[1], digits=4, format="g"),
            " : ", formatC(ints[3], digits=4, format="g"), "]", sep=""), adj=c(0,0), cex=0.8)
        text(mx*0.10,my*0.90, "Slope =", adj=c(1,0), cex=0.8)
        text(mx*0.12,my*0.90, paste(formatC(slos[2], digits=4, format="g"), " [", formatC(slos[1], digits=4, format="g"),
            " : ", formatC(slos[3], digits=4, format="g"), "]", sep="")  , adj=c(0,0), cex=0.8)
    }
    if (any(subtype==2)) {
        ranked = x$residuals[order(x$Di)]
        ylim   = c(-max(abs(ranked)),max(abs(ranked)))
        plot(ranked, ylab="Residuals", ylim=ylim, pch=pch, bg=bg)
        abline(0,0, col=colors[["ref"]], lwd=1.5)
    }
    if (any(subtype==3)) {
        ylim   = c(-max(abs(x$cusum)),max(abs(x$cusum)))
        plot(x$cusum, ylab="Cusum", type="l", ylim=ylim, lwd=2)
        abline(0,0, col=colors[["ref"]], lwd=1.5)
    }
    if (any(subtype==4)) {
        S = x$S[x$S> slos[2]-2.5*IQR(x$S, na.rm=T) & x$S< slos[2]+2.5*IQR(x$S, na.rm=T)]
        h = hist(S, xlab="Individual slopes (range: 5 x IQR)", col=colors[["bars"]], main="",...)
        d = density(S, na.rm=T)
        d$y = d$y*(max(h$counts)/max(d$y))
        lines(d, lwd=2, col=colors[["dens"]])
        abline(v=slos, col=colors[["ref2"]], lwd=c(0.5,1.5), lty=c(2,1))
    }
}

