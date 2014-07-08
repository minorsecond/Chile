{
  xname <- deparse(substitute(x)) # get name of variable
  zx <- (x - mean(x))/sd(x)
  wzx <- lag.listw(wfile,zx)
  morlm <- lm(wzx ~ zx)
  aa <- morlm$coefficients[1]
  mori <- morlm$coefficients[2]
  par(pty="s")
  plot(zx,wzx,xlab=xname,ylab=paste("Spatial Lag of ",xname))
  abline(aa,mori,col=2)
  abline(h=0,lty=2,col=4)
  abline(v=0,lty=2,col=4)
  title(paste("Moran Scatterplot I= ",format(round(mori,4))))
}