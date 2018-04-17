t.conf.plot <-
function(n,conf.level=0.95){
  x=seq(-4,4,0.1)
  curve(dt(x,n-1),-4,4)
  legend(-1.7,0.2,paste("1-a=",conf.level),bty="n")
  a=1-conf.level
  ta=qt(c(a/2,1-a/2),n-1)
  abline(v=c(ta),lty=3)
  ta
}
