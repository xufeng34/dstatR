z.conf.plot <-
function(conf.level=0.95){
  x=seq(-3,3,0.1)
  curve(dnorm(x),-3,3)
  legend(-1.3,0.3,paste("1-a=",conf.level),bty="n")
  a=1-conf.level
  za=qnorm(c(a/2,1-a/2))
  abline(v=c(za),lty=3)
  za
}
