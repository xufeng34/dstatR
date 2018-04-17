t.conf.int <-
function(x,conf.level=0.95) {
  n = length(x)
  xbar = mean(x)
  a=1-conf.level
  ta=qt(1-a/2,n-1)
  s=sd(x)
  se=s/sqrt(n)
  c(mean(x)- se* ta, mean(x)+se *ta)
}
