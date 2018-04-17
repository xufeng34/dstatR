z.conf.int <-
function(x,sigma,conf.level=0.95){
  n = length(x)
  a=1-conf.level
  Za=qnorm(1-a/2)
  se=sigma/sqrt(n)
  c(mean(x)- se*Za, mean(x) + se*Za)
}
