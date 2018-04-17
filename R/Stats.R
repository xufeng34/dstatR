Stats <-
function(x)
{
  if(is.vector(x))
     S=data.frame(n=length(x),min=min(x),max=max(x),
       mean=mean(x),sd=sd(x),median=median(x),IQR=IQR(x))
  else
     S=data.frame(n=nrow(x),min=apply(x,2,min),max=apply(x,2,max),
       mean=apply(x,2,mean),sd=apply(x,2,sd),
       median=apply(x,2,median),IQR=apply(x,2,IQR))
  S
}
