t.test1 <-
function(x,mu=0){
  n=length(x)
  xbar=mean(x)
  s=sd(x)
  se=s/sqrt(n)
  t=(xbar-mu)/se
  df=n-1
  p=2*pt(t,n-1)
  list(t=t,df=df,p=p)  # cat('t=',t,'df=',df,'p-value=',p)
}
