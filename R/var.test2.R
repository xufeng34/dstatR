var.test2 <-
function(x, y, mu=c(Inf, Inf), side=0){
   n1=length(x); n2=length(y)
   if (all(mu<Inf)){
      Sx2=sum((x-mu[1])^2)/n1; Sy2=sum((y-mu[2])^2)/n2
      df1=n1; df2=n2
   }
   else{
      Sx2=var(x); Sy2=var(y); df1=n1-1; df2=n2-1
   }
   F=Sx2/Sy2
   P=P_value(pf, F, df=c(df1, df2), side=side)
   list(S1=Sx2, S2=Sy2,df1=df1, df2=df2, F=F, p_value=P)
}
