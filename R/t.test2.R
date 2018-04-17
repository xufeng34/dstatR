t.test2 <-
function(x, y, sigma=c(-1, -1), var.equal=FALSE, side=0){
   n1=length(x); n2=length(y)
   xb=mean(x); yb=mean(y)
   if (all(sigma>=0)){
      z=(xb-yb)/sqrt(sigma[1]^2/n1+sigma[2]^2/n2)
      list(mu=xb-yb, df=n1+n2, Z=z, p_value= P_value(pnorm, z, side=side))
   }
   else{
      if (var.equal ==  TRUE){
         Sw=sqrt(((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2))
         t=(xb-yb)/(Sw*sqrt(1/n1+1/n2))
         df=n1+n2-2
      }
      else{
         S1=var(x); S2=var(y)
         df=(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
         t=(xb-yb)/sqrt(S1/n1+S2/n2)
      }
      list(mu=xb-yb, df=df, t=t, p_value= P_value(pt,t,df,side))
   }
}
