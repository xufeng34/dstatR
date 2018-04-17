P_value <-
function(cdf, x, df=numeric(0), side=0){
   k=length(df)
   P=switch(k+1, cdf(x), cdf(x, df), cdf(x, df[1], df[2]),cdf(x, df[1], df[2], df[3]))
   if (side<0)     P
   else if (side>0) 1-P
   else
   if (P<1/2)   2*P
     else        2*(1-P)
}
