stock.sim <-
function(N=100,File='',Seed=123){
  # N=100; Seed=123
  set.seed(Seed)
  R1=rbinom(N,1,0.3)+1; #sex=table(R1);sex
  R2=round(rnorm(N,40,10),0);R2[R2<18]<- 0.0; R2[R2>70]<- 0.0;
  #age=table(R2);age
  R3=rbinom(N,2,0.75)+1; #result=table(R3);result
  R4=rbinom(N,3,0.5)+1; #method=table(R4);method
  R5=rbinom(N,1,0.6)+1; #risk=table(R5);risk
  R6=rbinom(N,1,0.85)+1; #post=table(R6);post
  R7=round(runif(N,1,1100),1);R7[sample(1:N,1)]<-0
  #fund=table(as.integer(R7));fund; sum(fund)
  R8=rbinom(N,7,0.5)+1; #job=table(R8);job
  R9=rbinom(N,7,0.7)+1; #edu=table(R9);edu
  RX=data.frame(sex=R1,age=R2,result=R3,method=R4,risk=R5,
                post=R6,fund=R7,job=R8,edu=R9)
  if(File=='') RX
  else write.csv(RX,file=File,row.names=FALSE)
}
