Ftab <-
function(X){ #自定义计数频数表函数
  f=table(X);
  S=sum(f);
  P=f/S*100
  T=cbind(' 例数'=f,'构成比(%)'=round(P,2))
  print(rbind(T,'合计'=c(S,100)))
  invisible(f)
}
