Freq <-
function(X){ #自定义计量频数表函数
  H=hist(X,xlab='',main='')
  f=H$counts
  p=f/sum(f)*100; cp=cumsum(p)
  freq=data.frame(m=H$mids,f=f,p=p,cp=round(cp,2))
  names(freq)=c('组中值','频数','频率(%)','累计频数(%)')
  freq
}
