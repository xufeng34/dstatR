Freq <-
function(X){ #�Զ������Ƶ��������
  H=hist(X,xlab='',main='')
  f=H$counts
  p=f/sum(f)*100; cp=cumsum(p)
  freq=data.frame(m=H$mids,f=f,p=p,cp=round(cp,2))
  names(freq)=c('����ֵ','Ƶ��','Ƶ��(%)','�ۼ�Ƶ��(%)')
  freq
}