EDA <-
function(x){
  par(mfrow=c(2,2),mar=c(4,3,2,1),cex=0.8)   # ͬʱ��4��ͼ
  hist(x,main='Histgram',ylab='')            # ֱ��ͼ
  dotchart(x,main='Dotchart',ylab='')        # ��ͼ
  boxplot(x,horizontal=T,main='Boxplot')     # ��ʽͼ
  qqnorm(x,xlab='',ylab='');qqline(x)        # ��̬����ͼ
  par(mfrow=c(1,1))                          # �ָ���ͼ
}