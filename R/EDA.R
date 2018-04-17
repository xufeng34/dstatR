EDA <-
function(x){
  par(mfrow=c(2,2),mar=c(4,3,2,1),cex=0.8)   # 同时做4个图
  hist(x,main='Histgram',ylab='')            # 直方图
  dotchart(x,main='Dotchart',ylab='')        # 点图
  boxplot(x,horizontal=T,main='Boxplot')     # 箱式图
  qqnorm(x,xlab='',ylab='');qqline(x)        # 正态概率图
  par(mfrow=c(1,1))                          # 恢复单图
}
