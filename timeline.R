# data analysis
library("ggplot2")
library("magick")
ppi <- 300 # resolution of figures
ofset <- 100
freq <- 125

## 使い道がありそう．切り替えごとに負の走光性が発現しているかどうかが分かるようなプロット図を出すことが可能
timeline_diffx <- function(df,fl){
  for (i in 1:length(fl)) {
    #df.indivisual <- subset(df,df$id == i & diff(df$x) != 0)
    df.ind.R <- subset(df,df$id == i & df$arc == 0 & diff(df$x) != 0 & abs(diff(df$x) > 1))
    df.ind.L <- subset(df,df$id == i & df$arc == 180 & diff(df$x) != 0 & abs(diff(df$x) > 1))
    # t <- subset(df$t,df$id == i) #Left
    move.angle.SP.R <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0)
    move.angle.SP.L <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0)
    p <- 16
    lagMax <- 1800 / 4#on ACF
    large <- 0.3
    p <- 16
    f <- 125
    # task 
    # 時系列データ，横軸時間，縦軸diff(x)
    if(length(move.angle.SP.R != 0)){
      if(sum(subset(move.angle.SP.R,move.angle.SP.R > 0)) + sum(subset(move.angle.SP.R,move.angle.SP.R < 0)) != 0){
        # dx <- append(diff(df.indivisual$x),0,after = length(diff(df.indivisual$x)) + 1)
        dx.R <- append(diff(df.ind.R$x),0,after = length(diff(df.ind.R$x)) + 1)
        dx.L <- append(diff(df.ind.L$x),0,after = length(diff(df.ind.L$x)) + 1)
        # dx <- ifelse(dx > 0,1,-1)
        # dx <- ifelse(dx == 0,NULL,dx)
        # fname <- sub(".csv","timelne_x_NP_R.png",fl[i])
        # png(fname, width=8*ppi, height=4*ppi, res=ppi)
        # plot(1:length(dx),dx,col = 2,ylab="diff(x)[mm]",xlab = "t[s]",xlim = c(0,250000),cex = large,pch = p) #on #lim:experiment time
        # dev.off()
        
        fname <- sub(".csv","timeline_x_NP_R.png",fl[i])
        p <- ggplot(NULL) +
          geom_point(data = df.ind.R,aes(x = df.ind.R$t,y = dx.R)) +
          geom_point(data = df.ind.L,aes(x = df.ind.L$t,y = dx.L)) +
          stat_smooth()
        # p <- ggplot(df.indivisual,
        #             aes(x=df.indivisual$t / 1000,y=dx)) +
        #   geom_line()
        #      #geom_ribbon(fill = "blue",ymin = 0,ymax = dx)
        ggsave(file = fname, plot = p)
        # return(length(df.indivisual$t))
      }
    }
    
    # if(length(move.angle.SP.L != 0)){
    #   if(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)) + sum(subset(move.angle.SP.L,move.angle.SP.L < 0)) != 0){
    #     
    #   }
    # }
  }
}


# fname <- sub(".csv","hist_NP_L.png",fl[i])
# p <- ggplot(data.frame(move.angle.SP.Left), aes(move.angle.SP.Left)) +
#   geom_histogram(binwidth = bw) +
#   coord_fixed() + 
#   coord_flip() +
#   theme(axis.text.x = element_text(angle = rectangle, hjust = 1,size = 5)) +
#   scale_x_continuous(breaks=seq(-180,180,40),limits = c(-180, 180)) + 
#   coord_polar() +
#   xlab(NULL)+ylab(NULL)
# ggsave(file = fname, plot = p)

timeline_diffx(df,fl)