library(ggplot2)

# 速度の閾値を導出するためのプログラム

get.velocity <- function(dt){
  # 全試行のf周期での速度データを導出
  dx <- c()
  dy <- c()
  v <- c()
  for (i in 1:max(dt$id)) {
    dx <- c(dx,diff(subset(dt$x.V1,dt$id == i)))
    dy <- c(dy,diff(subset(dt$y.V1,dt$id == i)))
    v <- c(v,sqrt(dx^2 + dy^2))
  }
  return(v)
}
v.df <- data.frame(velocity = get.velocity(dt))

g <- ggplot(v.df,aes(x = velocity))
g <- g + 
     geom_histogram(binwidth = 0.2) +
     scale_x_continuous(breaks=seq(0,15,0.5))
plot(g)