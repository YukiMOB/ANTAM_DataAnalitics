# 平均移動速度の導出
velocity_average <- function(df,start,end){
  list.velocity_average <- c()
  for (i in start:end) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    average.velocity <- mean(abs(diff(x)))
    list.velocity_average <- c(list.velocity_average,average.velocity)
  }
  return(list.velocity_average)
}

# 統計解析
it10.list.velocity_average <- velocity_average(df.downsamp,1,5)
it40.list.velocity_average <- velocity_average(df.downsamp,6,10)
it160.list.velocity_average <- velocity_average(df.downsamp,11,15)