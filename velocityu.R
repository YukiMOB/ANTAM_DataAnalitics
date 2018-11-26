# 平均移動速度の導出
velocity_average <- function(df,start,end){
  list.velocity_average <- c()
  for (i in start:end) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    # average.velocity <- (mean(abs(diff(x)))) * 60
    average.velocity <- (sum(sqrt(diff(x)^2 + diff(y)^2))  / 30) / 10
    list.velocity_average <- c(list.velocity_average,average.velocity)
  }
  return(list.velocity_average)
}

# 統計解析
it10.list.velocity_average <- velocity_average(df,1,5)
it40.list.velocity_average <- velocity_average(df,6,10)
it160.list.velocity_average <- velocity_average(df,11,15)
staticLeft.list.velocity_average <- velocity_average(df,16,20)
staticRight.list.velocity_average <- velocity_average(df,21,25)
NoStimulus.list.Velocity_average <- velocity_average(df,1,15)

# 正規分布かどうかの確認用
# hist(c(it160.list.velocity_average))
