# 平均移動速度の導出
velocity_average <- function(dt,start,end){
  list.velocity_average <- c()
  for (i in start:end) {
    x <- subset(dt$x,dt$id == i)
    y <- subset(dt$y,dt$id == i)
    # average.velocity <- (mean(abs(diff(x)))) * 60
    average.velocity <- (sum(sqrt(diff(x)^2 + diff(y)^2))  / 30) / 10
    list.velocity_average <- c(list.velocity_average,average.velocity)
  }
  return(list.velocity_average)
}

# 統計解析
it10.list.velocity_average <- velocity_average(dt,1,5)
it40.list.velocity_average <- velocity_average(dt,6,10)
it160.list.velocity_average <- velocity_average(dt,11,15)
staticLeft.list.velocity_average <- velocity_average(dt,16,20)
staticRight.list.velocity_average <- velocity_average(dt,21,25)
NoStimulus.list.Velocity_average <- velocity_average(dt,1,15)

# 正規分布かどうかの確認用
# hist(c(it160.list.velocity_average))
