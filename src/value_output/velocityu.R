# 平均移動速度の導出
threshould <- 1.5
velocity_average <- function(dt,start,end){
  list.velocity_average <- c()
  for (i in start:end) {
    x <- subset(dt$x,dt$id == i)
    y <- subset(dt$y,dt$id == i)
    # average.velocity <- (mean(abs(diff(x)))) * 60
    v <- sqrt(diff(x)^2 + diff(y)^2)
    v <- subset(v,v > threshould)
    average.velocity <- (sum(v)  / 1800)
    list.velocity_average <- c(list.velocity_average,average.velocity)
  }
  return(list.velocity_average)
}

# 統計解析
it10.list.velocity_average <- velocity_average(dt,exNumlist[1],exNumlist[2])
it40.list.velocity_average <- velocity_average(dt,exNumlist[2] + 1,exNumlist[3]) 
it160.list.velocity_average <- velocity_average(dt,exNumlist[3] + 1,exNumlist[4])
static.list.velocity_average <- velocity_average(dt,exNumlist[4] + 1,exNumlist[6])
NoStimulus.list.Velocity_average <- velocity_average(dt,1,15)

# 正規分布かどうかの確認用
# hist(c(it160.list.velocity_average))
