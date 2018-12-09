# 総移動距離の導出
distance <- function(dt,start,end){
  list.distance <- c()
  for (i in start:end) {
    x <- subset(dt$x,dt$id == i)
    y <- subset(dt$y,dt$id == i)
    distance <- sum(sqrt((diff(x)^2) + (diff(y)^2) )) / 10 # mmをcmに
    
    # print(distance)
    list.distance <- c(list.distance,distance)
  }
  return(list.distance)
}
# 統計解析
# 5試行ごとなので
it10.distance.list <- distance(dt,exNumlist[1],exNumlist[2])
it40.distance.list <- distance(dt,exNumlist[2] + 1,exNumlist[3])
it160.distance.list <- distance(dt,exNumlist[3] + 1,exNumlist[4])
staticLeft.distance.list <- distance(dt,exNumlist[4] + 1,exNumlist[5])
staticRight.distance.list <- distance(dt,exNumlist[5] + 1,exNumlist[6])
NoStimulus.distance.list <- distance(dt.NoStimulus,1,15)

# 正規分布確認用
# hist(c(it10.distance.list))

