# 総移動距離の導出
distance <- function(df,start,end){
  list.distance <- c()
  for (i in start:end) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    distance <- sum(sqrt(diff(x)^2 + diff(y)^2))
    # print(distance)
    list.distance <- c(list.distance,distance)
  }
  return(list.distance)
}
# 統計解析
# 5試行ごとなので
it10.distance.list <- distance(df.downsamp,1,5)
it40.distance.list <- distance(df.downsamp,6,10)
it160.distance.list <- distance(df.downsamp,11,15)

# 正規分布確認用
hist(c(it10.distance.list))
