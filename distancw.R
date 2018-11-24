# 総移動距離の導出
residence_time <- function(df,fl){
  for (i in 1:length(fl)) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    distance <- sum(sqrt(diff(x)^2 + diff(y)^2))
    print(distance)
  }
}

# 統計解析

residence_time(df.downsamp,fl)