# 平均移動速度の導出
velocity_average <- function(df,fl){
  for (i in 1:length(fl)) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    average.velocity <- mean(abs(diff(x)))
    print(average.velocity)
  }
}

# 統計解析
velocity_average(df.downsamp,fl)