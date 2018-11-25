# ANTAM_Experiment_Datainput.Rでインプットしたデータフレームから，それぞれデータ解析が必要な値に変換する処理を行う
# data analysis
# 現在はダウンサンプリング のみ
ppi <- 300 # resolution of figures
ofset <- 100
freq <- 25
basefreq <- 125

### function ###
torajectory <- function(df,fl){
  mem_xy <- c(abs(min(df$x)),abs(min(df$y)),max(df$x),max(df$y))
  traj_abs = max(mem_xy)
  for (i in 1:length(fl)) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    mem_xy.noabs <- c(abs(min(x)),abs(min(y)),max(x),max(y))
    traj_noabs <- max(mem_xy.noabs)
  }
}

averaging <- function(n,frq){
  n.averaging <- c(0)
  count <- 1
  j <- 1
  for (i in 1:length(n)) {
    if(count >= frq){
      count <- 1
      j <- j + 1
      n.averaging <- append(n.averaging,0,after = length(n.averaging))
    }else{
      n.averaging[j] <- n.averaging[j] + n[i]
      count <- count + 1
    }
  }
  return(n.averaging)
}


### 基本データ ###
# ダウンサンプリング
downdampling <- function(df,freq){
  n <- seq(1,nrow(df),by = freq)
  return.downsamp <- data.frame(t = df$t[n], x = averaging(df$x,freq), y = averaging(df$y,freq),id = df$id[n],path = df$id[n],arc = df$arc[n],pos = df$pos[n])
  return(return.downsamp)
}


n <- seq(1, nrow(df), by = freq)
df.downsamp <- data.frame(t = df$t[n], x = averaging(df$x,freq), y = averaging(df$y,freq),id = df$id[n],path = df$id[n],arc = df$arc[n],pos = df$pos[n])

df.Nostimulus.downsamp <- downdampling(df.NoStimulus,freq)

