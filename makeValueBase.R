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

# ちょっと課題残ったので後回し
averaging <- function(value,frq,n){
  ds.list <- c(0)
  for (i in 1:max(n$id)) {
    df <- subset(n,n$id == i)
    ave <- 0
    ave.list <- c(0)
    print(df$t[2])
    for (j in 1:length(df$t)) {
      if(j %% freq == 0){
        ave <- ave / freq
        ave.list <- append(ave.list,ave,after = length(ave.list))
      }else{
        ave <- ave + value[j]
      }
    }
    ds.list <- append(ds.list,ave.list,after = length(ds.list))
  }
  return(ds.list)
}


### 基本データ ###
# ダウンサンプリング
downdampling <- function(df,freq){
  n <- seq(1,nrow(df),by = freq)
  return.downsamp <- data.frame(t = df$t[n], x = averaging(df$x,freq,df), y = averaging(df$y,freq,df),id = df$id[n],path = df$id[n],arc = df$arc[n],pos = df$pos[n])
  return(return.downsamp)
}


#n <- seq(1, nrow(df), by = freq)
#df.downsamp <- data.frame(t = df$t[n], x = averaging(df$x,freq), y = averaging(df$y,freq),id = df$id[n],path = df$id[n],arc = df$arc[n],pos = df$pos[n])

df.downsamp <- downdampling(df,freq)
#df.NoStimulus.downsamp <- downdampling(df.NoStimulus,freq)

