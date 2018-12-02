# ANTAM_Experiment_Datainput.Rでインプットしたデータフレームから，それぞれデータ解析が必要な値に変換する処理を行う
# data analysis
# 現在はダウンサンプリング のみ
ppi <- 300 # resolution of figures
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
