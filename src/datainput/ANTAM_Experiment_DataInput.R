library(data.table)
library(dplyr)

freq <- 25
options(scipen=100)

############ function #######################################
getdt <- function(fl){
  dt.return <- data.frame()
  t0 <- proc.time()
  for (i in 1:length(fl)) {
    print(proc.time() - t0)
    dt <- readtiles(fl[i],i)
    dt.return <- rbindlist(list(dt.return,dt))
    #dt.return[i, ] <- rbind(dt.return,dt)
  }
  dt <- as.data.frame(NULL)
  return(dt.return)
}

readtiles <- function(f,i){
  xcalib <- 0.0239
  ycalib <- 0.0254
  data <- fread(f,stringsAsFactors=FALSE)
  n <- seq(1,nrow(data),by = freq)
  x.downsamp<- valueDS(data$x * xcalib,n)
  y.downsamp <- valueDS(data$y * ycalib,n)
  data$arc <- ifelse(data$arc == 0,-1,data$arc)
  data$arc <- ifelse(data$arc == 220,180,data$arc)
  data$arc <- ifelse(data$arc == 210,180,data$arc)
  data$arc <- ifelse(data$arc == 40,0,data$arc)
  data$arc <- ifelse(data$arc == 30,0,data$arc)
  dt <- data.table(t = data$time[n], x = x.downsamp, y = y.downsamp,id = i,path = f,arc = data$arc[n],pos = data$pos[n])
  # dt <- data.table(t = data$time[n], x = x.downsamp, y = y.downsamp,id = i,path = f)
  
  dt <- subset(dt,dt$t > 30000 & dt$t < 1800000)
  #実験ごとにキャリブレーションが必要であれば実施
  return(dt)
}
calibration.partical <- function(dt,modification.value,suggested.value){
  ifelse(move.angle.SP.Left < SS & move.angle.SP.Left > -SS,-1,1)
  for(i in length(dt$arc)){
    if(dt$arc[i] == modification.value){
      dt$arc[i] = suggested.value
    }else{
      dt$arc[i] = dt$arc[i]
    }
  }
  return(dt$arc)
}

# valueのダウンサンプリング 
valueDS <- function(value,n){
  value.dt <- data.table()
  # s.list <- append(ds.list,value[1],after = length(ds.list))
  value.dt <- rbindlist(list(value.dt,list(value[1])))
  for (i in 2:length(n)) { # length(n) - 1がうまくいかないので初期値を3に
    v <- list(sum(value[n[i-1]:n[i]]) / freq)
    value.dt <- rbindlist(list(value.dt,v))
  }
  return(value.dt)
}

rate <- 12500
ppi <- 300 # resolution of figures

setwd("/Users/Yuuki/Documents/01.研究/2019/Experiment_row_Data/0513_Torocoid_experiment")
path <- "/Users/Yuuki/Documents/01.研究/2019/Experiment_row_Data/0513_Torocoid_experiment"
fl <- list.files(path,recursive=T, include.dirs=T,pattern = "mouse.*csv")
dt <- getdt(fl)

# 
# 
# # select file path and set file path including sub directory
# setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data/")
# path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data/"
# fl <- list.files(path,recursive=T, include.dirs=T,pattern = "mouse.*csv")
# dt <- getdt(fl)
# # path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_10_ジャーナル追加実験/SS_180
# 
# # Nostimulus
# setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験_NoStimulus_and_other/NoStimulus")
# path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験_NoStimulus_and_other/NoStimulus"
# fl.NoStimulus <- list.files(path,recursive=T, include.dirs=T,pattern = "mouse.*csv")
# dt.NoStimulus <- getdt(fl.NoStimulus)
# 
# 
# # データ解析で結果を保存したいところ
# # select file path and set file path including sub directory
# setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data")
# path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data"
# 
# 
# setwd("/Users/Yuuki/Documents/01.研究/2019/Experiment_row_Data/0409~0412/EX_data")
# path <- "/Users/Yuuki/Documents/01.研究/2019/Experiment_row_Data/0409~0412/EX_data"
