############ function #######################################
getdf <- function(fl){
  df.return <- as.data.frame(NULL)
  for (i in 1:length(fl)) {
    df <- readfiles(fl[i],i)
    df.return <- rbind(df.return,df)
  }
  df <- as.data.frame(NULL)
  return(df.return)
}

readfiles <- function(f,i){
  data <- read.csv(f,stringsAsFactors=FALSE)
  df <- data.frame(t = data[,1], x = 0.0239 * data$x, y = 0.0254 * data$y,id = i,path = f,arc = data$arc,pos = data$pos)
  #実験ごとにキャリブレーションが必要であれば実施
  df$arc <- ifelse(df$arc == 0,-1,df$arc)
  df$arc <- ifelse(df$arc == 220,180,df$arc)
  df$arc <- ifelse(df$arc == 210,180,df$arc)
  df$arc <- ifelse(df$arc == 40,0,df$arc)
  df$arc <- ifelse(df$arc == 30,0,df$arc)
  df <- subset(df,df$t > 30000 & df$t < 1800000)
  return(df)
}
calibration.partical <- function(df,modification.value,suggested.value){
  ifelse(move.angle.SP.Left < SS & move.angle.SP.Left > -SS,-1,1)
  for(i in length(df$arc)){
    
    if(df$arc[i] == modification.value){
      df$arc[i] = suggested.value
    }else{
      df$arc[i] = df$arc[i]
    }
  }
  return(df$arc)
}

rate <- 12500
ppi <- 300 # resolution of figures

# select file path and set file path including sub directory
setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data")
path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data"
fl <- list.files(path,recursive=T, include.dirs=T,pattern = "mouse.*csv")
df <- getdf(fl)
# path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_10_ジャーナル追加実験/SS_180

# Nostimulus
setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験_NoStimulus_and_other/NoStimulus")
path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験_NoStimulus_and_other/NoStimulus"
fl.NoStimulus <- list.files(path,recursive=T, include.dirs=T,pattern = "mouse.*csv")
df.NoStimulus <- getdf(fl.NoStimulus)


# データ解析で結果を保存したいところ
# select file path and set file path including sub directory
setwd("~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data")
path <- "~/OneDrive - cc.kyoto-su.ac.jp/共有データ/修士課程/2018_11_ジャーナル実験/data"
