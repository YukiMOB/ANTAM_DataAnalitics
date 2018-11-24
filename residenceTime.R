# 待機時間の導出
residence_time <- function(df,fl){
  for (i in 1:length(fl)) {
    len.residence <- length(subset(df$t,df$id == i & diff(df$x) == 0 & diff(df$y) == 0))
    len.no_residence <- length(subset(df$t,df$id == i & (diff(df$x) != 0 & diff(df$y) != 0)))
    print(len.no_residence)
    print(len.residence)
    #print("len.residence:" + len.residence + "len.noResidence:" + len.no_residence)
  }
}

residence_time(df.downsamp,fl)

