# data analysis
library("ggplot2")
library("magick")
exnum <-c("e","a","b","c","d")
experiment.condition <- 3
condition.trial <- 5

df.PT.R.row <- as.data.frame(NULL)
df.PT.L.row <- as.data.frame(NULL)
positive_negative_ratio_bar_each_criteria <- function(df,start,end){
  #Sector Size
  SS <- 180
  bw <- 20
  SS <- 90 # sector size
  fz <- 125
  df.PT.R <- as.data.frame(NULL)
  df.PT.L <- as.data.frame(NULL)
  for (i in start:end) {
    # LとRそれぞれからの刺激を受けた時のデータに変換
    move.angle.SP.L <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0) #RLeft
    move.angle.SP.R <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0) #Right
    # 2値化
    move.angle.SP.L <-  ifelse(move.angle.SP.L < 0,-1,1) #Left
    move.angle.SP.R <- ifelse(move.angle.SP.R > 0,1,-1) #Right
    
    # L
    if(length(move.angle.SP.L) != 0){
      if(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)) + sum(subset(move.angle.SP.L,move.angle.SP.L < 0)) != 0){
        c.PorN.L <- c(sum(subset(abs(move.angle.SP.L),move.angle.SP.L < 0)) / sum(abs(move.angle.SP.L))
                      ,sum(subset(abs(move.angle.SP.L),move.angle.SP.L > 0)) / sum(abs(move.angle.SP.L)))
        df.set <- data.frame(PorN_Left =c("Positive","Negative"),
                             value_Left = c(abs(c.PorN.L[1]),abs(c.PorN.L[2])),ExNum_Left = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.L <- rbind(df.PT.L,df.set)
        
        #バイナリの加算データをデータフレーム に格納
        df.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.L,move.angle.SP.L < 0))),
                                           sum(abs(subset(move.angle.SP.L,move.angle.SP.L > 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.L.row <- rbind(df.PT.L,df.set)
      }
    }
    # R
    if(length(move.angle.SP.R) != 0){
      if(sum(subset(move.angle.SP.R,move.angle.SP.R > 0)) + sum(subset(move.angle.SP.R,move.angle.SP.R < 0)) != 0){
        c.PorN.R <- c(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0))
                      ,sum(subset(abs(move.angle.SP.R),move.angle.SP.R < 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0)))
        df.set <- data.frame(PorN_Right =c("Positive","Negative"),
                             value_Right = c(abs(c.PorN.R[1]),abs(c.PorN.R[2])),ExNum_Right = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.R <- rbind(df.PT.R,df.set)
        
        #バイナリの加算データをデータフレーム に格納
        df.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.R,move.angle.SP.R > 0))),
                                           sum(abs(subset(move.angle.SP.R,move.angle.SP.R < 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.R.row <- rbind(df.PT.R.row,df.set.row)
      }
    }
  }
  return(c(df.PT.L,df.PT.R))
}

it30min_R.ratio.PN <- positive_negative_ratio_bar_each_criteria(df,21,25)
it30min_L.ratio.PN <- positive_negative_ratio_bar_each_criteria(df,16,20)
it160.ratio.PN <- positive_negative_ratio_bar_each_criteria(df,11,15)
it40.ratio.PN <- positive_negative_ratio_bar_each_criteria(df,6,10)
it10.ratio.PN <- positive_negative_ratio_bar_each_criteria(df,1,5)


# ex_bar_plot(df.PT.R,df.PT.L,"IT_10",1,10)
# ex_bar_plot(df.PT.R,df.PT.L,"IT_40",11,20)
# ex_bar_plot(df.PT.R,df.PT.L,"IT_160",21,30)
# ex_bar_plot(df.PT.R,df.PT.L,"IT_staticLefft",31,40)
# ex_bar_plot(df.PT.R,df.PT.L,"IT_staticRight",41,50)

# フィッシャーの検定
# 各条件の平均を導出
# it10.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_10",1,10)
# it40.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_40",11,20)
# it160.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_160",21,30)
# calib <- 1/1000
# デバッグ
# print(it10.mean * calib)
# print(it40.mean* calib)
# print(it160.mean* calib)
# p <- kruskal.test(x=list(it160.mean$valueR.Nega * calib,it40.mean$valueR.Nega * calib,it10.mean$valueR.Nega * calib))
# print(p)
# ft.R <- matrix(c(it160.mean$valueR.Pos * calib,it160.mean$valueR.Nega * calib
#                  ,it40.mean$valueR.Pos * calib,it40.mean$valueR.Nega * calib,
#                  it10.mean$valueR.Pos * calib,it10.mean$valueR.Nega * calib),nrow = 3,byrow = T)
# ft.L <- matrix(c(it160.mean$valueL.Pos * calib,it160.mean$valueL.Nega * calib
#                  ,it40.mean$valueL.Pos * calib,it40.mean$valueL.Nega * calib
#                  ,it10.mean$valueL.Pos * calib,it10.mean$valueL.Nega * calib),nrow = 3,byrow = T)
# 
# print(fisher.test(ft.L))
# print(fisher.test(ft.R))