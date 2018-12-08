# data analysis
# 刺激を提示しているときの移動の光に対するネガティブ比を導出,L刺激,R刺激両方で導出
library("ggplot2")
library("magick")

stop_motion <- 0 # ダンゴムシが停止している定義域のMAX値

dt.PT.R.row <- as.data.frame(NULL)
dt.PT.L.row <- as.data.frame(NULL)
positive_negative_ratio_bar_each_criteria <- function(dt,start,end){
  dt.PT.R <- as.data.frame(NULL)
  dt.PT.L <- as.data.frame(NULL)
  for (i in start:end) {
    # LとRそれぞれからの刺激を受けた時のデータに変換
    move.angle.SP.L <- subset(diff(dt$x),dt$id == i & dt$arc == 180 & diff(dt$x) != stop_motion) #RLeft
    move.angle.SP.R <- subset(diff(dt$x),dt$id == i & dt$arc == 0 & diff(dt$x) != stop_motion) #Right
    # 2値化
    move.angle.SP.L <-  ifelse(move.angle.SP.L < 0,-1,1) #Left
    move.angle.SP.R <- ifelse(move.angle.SP.R > 0,1,-1) #Right
    
    # L
    if(length(move.angle.SP.L) != 0){
      if(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)) + sum(subset(move.angle.SP.L,move.angle.SP.L < 0)) != 0){
        c.PorN.L <- c(sum(subset(abs(move.angle.SP.L),move.angle.SP.L < 0)) / sum(abs(move.angle.SP.L))
                      ,sum(subset(abs(move.angle.SP.L),move.angle.SP.L > 0)) / sum(abs(move.angle.SP.L)))
        dt.set <- data.frame(PorN_Left =c("Positive","Negative"),
                             value_Left = c(abs(c.PorN.L[1]),abs(c.PorN.L[2])),ExNum_Left = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        dt.PT.L <- rbind(dt.PT.L,dt.set)
        
        #バイナリの加算データをデータフレーム に格納
        dt.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.L,move.angle.SP.L < 0))),
                                           sum(abs(subset(move.angle.SP.L,move.angle.SP.L > 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        dt.PT.L.row <- rbind(dt.PT.L,dt.set)
      }
    }
    # R
    if(length(move.angle.SP.R) != 0){
      if(sum(subset(move.angle.SP.R,move.angle.SP.R > 0)) + sum(subset(move.angle.SP.R,move.angle.SP.R < 0)) != 0){
        c.PorN.R <- c(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0))
                      ,sum(subset(abs(move.angle.SP.R),move.angle.SP.R < 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0)))
        dt.set <- data.frame(PorN_Right =c("Positive","Negative"),
                             value_Right = c(abs(c.PorN.R[1]),abs(c.PorN.R[2])),ExNum_Right = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        dt.PT.R <- rbind(dt.PT.R,dt.set)
        
        #バイナリの加算データをデータフレーム に格納
        dt.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.R,move.angle.SP.R > 0))),
                                           sum(abs(subset(move.angle.SP.R,move.angle.SP.R < 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        dt.PT.R.row <- rbind(dt.PT.R.row,dt.set.row)
      }
    }
  }
  return(c(dt.PT.L,dt.PT.R))
}

# Pos/Nega Ratio L stimulus or R stimulus
static_right.PN.Ratio.list <- positive_negative_ratio_bar_each_criteria(dt,exNumlist[5] + 1,exNumlist[6])
static_left.PN.Ratio.list <- positive_negative_ratio_bar_each_criteria(dt,exNumlist[4] + 1,exNumlist[5])
it160.PN.Ratio.list <- positive_negative_ratio_bar_each_criteria(dt,exNumlist[3] + 1,exNumlist[4])
it40.PN.Ratio.list <- positive_negative_ratio_bar_each_criteria(dt,exNumlist[2] + 1,exNumlist[3])
it10.PN.Ratio.list <- positive_negative_ratio_bar_each_criteria(dt,exNumlist[1],exNumlist[2])
