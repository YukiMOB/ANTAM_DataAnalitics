# data analysis

# 刺激を提示しているときの移動の光に対するネガティブ比を導出,L刺激，R刺激では分けない
library("ggplot2")
library("magick")

stop_motion <- 0 # ダンゴムシが停止している定義域のMAX値

dt.PT.R.row <- as.data.frame(NULL)
dt.PT.L.row <- as.data.frame(NULL)
positive_negative_ratio_merge_LRStimulus <- function(dt,start,end){
  dt.PT.R <- as.data.frame(NULL)
  dt.PT.L <- as.data.frame(NULL)
  negative.ratio <- c()
  for (i in start:end) {
    # LとRそれぞれからの刺激を受けた時のデータに変換
    move.angle.SP.L <- subset(diff(dt$x),dt$id == i & dt$arc == 180 & diff(dt$x) != stop_motion) #RLeft
    move.angle.SP.R <- subset(diff(dt$x),dt$id == i & dt$arc == 0 & diff(dt$x) != stop_motion) #Right
    # 2値化
    move.angle.SP.L <-  ifelse(move.angle.SP.L < 0,-1,1) #Left
    move.angle.SP.R <- ifelse(move.angle.SP.R > 0,1,-1) #Right
    # 2値化 これをマージする
    stimulus.move <- move.angle.SP.L + move.angle.SP.R
    
    if(length(stimulus.move) != 0){
      # ここに処理を書く
      negative.ratio <- c(negative.ratio,sum(subset(abs(stimulus.move),stimulus.move < 0)) / sum(abs(stimulus.move))) 
      # バイナリ化した時のnegativeの値の総和 / 全体の総和 
    }
  }
  return(negative.ratio)
}

# Pos/Nega Ratio L stimulus or R stimulus
static_right.PN.Ratio.list <- positive_negative_ratio_merge_LRStimulus(dt,exNumlist[5] + 1,exNumlist[6])
static_left.PN.Ratio.list <- positive_negative_ratio_merge_LRStimulus(dt,exNumlist[4] + 1,exNumlist[5])
it160.PN.Ratio.list <- positive_negative_ratio_merge_LRStimulus(dt,exNumlist[3] + 1,exNumlist[4])
it40.PN.Ratio.list <- positive_negative_ratio_merge_LRStimulus(dt,exNumlist[2] + 1,exNumlist[3])
it10.PN.Ratio.list <- positive_negative_ratio_merge_LRStimulus(dt,exNumlist[1],exNumlist[2])
