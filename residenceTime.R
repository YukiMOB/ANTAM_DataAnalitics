# 滞留時間の導出
residence_time <- function(dt,start,end){
  list.no_residence <- c()
  for (i in start:end) {
    len.residence <- length(subset(dt$t,dt$id == i & diff(dt$x) == 0 & diff(dt$y) == 0))
    len.no_residence <- length(subset(dt$t,dt$id == i & (diff(dt$x) != 0 & diff(dt$y) != 0)))
    list.no_residence <- c(list.no_residence,len.residence/(len.residence + len.no_residence))
  }
  return(list.no_residence)
}
# 滞留時間と非滞留時間の比率(滞留時間/全体)
it10.residence_ratio.list <- residence_time(dt,1,5)
it40.residence_ratio.list <- residence_time(dt,6,10)
it160.residence_ratio.list <- residence_time(dt,11,15)
staticLeft.residence_ratio.list <- residence_time(dt,16,20)
staticRight.residence_ratio.list <- residence_time(dt,21,25)
NoStimulus.residence_ratio.list <- residence_time(dt,1,15)

