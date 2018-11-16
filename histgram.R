# histgram of negative phototaxis
histgram_NP <- function(df,fl){
  #Sector Size
  SS <- 180
  for (i in 1:length(fl)) {
    #move.angle.SP.Right <- subset((atan2(df$y,df$x) * 180 / pi),df$arc == 0 & df$id == i)
    #move.angle.SP.Left <- subset((atan2(df$y,df$x) * 180 / pi),df$arc == 180 & df$id == i)
    move.angle.SP.Right <- subset((atan2(diff(df$y),diff(df$x)) * 180 / pi),df$id == i & df$arc == 0)
    move.angle.SP.Left <- subset((atan2(diff(df$y),diff(df$x)) * 180 / pi),df$id == i & df$arc == 180)
    # x <- subset(df$x,df$id == i)
    # y <- subset(df$y,df$id == i)
    # move.angle <- (atan2(y,x) * 180 / pi)
    # # 
    # move.angle.SP.Right <- move.angle[arc==0] # arc = 0
    # move.angle.SP.Left <- move.angle[arc==180] # arc = 180 これVFの時どうしようか...
    if(length(move.angle.SP.Right != 0) || length(move.angle.SP.Left != 0)){
      fname <- sub(".csv","hist_SP.png",fl[i])
      png(fname, width=4*ppi, height=4 * 1.15 *ppi, res=ppi)
      hist(move.angle.SP.Right[move.angle.SP.Right != 0],col = "#ff00ff40", border = "#ff00ff", breaks = 20,xlim = c(-180,180),main = "Histgram of move direction for phototaxis",xlab = "direction(-180[deg] < θ < 180[deg]") # ピンク
      hist(move.angle.SP.Left[move.angle.SP.Left != 0],col = "#0000ff40", border = "#0000ff", breaks = 20, add = TRUE) # 青
      dev.off()
    }
  }
}