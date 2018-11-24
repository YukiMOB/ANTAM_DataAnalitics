# data analysis
library("ggplot2")
library("magick")


experiment.condition <- 3
condition.trial <- 5
nfbar <- function(df,fl){
  #Sector Size
  SS <- 180
  bw <- 20
  SS <- 90 # sector size
  fz <- 125
  for (i in 1:length(fl)) {
    # move.angle.SP.Right <- subset((atan2(diff(df$y),diff(df$x)) * 180 / pi),df$id == i & df$arc == 0 & diff(df$y) + diff(df$x) != 0)
    # div.PT.Right <- data.frame(PhotoTaxis = ifelse(move.angle.SP.Right < SS & move.angle.SP.Right > -SS,1,-1),data = move.angle.SP.Right)
    # move.angle.SP.Left <- subset((atan2(diff(df$y),diff(df$x)) * 180 / pi),df$id == i & df$arc == 180 & diff(df$y) + diff(df$x) != 0)
    # div.PT.Left <- data.frame(PhotoTaxis = ifelse(move.angle.SP.Left < SS & move.angle.SP.Left > -SS,-1,1),data = move.angle.SP.Left)
    # 
    move.angle.SP.Right <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0)
    div.PT.Right <- data.frame(PhotoTaxis = ifelse(move.angle.SP.Right > 0,1,-1))
    move.angle.SP.Left <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0)
    div.PT.Left <- data.frame(PhotoTaxis = ifelse(move.angle.SP.Left < 0,1,-1))
    
    # move.angle.SP.Right <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0)
    # div.PT.Right <- data.frame(PhotoTaxis = diff(df$x))
    # move.angle.SP.Left <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0)
    # div.PT.Left <- data.frame(PhotoTaxis = diff(df$x))
    
    df.PT.Right <- data.frame(PorN =c("Positive","Negative"),
                              sum.move = c(sum(subset(div.PT.Right$PhotoTaxis,div.PT.Right$PhotoTaxis == 1)) / sum(abs(div.PT.Right$PhotoTaxis)),
                                           sum(subset(div.PT.Right$PhotoTaxis,div.PT.Right$PhotoTaxis == -1)) / sum(abs(div.PT.Right$PhotoTaxis))),
                              Phototaxis = "P")
    df.PT.Left <-  data.frame(PorN =c("Positive","Negative"),
                              sum.move = c(sum(subset(div.PT.Left$PhotoTaxis,div.PT.Left$PhotoTaxis == 1)) / sum(abs(div.PT.Left$PhotoTaxis)),
                                           sum(subset(div.PT.Left$PhotoTaxis,div.PT.Left$PhotoTaxis == -1)) / sum(abs(div.PT.Left$PhotoTaxis))),
                              Phototaxis = "P")
    
    #df.PT.Right <- scale(df.PT.Right)
    #df.PT.Left <- scale(df.PT.Left)
    if(length(move.angle.SP.Right != 0) || length(move.angle.SP.Left != 0)){
      # Left
      fname <- sub(".csv","bar2_NP_L.png",fl[i])
      p <- ggplot(df.PT.Left,aes(x = Phototaxis,y= sum.move,fill = PorN)) +
        ylim(c(-1, 1)) +
        geom_bar(stat="identity",width = 0.1) +
        geom_text(aes(x = Phototaxis,y= sum.move,label = sprintf("%2.1f",abs(sum.move)))) +
        coord_flip()
      ggsave(file = fname, plot = p)
      xlab(NULL)+ylab(NULL)
      # Right
      fname <- sub(".csv","bar2_NP_R.png",fl[i])
      p <- ggplot(df.PT.Right,aes(x = Phototaxis,y= sum.move,fill = PorN)) +
        ylim(c(-1, 1)) +
        geom_bar(stat="identity",width = 0.1) + 
        geom_text(aes(x = Phototaxis,y= sum.move,label = sprintf("%2.1f",abs(sum.move)))) +
        coord_flip()
      ggsave(file = fname, plot = p)
      xlab(NULL)+ylab(NULL)
    }
  }
}

bar_single_output <- function(df,fl){
  #Sector Size
  SS <- 180
  bw <- 20
  SS <- 90 # sector size
  fz <- 125
  for (i in 1:length(fl)) {
    move.angle.SP.R <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0) #Right
    move.angle.SP.L <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0) #RLeft
    move.angle.SP.R <- ifelse(move.angle.SP.R > 0,1,-1) #Right
    move.angle.SP.L <-  ifelse(move.angle.SP.L < 0,-1,1) #Left
    if(length(move.angle.SP.R != 0)){
      if(sum(subset(move.angle.SP.R,move.angle.SP.R > 0)) + sum(subset(move.angle.SP.R,move.angle.SP.R < 0)) != 0){
        c.PorN.R <- c(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0)) / sum(abs(move.angle.SP.R))
                      ,sum(subset(abs(move.angle.SP.R),move.angle.SP.R < 0)) / sum(abs(move.angle.SP.R)))
        df.PT.R <- data.frame(PorN =c("PPT","NPT"),
                              value = c(abs(c.PorN.R[1]),abs(c.PorN.R[2])),ExNum = sprintf("No.%d.%d",i,i %% 5))
        fname <- sub(".csv","bar_x_NP_R.png",fl[i])
        p <- ggplot(df.PT.R,aes(x = ExNum,y= value,fill = PorN)) +
          ylim(c(0, 1)) +
          geom_bar(stat="identity",width = 0.1,position = "dodge") +
          geom_text(aes(x = ExNum,y= value,label = sprintf("%2.2f",abs(value)))) +
          coord_flip()
        ggsave(file = fname, plot = p) 
      }
    }
    if(length(move.angle.SP.L != 0)){
      if(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)) + sum(subset(move.angle.SP.L,move.angle.SP.L < 0)) != 0){
        c.PorN.L <- c(sum(subset(abs(move.angle.SP.L),move.angle.SP.L < 0)) / sum(abs(move.angle.SP.L))
                      ,sum(subset(abs(move.angle.SP.L),move.angle.SP.L > 0)) / sum(abs(move.angle.SP.L)))
        df.PT.L <- data.frame(PorN =c("PPT","NPT"),
                              value = c(abs(c.PorN.L[1]),abs(c.PorN.L[2])),ExNum = sprintf("No.%d.%d",i,i %% 5))
        fname <- sub(".csv","bar_x_NP_L.png",fl[i])
        p <- ggplot(df.PT.L,aes(x = ExNum,y= value,fill = PorN)) +
          ylim(c(0, 1)) +
          geom_bar(stat="identity",width = 0.1,position = "dodge") +
          geom_text(aes(x = ExNum,y= value,label = sprintf("%2.2f",abs(value)))) +
          coord_flip()
        ggsave(file = fname, plot = p) 
      }
    }
    
    # if(length(move.angle.SP.L != 0)){
    # move.angle.SP.L <- subset(diff(df$x),df$id == i & df$arc == 180)
    # c.PorN.L <- scale(c(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)),sum(subset(move.angle.SP.L,move.angle.SP.L < 0))),center = 0)
    # df.PT.L <- data.frame(PositivePhotoTaxis = subset(c.PorN.L,c.PorN.L > 0),NegativePhotoTaxis = subset(c.PorN.L,c.PorN.L < 0),ExNum = i)
    # 
    # fname <- sub(".csv","bar_x_NP_L.png",fl[i])
    # p <- ggplot(df.PT.L,aes(x = ExNum,y= Value,fill = PorN)) +
    #   ylim(c(-1, 1)) +
    #   geom_bar(stat="identity",width = 0.1) +
    #   geom_text(aes(x = c("PPT","NPT"),y= Value,label = sprintf("%2.2f",abs(sum.move)))) +
    #   coord_flip()
    # ggsave(file = fname, plot = p)
    # }
  }
}
# バーの試行ごとの出力
bar_bulk_output <- function(df,fl){
  #Sector Size
  SS <- 180
  bw <- 20
  SS <- 90 # sector size
  fz <- 125
  df.PT.R <- as.data.frame(NULL)
  df.PT.L <- as.data.frame(NULL)
  df.PT.R.row <- as.data.frame(NULL)
  df.PT.L.row <- as.data.frame(NULL)
  for (i in 1:length(fl)) {
    move.angle.SP.R <- subset(diff(df$x),df$id == i & df$arc == 0 & diff(df$x) != 0) #Right
    move.angle.SP.L <- subset(diff(df$x),df$id == i & df$arc == 180 & diff(df$x) != 0) #RLeft
    move.angle.SP.R <- ifelse(move.angle.SP.R > 0,1,-1) #Right
    move.angle.SP.L <-  ifelse(move.angle.SP.L < 0,-1,1) #Left
    if(length(move.angle.SP.R) != 0){
      if(sum(subset(move.angle.SP.R,move.angle.SP.R > 0)) + sum(subset(move.angle.SP.R,move.angle.SP.R < 0)) != 0){
        # print(sum(abs(move.angle.SP.R)))
        # print(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0)))
        c.PorN.R <- c(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0))
                      ,sum(subset(abs(move.angle.SP.R),move.angle.SP.R < 0)) / sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0 | move.angle.SP.R < 0)))
        df.set <- data.frame(PorN =c("Positive","Negative"),
                             value = c(abs(c.PorN.R[1]),abs(c.PorN.R[2])),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        # print(sum(subset(abs(move.angle.SP.R),move.angle.SP.R > 0)) / sum(abs(move.angle.SP.R)))
        df.PT.R <- rbind(df.PT.R,df.set)
        
        #バイナリの加算データをデータフレーム に格納
        df.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.R,move.angle.SP.R > 0))),
                                           sum(abs(subset(move.angle.SP.R,move.angle.SP.R < 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.R.row <- rbind(df.PT.R.row,df.set.row)
      }
    }
    if(length(move.angle.SP.L) != 0){
      if(sum(subset(move.angle.SP.L,move.angle.SP.L > 0)) + sum(subset(move.angle.SP.L,move.angle.SP.L < 0)) != 0){
        c.PorN.L <- c(sum(subset(abs(move.angle.SP.L),move.angle.SP.L < 0)) / sum(abs(move.angle.SP.L))
                      ,sum(subset(abs(move.angle.SP.L),move.angle.SP.L > 0)) / sum(abs(move.angle.SP.L)))
        df.set <- data.frame(PorN =c("Positive","Negative"),
                             value = c(abs(c.PorN.L[1]),abs(c.PorN.L[2])),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.L <- rbind(df.PT.L,df.set)
        
        #バイナリの加算データをデータフレーム に格納
        df.set.row <- data.frame(PorN =c("Positive","Negative"),
                                 value = c(sum(abs(subset(move.angle.SP.L,move.angle.SP.L < 0))),
                                           sum(abs(subset(move.angle.SP.L,move.angle.SP.L > 0)))),ExNum = sprintf("No.%d.%s",i,exnum[i %% 5 + 1]))
        df.PT.L.row <- rbind(df.PT.L.row,df.set.row)
      }
    }
  }
  ex_bar_plot(df.PT.R,df.PT.L,"IT_10",1,10)
  ex_bar_plot(df.PT.R,df.PT.L,"IT_40",11,20)
  ex_bar_plot(df.PT.R,df.PT.L,"IT_160",21,30)
  ex_bar_plot(df.PT.R,df.PT.L,"IT_staticLefft",31,40)
  ex_bar_plot(df.PT.R,df.PT.L,"IT_staticRight",41,50)
  
  # フィッシャーの検定
  # 各条件の平均を導出
  it10.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_10",1,10)
  it40.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_40",11,20)
  it160.mean <- df.mean(df.PT.R.row,df.PT.L.row,"IT_160",21,30)
  calib <- 1/1000
  # デバッグ
  # print(it10.mean * calib)
  # print(it40.mean* calib)
  # print(it160.mean* calib)
  
  # ft.R <- matrix(c(it160.mean$valueR.Pos * calib,it160.mean$valueR.Nega * calib
  #                  ,it40.mean$valueR.Pos * calib,it40.mean$valueR.Nega * calib,
  #                  it10.mean$valueR.Pos * calib,it10.mean$valueR.Nega * calib),nrow = 3,byrow = T)
  # ft.L <- matrix(c(it160.mean$valueL.Pos * calib,it160.mean$valueL.Nega * calib
  #                  ,it40.mean$valueL.Pos * calib,it40.mean$valueL.Nega * calib
  #                  ,it10.mean$valueL.Pos * calib,it10.mean$valueL.Nega * calib),nrow = 3,byrow = T)
  # 
  # print(fisher.test(ft.L))
  # print(fisher.test(ft.R))
}
# barの条件毎の出力
ex_bar_plot <- function(df.PT.R,df.PT.L,exname,first,end){
  #範囲指定
  df.PT.R <- df.PT.R[first:end,]
  df.PT.L <- df.PT.L[first:end,]
  #平均，標準偏差のバーデータの作成
  df.mean.R <- c(mean(c(df.PT.R$value[1],df.PT.R$value[3],df.PT.R$value[5],df.PT.R$value[7],df.PT.R$value[9])),
                 mean(c(df.PT.R$value[2],df.PT.R$value[4],df.PT.R$value[6],df.PT.R$value[8],df.PT.R$value[10])))
  df.set <- data.frame(PorN =c("Positive","Negative"),
                       value = c(df.mean.R[1],df.mean.R[2]),ExNum = sprintf("Mean"))
  df.PT.R <- rbind(df.PT.R,df.set)
  
  fname <- sprintf("%s_bar.R_x.pdf",exname)
  p <- ggplot(df.PT.R,aes(x = ExNum,y= value,fill = PorN)) +
    ylim(c(0, 1.2)) +
    geom_bar(stat="identity",width = 0.8,position = "dodge") +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(fill ="Positive / Negative") + #凡例
    geom_text(aes(x = ExNum,y= value,label = sprintf("%2.2f",abs(value)),hjust=-0.3),position = position_dodge(width=0.9),size = 8)  +
    coord_flip() +
    theme_bw() +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = 'none') +
    scale_fill_manual(values=c("#000000","#c8c8cb")) +
    scale_colour_manual(values=c("black","black"))
  ggsave(file = fname, plot = p)
  
  df.mean.L <- c(mean(c(df.PT.L$value[1],df.PT.L$value[3],df.PT.L$value[5],df.PT.L$value[7],df.PT.L$value[9])),
                 mean(c(df.PT.L$value[2],df.PT.L$value[4],df.PT.L$value[6],df.PT.L$value[8],df.PT.L$value[10])))
  df.set <- data.frame(PorN =c("Positive","Negative"),
                       value = c(df.mean.L[1],df.mean.L[2]),ExNum = sprintf("Mean"))
  df.PT.L <- rbind(df.PT.L,df.set)
  fname <- sprintf("%s_bar.L_x.pdf",exname)
  p <- ggplot(df.PT.L,aes(x = ExNum,y= value,fill = PorN)) +
    ylim(c(0, 1.2)) +
    geom_bar(stat="identity",width = 0.8,position = "dodge") +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(fill="Positive / Negative") + #凡例
    theme(legend.position=c(0.85, 0.3),legend.title=element_text(size=14)) + 
    geom_text(aes(x = ExNum,y= value,label = sprintf("%2.2f",abs(value)),hjust=-0.3),position = position_dodge(width=0.9),size = 8) +
    coord_flip() +
    theme_bw() +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = 'none') +
    scale_fill_manual(values=c("#000000","#c8c8cb")) +
    scale_colour_manual(values=c("black","black"))
  ggsave(file = fname, plot = p) 
}

df.mean <- function(df.PT.R,df.PT.L,exname,first,end){
  #範囲指定
  df.PT.R <- df.PT.R[first:end,]
  df.PT.L <- df.PT.L[first:end,]
  #平均，標準偏差のバーデータの作成
  #平均を出力
  df.mean.R <- c(mean(c(df.PT.R$value[1],df.PT.R$value[3],df.PT.R$value[5],df.PT.R$value[7],df.PT.R$value[9])),
                 mean(c(df.PT.R$value[2],df.PT.R$value[4],df.PT.R$value[6],df.PT.R$value[8],df.PT.R$value[10])))
  df.mean.L <- c(mean(c(df.PT.L$value[1],df.PT.L$value[3],df.PT.L$value[5],df.PT.L$value[7],df.PT.L$value[9])),
                 mean(c(df.PT.L$value[2],df.PT.L$value[4],df.PT.L$value[6],df.PT.L$value[8],df.PT.L$value[10])))
  return(data.frame(valueR.Pos = df.mean.R[1] * 16,valueR.Nega = df.mean.R[2] * 16,
                    valueL.Pos = df.mean.L[1] * 16,valueL.Nega = df.mean.L[2] * 16))
}

bar_bulk_output(df.downsamp,fl)

