library(ggplot2)

# それぞれのデータのプロット(様々なパラーメータ)の各種値，平均，標準誤差を出力

plot.graph.someValues <- function(){
  # データフレームで各条件のvalueをマージ
  df <- data.frame()
  df <- df.bind.valueAndConditions(df,static.PN.Ratio.list,"Static_30min","raw_rate")
  df <- df.bind.valueAndConditions(df,it160.PN.Ratio.list,"IT160s","raw_rate")
  df <- df.bind.valueAndConditions(df,it40.PN.Ratio.list,"IT40s","raw_rate")
  df <- df.bind.valueAndConditions(df,it10.PN.Ratio.list,"IT10s","raw_rate")
  # 平均値の値
  df <- df.bind.valueAndConditions(df,list(mean(static.PN.Ratio.list)),"Static_30min","mean")
  # df <- df.bind.valueAndConditions(df,mean(it160.PN.Ratio.list),"IT160s","mean")
  # df <- df.bind.valueAndConditions(df,mean(it40.PN.Ratio.list),"IT40s","mean")
  # df <- df.bind.valueAndConditions(df,mean(it10.PN.Ratio.list),"IT10s","mean")
  # 複数のデータを重ねてプロット(生の比率データ，平均値)
  g <- ggplot()
  g <- g + geom_point(data = df,aes(x = df$conditions,y = subset(df$value,df$meaning == "raw_rate")))
  #g <- g + geom_line(data = df,aes(x = df$conditions,y = subset(df$value,df$meaning == "mean")))
  return(g)
}

df.bind.valueAndConditions <- function(df.base,add.value,add.condition,add.meaning){
  return.df <- rbind(df.base,data.frame(value = add.value,conditions = add.condition,meaning = add.meaning))
  return(return.df)
}

### プロット

ratio <- plot(plot.graph.someValues())
