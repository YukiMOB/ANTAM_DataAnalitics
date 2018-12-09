library(ggplot2)

# それぞれのデータのプロット(様々なパラーメータ)の各種値，平均，標準誤差を出力

plot.graph.someValues <- function(){
  # データフレームで各条件のvalueをマージ
  df <- data.frame()
  df <- df.bind.valueAndConditions(df,static.PN.Ratio.list,"Static_30min")
  df <- df.bind.valueAndConditions(df,it160.PN.Ratio.list,"IT160s")
  df <- df.bind.valueAndConditions(df,it40.PN.Ratio.list,"IT40s")
  df <- df.bind.valueAndConditions(df,it10.PN.Ratio.list,"IT10s")
  g <- ggplot(df,aes(x = df$conditions,y = df$value)) +
    geom_point()
  return(g)
}

df.bind.valueAndConditions <- function(df.base,add.value,add.condition){
  return.df <- rbind(df.base,data.frame(value = add.value,conditions = add.condition))
  return(return.df)
}

### プロット

ratio <- plot(plot.graph.someValues())
