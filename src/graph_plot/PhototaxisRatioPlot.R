library(ggsignif)
library(ggplot2)
theme_set(theme_classic(base_size = 18, base_family = "Helvetica"))


# それぞれのデータのプロット(様々なパラーメータ)の各種値，平均，標準誤差を出力

# plot.graph <- function(value.static,value.160,value.40,value.10,ylabel,ymax){
plot.graph <- function(value.160,value.40,value.10,ylabel,ymax){
  # データフレームで各条件のvalueをマージ
  df.value <- data.frame()
  # df.value <- df.bind.valueAndConditions(df.value,value.static,"Static(30min)")
  df.value <- df.bind.valueAndConditions(df.value,value.160 ,"5400 lx")
  df.value <- df.bind.valueAndConditions(df.value,value.40 ,"580 lx")
  df.value <- df.bind.valueAndConditions(df.value,value.10 ,"90 lx")
  # 平均値の値
  df.mean.sd <- data.frame()
  # df.mean.sd <- df.bind.mean.SE.Conditions(df.mean.sd,0.5,0,"0Sample")
  # df.mean.sd <- df.bind.mean.SE.Conditions(df.mean.sd,mean(value.static),sd(value.static) / sqrt(length(value.static)),"Static(30min)")
  df.mean.sd <- df.bind.mean.SE.Conditions(df.mean.sd,mean(value.160 ),sd(value.160 ) / sqrt(length(value.160 )),"5400 lx")
  df.mean.sd <- df.bind.mean.SE.Conditions(df.mean.sd,mean(value.40 ),sd(value.40 ) /  sqrt(length(value.40 )),"580 lx")
  df.mean.sd <- df.bind.mean.SE.Conditions (df.mean.sd,mean(value.10 ),sd(value.10 ) /  sqrt(length(value.10 )),"90 lx")
  
  # エラーバー定義
  errors <- aes(x = df.mean.sd$conditions,ymax = df.mean.sd$value + df.mean.sd$se,
                ymin = df.mean.sd$value - df.mean.sd$se)
  # 複数のデータを重ねてプロット(生の比率データ，平均値)
  g <- ggplot() + geom_signif()
  g <- g + geom_bar(data = df.mean.sd,aes(x = df.mean.sd$conditions,y = df.mean.sd$value),stat = "identity"
                    ,colour="black", fill="white",width = 0.5) + 
    geom_errorbar(errors,width = 0.25)
    # geom_text(aes(x = df.mean.sd$conditions, # 平均をプロットしたい時のみ表示
    #               y = df.mean.sd$value),
    #           label = sprintf("%2.3f",df.mean.sd$value), 
    #           hjust = 1.5,
    #           vjust = -1.0)
  g <- g + geom_point(data = df.value,aes(x = df.value$conditions,y = df.value$value))
  g <- g + xlab("Conditions") + ylab(ylabel) + ylim(0,ymax) + 
       theme_classic() +
       theme(axis.text=element_text(size=25),
          axis.title=element_text(size=30,face="bold"),
          legend.position = 'none')
  plot(g)
}


df.bind.valueAndConditions <- function(df.base,add.value,add.condition){
  return.df <- rbind(df.base,data.frame(value = add.value,conditions = add.condition))
  return(return.df)
}
df.bind.mean.SE.Conditions <- function(df.base,add.value,add.se,add.condition){
  return.df <- rbind(df.base,data.frame(value = add.value,se = add.se,conditions = add.condition))
  return(return.df)
}

### プロット  value.static <- static.list.velocity_average
plot.graph(# static.list.velocity_average,
           it160.list.velocity_average,
           it40.list.velocity_average,
           it10.list.velocity_average,
           "Velocity(mm/sec)",20)

plot.graph(# static.PN.Ratio.list,
           it160.PN.Ratio.list,
           it40.PN.Ratio.list,
           it10.PN.Ratio.list,
           "Negative Ratio",1.0)

plot.graph(# static.residence_ratio.list,
           it160.residence_ratio.list,
           it40.residence_ratio.list,
           it10.residence_ratio.list,
           "Residence Ratio",1.0)
