library(ggplot2)
#  リファクタリングしたいいいい
# plot(c(1,2,3),c(mean(it160.distance.list),mean(it40.distance.list),mean(it10.distance.list)))
# plot(c(1,2,3),c(mean(it160.list.velocity_average),mean(it40.list.velocity_average),mean(it10.list.velocity_average)))
# plot(c(1,2,3),c(mean(it160.residence_ratio.list),mean(it40.residence_ratio.list),mean(it10.residence_ratio.list)))

# それぞれの値を入れたいならこうして．ただし，無刺激入ると試行数が合わないのでプロットできません
# value = c(it160.distance.list,
#           it40.distance.list,
#           it10.distance.list)

# 総移動距離の折れ線グラフとエラーバー
plot_distance <- function(){
  df.parameter <- data.frame(mean = c(mean(staticLeft.distance.list),
                                      mean(staticRight.distance.list),
                                      mean(it160.distance.list),
                                      mean(it40.distance.list),
                                      mean(it10.distance.list),
                                      mean(NoStimulus.distance.list)),
                             sd = c(sd(staticLeft.distance.list),
                                    sd(staticRight.distance.list),
                                    sd(it160.distance.list),
                                    sd(it40.distance.list),
                                    sd(it10.distance.list),
                                    sd(NoStimulus.distance.list)),
                             conditions = c(1,2,3,4,5,6))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6),
                       labels = c("static Left","static Right","160 s","40 s","10 s","No_Stimulus")) +
    geom_line() +
    geom_errorbar(errors,width = 0.2) +
    theme_bw() +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = 'none') +
    xlab("Conditions") +
    ylab("dinstance cm")
    # geom_point(aes(x = conditions,y = value))
  plot(g)
}

plot_velocity <- function(){
  df.parameter <- data.frame(mean = c(mean(staticLeft.list.velocity_average),
                                      mean(staticRight.list.velocity_average),
                                      mean(it160.list.velocity_average),
                                      mean(it40.list.velocity_average),
                                      mean(it10.list.velocity_average),
                                      mean(NoStimulus.list.Velocity_average)),
                             sd = c(sd(staticLeft.list.velocity_average),
                                    sd(staticRight.list.velocity_average),
                                    sd(it160.list.velocity_average),
                                    sd(it40.list.velocity_average),
                                    sd(it10.list.velocity_average),
                                    sd(NoStimulus.list.Velocity_average)),
                             conditions = c(1,2,3,4,5,6))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6),
                       labels = c("static Left","static Right","160 s","40 s","10 s","No_Stimulus")) +
    geom_line() +
    geom_errorbar(errors,width = 0.2) + 
    theme_bw() +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = 'none') +
    xlab("Conditions") +
    ylab("Velocity cm/min")
  plot(g)
}


plot_residence_ratio <- function(){
  df.parameter <- data.frame(mean = c(mean(staticLeft.residence_ratio.list),
                                      mean(staticRight.residence_ratio.list),
                                      mean(it160.residence_ratio.list),
                                      mean(it40.residence_ratio.list),
                                      mean(it10.residence_ratio.list),
                                      mean(NoStimulus.residence_ratio.list)),
                             sd = c(sd(staticLeft.residence_ratio.list),
                                    sd(staticRight.residence_ratio.list),
                                    sd(it160.residence_ratio.list),
                                    sd(it40.residence_ratio.list),
                                    sd(it10.residence_ratio.list),
                                    sd(NoStimulus.residence_ratio.list)),
                             conditions = c(1,2,3,4,5,6))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6),
                       labels = c("static Left","static Right","160 s","40 s","10 s","No_Stimulus")) +
      geom_line() +
      geom_errorbar(errors,width = 0.2) + 
      theme_bw() +
      theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = 'none') +
      xlab("Conditions") +
      ylab("rario of residencetime")
  plot(g)
}

