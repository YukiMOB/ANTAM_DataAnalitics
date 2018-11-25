library(ggplot2)

# plot(c(1,2,3),c(mean(it160.distance.list),mean(it40.distance.list),mean(it10.distance.list)))
# plot(c(1,2,3),c(mean(it160.list.velocity_average),mean(it40.list.velocity_average),mean(it10.list.velocity_average)))
# plot(c(1,2,3),c(mean(it160.residence_ratio.list),mean(it40.residence_ratio.list),mean(it10.residence_ratio.list)))

# 総移動距離の折れ線グラフとエラーバー
plot_distance <- function(){
  df.parameter <- data.frame(mean = c(mean(it160.distance.list),
                                      mean(it40.distance.list),
                                      mean(it10.distance.list)),
                             sd = c(sd(it160.distance.list),
                                    sd(it40.distance.list),
                                    sd(it10.distance.list)),
                             value = c(it160.distance.list,
                                       it40.distance.list,
                                       it10.distance.list),
                             conditions = c(1,2,3))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
    scale_x_continuous(breaks = c(1,2,3),
                       labels = c("160 s","40 s","10 s")) +
    geom_line() +
    geom_errorbar(errors,width = 0.2) + 
    geom_point(aes(x = conditions,y = value))
  plot(g)
}

plot_velocity <- function(){
  df.parameter <- data.frame(mean = c(mean(it160.list.velocity_average),
                                      mean(it40.list.velocity_average),
                                      mean(it10.list.velocity_average)),
                             sd = c(sd(it160.list.velocity_average),
                                    sd(it40.list.velocity_average),
                                    sd(it10.list.velocity_average)),
                             value = c(it160.list.velocity_average,
                                       it40.list.velocity_average,
                                       it10.list.velocity_average),
                             conditions = c(1,2,3))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
    scale_x_continuous(breaks = c(1,2,3),
                       labels = c("160 s","40 s","10 s")) +
    geom_line() +
    geom_errorbar(errors,width = 0.2) + 
    geom_point(aes(x = conditions,y = value))
  plot(g)
}

# plot_residence_ratio <- function(){
  # df.parameter <- data.frame(mean = c(mean(it160.residence_ratio.list),
  #                                     mean(it40.residence_ratio.list),
  #                                     mean(it10.residence_ratio.list)),
  #                           sd = c(sd(it160.residence_ratio.list),
  #                                  sd(it40.residence_ratio.list),
  #                                  sd(it10.residence_ratio.list)),
  #                           conditions = c(1,2,3))
  # errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
  #               ymin = df.parameter$mean - df.parameter$sd)
  # g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
  #   scale_x_continuous(breaks = df.parameter$conditions,
  #                      labels = c("160 s","40 s","10 s")) +
  #   geom_line() +
  #   geom_point(df.parameter,aes(x = conditions,y = value)) +
  #   geom_errorbar(errors,width = 0.2)
  # 
  # plot(g)
# }

plot_residence_ratio <- function(){
  df.parameter <- data.frame(mean = c(mean(it160.residence_ratio.list),
                                      mean(it40.residence_ratio.list),
                                      mean(it10.residence_ratio.list)),
                             sd = c(sd(it160.residence_ratio.list),
                                    sd(it40.residence_ratio.list),
                                    sd(it10.residence_ratio.list)),
                             value = c(it160.residence_ratio.list,
                                       it40.residence_ratio.list,
                                       it10.residence_ratio.list),
                             conditions = c(1,2,3))
  errors <- aes(ymax = df.parameter$mean + df.parameter$sd,
                ymin = df.parameter$mean - df.parameter$sd)
  g <- ggplot(df.parameter,aes(x = conditions,y = mean)) +
      scale_x_continuous(breaks = c(1,2,3),
                         labels = c("160 s","40 s","10 s")) +
      geom_line() +
      geom_errorbar(errors,width = 0.2) + 
      geom_point(aes(x = conditions,y = value))
  plot(g)
}
plot_distance()
