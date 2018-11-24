library(NSM3)
# plot(c(1,2,3),c(mean(it160.distance.list),mean(it40.distance.list),mean(it10.distance.list)))
# plot(c(1,2,3),c(mean(it160.list.velocity_average),mean(it40.list.velocity_average),mean(it10.list.velocity_average)))
# plot(c(1,2,3),c(mean(it160.residence_ratio.list),mean(it40.residence_ratio.list),mean(it10.residence_ratio.list)))

# クラスカル・ウォリス検定 : 距離
kruskal.test(x=list(it160.distance.list,it40.distance.list,it10.distance.list))
kruskal.test(x=list(it160.list.velocity_average,it40.list.velocity_average,it10.list.velocity_average))
kruskal.test(x=list(it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list))
#Steel.Dwass(it160.distance.list,it40.distance.list,it10.distance.list)

# スティール・ドゥワスの多重比較
# data <- c(
#   it160.residence_ratio.list, # 第 1 群のデータ，11 例
#   it40.residence_ratio.list,      # 第 2 群のデータ，10 例
#   it10.residence_ratio.list    # 第 3 群のデータ，10 例
# )
# group <- rep(1:3, c(5, 5, 5))                     # 群の識別変数
# Steel.Dwass(data, group)