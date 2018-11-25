# クラスカル・ウォリス検定 : 距離
kruskal.test(x=list(it160.distance.list,it40.distance.list,it10.distance.list))
kruskal.test(x=list(it160.list.velocity_average,it40.list.velocity_average,it10.list.velocity_average))
kruskal.test(x=list(it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list))
kruskal.test(x=list(subset(it160.ratio.PN$value_Left,it160.ratio.PN$PorN_Left == "Negative")
                    ,subset(it40.ratio.PN$value_Left,it40.ratio.PN$PorN_Left == "Negative")
                    ,subset(it10.ratio.PN$value_Left,it10.ratio.PN$PorN_Left == "Negative")))

kruskal.test(x=list(subset(it160.ratio.PN$value_Right,it160.ratio.PN$PorN_Right == "Negative")
                    ,subset(it40.ratio.PN$value_Right,it40.ratio.PN$PorN_Right == "Negative")
                    ,subset(it10.ratio.PN$value_Right,it10.ratio.PN$PorN_Right == "Negative")))

#Steel.Dwass(it160.distance.list,it40.distance.list,it10.distance.list)

# スティール・ドゥワスの多重比較
# data <- c(
#   it160.residence_ratio.list, # 第 1 群のデータ，11 例
#   it40.residence_ratio.list,      # 第 2 群のデータ，10 例
#   it10.residence_ratio.list    # 第 3 群のデータ，10 例
# )
# group <- rep(1:3, c(5, 5, 5))                     # 群の識別変数

data <- c(
  subset(it160.ratio.PN$value_Left,it160.ratio.PN$PorN_Left == "Negative"), # 第 1 群のデータ，11 例
  subset(it40.ratio.PN$value_Left,it40.ratio.PN$PorN_Left == "Negative"),      # 第 2 群のデータ，10 例
  subset(it10.ratio.PN$value_Left,it10.ratio.PN$PorN_Left == "Negative")    # 第 3 群のデータ，10 例
)
group <- rep(1:3, c(5, 5, 5))  

Steel.Dwass(data, group)
