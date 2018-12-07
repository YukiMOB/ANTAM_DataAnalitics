# 関数の処理と，呼び出し処理を別箇にする
options(digits = 4)
options(scipen=1)
# 二項検定
# L
get.pValue.binom.test(subset(it160.PN.Ratio.list$value_Right,it160.PN.Ratio.list$PorN_Right == "Negative"))
get.pValue.binom.test(subset(it40.PN.Ratio.list$value_Right,it40.PN.Ratio.list$PorN_Right == "Negative"))
get.pValue.binom.test(subset(it10.PN.Ratio.list$value_Right,it10.PN.Ratio.list$PorN_Right == "Negative"))
get.pValue.binom.test(subset(static_right.PN.Ratio.list$value_Right,static_right.PN.Ratio.list $PorN_Right == "Negative"))

# R
get.pValue.binom.test(subset(it160.PN.Ratio.list$value_Left,it160.PN.Ratio.list$PorN_Left == "Negative"))
get.pValue.binom.test(subset(it40.PN.Ratio.list$value_Left,it40.PN.Ratio.list$PorN_Left == "Negative"))
get.pValue.binom.test(subset(it10.PN.Ratio.list$value_Left,it10.PN.Ratio.list$PorN_Left == "Negative"))
get.pValue.binom.test(subset(static_left.PN.Ratio.list$value_Left,static_left.PN.Ratio.list$PorN_Left == "Negative"))

# クラスカル・ウォリス検定 : 距離
kruskal.test(x=list(staticLeft.distance.list,staticRight.distance.list,it160.distance.list,it40.distance.list,it10.distance.list,NoStimulus.distance.list))
kruskal.test(x=list(staticLeft.list.velocity_average,staticRight.list.velocity_average,it160.list.velocity_average,it40.list.velocity_average,it10.list.velocity_average,NoStimulus.list.Velocity_average))
kruskal.test(x=list(staticLeft.residence_ratio.list,staticRight.residence_ratio.list,it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list,NoStimulus.residence_ratio.list))
kruskal.test(x=list(subset(static_left.PN.Ratio.list$value_Left,static_left.PN.Ratio.list$PorN_Left == "Negative")
                    ,subset(it160.PN.Ratio.list$value_Left,it160.PN.Ratio.list$PorN_Left == "Negative")
                    ,subset(it40.PN.Ratio.list$value_Left,it40.PN.Ratio.list$PorN_Left == "Negative")
                    ,subset(it10.PN.Ratio.list$value_Left,it10.PN.Ratio.list$PorN_Left == "Negative")))

kruskal.test(x=list(subset(static_right.PN.Ratio.list$value_Right,static_right.PN.Ratio.list$PorN_Right == "Negative")
                    ,subset(it160.PN.Ratio.list$value_Right,it160.PN.Ratio.list$PorN_Right == "Negative")
                    ,subset(it40.PN.Ratio.list$value_Right,it40.PN.Ratio.list$PorN_Right == "Negative")
                    ,subset(it10.PN.Ratio.list$value_Right,it10.PN.Ratio.list$PorN_Right == "Negative")))


# Steel Dwass
data <- c(
  subset(static_left.PN.Ratio.list$value_Left,static_left.PN.Ratio.list$PorN_Left == "Negative"),
  subset(it160.PN.Ratio.list$value_Left,it160.PN.Ratio.list$PorN_Left == "Negative"), # 第 1 群のデータ，11 例
  subset(it40.PN.Ratio.list$value_Left,it40.PN.Ratio.list$PorN_Left == "Negative"),      # 第 2 群のデータ，10 例
  subset(it10.PN.Ratio.list$value_Left,it10.PN.Ratio.list$PorN_Left == "Negative")    # 第 3 群のデータ，10 例
)
group <- rep(1:4, c(6,5,7,6))  

Steel.Dwass(data, group)

data <- c(
  subset(static_right.PN.Ratio.list$value_Right,static_right.PN.Ratio.list$PorN_Right == "Negative"),
  subset(it160.PN.Ratio.list$value_Right,it160.PN.Ratio.list$PorN_Right == "Negative"), # 第 1 群のデータ，11 例
  subset(it40.PN.Ratio.list$value_Right,it40.PN.Ratio.list$PorN_Right == "Negative"),      # 第 2 群のデータ，10 例
  subset(it10.PN.Ratio.list$value_Right,it10.PN.Ratio.list$PorN_Right == "Negative")    # 第 3 群のデータ，10 例
)
group <- rep(1:4, c(6,5, 7, 5))  

Steel.Dwass(data, group)