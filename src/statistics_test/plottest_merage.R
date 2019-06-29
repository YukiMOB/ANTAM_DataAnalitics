# 関数の処理と，呼び出し処理を別箇にする
options(digits = 5)
options(scipen=100)
# 二項検定
# L
get.pValue.binom.test(it160.PN.Ratio.list)
get.pValue.binom.test(it40.PN.Ratio.list)
get.pValue.binom.test(it10.PN.Ratio.list)
get.pValue.binom.test(subset(static_.PN.Ratio.list,static.PN.Ratio.list == "Negative"))

# クラスカル・ウォリス検定
# 距離
kruskal.test(x=list(staticLeft.distance.list,staticRight.distance.list,it160.distance.list,it40.distance.list,it10.distance.list,NoStimulus.distance.list))
# 速度
kruskal.test(x=list(static.list.velocity_average,
                    it160.list.velocity_average,
                    it40.list.velocity_average,
                    it10.list.velocity_average))
#滞留時間
kruskal.test(x=list(staticLeft.residence_ratio.list,staticRight.residence_ratio.list,it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list,NoStimulus.residence_ratio.list))

kruskal.test(x=list(static.PN.Ratio.list
                    ,it160.PN.Ratio.list
                    ,it40.PN.Ratio.list
                    ,it10.PN.Ratio.list))

# 条件間 個体差

# Steel Dwass

# Negative / Positive
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

# residence time
# スティール・ドゥワスの多重比較
data <- c(
  staticLeft.residence_ratio.list,
  staticRight.residence_ratio.list,
  it160.residence_ratio.list, # 第 1 群のデータ，11 例
  it40.residence_ratio.list,      # 第 2 群のデータ，10 例
  it10.residence_ratio.list    # 第 3 群のデータ，10 例
)
group <- rep(1:5, c(6,5,7,5,6))  
Steel.Dwass(data, group)
# group <- rep(1:3, c(5, 5, 5))                     # 群の識別変数

