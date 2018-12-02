# クラスカル・ウォリス検定 : 距離
kruskal.test(x=list(it160.distance.list,it40.distance.list,it10.distance.list,NoStimulus.distance.list))
kruskal.test(x=list(it160.list.velocity_average,it40.list.velocity_average,it10.list.velocity_average,NoStimulus.list.Velocity_average))
kruskal.test(x=list(it160.residence_ratio.list,it40.residence_ratio.list,it10.residence_ratio.list,NoStimulus.residence_ratio.list))
kruskal.test(x=list(subset(it30min_L.ratio.PN$value_Left,it30min_L.ratio.PN$PorN_Left == "Negative")
                    ,subset(it160.ratio.PN$value_Left,it160.ratio.PN$PorN_Left == "Negative")
                    ,subset(it40.ratio.PN$value_Left,it40.ratio.PN$PorN_Left == "Negative")
                    ,subset(it10.ratio.PN$value_Left,it10.ratio.PN$PorN_Left == "Negative")))

kruskal.test(x=list(subset(it30min_R.ratio.PN$value_Right,it30min_R.ratio.PN$PorN_Right == "Negative")
                    ,subset(it160.ratio.PN$value_Right,it160.ratio.PN$PorN_Right == "Negative")
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
  subset(it30min_L.ratio.PN$value_Left,it30min_L.ratio.PN$PorN_Left == "Negative"),
  subset(it160.ratio.PN$value_Left,it160.ratio.PN$PorN_Left == "Negative"), # 第 1 群のデータ，11 例
  subset(it40.ratio.PN$value_Left,it40.ratio.PN$PorN_Left == "Negative"),      # 第 2 群のデータ，10 例
  subset(it10.ratio.PN$value_Left,it10.ratio.PN$PorN_Left == "Negative")    # 第 3 群のデータ，10 例
)
group <- rep(1:4, c(5,5, 5, 5))  

Steel.Dwass(data, group)

data <- c(
  subset(it30min_R.ratio.PN$value_Right,it30min_R.ratio.PN$PorN_Right == "Negative"),
  subset(it160.ratio.PN$value_Right,it160.ratio.PN$PorN_Right == "Negative"), # 第 1 群のデータ，11 例
  subset(it40.ratio.PN$value_Right,it40.ratio.PN$PorN_Right == "Negative"),      # 第 2 群のデータ，10 例
  subset(it10.ratio.PN$value_Right,it10.ratio.PN$PorN_Right == "Negative")    # 第 3 群のデータ，10 例
)
group <- rep(1:4, c(5,5, 5, 5))  

Steel.Dwass(data, group)


Steel.Dwass <- function(data,group)
{
  OK <- complete.cases(data, group)
  data <- data[OK]
  group <- group[OK]
  n.i <- table(group)
  ng <- length(n.i)
  t <- combn(ng, 2, function(ij) {
    i <- ij[1]
    j <- ij[2]
    r <- rank(c(data[group == i], data[group == j]))
    R <- sum(r[1:n.i[i]])
    N <- n.i[i]+n.i[j]
    E <- n.i[i]*(N+1)/2
    V <- n.i[i]*n.i[j]/(N*(N-1))*(sum(r^2)-N*(N+1)^2/4)
    return(abs(R-E)/sqrt(V))
  })
  p <- ptukey(t*sqrt(2), ng, Inf, lower.tail=FALSE)
  result <- cbind(t, p)
  rownames(result) <- combn(ng, 2, paste, collapse=":")
  return(result)
}
