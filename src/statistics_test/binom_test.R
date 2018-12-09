# 比率の二項検定の実施
options(digits = 4)
ratio.binom.test <- function(num){
  num <- as.integer(num * 100) # 比率を整数値に変換
  out <- binom.test(c(num,100 - num),p = 0.5)
  return(out$p.value)
}

# 与えられたリストに対して，順々にテストを実施する
get.pValue.binom.test <- function(value.lis){
  p.list <- c()
  for (i in 1:length(value.lis)) {
    p.list <- c(p.list,ratio.binom.test(value.lis[i]))
  }
  print(value.lis)
  return(p.list)
}

