#Steel.Dwass(it160.distance.list,it40.distance.list,it10.distance.list)

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

