ofset <- 100
freq <- 5
basefreq <- 125
options(scipen=100)

downdampling <- function(dt.original,freq){
  dt.return <- as.data.frame(NULL)
  for (i in 1:max(dt.original$id)) {
    dt <-  subset(dt.original,dt.original$id == i)
    n <- seq(1,nrow(dt),by = freq)
    x.downsamp<- valueDS(dt$x,n)
    y.downsamp <- valueDS(dt$y,n)
    dt.ds <- data.frame(t = dt$t[n], x = x.downsamp, y = y.downsamp,id = dt$id[n],path = dt$id[n],arc = dt$arc[n],pos = dt$pos[n])
    dt.return <- rbind(dt.return,dt.ds)
  }
  return(dt.return)
}

valueDS <- function(value,n){
  ds.list <- c(0)
  ds.list <- append(ds.list,value[1],after = length(ds.list))
  for (i in 3:length(n)) { # length(n) - 1がうまくいかないので初期値を3に
    ds.list <- append(ds.list,mean(value[n[i] - 1:n[i]]),after = length(ds.list))
  }
  return(ds.list)
}

dt.downsamp <- downdampling(dt,freq)
dt.NoStimulus.downsamp <- downdampling(dt.NoStimulus,freq)
