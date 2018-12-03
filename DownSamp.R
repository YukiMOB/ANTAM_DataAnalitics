ofset <- 100
freq <- 25
basefreq <- 125
options(scipen=100)

downdampling <- function(df.original,freq){
  df.return <- as.data.frame(NULL)
  for (i in 1:max(df.original$id)) {
    df <-  subset(df.original,df.original$id == i)
    n <- seq(1,nrow(df),by = freq)
    x.downsamp<- valueDS(df$x,n)
    y.downsamp <- valueDS(df$y,n)
    df.ds <- data.frame(t = df$t[n], x = x.downsamp, y = y.downsamp,id = df$id[n],path = df$id[n],arc = df$arc[n],pos = df$pos[n])
    df.return <- rbind(df.return,df.ds)
  }
  return(df.return)
}

valueDS <- function(value,n){
  ds.list <- c(0)
  ds.list <- append(ds.list,value[1],after = length(ds.list))
  for (i in 3:length(n)) { # length(n) - 1がうまくいかないので初期値を3に
    ds.list <- append(ds.list,mean(value[n[i] - 1:n[i]]),after = length(ds.list))
  }
  return(ds.list)
}

df.downsamp <- downdampling(df,freq)
df.NoStimulus.downsamp <- downdampling(df.NoStimulus,freq)