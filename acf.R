# data analysis
ppi <- 300 # resolution of figures
ofset <- 100
freq <- 125

getacf <- function(df,fl){
  lagMax <- 1800 / 4#on ACF
  for (i in 1:length(fl)) {
    x <- subset(df$x,df$id == i)
    y <- subset(df$y,df$id == i)
    #png(fname, width=4*ppi, height=4 * 1.15 *ppi, res=ppi)
    #acf.data <- acf(diff(x),lag.max <- 50000,plot = FALSE)
    fname <- sub(".csv","acf_x.png",fl[i])
    png(fname, width=4*ppi, height=4 * 1.15 *ppi, res=ppi)
    acf.data <- acf(diff(x),lag.max <- 50000,ylim = c(-1,1),main="ACF",ylab = "ACF")
    #plot(,acf.data,"l",ylim = c(-1,1),main=title,xlim = c(0,2000),xlab = "t(s)", ylab = "ACF")
    dev.off()
  }
}