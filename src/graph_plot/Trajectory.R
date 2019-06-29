# data analysis
ppi <- 300 # resolution of figures
ofset <- 100
options(scipen=100)

trajectory <- function(dt,fl){
  ## 前処理
  mem_xy <- c(abs(min(dt$x)),abs(min(dt$y)),max(dt$x),max(dt$y))
  traj_abs = max(mem_xy)
  for (i in 1:length(fl)) {
    x <- subset(dt$x,dt$id == i)
    y <- subset(dt$y,dt$id == i)
    
    # スケールを合わせたデータ
    fname <- sub(".csv","traj_abs.png",fl[i])
    png(fname, width=4*ppi, height=4 * 1.15 *ppi, res=ppi)
    plot(x,y,xlim = c(-traj_abs, traj_abs), ylim = c(-traj_abs, traj_abs),xlab = "x (mm)", ylab = "y (mm)", type = "p", col = 1, cex = 0.01, cex.axis = 0.8)
    dev.off()
    
    mem_xy.noabs <- c(abs(min(x)),abs(min(y)),max(x),max(y))
    traj_noabs <- max(mem_xy.noabs)
    # スケールを合わせてないデータ
    fname <- sub(".csv","traj_no_abs.png",fl[i])
    png(fname, width=4*ppi, height=4 * 1.15 *ppi, res=ppi)
    plot(x,y,xlim = c(-traj_noabs, traj_noabs), ylim = c(-traj_noabs, traj_noabs),xlab = "x (mm)", ylab = "y (mm)", type = "p", col = 1, cex = 0.01, cex.axis = 0.8)
    dev.off()
  }
}

trajectory(dt,fl)
