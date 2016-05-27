file.path="/home/evgeni/Templates/temp/t4/15.csv"
data <- GEN_AllPreparationLabsFile(file.path=file.path, var.list = c(26, 27, 28, 23, 24, 25), profit = 3, draw=9, m=12, 
 								   q.hi = 0.6, q.low = FALSE, low = FALSE, hi = TRUE, one.scale=FALSE,  tslab = TRUE, 
 								   trend.filter = TRUE)
data$profit <- NULL
data$draw <- NULL
data$var0 <- (1/data$var4) + (1/data$var5) + (1/data$var6)
data$var4 <- NULL; data$var5 <- NULL; data$var6 <- NULL
clustPar.data <- CLU_CalcKmean.Parameters(data, iter.max = 100, plusplus = FALSE, test.range = 30)
CLU_PlotKmean.SS(ss.df = clustPar.data[1], n.opt = clustPar.data[[2]])
clustFull.data <- CLU_CalcKmean(data, clustPar.data[[2]], plusplus = FALSE, var.digits = 3)
clustFull.data[2]
CLU_PlotKmean.Clusters(data.list=clustFull.data, cluster.color = FALSE, dimension = "3d", 
                       plot.title = "ClustersPlot", xaxis.name = "FastMA", yaxis.name = "SlowMA", 
                       zaxis.name = "PER", 
                       point.size = 4, point.opacity = 1, point.line.width = 0.7, point.line.opacity = 0.5,
                       center.size = 20, center.color = "black") 
