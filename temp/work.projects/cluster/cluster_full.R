file.path="/home/evgeni/Templates/temp/t4/12.csv"
data <- GEN_AllPreparationLabsFile(file.path=file.path, var.list = c(26, 27, 28), profit=3, draw=9, m=12, q.hi=0.6, 
									hi = TRUE)
data$profit <- NULL
data$draw <- NULL
clustPar.data <- CLU_CalcKmean.Parameters(data, iter.max = 100, plusplus = FALSE, test.range = 30)
CLU_PlotKmean.SS(ss.df = clustPar.data[1], n.opt = clustPar.data[[2]])
clustFull.data <- CLU_CalcKmean(data, clustPar.data[[2]], plusplus = FALSE, var.digits = 3)
clustFull.data[2]
CLU_PlotKmean.Clusters(data.list=clustFull.data, cluster.color = FALSE, dimension = "3d", 
                       plot.title = "ClustersPlot", xaxis.name = "FastMA", yaxis.name = "SlowMA", 
                       zaxis.name = "PER", 
                       point.size = 4, point.opacity = 1, point.line.width = 0.7, point.line.opacity = 0.5,
                       center.size = 20, center.color = "black") 
