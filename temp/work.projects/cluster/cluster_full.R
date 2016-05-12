file.path="/home/evgeni/Templates/temp/t4/11.csv"
data <- GEN_AllPreparationLabsFile(file.path=file.path, var.list = c(26, 27, 28), profit=3, draw=9, m=12, q.hi=0.5)
data$profit <- NULL
data$draw <- NULL
clustPar.data <- CLU_CalcClusterParameters(data)
CLU_ChartSSFrame(ss.df = clustPar.data[1], n.opt = clustPar.data[[2]])
clustFull.data <- CLU_CalcCluster(data, clustPar.data[[2]])
clustFull.data