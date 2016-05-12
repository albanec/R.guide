file.path1.1="/home/evgeni/Templates/temp/t3/HL_cls_sma/11_13.csv"
file.path1.2="/home/evgeni/Templates/temp/t3/HL_cls_sma/11_13.csv"
file.path1.2="/home/evgeni/Templates/temp/t3/HL_cls_sma/14.csv"
file.path1.3="/home/evgeni/Templates/temp/t3/HL_cls_sma/15.csv"
file.path1.4="/home/evgeni/Templates/temp/t3/HL_cls_sma/16.csv"
file.path2.1="/home/evgeni/Templates/temp/t3/HL2_cls_sma/11_13.csv"
file.path2.2="/home/evgeni/Templates/temp/t3/HL2_cls_sma/14.csv"
file.path2.3="/home/evgeni/Templates/temp/t3/HL2_cls_sma/15.csv"
file.path2.4="/home/evgeni/Templates/temp/t3/HL2_cls_sma/16.csv"
file.path3.1="/home/evgeni/Templates/temp/t3/cls_cls_sma/11-13.csv"
file.path3.1="/home/evgeni/Templates/temp/t3/cls_cls_sma/11_13.csv"
file.path3.2="/home/evgeni/Templates/temp/t3/cls_cls_sma/14.csv"
file.path3.3="/home/evgeni/Templates/temp/t3/cls_cls_sma/15.csv"
file.path3.4="/home/evgeni/Templates/temp/t3/cls_cls_sma/16.csv"
#
data1.11_13 <- GEN_AllPreparationLabsFile(file.path=file.path1.1, var.list = c(26, 27, 28), profit=2, draw=8, m=26, q.hi=0.5)
data1.14 <- GEN_AllPreparationLabsFile(file.path=file.path1.2, var.list = c(26, 27, 28), profit=2, draw=8, m=9, q.hi=0.5)
data1.15 <- GEN_AllPreparationLabsFile(file.path=file.path1.3, var.list = c(26, 27, 28), profit=2, draw=8, m=12, q.hi=0.5)
data1.16 <- GEN_AllPreparationLabsFile(file.path=file.path1.4, var.list = c(26, 27, 28), profit=2, draw=8, m=4, q.hi=0.5)
#
data2.11_13 <- GEN_AllPreparationLabsFile(file.path=file.path2.1, var.list = c(26, 27, 28), profit=2, draw=8, m=26, q.hi=0.5)
data2.14 <- GEN_AllPreparationLabsFile(file.path=file.path2.2, var.list = c(26, 27, 28), profit=2, draw=8, m=9, q.hi=0.5)
data2.15 <- GEN_AllPreparationLabsFile(file.path=file.path2.3, var.list = c(26, 27, 28), profit=2, draw=8, m=12, q.hi=0.5)
data2.16 <- GEN_AllPreparationLabsFile(file.path=file.path2.4, var.list = c(26, 27, 28), profit=2, draw=8, m=4, q.hi=0.5)
#
data3.11_13 <- GEN_AllPreparationLabsFile(file.path=file.path3.1, var.list = c(26, 27, 28), profit=2, draw=8, m=26, q.hi=0.5)
data3.14 <- GEN_AllPreparationLabsFile(file.path=file.path3.2, var.list = c(26, 27, 28), profit=2, draw=8, m=9, q.hi=0.5)
data3.15 <- GEN_AllPreparationLabsFile(file.path=file.path3.3, var.list = c(26, 27, 28), profit=2, draw=8, m=12, q.hi=0.5)
data3.16 <- GEN_AllPreparationLabsFile(file.path=file.path3.4, var.list = c(26, 27, 28), profit=2, draw=8, m=4, q.hi=0.5)
#
temp.11 <-data1.11_13
temp.12 <-data1.14 
temp13 <-data1.15 
temp14 <-data1.16 
temp21 <-data2.11_13 
temp22 <-data2.14 
temp23 <-data2.15 
temp24 <-data2.16 
temp31 <-data3.11_13 
temp32 <-data3.14 
temp33 <-data3.15 
temp34 <-data3.16 
#
data1.11_13$profit <- NULL
data1.14$profit <- NULL 
data1.15$profit <- NULL 
data1.16$profit <- NULL
data2.11_13$profit <- NULL 
data2.14$profit <- NULL 
data2.15$profit <- NULL 
data2.16$profit <- NULL 
data3.11_13$profit <- NULL 
data3.14$profit <- NULL 
data3.15$profit <- NULL 
data3.16$profit <- NULL 
data1.11_13$draw <- NULL
data1.14$draw <- NULL 
data1.15$draw <- NULL 
data1.16$draw <- NULL
data2.11_13$draw <- NULL 
data2.14$draw <- NULL 
data2.15$draw <- NULL 
data2.16$draw <- NULL 
data3.11_13$draw <- NULL 
data3.14$draw <- NULL 
data3.15$draw <- NULL 
data3.16$draw <- NULL 
#
clustPar.data1.11_13 <- CLU_CalcClusterParameters(data1.11_13)
CLU_ChartSSFrame(ss.df = clustPar.data1.11_13[1], n.opt = clustPar.data1.11_13[[2]])
clustFull.data1.11_13 <- CLU_CalcCluster(data1.11_13, clustPar.data1.11_13[[2]])
#
clustPar.data1.14 <- CLU_CalcClusterParameters(data1.14)
CLU_ChartSSFrame(ss.df = clustPar.data1.14[1], n.opt = clustPar.data1.14[[2]])
clustFull.data1.14 <- CLU_CalcCluster(data1.14, clustPar.data1.14[[2]])
#
clustPar.data1.15 <- CLU_CalcClusterParameters(data1.15)
CLU_ChartSSFrame(ss.df = clustPar.data1.15[1], n.opt = clustPar.data1.15[[2]])
clustFull.data1.15 <- CLU_CalcCluster(data1.15, clustPar.data1.15[[2]])
#
clustPar.data1.16 <- CLU_CalcClusterParameters(data1.16)
CLU_ChartSSFrame(ss.df = clustPar.data1.16[1], n.opt = clustPar.data1.16[[2]])
clustFull.data1.16 <- CLU_CalcCluster(data1.16, clustPar.data1.16[[2]])
#
clustPar.data2.11_13 <- CLU_CalcClusterParameters(data2.11_13)
CLU_ChartSSFrame(ss.df = clustPar.data2.11_13[1], n.opt = clustPar.data2.11_13[[2]])
clustFull.data2.11_13 <- CLU_CalcCluster(data2.11_13, clustPar.data2.11_13[[2]])
#
clustPar.data2.14 <- CLU_CalcClusterParameters(data2.14)
CLU_ChartSSFrame(ss.df = clustPar.data2.14[1], n.opt = clustPar.data2.14[[2]])
clustFull.data2.14 <- CLU_CalcCluster(data2.14, clustPar.data2.14[[2]])
#
clustPar.data2.15 <- CLU_CalcClusterParameters(data2.15)
CLU_ChartSSFrame(ss.df = clustPar.data2.15[1], n.opt = clustPar.data2.15[[2]])
clustFull.data2.15 <- CLU_CalcCluster(data2.15, clustPar.data2.15[[2]])
#
clustPar.data2.16 <- CLU_CalcClusterParameters(data2.16)
CLU_ChartSSFrame(ss.df = clustPar.data2.16[1], n.opt = clustPar.data2.16[[2]])
clustFull.data2.16 <- CLU_CalcCluster(data2.16, clustPar.data2.16[[2]])
clustPar.data3.11_13 <- CLU_CalcClusterParameters(data3.11_13)
CLU_ChartSSFrame(ss.df = clustPar.data3.11_13[1], n.opt = clustPar.data3.11_13[[2]])
clustFull.data3.11_13 <- CLU_CalcCluster(data3.11_13, clustPar.data3.11_13[[2]])
#
clustPar.data3.14 <- CLU_CalcClusterParameters(data3.14)
CLU_ChartSSFrame(ss.df = clustPar.data3.14[1], n.opt = clustPar.data3.14[[2]])
clustFull.data3.14 <- CLU_CalcCluster(data3.14, clustPar.data3.14[[2]])
#
clustPar.data3.15 <- CLU_CalcClusterParameters(data3.15)
CLU_ChartSSFrame(ss.df = clustPar.data3.15[1], n.opt = clustPar.data3.15[[2]])
clustFull.data3.15 <- CLU_CalcCluster(data3.15, clustPar.data3.15[[2]])
#
clustPar.data3.16 <- CLU_CalcClusterParameters(data3.16)
CLU_ChartSSFrame(ss.df = clustPar.data3.16[1], n.opt = clustPar.data3.16[[2]])
clustFull.data3.16 <- CLU_CalcCluster(data3.16, clustPar.data3.16[[2]])