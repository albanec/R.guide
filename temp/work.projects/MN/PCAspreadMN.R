#
library("rusquant")
requare(lib.R)
#
work.dir <- ""
TickerList <- "TickerList.csv"
FrameList <- "FrameList.csv"
from.date <- "2014-01-01" 
to.date <- "2016-01-01" 
description <- FALSE
period <- c("5min", "15min")
approx <- FALSE
price <- "SR"
#
setwd(work.dir)
# загрузка исходников данных
GetData.TickerList(TickerList, from.date, to.date, period)
# расширение данных 
TimeExpand.data(TickerList, FrameList, period, description=FALSE)
# генерация xts для pca (за один TF) 
	#data <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price)
# генерация xts для pca (по фреймам и периодам)
DataPrepareForGroupPCA(TickerList, FrameList, description, period,  approx, price)
# вычисление PCA
PCAcompute(TickerList, period, tframe, KGfactor=TRUE) 


