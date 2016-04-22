#
library("rusquant")
library("tseries")
#
work.dir <- ""
ticker.list <- "TickerList.csv"
frame.list <- "FrameList.csv"
from.date <- "2015-01-01" 
to.date <- "2016-04-19" 
description <- FALSE
period <- c("5min", "15min")
approx <- FALSE
price <- "LR"
#
setwd(work.dir)
source("libGeneric.R")
source("libPCA.R")
# загрузка исходников данных
GEN_GetDataTickerListCSV(ticker.list, from.date, to.date, period)
# расширение данных 
GEN_TimeExpandData(ticker.list, frame.list, period, description=FALSE)
# генерация xts для pca (за один TF) 
	#data <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price)
# генерация xts для pca (по фреймам и периодам)
PCA_ExpandData(ticker.list, frame.list, description, period,  approx, price)
# вычисление PCA
data <- GEN_SaveXTStoCSV(name = MergedData.TickerList.LR, period="5min", tframe=1)
pca.equity <- PCA_ComputePCA(data, period="5min", tframe=1, price = "LR")
n.PC <- PCA_DFtestPCA (pca.equity)
#






