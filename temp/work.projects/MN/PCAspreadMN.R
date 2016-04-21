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
source("libmn.R")
#
setwd(work.dir)
# загрузка исходников данных
GetTickerListData.toCSV(ticker.list, from.date, to.date, period)
# расширение данных 
TimeExpand.data(ticker.list, frame.list, period, description=FALSE)
# генерация xts для pca (за один TF) 
	#data <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price)
# генерация xts для pca (по фреймам и периодам)
ExpandDataPrepareForPCA.toSCV(ticker.list, frame.list, description, period,  approx, price)
# вычисление PCA
PCAcompute(TickerList, period, tframe, KGfactor=TRUE) 


