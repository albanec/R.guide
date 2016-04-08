#
library("rusquant")
requare(lib.R)
#
work.dir <- ""
TickerList <- "TickerList.csv"
FrameList <- "TickerList.csv"
from.date <- "2014-01-01" 
to.date <- "2016-01-01" 
description <- ""
period <- c("5min", "5min")
approx <- FALSE
#
setwd(work.dir)
# загрузка исходников данных
GetData.TickerList(TickerList, from.date, to.date, period)
# расширение данных 
TimeExpand.data(TickerList, FrameList, period, description=FALSE)
# генерация xts для pca (за один TF) 
data.matrix <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price, out.name)
# генерация xts для pca (по группам)
DataPrepareForGroupPCA <- function (GroupList, FrameList, description, period=c("5min", "15min"),  approx, price) {
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nframe <- nrow(FrameList)
	tframe <- seq(1, nframe)
	nperiod <- length(period)
	for (i in 1:nperiod) {
		for (t in 1:nframe) {
			out.name <- paste(".", GroupList, ".", period[i], ".", tframe[t], sep="") 
			data.matrix <- DataPrepareForPCA (TickerList=GroupList, description, period[i], tframe[t], approx, price, out.name)		
			out.name <- as.character(paste("data.matrix", GroupList, ".", period[i], ".", tframe[t], sep="") )	 
			# вычисление PC			
			cat ("PCA start:", "\t", GroupList, "\n")
			p <- princomp(data.matrix)
			loadings <- p$loadings[]
			# выбираем первые 10 (основное !!! )
			components <- loadings[,1:10] 
			# normalize all selected components to have total weight = 1
			components <- components / rep.row(colSums(abs(components)), ncol(data.matrix))
		}	
	} 
} 
# вычисление PC
p <- princomp(data.matrix)
# веса
loadings <- p$loadings[]
# выбираем первые 10 (основное !!! )
components <- loadings[,1:10] 
# normalize all selected components to have total weight = 1
components <- components / rep.row(colSums(abs(components)), ncol(data.matrix))

# note that first component is market, and all components are orthogonal i.e. not correlated to market
n <- ncol(data.matrix)
market <- data.matrix %*% rep(1/n,n)
temp <- cbind(market, data.matrix %*% components)
colnames(temp)[1] = 'Market'   
# проверка на корреляцию
round(cor(temp, use='complete.obs',method='pearson'),2)
# the variance of each component is decreasing
round(100*sd(temp,na.rm=T),2)
#*****************************************************************
# исследование PC на стационарность (DF-тест)
#******************************************************************     
# считаем equity
m <- 1 + (-data.matrix %*% components)
equity <- apply(m, 2,  cumprod)
equity <- as.xts(equity, index(data.matrix))
# тест на стационарность
for (i in 1:10) {
	a <- adf.test(as.numeric(equity[, i]))
}
#*****************************************************************
#Графики
#*****************************************************************




