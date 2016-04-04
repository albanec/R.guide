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
approax <- FALSE
#
setwd(work.dir)
# загрузка исходников данных
GetData.TickerList(TickerList, from.date, to.date, period)
# расширение данных 
TimeExpand.data(TickerList, FrameList, period, description)
# конвертирование в матрицу
data.matrix <- DataForPCA (TickerList, description, period, approax)
# вычисление PC
p <- princomp(data.matrix)
loadings = p$loadings[]

# 



# look at the first 4 principal components  
components = loadings[,1:4]
 
# normalize all selected components to have total weight = 1
components = components / repRow(colSums(abs(components)), len(tickers))
 
# note that first component is market, and all components are orthogonal i.e. not correlated to market
market = ret[1:250,] %*% rep(1/n,n)
temp = cbind(market, ret[1:250,] %*% components)
    colnames(temp)[1] = 'Market'   
     
round(cor(temp, use='complete.obs',method='pearson'),2)
 
# the variance of each component 