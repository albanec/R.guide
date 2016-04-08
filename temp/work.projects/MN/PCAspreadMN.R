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
data.matrix <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price)
# генерация xts для pca (по группам)
DataPrepareForGroupPCA <- function (GroupList, FrameList, description, period=c("5min", "15min"),  approx, price) {
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nframe <- nrow(FrameList)
	tframe <- seq(1, nframe)
	nperiod <- length(period)
	for (i in 1:nperiod) {
		for (t in 1:nframe) {
			data.matrix <- DataPrepareForPCA (TickerList=GroupList, description=description, period=period[i], tframe=tframe[t], approx=approx, price=price)		
			#out.name <- as.character(paste("DataMatrix", GroupList, ".", period[i], ".", tframe[t], sep="") )	 
			#assign(out.name, data.matrix)
			#return(out.name)	
		}	
	} 
} 
PCAcompute <- function(GroupList, period, tframe) {
	# выгрузка данных
	filename <- paste("MergedData.", GroupList, sep="")
	data.matrix <- ReadCSVtoXTS (name=filename, period, tframe)
	# вычисление PC			
	cat ("PCA start:", "\t", GroupList, "\n")
	pca.data <- princomp(data.matrix)
	loadings <- pca.data$loadings[]
	evec <- pca.dates$rotation[] #eigen vectors
	eval <- pca.dates$sdev^2 #eigen values
	inv.evec <- solve(evec) #inverse of eigenvector
	pc.port <- inv.evec %*% t(ret)
	# Критерий  Кайзера-Гуттмана  рекомендует оставить для дальнейшего анализа только те главные компоненты,
	# собственные значения которых превышают среднее (собственных значений):
	#ev <- pca.data$sdev
	#ev <- which.max(ev>mean(ev))
	#cat("PCA numbers", ev)
	# выбираем первые 10 (основное !!! )
	components <- loadings[,1:10] 
	# normalize all selected components to have total weight = 1
	components <- components / rep.row(colSums(abs(components)), ncol(data.matrix))
	loadings.filename <- print("Loadings", Grouplist, period, tframe, "csv", sep=".")
	write.table(loadings, file=loadings.filename, sep=",")
	components.filename <- print("Components", Grouplist, period, tframe, "csv", sep=".")
	write.table(components, file=components.filename, sep=",")
	barplot(height=pca.data$sdev[1:10]/pca.data$sdev[1])
}

VisualizePCA <- 
# вычисление PC
p <- princomp(data.matrix)
# веса
loadings <- p$loadings[]
# выбираем первые 10 (основное !!! )
components <- loadings[] 
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
#
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




