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
data <- DataPrepareForPCA (TickerList, description, period, tframe, approx, price)
# генерация xts для pca (по группам)
DataPrepareForGroupPCA <- function (GroupList, FrameList, description, period=c("5min", "15min"),  approx, price) {
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nframe <- nrow(FrameList)
	tframe <- seq(1, nframe)
	nperiod <- length(period)
	for (i in 1:nperiod) {
		for (t in 1:nframe) {
			data <- DataPrepareForPCA (TickerList=GroupList, description=description, period=period[i], tframe=tframe[t], approx=approx, price=price)		
			#out.name <- as.character(paste("DataMatrix", GroupList, ".", period[i], ".", tframe[t], sep="") )	 
			#assign(out.name, data)
			#return(out.name)	
		}	
	} 
} 
PCAcompute <- function(GroupList, period, tframe) {
	# выгрузка данных
	filename <- paste("MergedData.", GroupList, sep="")
	data <- ReadCSVtoXTS (name=filename, period, tframe)
	# вычисление PC			
	cat ("PCA start:", "\t", GroupList, "\n")
	pca.data <- princomp(data)
	# нагрузки
	loadings <- pca.data$loadings[]
	# ???
	evec <- pca.dates$rotation[] #eigen vectors
	eval <- pca.dates$sdev^2 #eigen values
	inv.evec <- solve(evec) #inverse of eigenvector
	pc.port <- inv.evec %*% t(ret)
	# ???
	# Критерий  Кайзера-Гуттмана  рекомендует оставить для дальнейшего анализа только те главные компоненты,
	# собственные значения которых превышают среднее (собственных значений):
	#ev <- pca.data$sdev
	#ev <- which.max(ev>mean(ev))
	#cat("PCA numbers", ev)
	# выбираем не больше первых 10 (основное !!! )
	n.PC <- ncol(loadings)
	if (n.PC > 10) {
		n.PC <- 10
	} 
	components <- loadings[,1:n.PC]
	# нормализуем компоненты - в сумме должны дать 1 (получаем веса)
	n <- ncol(data)
	components <- components / rep.row(colSums(abs(components)), n)
	# note that first component is market, and all components are orthogonal i.e. not correlated to market
	market <- data %*% rep(1/n,n)
	temp <- cbind(market, data %*% components)
    colnames(temp)[1] <- 'Market' 
    temp <- as.xts(temp, index(data))
    # корреляция между PC (по Пирсону)
	cor.table <- round(cor(temp, use='complete.obs',method='pearson'),2)
 	# дисперсия компонент
	vol.table <- round(100*sd(temp,na.rm=T),2)
	# проверка на стационарность получившихся PC
	if (DF.test==TRUE) {
		m <- 1 + (-data %*% components)
		equity <- apply(m, 2,  cumprod)
		equity <- as.xts(equity, index(data))
 		# test for stationarity ( mean-reversion )
		df.value <- rep(NA, n.PC)
		for (i in 1:n.PC) {
			df.value[i] <- adf.test(as.numeric(equity[, i]))$p.value			
		}
		statPC <- which.min(df.value)
		cat("Best DF-test result...", "\t", df.value[statPC], "\t", "PC:", statPC, "\n")
	} 
		# сохраняем нагрузки 
	loadings.filename <- print("Loadings", Grouplist, period, tframe, "csv", sep=".")
	write.table(loadings, file=loadings.filename, sep=",")
	#
	components.filename <- print("Components", Grouplist, period, tframe, "csv", sep=".")
	write.table(components, file=components.filename, sep=",")
	barplot(height=pca.data$sdev[1:10]/pca.data$sdev[1])
}

