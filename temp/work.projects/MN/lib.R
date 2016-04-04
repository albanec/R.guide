GetData <- function (ticker, from.date, to.date=Sys.Date(), period="15min") {
	# функция загрузки тикера с Финам
	require(rusquant) 
	# загрузка данных
	# дата в формате "2015-01-01"
	cat("\t", "Download Source Data...", "\n")
	data <- getSymbols(ticker, from=from.date, to=to.date, period=period, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	return(data)
}
#
#
SaveXTStoCSV <- function (data, name, period=FALSE, tframe=FALSE) {
	# функция чтения XTS рядов из csv
	require(zoo)
	# функция сохранения .csv с нужным именем
	filename <- name
	if (period != FALSE) {
		filename <- paste(name, period, sep=".")
	}
	if (tframe != FALSE) {
		filename <- paste(filename, tframe, sep=".")
	}
	filename <- paste(filename, "csv", sep=".")
	write.zoo(data, file=filename, sep=",")
	cat("Save OK : ", "\t", filename, "\n")
}
ReadCSVtoXTS <- function (name, period=FALSE, tframe=FALSE) {
	# функция записи XTS рядов в файл
	require(xts)
	# функция сохранения .csv с нужным именем
	filename <- name
	if (period != FALSE) {
		filename <- paste(name, period, sep=".")
	}
	if (tframe != FALSE) {
		filename <- paste(filename, tframe, sep=".")
	}
	filename <- paste(filename, "csv", sep=".")
	data <- read.csv(file=filename)
	data <- xts(data[,-1], order.by=as.POSIXct(data$Index))
	cat("Read OK : ", "\t", filename, "\n")
	return(data)
}
#
#get.TickerList.data(TickerList="TickerList", from.date=Sys.Date()-10, to.date=Sys.Date(), period="15min", maxretryattempts=5, TimeCheck=FALSE)
#script 
GetData.TickerList <- function (TickerList="TickerList.csv", from.date, to.date, period, maxretryattempts=5, description=FALSE) {
	# функция загрузки листа котировок за период from/to.date 
		# на выходе - .csv файлы
		# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период (время вида "5 min"))
		# TickerList - файл, сожержащий список нужных тикеров 
		# description - добавить описание к названию файла
	require(rusquant)
	cat("Info: Current work.dir:", getwd())	
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F) 
	StockData <- new.env() #Make a new environment for quantmod to store data in
	data.name.list <- StocksNameList(TickerList=TickerList, description)
	nstocks <- nrow(data.name.list) #The number of stocks to download
	nperiod <- length(period)
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	for (i in 1:nstocks){
		# цикл загрузки с max количеством попыток
		for(t in 1:maxretryattempts){
			cat( "(", i , "/" , nstocks, ")", "Downloading: ", StocksList[i,1] , "\t Attempt: ", t , "/", maxretryattempts,"\n")
			data <- GetData(ticker=StocksList[i,1], from.date=from.date, to.date=to.date, period=period.min)
			if (exists("data")) {
				cat( "(", i , "/" , nstocks, ")", "Downloading ", StocksList[i,1] , "\t complete", "\n")
  				break
			}
		}
		data <- na.omit(data)
   		data$SR <- Delt(data$Close, type="arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type="log")
		data$LR[1] <- 0
   		data.name <- as.character(data.name.list[i])
   		SaveXTStoCSV(data=data, name=data.name, period=period.min)
	}
}
#
StocksNameList <- function (TickerList, description=FALSE) {
	# вспомогательная
	# функция генерации листа имен тикеров (data.name.list)
		# на вход  - csv со списком необходимых тикеров
 	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F) 
	nstocks <- nrow(StocksList)
	FirstTime <- TRUE
	for (i in 1:nstocks){
		if (description!=FALSE) {
			data.name <- paste(StocksList[i,1], description, sep=".")	
		} else {
			data.name <- paste(StocksList[i,1])
		}
   		if (FirstTime==TRUE) {
   			data.name.list <- data.name
   			data.name.list <- data.name.list
   			FirstTime <- FALSE
   		} else {
   			data.name.list <- rbind(data.name.list, data.name)
   		}
	}
	return(data.name.list)
}
#
TimeExpand.data <- function(TickerList, FrameList, period, description=FALSE) {
	# функция выделения данных по tf и временному интервалу
		# выдает .csv
	# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# TickerList - файл, сожержащий список нужных тикеров 
	# FrameList - файл, сожержащий список нужных временных интервалов
		# даты должны быть записаны в виде '2014-12-01/2014-12-31'
	cat( "Start DataExpand by FrameList :", "\n")
	cat( "Generate DataNameList...", "\n")
	data.name.list <- StocksNameList(TickerList, description)
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	nframe <- nrow(FrameList)
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data <- ReadCSVtoXTS(name=data.name, period=period[1], tframe=FALSE)
		cat ("Expand...", "\t", data.name, "\n")
		for (n in 1:nframe) {
			# цикл time.frame'а
			cat ("Expand...", "\t", data.name, "for TimeFrame ", FrameList[n, 1], "\n")
			window <- FrameList[n, 1] 
			for (t in 1:nperiod) {
				# цикл периода
				p <- period[t]
				cat ("Expand...", "\t", data.name, "for Period ", p, "\n")
				if (p=="5min") {
					p <- "mins"
					k <- 5
				}
				if (p=="10min") {
					p <- "mins"
					k <- 10
				}
				if (p=="15min") {
					p <- "mins"
					k <- 15
				}
				if (p=="30min") {
					p <- "mins"
					k <- 30
				}
				if (p=="1hour") {
					p <- "hours"
					k <- 1
				}
				if (p=="1day") {
					p <- "days"
					k <- 1
				}
				data <- data[window]
				ends <- endpoints(data, p, k)
				data <- data[ends]
				p <- paste(k, p, sep="")
				SaveXTStoCSV(data=data, name=data.name, period=p, tframe=n)
			}
		}
	}
	cat( "Expand StocksData...", "\t", "complete", "\n")
}
#
DataForPCA <- function (TickerList, type, description, period, approax=FALSE) {
	data <- MergeForMatrix(type, TickerList, description, period, approax)
	data <- BindToMatrix(data, load.csv=FALSE, SaveFile="Matrix.csv")
	return(data)
} 
#
MergeForMatrix <- function (price=="Close", TickerList, description=FALSE, period, approax==FALSE) {
	# функция объединения данных в один XTS и устранение NA значений 
		# NA можно убрать простым na.locf и аппроксимацией
	#
	# выгрузка данных по data.name.list
	cat( "Generate DataNameList...", "\n")
	data.name.list <- StocksNameList(TickerList, description) 
	nstocks <- nrow(data.name.list)
	period.min <- period[1]
	FirstTime <- TRUE
	#  чтение и объединение данных
	for (i in 1:nstocks) {
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data.name <- as.character(data.name.list[i])
		data <- ReadCSVtoXTS(name=data.name, period=period.min, tframe=FALSE)
		assign(data.name, data) 
		if (FirstTime==TRUE) {
			MergedData <- data.name
		} else {
			MergedData <- merge(MergedData, data.name)
		}
	}	
	cat("Merging StocksData:", "\t", "complete", "\n")
	# нормализация NA-значений
	if (approax==FALSE) {
		# нормализация без аппроксимации (с пом-ю na.locf)
		cat( "Normalize StocksData...", "\t", "without approx", "\n") 
		MergedData <- na.locf(MergedData)
	} else	{
		# аппроксимация NA
		cat( "Normalize StocksData...", "\t", "with approx", "\n") 
		MergedData <- na.approax(MergedData)
	}
	MergedData <- na.omit(MergedData)
	}
	cat( "Save Data...", "\n") 
	SaveXTStoCSV(data=MergedData, name="MergedData")
	return (MergedData)
}
#
BindToMatrix <- function (data, load.csv=FALSE, SaveFile="Matrix.csv") {
	# функция преобразования объединенного xts в матрицу
	# на вход подается MergedData xts либо напрямую, либо через чтение .csv
	if (load.csv==TRUE) {
		data <- ReadCSVtoXTS(name="MergedData")
	}
	#преобразование в матрицу 
	data <- as.matrix(data, nrow=nrow(data), ncol=ncol(data))
	write.table(data, file=SaveFile, sep=",")
	return (data)
}
#

