GetData <- function (ticker, from.date, to.date=Sys.Date(), period="15min") {
	require(rusquant) 
	# загрузка данных
	# дата в формате "2015-01-01"
	print("Download Source Data...")
	data <- getSymbols(ticker, from=from.date, to=to.date, period=period, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	return(data)
}
#
StocksNameList <- function (work.dir, TickerList, description=FALSE) {
	setwd(work.dir)
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
SaveXTStoCSV <- function (data, name, period=FALSE, tframe=FALSE) {
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
	cat( "Save OK : ", "\t", filename, "\n")
}
ReadCSVtoXTS <- function (name, period=FALSE, tframe=FALSE) {
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
	return(data)
}
#
TimeCheck.data <- function (data.name.list, TickerList=0, description=FALSE, work.dir=FALSE, period) {
	# функция нормализации набора xts по времени и запись данных в .csv
	cat( "Normalize Data by Date", "\n")
	if (data.name.list==FALSE) {
		data.name.list <- StocksNameList(work.dir, TickerList, description) 
		n <- nrow(data.name.list)
		period.min <- period[1]
		for (i in 1:n) {
			data.name <- as.character(data.name.list[i])
			data <- ReadCSVtoXTS(name=data.name, period=period.min, tframe=FALSE)
			assign(data.name, data) 	
		}	
	} 
	nstocks <- nrow(data.name.list)
	n <- rep(NA, nstocks)
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		data <- get(data.name)
		n[i] <- nrow(data)
	}
	n <- which.min(n)
	nrow.min.name <- as.character(data.name.list[n])
	cat( "MIN Date objects: ", "\t", nrow.min.name, "\n")
	cat( "TimeCheck...", "\n")	
	# временной ряз из nrow.min файла
	time.min <- time(nrow.min.name)
	for (i in 1:nstocks) {
		if (i != n) {
			data.name <- as.character(data.name.list[i])
			data <- get(data.name)
			data <- data[which(time(data)==time.min)]
			# запись в файл
			SaveXTStoCSV(data=data, name=data.name)
			cat( "Calculation TimeCheck:", data.name, "\n")
		} else {
			data.name <- as.character(data.name.list[i])
			data <- get(data.name)
			# запись в файл
			SaveXTStoCSV(data=data, name=data.name)
			cat( "Calculation TimeCheck:", data.name, "\n")
		}		
	}
	cat( "Calculation TimeCheck:", "\t All Completed ", "\n")	
}

# параметры выгрузки данных
TickerList <- "TickerList.csv"  # CSV с листом тикеров
FrameList <- "FrameList.csv"
startDate = as.Date("2005-01-13") #S 
maxretryattempts <- 5 # попытки загрузки на тикер

#get.TickerList.data(work.dir="/home/evgeni/temp/R/", TickerList="TickerList", from.date=Sys.Date()-10, to.date=Sys.Date(), period="15min", maxretryattempts=5, TimeCheck=FALSE)
#script 
GetData.TickerList <- function (work.dir, TickerList="TickerList.csv", from.date, to.date, period, maxretryattempts=5, 
									TimeCheck=FALSE, description="source",
									TimeExpand=FALSE, FrameList="FrameList") {
	require(rusquant)
	# функция загрузки листа котировок за период 
		# на выходе - .csv файлы
	# work.dir - рабочая папака, в которой работает скрипт
	# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# TickerList - файл, сожержащий список нужных тикеров 
	# description - добавить описание к файлу
	setwd(work.dir)
	print(paste("Current work.dir:", getwd()))	
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F) 
	StockData <- new.env() #Make a new environment for quantmod to store data in
	data.name.list <- StocksNameList(work.dir, TickerList=TickerList, description)
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
  				data <- Cl(data)
   				break
			}
		}
		data <- na.omit(data)
   		data$SR <- Delt(data$Close, type="arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type="log")
		data$LR[1] <- 0
   		data.name <- as.character(data.name.list[i])
   		assign(data.name, data)
	}
	# нормализации по time & сохранение
	if (TimeCheck == TRUE) {
		# нормализуем и сохраняем
		TimeCheck.data(data.name.list)
	} else {
		# просто сохраняем
		cat( "Save Data to .csv:", "\n")
		for (i in 1:nstocks) {
			data.name <- as.character(data.name.list[i])
			data <- get(data.name)
			SaveXTStoCSV(data=data, name=data.name, period=period.min)
		}
	}	
	if (TimeExpand==TRUE) {
		for (i in 1:nstocks) {
			data.name <- as.character(data.name.list[i])
			data <- get(data.name)
			# увеличиваем выборку и сохраняем
			TimeExpand.data(data.name.list=data.name.list, FrameList, period)
		}
	}
	return(data.name.list)
}
#
TimeExpand.data <- function(work.dir=0, data.name.list, FrameList="FrameList", period, TickerList=0, description="source") {
	# функция выделения данных по tf и временному интервалу
		# выдает .csv
	# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# TickerList - файл, сожержащий список нужных тикеров 
	# FrameList - файл, сожержащий список нужных временных интервалов
		# даты должны быть записаны в виде '2014-12-01/2014-12-31'
	cat( "DataExpand by FrameList :", "\n")
	if (data.name.list==FALSE) {
		setwd(work.dir)
		data.name.list <- StocksNameList(work.dir, TickerList, description)
		n <- nrow(data.name.list)
		period.min <- period[1]
		for (i in 1:n) {
			data.name <- as.character(data.name.list[i])
			data <- ReadCSVtoXTS(name=data.name, period=period[1], tframe=FALSE)
			assign(data.name, data) 	
		}	
	}
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	nframe <- nrow(FrameList[,1])
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		data <- get(data.name)
		for (t in 1:nframe) {
			window <- FrameList[, t] 
			for (f in 1:nperiod) {
				p <- period[f]
				data <- data[window]
				data <- to.period(data, period=p, indexAt="startof")
				SaveXTStoCSV(data=data, name=data.name, period=p, tframe=window)
			}
		}
	}
}
dates <- seq.POSIXt(from = from.date, to = to.date, by = "15 min")
data <- merge(y, dates)