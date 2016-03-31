get.data <- function (ticker, from.date, to.date=Sys.Date(), period="15min") {
	require(rusquant) 
	# загрузка данных
	# дата в формате "2015-01-01"
	print("Download Source Data...")
	data <- getSymbols(ticker, from=from.date, to=to.date, period=period, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	
	return(data)
}
#
stocks.name.list <- function (TickerList) {
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F)
	nstocks <- nrow(StocksList)
	FirstTime <- TRUE
	for (i in 1:nstocks){
		data.name <- paste(StocksList[i,1], "data", sep=".")
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
SaveCSV <- function (data, name, period=FALSE, tframe=FALSE) {
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
	write.zoo(data, file=filename)
}
ReadCSV <- function (name, period=FALSE, tframe=FALSE) {
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
	write.zoo(data, file=filename)
}
#
time.check <- function (data.name.list) {
	# функция нормализации набора xts по времени и запись данных в .csv
	nstocks <- nrow(data.name.list)
	n <- rep(NA, nstocks)
	for (i in 1:nstocks) {
		name.temp <- as.character(data.name.list[i])
		data.temp <- get(name.temp)
		n[i] <- nrow(data.temp)
	}
	n <- which.min(n)
	nrow.min.name <- as.character(data.name.list[[n]])
	cat( "MIN time objects: ", "\t\t", nrow.min.name, "\n")
	cat( "TimeCheck...", "\n")	
	time.min <- time(nrow.min.name)
	for (i in 1:nstocks) {
		if (i != n) {
			name.temp <- as.character(data.name.list[i])
			data.temp <- get(name.temp)
			data.temp <- data.temp[which(time(data.temp)==time.min)]
			SaveCSV(data=data.temp, name=name.temp)
			cat( "Calculation TimeCheck:", name.temp, "\n")
		} else {
			name.temp <- as.character(data.name.list[i])
			data.temp <- get(name.temp)
			SaveCSV(data=data.temp, name=name.temp)
			cat( "Calculation TimeCheck:", name.temp, "\n")
		}		
	}
	cat( "Calculation TimeCheck:", "\t\t All Completed ", "\n")	
}

# параметры выгрузки данных
TickerList <- "TickerList.csv"  # CSV с листом тикеров
FrameList <- "FrameList.csv"
startDate = as.Date("2005-01-13") #S 
maxretryattempts <- 5 # попытки загрузки на тикер

#get.TickerList.data(work.dir="/home/evgeni/temp/R/", TickerList="TickerList", from.date=Sys.Date()-10, to.date=Sys.Date(), period="15min", maxretryattempts=5, TimeCheck=FALSE)
#script 
get.TickerList.data <- function (work.dir, TickerList="TickerList", from.date, to.date, period, maxretryattempts=5, TimeCheck=FALSE) {
	# функция загрузки листа котировок за период 
		# на выходе - .csv файлы
	# work.dir - рабочая папака, в которой работает скрипт
	# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# TickerList - файл, сожержащий список нужных тикеров 
	require(rusquant)
	setwd(work.dir)
	print(paste("Current work.dir:", getwd()))	
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F)
	StockData <- new.env() #Make a new environment for quantmod to store data in
	nstocks <- nrow(StocksList) #The number of stocks to download
	nperiod <- length(period)
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	# загрузка данных
	for (i in 1:nstocks){
		# цикл загрузки с max количеством попыток
		for(t in 1:maxretryattempts){
			cat( "(", i , "/" , nstocks, ")", "Downloading: ", StocksList[i,1] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
			data <- get.data(ticker=StocksList[i,1], from.date=from.date, to.date=to.date, period=period.min)
			if (exists("data")) {
				cat( "(", i , "/" , nstocks, ")", "Downloading ", StocksList[i,1] , "\t\t complete", "\n")
  				data <- Cl(data)
   				break
			}
		}
		data <- na.omit(data)
   		data$SR <- Delt(data$Close, type="arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type="log")
		data$LR[1] <- 0
   		data.name <- paste(StocksList[i,1], "data", sep=".")
   		if (FirstTime==TRUE) {
   			data.name.list <- data.name
   			data.name.list <- data.name.list
   			FirstTime <- FALSE
   		} else {
   			data.name.list <- rbind(data.name.list, data.name)
   		}
   		assign(data.name, data)
	}
	# нормализации по time 
	if (TimeCheck == TRUE) {
		# нормализуем и сохраняем
		time.check(data.name.list)
	} else {
		# просто сохраняем
		for (i in 1:nstocks) {
			name.temp <- as.character(data.name.list[i])
			data.temp <- get(name.temp)
			SaveCSV(data=data.temp, name=name.temp, period=period.min)
		}
	}
	#return(data.name.list)
}
#
get.TimeExpand.data <- function(work.dir, TickerList="TickerList", FrameList="FrameList", period) {
	# функция выделения данных по tf и временному интервалу
		# выдает .csv
	# period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# TickerList - файл, сожержащий список нужных тикеров 
	# FrameList - файл, сожержащий список нужных временных интервалов
		# даты должны быть записаны в виде '2014-12-01/2014-12-31'
	setwd(work.dir)
	paste("ExpandData to FrameList")
	data.name.list <- stocks.name.list(TickerList)
	FrameList <- read.csv(FrameList, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	ntime <- nrow(FrameList[,1])
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- paste(data.name.list[i], "data", period.min, sep=".")
		filename <- paste(data.name, "csv", sep=".")
		temp.data <- as.xts(read.zoo(file=filename))
		for (t in 1:ntime) {
			window <- FrameList[t] 
			for (f in 1:nperiod) {
				p <- period[f]
				temp.data2 <- temp.data[window]
				temp.data2 <- to.period(temp.data2, period=p, indexAt="startof")
				SaveCSV(data=temp.data2, name=data.name, period=p, tframe=window)
			}
		}
	}
}
