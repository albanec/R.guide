get.data <- function (ticker, from.date, to.date=Sys.Date(), frame="15min") {
	require(rusquant) 
	# загрузка данных
	# дата в формате "2015-01-01"
	print("Download Source Data")
	data <- getSymbols(ticker, from=from.date, to=to.date, period=frame, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	
	return(data)
}

# параметры выгрузки данных
tickerlist <- "sp500.csv"  # CSV с листом тикеров
savefilename <- "stockdata.RData" # файл, в котором будут лежать конечные данные
startDate = as.Date("2005-01-13") #S 
maxretryattempts <- 5 # попытки загрузки на тикер

#script 
get.TickerList.data <- function (work.dir, TickerList="TickerList", from.date, to.date, frame, maxretryattempts=5) {
	# функция загрузки листа котировок за период 
		# на выходе - .csv файлы
	# work.dir - рабочая папака, в которой работает скрипт
	# frame - вектор, содержащий нужные tf (в порядке возрастания) или один tf
	# TickerList - файл, сожержащий список нужных тикеров 
	require(rusquant)
	setwd(work.dir)
	print(paste("Current work.dir:", getwd()))	
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F)
	StockData <- new.env() #Make a new environment for quantmod to store data in
	nstocks <- length(StocksList[,1]) #The number of stocks to download
	nframe <- length(frame)
	# если фреймы - вектор, то расположены по-возвастанию
	frame.min <- frame[1]
	FirstTime <- TRUE 
	# загрузка данных
	for (i in 1:nstocks){
		for(t in 1:maxretryattempts){
			cat( "(", i , "/" , nstocks, ")", "Downloading ", StocksList[i,1] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
			data <- get.data(ticker=StocksList[i,1], from.date=from.date, to.date=to.date, frame=frame[1]])
			if (exists("data")) {
				cat( "(", i , "/" , nstocks, ")", "Downloading", StocksList[i,1] , "\t\t complete", "\n")
  				data <- Cl(data)
   				break
			}
   		data$SR <- Delt(data$Close, type="arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type="log")
		data$LR[1] <- 0
   		data.name <- paste(StocksList[i,1], frame.min, "data", sep=".")
   		if (FirstTime==TRUE) {
   			data.name.list <- data.name
   			FirstTime <- FALSE
   		} else {
   			data.name.list <- rbin(data.name.list, data.name)
   		}
   		assign(data.name, data)
	}
	# нормализации по time 
	if (time.check == TRUE) {
		for (i in 1:nstocks) {
			data.name.temp <- get(data.name.list[i])
			n <- rep(NA, nstocks)
			n[i] <- nrow(data.temp)
		}
		n <- wich.min(n)] 
		nrow.min.name <- data.name.list[n] 
		cat( "MIN time objects", "\t\t", nrow.min.name, "\n")
		cat( "Calculation data objects", "\t\t ...........", "\n")
		time.min <- time(nrow.min.name)
		for (i in 1:nstocks) {
			if (i != n) {
				data.temp <- get(data.name.list[i])
				data.temp <- data.temp[which(time(data.temp)==time.min)]
				assign(data.name.list[i], data.temp)		
				filename <- paste(data.name.list[i], "csv", sep=".")
				write.csv(data.temp, file=filename, sep="\t")
			} else {
				data.temp <- get(data.name.list[i])
				filename <- paste(data.name.list[i], "csv", sep=".")
				write.csv(data.temp, file=filename, sep="\t")	
			}		
		}
	} else {
		for (i in 1:nstocks) {
			data.temp <- get(data.name.list[i])
			filename <- paste(data.name.list[i], "csv", sep=".")
			write.csv(data.temp, file=filename, sep="\t")
		}
	}
}

get.TimeExpand.data <- function(work.dir, TickerList="TickerList", DateList="DateList", frame) {
	# функция выделения данных по tf и временному интервалу
		# выдает .csv
	# frame - вектор, содержащий нужные tf (в порядке возрастания)
	# TickerList - файл, сожержащий список нужных тикеров 
	# DateList - файл, сожержащий список нужных временных интервалов
		# даты должны быть записаны в виде '2014-12-01/2014-12-31'
	setwd(work.dir)
	paste("ExpandData to DateList")
	StocksList <- read.csv(TickerList, header = F, stringsAsFactors = F)
	DateList <- read.csv(DateList, header = F, stringsAsFactors = F)
	nstocks <- length(StocksList[,1])
	ntime <- length(DateList[,1])
	nframe <- length(frame)
	for (i in 1:nstocks) {
		data.name <- paste(StocksList[i,1], frame.min, "data", sep=".")
		filename <- paste(data.name, "csv", sep=".")
		temp.data <- read.scv(file=filename, sep="\t")
		for (t in 1:ntime) {
			window <- DateList[t] 
			for (f in 1:nframe) {
				p <- frame[f]
				temp.data2 <- temp.data[window]
				temp.data2 <- to.period(temp.data2, period=p, indexAt="startof")
				filename <- paste(data.name, window, p, "csv", sep=".")
				write.csv(data.temp2, file=filename, sep="\t")
			}
		}
	}
}
		