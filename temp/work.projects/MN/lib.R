rep.row<-function(x,n){
	# ----------
	# Общее описание:
	# 	функция создает матрицу из n строк вектора x
	# Входные данные:
	# 	x - вектор, который надо повторить 
	#	n - количество нужных строк
	# Выходные данные:
	#	матрица m, состоящая их n сток вектора x
	# ----------
	#
	m <- matrix(rep(x,each = n),nrow = n)
 	return(m)
}
#
rep.col<-function(x,n){
	# ----------
	# Общее описание:
	# 	функция создает матрицу из n столбцов вектора x
	# Входные данные:
	# 	x - вектор, который надо повторить 
	#	n - количество нужных столбцов
	# Выходные данные:
	#	матрица m, состоящая их n столбцов вектора x
	# ----------
	# 
	m <- matrix(rep(x,each = n), ncol = n, byrow = TRUE)
	return(m)
}
#
GetData <- function (ticker, from.date, to.date = Sys.Date(), period = "15min") {
	# ----------
	# Общее описание:
	# 	функция загрузки тикера с Финам + переименовывает столбцы
	# Входные данные:
	# 	ticker - нужный тикер
	#	from.date - дата-старт
	#	to.date - дата-стоп
	#	period - период свечей
	# Выходные данные:
	#	xts массив "data"
	# Зависимости:
		require(rusquant) 	
	# ----------
	#
	# загрузка данных
	# дата в формате "2015-01-01"
	cat("\t", "Download Source Data...", "\n")
	data <- getSymbols(ticker, from = from.date, to = to.date, period = period, src = "Finam", auto.assign = FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	return(data)
}
#
SaveXTStoCSV <- function (data, name, period = FALSE, tframe = FALSE) {
	# ----------
	# Общее описание:
	# 	функция записи XTS рядов в .csv файл 	
	# Входные данные:
	# 	data - нужный xts
	#	name - название файла
	#	period - указать в название период свечей
	#	tframe - указать в названии тайм-фрейм
	# Выходные данные:
	#	сохраненный .csv файл
	# Зависимости:
		require(zoo)
	# ----------	
	#
	filename <- name
	if (period !=  FALSE) {
		filename <- paste(filename, period, sep = ".")
	}
	if (tframe !=  FALSE) {
		filename <- paste(filename, tframe, sep = ".")
	}
	filename <- paste(filename, "csv", sep = ".")
	write.zoo(data, file = filename, sep = ",")
	cat("Save OK : ", "\t", filename, "\n")
}
#
ReadCSVtoXTS <- function (name, period = FALSE, tframe = FALSE) {
	# ----------
	# Общее описание:
	# 	функция чтения XTS рядов из .csv файлов
	# Входные данные:
	#	name - название файла
	#	period - указать в название период свечей
	#	tframe - указать в названии тайм-фрейм
	# Выходные данные:
	#	xts ряд, полученный из файла
	# Зависимости:
		require(xts)
	# ----------
	# 
	filename <- name
	if (period !=  FALSE) {
		filename <- paste(name, period, sep = ".")
	}
	if (tframe !=  FALSE) {
		filename <- paste(filename, tframe, sep = ".")
	}
	filename <- paste(filename, "csv", sep = ".")
	data <- read.csv(file = filename)
	data <- xts(data[,-1], order.by = as.POSIXct(data$Index))
	cat("Read OK : ", "\t", filename, "\n")
	return(data)
}
#
GetTickerListData.toCSV <- function (ticker.list = "TickerList.csv", from.date, to.date, period, maxretryattempts = 5, description = FALSE) {
	# ----------
	# Общее описание:
	# 	функция загрузки листа котировок за период from/to.date 
	# Входные данные:
	#	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период (время вида "5 min"))
	#	from.date - дата-старт
	#	to.date - дата-стоп
	#	ticker.list - файл, сожержащий список нужных тикеров
	# 	description - добавить описание к названию файла 
	# 	maxretryattempts - количество попыток загрузки каждого тикера
	# Выходные данные:
	#	.csv файлы
	# Зависимости:
		require(rusquant)
	# ----------
	#
	cat("Info: Current work.dir:", getwd())	
	stocks.list <- read.csv(ticker.list, header = F, stringsAsFactors = F) 
	stocks.data <- new.env() #Make a new environment for quantmod to store data in
	data.name.list <- StocksNameList(ticker.list = ticker.list, description)
	nstocks <- nrow(data.name.list) #The number of stocks to download
	nperiod <- length(period)
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	for (i in 1:nstocks){
		# цикл загрузки с max количеством попыток
		for(t in 1:maxretryattempts){
			cat( "(", i , "/" , nstocks, ")", "Downloading: ", stocks.list[i,1] , "\t Attempt: ", t , "/", maxretryattempts,"\n")
			data <- GetData(ticker = stocks.list[i,1], from.date = from.date, to.date = to.date, period = period.min)
			if (exists("data")) {
				cat( "(", i , "/" , nstocks, ")", "Downloading ", stocks.list[i,1] , "\t complete", "\n")
  				break
			}
		}
		data <- na.omit(data)
   		data$SR <- Delt(data$Close, type = "arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type = "log")
		data$LR[1] <- 0
   		data.name <- as.character(data.name.list[i])
   		SaveXTStoCSV(data = data, name = data.name, period = period.min)
	}
}
#
StocksNameList <- function (ticker.list, description = FALSE) {
	# ----------
	# Общее описание:
	# 	вспомогательная для GetTickerListData.toCSV()
	# 	функция генерации листа имен тикеров (data.name.list)
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров
	# 	description - добавить описание к названию файла 
	# Выходные данные:
	#	data.name.list
	# ----------
	# 
 	stocks.list <- read.csv(ticker.list, header = F, stringsAsFactors = F) 
	nstocks <- nrow(stocks.list)
	FirstTime <- TRUE
	for (i in 1:nstocks){
		if (description!= FALSE) {
			data.name <- paste(stocks.list[i,1], description, sep = ".")	
		} else {
			data.name <- paste(stocks.list[i,1])
		}
   		if (FirstTime == TRUE) {
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
TimeExpand.data <- function(ticker.list, frame.list, period, description = FALSE) {
	# ----------
	# Общее описание:
	# 	функция выделения данных по tf и временному интервалу
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	frame.list - файл, сожержащий список нужных временных интервалов	
	# 		даты должны быть записаны в виде '2014-12-01/2014-12-31'	
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# Выходные данные:
	#	выдает .csv
	# ----------
	# 
	cat( "Start DataExpand by FrameList :", "\n")
	cat( "Generate DataNameList...", "\n")
	data.name.list <- StocksNameList(ticker.list, description)
	frame.list <- read.csv(frame.list, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	nframe <- nrow(frame.list)
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data.source <- ReadCSVtoXTS(name = data.name, period = period[1], tframe = FALSE)
		cat ("Expand...", "\t", data.name, "\n")
		for (n in 1:nframe) {
			# цикл time.frame'а
			cat ("Expand...", "\t", data.name, "for TimeFrame ", frame.list[n, 1], "\n")
			window <- frame.list[n, 1] 
			for (t in 1:nperiod) {
				# цикл периода
				p <- period[t]
				cat ("Expand...", "\t", data.name, "for Period ", p, "\n")
				if (p == "5min") { 
					p1 <- "mins"
					k <- 5
				}
				if (p == "10min") {
					p1 <- "mins"
					k <- 10
				}
				if (p == "15min") {
					p1 <- "mins"
					k <- 15
				}
				if (p == "30min") {
					p1 <- "mins"
					k <- 30
				}
				if (p == "1hour") {
					p1 <- "hours"
					k <- 1
				}
				if (p == "1day") {
					p1 <- "days"
					k <- 1
				}
				data <- data.source[window]
				ends <- endpoints(data, p1, k)
				data <- data[ends]
				SaveXTStoCSV(data = data, name = data.name, period = p, tframe = n)
			}
		}
	}
	cat( "Expand StocksData...", "\t", "complete", "\n")
}
#
DataPrepareForPCA <- function (ticker.list, price, description, period, tframe, approx = FALSE) {
	# ----------
	# Общее описание:
	# 	функция подготовки данных к PCA
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	tframe - тайм фрейм (ОДНО значение)
	# 	period - период свечей (ОДНО значение)
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	data - xts ряд объединенных подготовленных значений (по всему портфелю)
	# ----------
	# 
	cat( "Start DataPrepareForPCA...", "\n")
	data <- MergeForPCA(price  =  price, ticker.list = ticker.list, description = description, period = period, tframe = tframe, approx = approx)
	cat( "Merging Data...", "\t", "done", "\n")
	#data <- BindToMatrix(data, load.csv = FALSE, save.filename = "Matrix.csv")
	cat( "Create MatrixForPCA...", "\t", "done", "\n")
	return(data)
} 
#
MergeForPCA <- function (price = "SR", ticker.list, description = FALSE, period, tframe, approx = FALSE) {
	# ----------
	# Общее описание:
	# 	функция объединения данных в один XTS и устранение NA значений
		# NA можно убрать простым na.locf и аппроксимацией
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	tframe - тайм фрейм
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	merged.data - xts ряд объединенных значений (по всему портфелю)
	# ----------
	# 
	cat( "Generate DataNameList...", "\n")
	data.name.list <- StocksNameList(ticker.list = ticker.list, description = description) 
	nstocks <- nrow(data.name.list)
	FirstTime <- TRUE
	#  чтение и объединение данных
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data <- ReadCSVtoXTS(name = data.name, period = period, tframe = tframe) 
		if (price == "Open") {
			data <- data$Open
			col.name <- paste(data.name, "Open")
			names(data) <- c(col.name)
		}
		if (price == "Close") {
			data <- data$Close
			col.name <- paste(data.name, "Close")
			names(data) <- c(col.name)	
		}
		if (price == "SR") {
			data <- data$SR
			col.name <- paste(data.name, "SR")
			names(data) <- c(col.name)
		}
		if (price == "LR") {
			data <- data$LR
			col.name <- paste(data.name, "LR")
			names(data) <- c(col.name)
		}
		if (FirstTime == TRUE) {
			FirstTime <- FALSE
			merged.data <- data
		} else {
			merged.data <- merge(merged.data, data)
		}
	}	
	cat("Merging StocksData:", "\t", "complete", "\n")
	# нормализация NA-значений
	if (approx == FALSE) {
		# нормализация без аппроксимации (с пом-ю na.locf)
		cat( "Normalize StocksData...", "\t", "without approx", "\n") 
		merged.data <- na.locf(merged.data)
	} else	{
		# аппроксимация NA
		cat( "Normalize StocksData...", "\t", "with approx", "\n") 
		merged.data <- na.approx(merged.data)
	}
	merged.data <- na.omit(merged.data)
	cat( "Save Data...", "\n") 
	filename <- paste("MergedData", ticker.list = ticker.list, sep = ".")
	SaveXTStoCSV(data = merged.data, name = filename, period = period, tframe = tframe)
	#write.table(merged.data, file = filename, sep = ",")
	return (merged.data)
}
#
BindToMatrix <- function (data, load.filename = FALSE, save.filename = "Matrix.csv") {
	# ----------
	# Общее описание:
	# 	функция преобразования объединенного xts в матрицу
		# на вход подается merged.data xts либо напрямую, либо через чтение .csv (чтение нужно для 
		# независимого от DataPrepareForPCA() использования
	# Входные данные:
	#	data - подготовленный и объединенный xts (merged.data)
	# 	load.filename - если данные необходимо считать из .csv, то это путь
	# Выходные данные:
	#	data - матрица для PCA 
		# + матрица сохраняется в save.filename
	# ----------
	# 
	if (load.filename != FALSE) {
		cat( "Loading Data for BindToMatrix...", "\n")
		data <- ReadCSVtoXTS(name = load.filename)
	}
	#преобразование в матрицу 
	cat( "Create Matrix...", "\n")
	#data <- data.frame(date = index(data), coredata(data))
	data <- as.matrix(data, nrow = nrow(data), ncol = ncol(data))
	write.table(data, file = save.filename, sep = ",")
	return (data)
}
#
ExpandDataPrepareForPCA.toSCV <- function (ticker.list, frame.list, description, period,  approx, price) {
	# ----------
	# Общее описание:
	# 	генерирует большое количество данных для PCA (расширенное по периодам/тайм-фреймам)
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	frame.list - лист тайм фреймов 
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	data - матрица для PCA 
		# + матрица сохраняется в save.filename
	# ----------
	# 
	frame.list <- read.csv(frame.list, header  =  F, stringsAsFactors  =  F)
	nframe <- nrow(frame.list)
	tframe <- seq(1, nframe)
	nperiod <- length(period)
	for (i in 1:nperiod) {
		for (t in 1:nframe) {
			data <- DataPrepareForPCA (ticker.list = ticker.list, description = description, period = period[i], tframe = tframe[t], approx = approx, price = price)
		}	
	} 
}
#
PCAcompute <- function(data, period, tframe, KG.test = FALSE, DF.test = TRUE) {
	# ----------
	# Общее описание:
	# 	вычисление PC + тестирует данные
	# Входные данные:
	#	data - xts, содержащий подготовленные данные 
	# 	period - данные по периоду свечей (нужно для корректоного имени .csv)
	#	tframe - данные по тайп-фрейму (нужно для корректоного имени .csv)
	#	KG.test - необходимость теста Кайзера-Гуттмана (проверки PC на значимость)
	# 	DF.test - необходимость теста Дика-Фулера (поверка PC на стационарность)
	# Выходные данные:
	#	data - матрица для PCA 
	# ----------
	# 			
	cat ("PCA start:", "\t", ticker.list, "\n")
	pca.data <- princomp(data)
	# нагрузки
	loadings <- pca.data$loadings[]
	# Критерий  Кайзера-Гуттмана  рекомендует оставить для дальнейшего анализа только те главные компоненты,
	# дисперсия которых превышают среднюю:
	if (KG.test == TRUE) {
		ev <- as.numeric(pca.data$sdev)
		ev <- which.min(ev>mean(ev))
		cat("KG factor PCA numbers:", "\t", ev, "\n")	
		#n.PC <- ev
	} else {
		# выбираем не больше первых 10 PC
		n.PC <- ncol(loadings)
		if (n.PC > 10) {
			n.PC <- 10
		}	
	} 
	# вычисление компонент
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
	cor.table <- round(cor(temp, use = 'complete.obs',method = 'pearson'),2)
 	# дисперсия PC
	vol <- round(100*sd(temp,na.rm = T),2)
	# проверка на стационарность получившихся PC
	if (DF.test == TRUE) {
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
	loadings.filename <- paste("Loadings", ticker.list, period, tframe, "csv", sep = ".")
	write.table(loadings, file = loadings.filename, sep = ",")
	# сохраняем веса
	components.filename <- paste("Components", ticker.list, period, tframe, "csv", sep = ".")
	write.table(components, file = components.filename, sep = ",")
	#barplot(height = pca.data$sdev[1:10]/pca.data$sdev[1])
	return()
}

	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- ReadCSVtoXTS (name = filename, period, tframe)
