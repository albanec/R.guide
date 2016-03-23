library("rusquant")
library("quantmod")
library("PerformanceAnalytics")

#########
		###########
#описание функций 
#
get.data <- function (ticker, from.date, to.date=Sys.Date(), frame="15min") {
	require(rusquant) 
	# загрузка данных
	# дата в формате "2015-01-01"
	print("Download Source Data")
	data <- getSymbols(ticker, from=from.date, to=to.date, period=frame, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	
	return(data)
}
#

exrem <- function(x) {
	# функция для перехода к состояниям (фильтрация сигналов)
	x$a <- na.locf( x )
	x$a <- ifelse( is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a )
	ind <- which( x$a != lag(x$a) )
	x$y <- rep( NA, length(x$a) )
	x$y[ind] = x$a[ind]
	x$y[1] <- x$a[1]
	return(x$y)
}

nameOfStrategy <- "PSAR and 2SMA cross"
strategy.psar.2sma <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2, state=TRUE) {
	require(rusquant) 
	# описание psar.2sma стратегии 
	data$sma <- SMA(Cl(data), slow.sma)
	data$fma <- SMA(Cl(data), fast.sma)
	data$sar <- SAR(data, accel = c(accel.start, accel.max))
	data$sig.sma <- ifelse( 
		data$fma > data$sma, 1, 
		ifelse(data$fma < data$sma, -1, 0)
	)
	data$sig.sar <- ifelse(
		data$Close > data$sar, 1, 
		ifelse(data$Close < data$sar, -1, 0)
	)
	data$pos <- sign(data$sig.sma + data$sig.sar)
	data$pos <- lag(data$pos)
	if (state==TRUE) {
		data$state <- exrem(data$pos)
	} else {
		colnames(data)[colnames(data)=="pos"] <- "state"
	}
	data <- na.omit(data)
	return(data)
}		

calc.returns <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) {
	require(rusquant) 
	# расчет доходностей
	if (abs==TRUE) { 	
		if (reinvest==TRUE) {
			data$w <- data$state[[1]] * s0/data$Open[[1]]
			data$w <- trunc(data$w)
			data$equity <- s0
			data$margin <- 0
			for ( i in 2:nrow(data) ) { 
				data$margin[i] <- data$w[[i-1]] * ( data$Open[[i]] - data$Open[[i-1]] )
				data$equity[i] <- (data$equity[[i-1]] + data$margin[[i]]) / pip
				data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
				data$w[i] <- trunc(data$w[i])
			} 
		} else {
			data$w <- 1 
			data$margin <- lag(data$state) * ( data$Open-lag(data$Open) ) / pip
			data$margin[1] <- 0
			data$equity <- cumsum(data$margin)
		}
	}
	if (SR==TRUE) {
		if (reinvest==TRUE) {
			data$SR <- lag(data$state)*Delt(data$Open, type="arithmetic")
			data$SR[1] <- 0
			data$margin <- cumprod(data$SR + 1) 
			data$margin[1] <- 0
			data$equity <- s0*data$margin
		} else {
			data$SR <- lag(data$state)*Delt(data$Open, type="arithmetic")
			data$SR[1] <- 0
			data$margin <- cumprod(data$SR + 1) - 1
			data$equity <- data$Open[[1]] * as.numeric(data$margin)
		}
	}
	if (LR==TRUE) {
		#if (reinvest==TRUE) {
			#
		#} else {
			data$LR <- lag(data$state)*Delt(data$x, type="log")
			data$LR[1] <- 0
			data$margin <- cumsum(data$LR)
			data$equity <- data$Open[[1]] * (exp(as.numeric(last(data$margin))) - 1)
		#}
	}
	return(data)
}

calc.profit <- function(data, s0=0, reinvest=TRUE) {
	require(rusquant) 
	# расчет итогового профита
	if (reinvest==TRUE) {
		profit <- as.numeric(last(data$equity) - s0)		
	} else {
		profit <- as.numeric(last(data$equity))	
	}
	return (profit)
}
# 
dataset.psar.2sma <- function(data, state=FALSE, pip=1, s0=1, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=FALSE, ) {
	require(rusquant) 
	require(PerformanceAnalytics)
	# функция формирования итогового фрейма данных по всем наборам переменных работы стратегии
	firstRun <- TRUE
	for (fast.sma in seq(1, 99, 1)) {
		for(slow.sma in seq(fast.sma+1, 100, 1)) {
			for (accel.max in seq(0.05, 0.4, 0.01)) {
				for (accel.start in seq(0.01, accel.max, 0.01)) {
					temp.data <- strategy.psar.2sma(data, state)
					# вычисление абсолютных доходностей
					temp.data <- calc.returns(temp.data, pip, s0, abs=TRUE, reinvest)
					# вычисление SR и запись значений
					temp.data2 <- calc.returns(temp.data, pip, s0, SR=TRUE, reinvest)
					temp.data$SR <- temp.data2$SR
					temp.data$SR.margin <- temp.data2$margin
					temp.data$SR.equity <- temp.data2$equity
					remove(temp.data2)
					# расчет метрик продуктивности
					RTable <- RatioTable(temp.data$SR)
					# расчет временных метрик

					# расчет итогового профита
					profit <- calc.profit(temp.data, s0, reinvest)
					# формирования итогового блока данных
					results <- cbind(fast.sma, slow.sma, accel.start, accel.max, s0, profit, (s0 + profit))
					if(firstRun){
			  			firstRun <- FALSE
			  			colnames(results) <- c("Fast SMA","Slow SMA","Accel Start", "Accel Max", "Start balance", "Profit", "End Balance")	
		  				final.results <- results 
		  			} else {
			  		final.results <- rbind(final.results, results)
		  			}
				}
			}
 		}	
	}
	return(results)
}

	
# лист переменных
# get.data <- function (ticker="SiH6", from.date, to.date=Sys.Date(), frame="15min")
# strategy.psar.2sma <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2, state=TRUE)
# calc.returns <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) 
# calc.profit <- function(data, s0=0, abs==FALSE, SR==FALSE, LR==FALSE, reinvest==FALSE)
#
data <- get.data(from.date=Sys.Date()-14)
temp.data <- data
head(temp.data, 3)
#
data <- strategy.psar.2sma(data=data, slow.sma=20, fast.sma=8)
#
data <- calc.returns(data=data, pip=1, abs=TRUE, reinvest=FALSE)
profit <- calc.profit(data=data, reinvest=FALSE)
#
