library("rusquant")
library("quantmod")
library("PerformanceAnalytics")

#########
		###########
#описание функций 
#
# загрузка данных
	# дата в формате "2015-01-01"
get.data <- function (ticker, from.date, to.date=Sys.Date(), frame="15min") {
	data <- getSymbols(ticker, from=from.date, to=to.date, period=frame, src="Finam", auto.assign=FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	return(data)
}
#
# функция для перехода к состояниям (фильтрация)
exrem <- function(x) {
	x$a <- na.locf( x )
	x$a <- ifelse( is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a )
	ind <- which( x$a != lag(x$a) )
	x$y <- rep( NA, length(x$a) )
	x$y[ind] = x$a[ind]
	x$y[1] <- x$a[1]
	return(x$y)
}
# psar.2sma стратегия 
nameOfStrategy <- "PSAR and 2SMA cross"
strategy.psar.2sma <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2, state=TRUE) {
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
	}
	data <- na.omit(data)
	return(data)
}		

# расчет доходностей
calc.returns <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) {
	if (abs==TRUE) { 	
		if (reinvest==TRUE) {
			data$w <- data$state[[1]] * s0/data$Open[[1]]
			data$w <- round(data$w, 0)
			data$equity <- s0
			data$margin <- 0
			for ( i in 2:nrow(data) ) { 
				data$margin[i] <- data$w[[i-1]] * ( data$Open[[i]] - data$Open[[i-1]] )
				data$equity[i] <- (data$equity[[i-1]] + data$margin[[i]]) / pip
				data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
				data$w[i] <- round(data$w[i], 0)
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
# расчет итогово профита
profit.calc <- function(data, s0=0, reinvest=TRUE) {
	if (reinvest==TRUE) {
		profit <- as.numeric(last(data$equity) - s0)		
	} else {
		profit <- as.numeric(last(data$equity))	
	}
	return (profit)
}
# 
dataset.psar.2sma <- function(data, state, pip, s0, abs, SR, LR, reinvest, ) {
	firstRun <- TRUE
	for (fast.sma in seq(1, 99, 1)) {
		for(slow.sma in seq(fast.sma+1, 100, 1)) {
			for (accel.max in seq(0.05, 0.4, 0.01)) {
				for (accel.start in seq(0.01, accel.max, 0.01)) {
					temp.data <- strategy.psar.2sma(data, state)
					temp.data <- calc.returns(temp.data, pip, s0, abs, SR, LR, reinvest)
					profit <- profit.calc(temp.data, s0, reinvest)
					results <- cbind(fast.sma, slow.sma, accel.start, accel.max, s0, profit, (s0 + profit))
					if(firstRun){
			  			firstRun <- FALSE
			  			colnames(results) <- c("Fast SMA","Slow SMA","Accel Start", "Accel Max", "Start balance", "Profit", "End Balance")	
		  				final.results <- results 
		  			} else {
			  		final.results <- rbind(final.results, final.results)
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
# returns.calc <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) 
# profit.calc <- function(data, s0=0, abs==FALSE, SR==FALSE, LR==FALSE, reinvest==FALSE)
#
data <- get.data(from.date=Sys.Date()-14)
temp.data <- data
head(temp.data, 3)
#
data <- strategy.psar.2sma(data=data, slow.sma=20, fast.sma=8)
#
data <- returns.calc(data=data, pip=1, abs=TRUE, reinvest=FALSE)
profit <- profit.calc(data=data, reinvest=FALSE)
#
