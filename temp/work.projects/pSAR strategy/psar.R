library("rusquant")
#########
		###########
#описание функций 
#
# загрузка данных
	# дата в формате "2015-01-01"
get.data <- function (ticker="SiH6", from.date, to.date=Sys.Date(), frame="15min") {
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
	if (state==TRUE) {
		data$state <- exrem(data$pos)
	}
	data <- na.omit(data)
	return(data)
}		

# расчет доходностей
returns.calc <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) {
	if (abs==TRUE) { 	
		if (reinvest==TRUE) {
			data$w <- data$state[[1]] * s0/data$Close[[1]]
			# data$w <- order(data$w, 0)
			data$ret <- 0
			data$equity <- s0
			data$delta <- 0
			for ( i in 2:nrow(data) ) { 
				data$delta[i] <- data$w[[i-1]] * ( data$Close[[i]] - data$Close[[i-1]] )
				data$equity[i] <- (data$equity[[i-1]] + data$delta[[i]]) / pip
				data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Close[[i]]
				# data$w[i] <- round(data$w[i], 0)
			} 
		} else {
			data$ret <- lag(data$state) * ( data$Close-lag(data$Close) ) / pip
			data$ret[1] <- 0
			data$equity <- cumsum(data$ret)
		}
	}
	if (SR==TRUE) {
		if (reinvest==TRUE) {
			data$SR <- Delt(data$Close, type="arithmetic")
			data$SR[1] <- 0
			data$equity <- s0*cumprod(data$SR + 1)
		} else {
			data$SR <- Delt(data$Close, type="arithmetic")
			data$SR[1] <- 0
			data$equity <- cumprod(data$SR + 1) - 1
		}
	}
	if (LR==TRUE) {
		#if (reinvest==TRUE) {
			#
		#} else {
			data$LR <- Delt(data$x, type="log")
			data$LR[1] <- 0
			data$equity <- cumsum(data$LR)	
		#}
	}
	return(data)
}
profit.calc <- function(data, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=FALSE) {
	if (abs==TRUE) {
		if (reinvest==TRUE) {
			profit <- as.numeric(last(data$equity) - s0)		
		} else {
			profit <- as.numeric(last(data$delta))	
		}
	}
	if (SR==TRUE) {
		if (reinvest==TRUE) {
			profit <- as.numeric(last(data$equity) - s0)		
		} else {
			profit <- data$Close[[1]] * as.numeric(last(data$equity))
		}
	}
	if (LR==TRUE) {
		#if (reinvest==TRUE) {
			#
		#} else {
		profit <- data$Close[[1]] * (exp(as.numeric(last(data$equity))) - 1)	
		#}
	}
	return (profit)
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
data11 <- returns.calc(data=data, pip=1, abs=TRUE, reinvest=FALSE)
data12 <- returns.calc(data=data, pip=1, s0=100000, abs=TRUE, reinvest=TRUE)
profit11 <- profit.calc(data=data11, abs=TRUE, reinvest=FALSE)
profit12 <- profit.calc(data=data12, s0=100000, abs=TRUE, reinvest=TRUE)
#
data21 <- returns.calc(data=data, pip=1, SR=TRUE, reinvest=FALSE)
data22 <- returns.calc(data=data, pip=1, s0=100000, SR=TRUE, reinvest=TRUE)
profit11 <- profit.calc(data=data21, SR=TRUE, reinvest=FALSE)
profit12 <- profit.calc(data=data22, s0=100000, SR=TRUE, reinvest=TRUE)
#
data31 <- returns.calc(data=data, pip=1, LR=TRUE, reinvest=FALSE)
profit31 <- profit.calc(data=data31, s0=100000, LR=TRUE)
