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
TradeID <- function(states) {
		x <- diff(states); x[1] <- states[1]
		cumsum( as.numeric( as.logical(abs(x)) & abs(states) )) * abs(states)
	
	}
# psar.2sma стратегия
strategy.psar.2sma <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2, ) {
	data$sma <- SMA(Cl(data), slow.smad)
	data$fma <- SMA(Cl(data), fast.sma)
	data$sar <- SAR(data, accel = c(accel.start, accel.max))
	data$sig.sma <- ifelse( data$fma > data$sma, 1, 
						ifelse(data$fma < data$sma, -1, 0)
					)
	data$sig.sar <- ifelse(
		data$Close > data$sar, 1, 
		ifelse(data$Close < data$sar, -1, 0)
			)
	data$pos <- sign(data$sig.sma + data$sig.sar)
	data$states <- exrem(data$pos)
	data <- na.omit(data)

return(data)
}		
# расчет доходностей
returns.calc <- function(data, pip, abs=FALSE, SR=FALSE, reinvest=TRUE) {
	if (abs==TRUE) {
		if (reinvest==TRUE) {
			data$w[1] <- data$state[[1]] * s0/data$Close[[1]]
			data$delta[1] <- 0
			data$equity.abs[1] <- s0
			for ( i in 2:nrow(data) ) { 
				data$delta[i] <- data$w[[i-1]] * ( data$Close[[i]] - data$Close[[i-1]] )
				data$equity.abs[i] <- data$equity.abs[[i-1]] + data$delta[[i]]
				data$w[i] <- data$state[[i]] * data$equity.abs[[i]] / data$Close[[i]]
			}
		}
		data$ret <- lag(data$state) * ( data$Close−lag(data$Close) ) / pip
		data$ret[1] <- 0
		data$equity <- cumsum(data$ret)
	}
	if (SR==TRUE) {
		if (reinvest==TRUE) {
			data$R <- ROC(data$Close, type="discrete")
			sata$R[1] <- 0
			data$equity <- s0*cumprod(data$R + 1)
			profit <- as.numeric(last(data$equity) - s0)
		}
		data$SR <- Delt(data$Close, type="arithmetic")
		data$R[1] <- 0
		data$equity <- cumprod(data$R + 1) - 1
		profit <- data$x[[1]]*as.numeric(last(data$equity))
	}
return(data)
}


data <- get.data(from.date = Sys.Date()-14 )
temp.data <- data
head(temp.data, 3)


		data$sigDN <- Cross(data$sar, data$Close)
		# точки позиций
			data$sig.sar <- (data$sigUp - data$sigDn)
			data.temp <- data[ data$sig.sar != 0 ]
		# убираем лишнее и добавляем очишенные данные в ряд
			data.temp$sig.clean <- exrem(data.temp$sig)
		# убираем лишние столбцы
			data.temp$sig <- NULL
			data.temp <- na.omit(data.temp)
			data$sig <- NULL
		#
		data.temp2 <- merge.xts(data, data.temp$sig.clean)
		colnames(data.temp2)[ ncol(data.temp2) ] <- "sig"
		data.temp2 <- na.locf( data.temp2, "sig", "state" )
		data.temp2$sig[ is.na(data.temp2$sig) ] <- 0
		data.temp2$state[ is.na(data.temp2$state) ] <- 0
	}


		


temp.data <- indicators(data=temp.data, slow.period = 80, fast.period = 20, accel.start, accel.max)




#
pip <- 0.0001
data.temp2$ret <- lag(data.temp2$state) * ( data.temp2$Close−lag(data.temp2$Close) ) / pip
data.temp2$ret[1] <- 0
data.temp2$equity <- cumsum(data.temp2$ret)