library("rusquant")


# загрузка данных
temp.data <- getSymbols("SiH6", from="2015-01-01", to=Sys.Date(), period="5min", src="Finam", auto.assign=FALSE)
names(temp.data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
data <- temp.data
head(data, 3)

###
Cross <- function(x1,x2) {
		x <- diff(x1>x2)
		x[1] <- 0
		x[x<0] <- 0
	return(sign(x))
	}
# добавляем индикаторы
	indicators <- function (data, slow.period, fast.period) {
		data$sma <- SMA(Cl(data), slow.period)
		data$fma <- SMA(Cl(data), fast.period)
		data$sar <- SAR(data, accel = c(accel.start, accel.max))
	return(data)
	}
# описание стратегии
	strategy <- function (data) {
		data$sig.sma <- ifelse(data$fma > data$sma, 1, -1)
		data$sigUP <- Cross(data$Close, data$sar)
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


		exrem <- function(x) {
						x$a <- na.locf( x )
						x$a <- ifelse( is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a )
						ind <- which( x$a != lag(x$a) )
						x$y <- rep( NA, length(x$a) )
						x$y[ (ind-1) ] = x$a[ind]
						return(x$y)
					}


temp.data <- indicators(data=temp.data, slow.period = 80, fast.period = 20, accel.start, accel.max)




#
pip <- 0.0001
data.temp2$ret <- lag(data.temp2$state) * ( data.temp2$Close−lag(data.temp2$Close) ) / pip
data.temp2$ret[1] <- 0
data.temp2$equity <- cumsum(data.temp2$ret)