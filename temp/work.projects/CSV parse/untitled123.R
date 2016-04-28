parse.csv <- function (file.path=file.path, var1, var2, var3, profit=profit, draw=draw, sort=FALSE, var.names=TRUE) {
    # подгрузка файлов 
    file <- read.table(file=file.path, header=F, sep = ";", as.is=T)   
    # выделяем нужные параметры
    var1.name <- file[[1, var1]]
    var2.name <- file[[1, var2]]
    var3.name <- file[[1, var3]]
    # var3.name <- file[[1, var3]]
    profit.name <- file[[1, profit]] 
    draw.name <- file[[1, draw]] 
    cat( "############", "\n",
         "Парсинг файла:", ".......... ", file.path, "\n")
    cat(sep = "\n", "############",
        "Выбраны переменные & тепловой параметр:",
        "" )
    cat("var1: ", ".......... ", var1.name, "\n")
    cat("var2: ", ".......... ", var2.name, "\n")
    cat("var3: ", ".......... ", var3.name, "\n")
    cat("profit: ", ".......... ", profit.name, "\n")
    cat("draw:   ", ".......... ", draw.name, "\n")
    cat(sep = "\n", "############",
        "Парсинг ......", 
        "" )
    # выборка данных 
    # чистим от лишнего
    file <- file[-1,]
    file <- file[, colSums(is.na(file)) == 0]
    #
    temp.t <- nrow(file) 
    temp.frame <- rep(NA, temp.t)
    temp.frame <- data.frame(temp.frame)
    #
    temp.frame$var1 <- as.numeric( gsub("\\,", ".", file[[var1]]) ) 
    temp.frame$var1 <- as.numeric( gsub("\\s", "", temp.frame$var1) ) 
    temp.frame$var2 <- as.numeric( gsub("\\,", ".", file[[var2]]) )
    temp.frame$var2 <- as.numeric( gsub("\\s", "", temp.frame$var2) )
    temp.frame$var3 <- as.numeric( gsub("\\,", ".", file[[var3]]) )
    temp.frame$var3 <- as.numeric( gsub("\\s", "", temp.frame$var3) )
    temp.frame$profit <- as.numeric( gsub("\\,", ".", file[[profit]]) )
    temp.frame$profit <- as.numeric( gsub("\\s", "", temp.frame$profit) )
    temp.frame$draw <- as.numeric( gsub("\\,", ".", file[[draw]]) )
    temp.frame$draw <- as.numeric( gsub("\\s", "", temp.frame$draw) )
    temp.frame$temp.frame <- NULL
    # сортировка по профиту
    if (sort==TRUE) {
        temp.frame <- temp.frame[order(-temp.frame$profit),]
        cat( "############", "\n",
             "Сортировка по данных по profit'у", "\n")	
    }
    if (var.names==TRUE) {
    #    colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)	
        colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name, draw.name)	
        cat( "############", "\n",
             "Столбцы проименованы", "\n")
    }
    cat(sep = "\n", "############",
        "Готово.", 
        "############" )
    
    return (temp.frame)
}
quant.file <- function (data, var, q.hi=0, q.low=0, two=FALSE, low=FALSE, hi=FALSE, abs=FALSE) {
    if (two == TRUE) {
        # подготовка данных
        data <- data[order(-data[[var]]), ]
        # вычисление квантилей
        ifelse(abs==FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
        n.hi <- which( data[, var] < q.hi.value )
        ifelse(abs==FALSE, q.low.value <- quantile(data[[var]], q.low), q.low.value <- as.numeric(q.low)) 
        data <- data[n.hi,]
        n.low <- which( data[, var] > q.low.value )
        data <- data[n.low,]
        } 
    if (hi == TRUE) {
        data <- data[order(-data[[var]]), ]
        ifelse(abs==FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
        n.hi <- which( data[, var] > q.hi.value )   
        data <- data[n.hi,]
        }
    if (low==TRUE) {
        data <- data[order(-data[[var]]), ]
        ifelse (abs==FALSE, q.low.value <- as.numeric(quantile(data[[var]], q.low)), q.low.value <- as.numeric(q.low)) 
        n.low <- which( data[, var] < q.low.value )
        data <- data[n.low,]
        }
    #   
    return (data)
}


	data.1.11_13 <- AllInOne(file.path="/home/rs-evgeni/temp/t3/HL_cls_sma/11-13.csv", 
	                                  var1=26, var2=27, var3=28, profit=2, draw=8, m=26, q.hi=0.5)
	p1.11_13 <- plot_ly(x=data.1.11_13[[1]], y=data.1.11_13[[2]], z=data.1.11_13[[3]], 
	        type="scatter3d", mode="markers", color=data.1.11_13[[6]], 
	         colors=mycolors, marker = list(size = 4))
	#
	data.1.14 <- AllInOne(file.path="/home/rs-evgeni/temp/t3/HL_cls_sma/14.csv", 
	                                  var1=26, var2=27, var3=28, profit=2, draw=8, m=9, q.hi=0.5)
	p1.14 <- plot_ly(x=data.1.14[[1]], y=data.1.14[[2]], z=data.1.14[[3]], 
	        type="scatter3d", mode="markers", color=data.1.14[[6]], 
	         colors=mycolors, marker = list(size = 4))
	#
	data.1.15 <- AllInOne(file.path="/home/rs-evgeni/temp/t3/HL_cls_sma/15.csv", 
	                                  var1=26, var2=27, var3=28, profit=2, draw=8, m=12, q.hi=0.5)
	p1.15 <- plot_ly(x=data.1.15[[1]], y=data.1.15[[2]], z=data.1.15[[3]], 
	        type="scatter3d", mode="markers", color=data.1.15[[6]], 
	         colors=mycolors, marker = list(size = 4))
	#
	data.1.16 <- AllInOne(file.path="/home/rs-evgeni/temp/t3/HL_cls_sma/16.csv", 
	                                  var1=26, var2=27, var3=28, profit=2, draw=8, m=4,  q.hi=0.5)
	p1.16 <- plot_ly(x=data.1.16[[1]], y=data.1.16[[2]], z=data.1.16[[3]], 
	        type="scatter3d", mode="markers", color=data.1.16[[6]], 
	         colors=mycolors, marker = list(size = 4))



	data.1.11_13 <- AllInOne(file.path="/home/rs-evgeni/temp/t3/HL_cls_sma/11-13.csv", 
	                                  var1=26, var2=27, var3=28, profit=2, draw=8, m=26, q.hi=0.5)
	
	GiveMeMorePlots <- function (data) {
		data <- data[order(-data$var3), ]
		new.begining <- TRUE
		plot.vector <- c()
		mycolors <-  rainbow(30, start=0.3, end=0.95)
		FirstTime <- TRUE
		for ( i in 1:nrow(data)) {
			if (data$var3[i] == (data$var3[i+1] -1)) {
				per.num <- data$var3[i]
				if (FirstTime == TRUE) {
					temp.data <- data[i, ]
					FirstTime <- FALSE
				} else {
					temp.data <- rbind(temp.data, data[i, ])
				}
			} else {
				FirstTime <- TRUE
				new.begining <- TRUE
				i.end <- i
				temp.data <- rbind(temp.data, data[i, ])
				temp.var <- paste("plot", per.num, sep=".")
				temp.data.name <- paste("temp.data", per.num, sep=".")
				plot.vector <- c(plot.vector, temp.var)
				assign(temp.var, plot_ly(x=temp.data[[1]], y=temp.data[[2]], mode="markers", color=temp.data[[6]], 
										colors=mycolors, marker = list(size = 4), layout(title = temp.var)))
				assign(temp.data.name, temp.data)
			}
		}
		plot.count <- length(plot.vector)
		FirstTime <- TRUE
		for (i in 1:plot.count) {
			if (FirstTime == TRUE) {
				FirstTime <- FALSE
				temp.var <- paste(plot.vector[i])
			} else {
				temp.var <- paste(plot.vector[i], sep=",")
			}
		}
		p <- eval(parse(text = paste("subplot(", temp.var, ",", "nrows = ", round(plot.count/4)+1, ")", sep="")))
		return (p)
	}

	p1.11_13 <- plot_ly(x=data.1.11_13[[1]], y=data.1.11_13[[2]], z=data.1.11_13[[3]], 
	        type="scatter3d", mode="markers", color=data.1.11_13[[6]], 
	         colors=mycolors, marker = list(size = 4))
