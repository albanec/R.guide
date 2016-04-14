library(data.table)

# returns string w/o leading whitespace
trim.leading <- function (x) sub("^\\s+", "", x)
# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# функция-парсер
parse.csv <- function (file.path=file.path, var1=26, var2=27, var3=28, profit=profit, sort=TRUE, var.names=TRUE) {
	# подгрузка файлов 
		file <- read.table(file=file.path, header=F, sep = ";", as.is=T)   
	# выделяем нужные параметры
		var1.name <- file[[1, var1]]
		var2.name <- file[[1, var2]]
		var3.name <- file[[1, var3]]
		profit.name <- file[[1, profit]] 
		cat( "############", "\n",
			"Парсинг файла:", ".......... ", file.path, "\n")
		cat(sep = "\n", "############",
						"Выбраны переменные & тепловой параметр:",
						"" )
		cat("var1: ", ".......... ", var1.name, "\n")
		cat("var2: ", ".......... ", var2.name, "\n")
		cat("var3: ", ".......... ", var3.name, "\n")
		cat("profit: ", ".......... ", profit.name, "\n")
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
		temp.frame$temp.frame <- NULL
		# сортировка по профиту
		if (sort==TRUE) {
			temp.frame <- temp.frame[order(-temp.frame$profit),]
			cat( "############", "\n",
			"Сортировка по данных по profit'у", "\n")	
		}
		if (var.names==TRUE) {
			colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)	
			cat( "############", "\n",
			"Столбцы проименованы", "\n")
		}
	cat(sep = "\n", "############",
						"Готово.", 
					"############" )
	
	return (temp.frame)
}

# функция-квантиль 
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
 
# функция сравнения (выдает матрицу сопоставления)
compare.df <- function (file1, file2, rec=FALSE, p.diff=TRUE) {
	# добавление идентификатора строк и сортировка 
		temp.file1 <- file1
		temp.file2 <- file2
		file1$profit <- NULL
		file2$profit <- NULL
		if (rec == TRUE) {
			file1$recovery <- NULL
			file2$recovery <- NULL
		}
		
		file1$var0 <- apply(file1, 1, paste, collapse='')
		file2$var0 <- apply(file2, 1, paste, collapse='')
		file1$var0  <- as.numeric(file1$var0)
		file2$var0  <- as.numeric(file2$var0)
		file1$profit <- temp.file1$profit  
		file2$profit <- temp.file2$profit
		if (rec == TRUE) {
			file1$recovery <- temp.file1$recovery  
			file2$recovery <- temp.file2$recovery 
		}
		file1 <- file1[order(-file1$var0),]
		file2 <- file2[order(-file2$var0),]
		file1$type <- "file1"
		file2$type <- "file2"
		remove(temp.file1)
		remove(temp.file2)
	# проверка на совпадение
		l <- rbind(file1, file2)
		t1 <- duplicated(l$var0, fromLast = TRUE)
		t2 <- duplicated(l$var0)
		file1 <- l[t1,]
		file2 <- l[t2,]
	# вычесление изменения профита и суммы за два периода
		file1$profit2 <- file2$profit
		if (rec == TRUE) {
			file1$recovery2 <- file2$recovery
			}
		if (p.diff == TRUE) {
			file1$profit.dif <- abs(file1$profit - file1$profit2)
			file1$profit.sum <- (file1$profit + file1$profit2)
		}
		file1$var0 <- NULL
		file1$type <- NULL
	#
	return(file1)
}

###################################################
							###################################################

profit <- 3
recover <- 24
file.path1 <- ""
file.path2 <- ""

# парсинг данных и подготовка
	file1 <- parse.csv(file.path=file.path1, var1=26, var2=27, var3=28, profit=profit, sort=FALSE, var.names=FALSE)
	file2 <- parse.csv(file.path=file.path2, var1=26, var2=27, var3=28, profit=profit, sort=FALSE, var.names=FALSE)
	#
	file1.temp <- parse.csv(file.path=file.path1, var1=26, var2=27, var3=28, profit=recover, sort=FALSE, var.names=FALSE)
	file2.temp <- parse.csv(file.path=file.path2, var1=26, var2=27, var3=28, profit=recover, sort=FALSE, var.names=FALSE)
	#
	file1$recovery <- file1.temp$profit
	file2$recovery <- file2.temp$profit
	remove(file1.temp); remove(file2.temp)
# обработка по equity
	temp.data <- compare.df(file1, file2, rec=TRUE, p.diff=TRUE)
	temp.data <- quant.file(data=temp.data, var=8, q.low=0.2, low=TRUE)
	temp.data <- temp.data[order(-temp.data$profit.sum),]
# данные для графиков
	mycolors <-  rainbow(30, start=0.3, end=0.95)
	var1.name <- colnames(temp.data)[1]
	var2.name <- colnames(temp.data)[2]
	var3.name <- colnames(temp.data)[3]
# график (profit без заморочек)
	p1 <- plot_ly(x=temp.data[[1]], y=temp.data[[2]], z=temp.data[[3]], type="scatter3d", mode="markers", color=temp.data[[9]], colors=mycolors) %>% 
		layout(title = "Profit без учета recovery", 
			scene = list( 
				xaxis = list(title = "var1.name" ),
       	    	yaxis = list(title = "var2.name" ), 
       	    	zaxis = list(title = "var3.name" )
       		  	) 
        	)
# учет recovery
	temp.data2 <- temp.data
	temp.data$recovery.diff <- abs(temp.data$recovery - temp.data$recovery2)
	# сглаженность данных
	temp.data <- quant.file(data=temp.data, var=10, q.hi=0.9, hi=TRUE)
	# 
	temp.data <- quant.file(data=temp.data, var=5, q.hi=0.5, hi=TRUE, abs=TRUE)
	temp.data <- quant.file(data=temp.data, var=7, q.hi=0.5, hi=TRUE, abs=TRUE)
	temp.data <- temp.data[order(-temp.data$profit.sum),]
# график (с учётом recovery)
	p2 <- plot_ly(x=temp.data[[1]], y=temp.data[[2]], z=temp.data[[3]], type="scatter3d", mode="markers", color=temp.data[[9]], colors=mycolors) %>% 
		layout(title = "Profit без учета recovery", 
			scene = list( 
				xaxis = list(title = "var1.name" ),
       	    	yaxis = list(title = "var2.name" ), 
       	    	zaxis = list(title = "var3.name" )
       	  		) 
        	)
