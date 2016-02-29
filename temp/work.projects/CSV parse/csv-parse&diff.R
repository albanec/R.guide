library(data.table)

# функция-парсер
parse.csv <- function (file.path=file.path, var1=26, var2=27, var3=28, profit=profit, sort=TRUE, var.names=TRUE) {
	# подгрузка файлов 
		file <- read.table(file=file.path, header=F, sep = ";", as.is=T)   

	# выделяем нужные параметры
		var1.name <- file[[1, var1]]
		var2.name <- file[[1, var2]]
		var3.name <- file[[1, var3]]
		profit.name <- file[[1, profit]] 
		cat( "############", "\n"
			"Парсинг файла:", ".......... " file.path, "\n")
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
		temp.frame <- rep(NA, max(temp.t))
		temp.frame <- data.frame(temp.frame)
		#
		temp.frame$var1 <- as.numeric( gsub("\\,", ".", file[[var1]]) ) 
		temp.frame$var2 <- as.numeric( gsub("\\,", ".", file[[var2]]) )
		temp.frame$var3 <- as.numeric( gsub("\\,", ".", file[[var3]]) )
		temp.frame$profit <- as.numeric( gsub("\\,", ".", file[[profit]]) )
		temp.frame$temp.frame <- NULL
		# сортировка по профиту
		if (sort==TRUE) {
			temp.frame <- temp.frame[order(-temp.frame$profit),]	
		}
		if (var.names==TRUE) {
			colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)	
		}
		
	cat(sep = "\n", "############",
						"Готово.", 
					"############" )
	
	return (temp.frame)
}

# функция-квантиль 
quant.file <- function (data, var, q.hi, q.low=0, two.q=FALSE) {
	q.hi.value <- quantile(data[[var]], q.hi) 
	n.hi <- match(q.hi.value, data[[var]]) 
	t <- ncol(data)
	if (two.q == TRUE) {
		n.low <- match(q.low.value, data[[var]])
		temp.frame <- matrix(NA, ncol=t, nrow=abs(n.hi - n.low) )
		temp.frame <- data.frame(temp.frame)
		colnames(temp.frame) <- colnames(data)
		for (i in n.low:n.hi) {
			temp.frame[i, ] <- data[i, ]
			}	
		} 
		else {
			temp.frame <- matrix(NA, ncol=t, nrow=n.hi )
			temp.frame <- data.frame(temp.frame)
			colnames(temp.frame) <- colnames(data)
			for (i in 1:n.hi) {
				temp.frame[i, ] <- data[i, ]
				}		
		}
	return (temp.frame)
}
 
# функция сравнения (выдает матрицу сопоставления)
compare.df <- function (file1, file2 ) {
	# добавление идентификатора строк и сортировка 
		temp.file1 <- file1
		temp.file2 <- file2
		file1$profit <- NULL
		file1$var0 <- apply(file1, 1, paste, collapse='')
		file1$var0  <- as.numeric(file1$var0)
		file1$profit <- temp.file1$profit  
		file1 <- file1[order(-file1$var0),]
		file1$type <- "file1"
		file2$profit <- NULL
		file2$var0 <- apply(file2, 1, paste, collapse='')
		file2$var0  <- as.numeric(file2$var0)
		file2$profit <- temp.file2$profit   
		file2 <- file2[order(-file2$var0),]
		file2$type <- "file2"
		remove(temp.file1)
		remove(temp.file2)
	# проверка на совпадение
		l <- rbind(file1, file2)
		t1 <- duplicated(l$var0, fromLast = TRUE)
		t2 <- duplicated(l$var0)
		file1 <- l[t1,]
		file2 <- l[t2,]
	# вычесление изменения профита
		file1$profit2 <- file2$profit
		file1$profit.dif <- abs(file1$profit - file1$profit2)
}

###################################################
							###################################################

profit <- 3
file.path1 <- ""
file.path2 <- ""

file1 <- parse.csv(file.path=file.path1, var1=26, var2=27, var3=28, profit=profit, sort=FALSE, var.names=FALSE)
file2 <- parse.csv(file.path=file.path2, var1=26, var2=27, var3=28, profit=profit, sort=FALSE, var.names=FALSE)

temp.data <- compare.df(file1, file2)

q.hi <- ?; q.low <- ?
temp.data <- quant.file(data, var, q.hi, q.low, two.q=TRUE)


# график
mycolors <-  rainbow(30, start=0.3, end=0.95)
var1.name <- colnames(parse.file)[1]
var2.name <- colnames(parse.file)[2]
var3.name <- colnames(parse.file)[3]
p <- plot_ly(x=parse.file[[1]], y=parse.file[[2]], z=parse.file[[3]], type="scatter3d", mode="markers", color=parse.file[[4]], colors=mycolors) %>% 
	layout(title = "Тепловая карта", 
		scene = list( 
			xaxis = list(title = "var1.name" ),
           	yaxis = list(title = "var2.name" ), 
           	zaxis = list(title = "var3.name" )
         	) 
        )