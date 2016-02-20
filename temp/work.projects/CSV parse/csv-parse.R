library("quantmod")
library("plotly")
#
cat(sep = "\n", "############",  
				"Парсинг .CSV", 
				"############", "")
# предварительная настройка 
		cat(sep = "\n", "############", 
						"Предварительная настройка:", 
						"" )
	# определяемся с рабочей директорией	
		wd <- getwd()
		var1 <- NA; var2 <- NA; var3 <- NA; profit <- NA
		cat("Текущий рабочий католог", sep=" .......... ", wd)
	# путь к файлу
		cat("Путь к файлy: ", "\n")
		file.path <- readline()
		cat("Путь к файлy: ", ".......... ", file.path, "\n")
	# номера переменных
		cat(sep = "\n", "############",
						"Номера столбцов переменных & теплового параметра:",
						"" )
		cat("var1: ", "\n")
		var1 <- readline()
		cat("var1: ", ".......... ", var1, "\n")
		cat("var2: ", "\n")
		var2 <- readline()
		cat("var2: ", ".......... ", var2, "\n")
		cat("var3: ", "\n")
		var3 <- readline()
		cat("var3: ", ".......... ", var3, "\n")
		cat("profit: ", "\n")
		profit <- readline()
		cat("profit: ", ".......... ", profit, "\n")

# функция-парсер
parse.csv <- function (file.path=file.path, var1=26, var2=27, var3=28, profit=profit) {
	# подгрузка файлов 
		file <- read.table(file=file.path, header=F, sep = ";", as.is=T)   

	# выделяем нужные параметры
		var1.name <- file[[1, var1]]
		var2.name <- file[[1, var2]]
		var3.name <- file[[1, var3]]
		profit.name <- file[[1, profit]] 
		cat(sep = "\n", "############",
						"Выбраны переменные & тепловой параметр:",
						"" )
		cat("var1: ", ".......... ", var1.name, "\n")
		cat("var2: ", ".......... ", var2.name, "\n")
		cat("var3: ", ".......... ", var3.name, "\n")
		cat("profit: ", ".......... ", profit.name, "\n")
	
	cat(sep = "\n", "############",
						"Парсинг", 
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
		temp.frame <- temp.frame[order(-temp.frame$profit),]
		colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)
	cat(sep = "\n", "############",
						"Готово.", 
					"############" )
	
	return (temp.frame)
}	

# функция-квантиль 
quant.file <- function (data, var, q) {
	q.value <- quantile(data[[var]], q) 
	n <- match(q.value, data[[var]]) 
	t <- rep(NA, n)
	temp.frame <- data.frame(t, t, t, t)
	colnames(temp.frame) <- colnames(data)
	for (i in 1:n) {
		temp.frame[i, ] <- data[i, ]
	}
	
	return (temp.frame)
}

# загрузка данных
parse.file <- parse.csv(file.path=file.path, var1=26, var2=27, var3=28, profit=profit)
parse.file <- quant.file(data=parse.file, var=4, q=0.8)
#
#
cat(sep = "\n", "############",
				"Оптимальная торговля:", 
					"" )
cat("var1: ", ".......... ", file[1,1] , "\n")
cat("var2: ", ".......... ", file[1,2], "\n")
cat("var3: ", ".......... ", file[1,3], "\n")
cat("profit: ", ".......... ", file[1,4], "\n")

cat(sep = "\n", "############",
				"Строим график:", 
					"" )

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


