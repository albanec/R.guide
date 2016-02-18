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

	cat(sep = "\n", "############",
						"Готово.", 
					"############" )
	
	return (temp.frame)
}	

# загрузка данных
file <- parse.csv(file.path=file.path, var1=var1, var2=var2, var3=var3, profit=profit)
# сортировка по профиту
file <- file[order(-file$profit),]

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
mycolors <-  rainbow(30, start=0.15, end=0.95)
plot_ly(x=file2$var1, y=file2$var2, z=file2$var3, type="scatter3d", mode="markers", color=file2$profit, colors=mycolors) %>%
	layout(title = "Тепловая карта",
         scene = list(
           xaxis = list(title = var1.name), 
           yaxis = list(title = var2.name), 
           zaxis = list(title = var3.name)
           )
         )


