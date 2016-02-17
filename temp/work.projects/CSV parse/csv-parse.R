library("quantmod")

# Парсинг .CSV файлов для оптимизации
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
		cat("Путь к файлy 1: ", "\n")
		file.path1 <- readline()
		cat("Путь к файлy 1: ", ".......... ", file.path1, "\n")
		cat("Путь к файлy 2: ", "\n")
		file.path2 <- readline()
		cat("Путь к файлy 2: ", ".......... ", file.path2, "\n")
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


parse.csv <- function (file.path1, file.path2, var1=26, var2=27, var3=28, profit) {
	# подгрузка файлов 
		file1 <- read.table(file=file.path1, header=F, sep = ";", as.is=T) 
		file2 <- read.table(file=file.path2, header=F, sep = ";", as.is=T) 
		temp.t <- c( nrow(file1), nrow(file2) )
		temp.frame <- rep(NA, max(temp.t))
		temp.frame <- data.frame(temp.frame) 

	# выделядем нужные параметры
		var1.name <- file1[[1, var1]]
		var2.name <- file1[[1, var2]]
		var3.name <- file1[[1, var3]]
		profit.name <- file1[[1, profit]] 
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
		temp.file1 <- file1
		temp.file2 <- file2
		temp.file1[1, ] <- NA
		temp.file2[1, ] <- NA
		temp.frame$file1.var1 <- temp.file1[var1] 
		temp.frame$file1.var2 <- temp.file1[var2] 
		temp.frame$file1.var3 <- temp.file1[var3]
		temp.frame$file1.profit <- temp.file1[profit]
		temp.frame$file2.var1 <- temp.file2[var1]
		temp.frame$file2.var2 <- temp.file2[var2]
		temp.frame$file2.var3 <- temp.file2[var3]
		temp.frame$file2.profit <- temp.file2[profit]
	return (temp.frame)
}	

	

