GEN_ParseCSV <- function (file.path=file.path, var.list 'var1, var2, var3', profit=profit, draw=draw, sort=FALSE, var.names=TRUE) {
    # ----------
    # Общее описание:
    #   функция для парсинга .csv файлов (заточена под выгрузку данных из WelsLAB & TSlab)
    # Входные данные:
    #   file.path: путь к файлу (абсолютный путь)
    #   var.list: лист с номерами столбцов нужных переменных (любое количество, функция подстраивается под нужное число)
    #   profit: номер столбца с профитом
    #   draw: номер столбца с данными по просадкам (нужно для нормирования доходностей)
    #   sort: нужна ли сортировка занчений по доходности
    #   var.names: выгрузить из исходного файла имена переменных
    # Выходные данные:
    #   data: выгруженные из .csv данные
    # ----------
    #
    # считывание файла 
    file <- read.table(file=file.path, header=F, sep = ";", as.is=T)   
    # чистим от лишнего 
    file <- file[-1,]
    file <- file[, colSums(is.na(file)) == 0]
    # temp дата-фрейм 
    # выделяем нужные параметры
    # profit/draw
    profit.name <- file[[1, profit]] 
    draw.name <- file[[1, draw]]

    n.vars <- length(var.list)
    for (i in 1:n.vars) {

        var1.name <- file[[1, var1]]    
    }
    
    var2.name <- file[[1, var2]]
    var3.name <- file[[1, var3]]
    # var3.name <- file[[1, var3]]
     
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