function (file.path=file.path, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=TRUE) {
    # подгрузка файлов 
    file <- read.table(file=file.path, header=F, sep = ",", as.is=T)   
    # выделяем нужные параметры
    var1.name <- file[[1, var1]]
    var2.name <- file[[1, var2]]
    var3.name <- file[[1, var3]]
    var4.name <- file[[1, var4]]
    var5.name <- file[[1, var5]]
    var6.name <- file[[1, var6]]
    var7.name <- file[[1, var7]]
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
    cat("var4: ", ".......... ", var4.name, "\n")
    cat("var5: ", ".......... ", var5.name, "\n")
    cat("var6  ", ".......... ", var6.name, "\n")
    cat("var7  ", ".......... ", var7.name, "\n")
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
    temp.frame$var4 <- as.numeric( gsub("\\,", ".", file[[var4]]) )
    temp.frame$var4 <- as.numeric( gsub("\\s", "", temp.frame$var4) )
    temp.frame$var5 <- as.numeric( gsub("\\,", ".", file[[var5]]) )
    temp.frame$var5 <- as.numeric( gsub("\\s", "", temp.frame$var5) )
    temp.frame$var6 <- as.numeric( gsub("\\,", ".", file[[var6]]) )
    temp.frame$var6 <- as.numeric( gsub("\\s", "", temp.frame$var6) )
    temp.frame$var7 <- as.numeric( gsub("\\,", ".", file[[var7]]) )
    temp.frame$var7 <- as.numeric( gsub("\\s", "", temp.frame$var7) )
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
        colnames(temp.frame) <- c(var1.name, var2.name, var3.name, var4.name, var5.name, var6.name, var7.name,profit.name)	
        cat( "############", "\n",
             "Столбцы проименованы", "\n")
    }
    cat(sep = "\n", "############",
        "Готово.", 
        "############" )
    
    return (temp.frame)
}
#
var1 <- 2; var2 <- 3; var3 <- 4; var4 <- 5; var5 <- 6; var6 <- 7; var7 <- 8
profit <- 9
draw <- 15
#
file.path1.1 <- "/home/rs-evgeni/temp/t3/11_13.csv"
file.path2.1 <- "/home/rs-evgeni/temp/t3/14.csv"
file.path3.1 <- "/home/rs-evgeni/temp/t3/15.csv"
file.path4.1 <- "/home/rs-evgeni/temp/t3/16.csv"
#
file.path1.2 <- "/home/rs-evgeni/temp/t3/11_13_2.csv"
file.path2.2 <- "/home/rs-evgeni/temp/t3/14_2.csv"
file.path3.2 <- "/home/rs-evgeni/temp/t3/15_2.csv"
file.path4.2 <- "/home/rs-evgeni/temp/t3/16_2.csv"
#
file.path1.3 <- "/home/rs-evgeni/temp/t3/11_13_3.csv"
file.path2.3 <- "/home/rs-evgeni/temp/t3/14_3.csv"
file.path3.3 <- "/home/rs-evgeni/temp/t3/15_3.csv"
file.path4.3 <- "/home/rs-evgeni/temp/t3/16_3.csv"
#
BotBinNumGeneration <- function (n) {
    decimals <- seq(0, 2^n-1)
    m <- sapply(decimals,function(x){ as.integer(intToBits(x))})
    m <- head(m ,3)
    m <- t(head(m, n))
    return(m)
}
#
m <- BotBinNumGeneration(3)
bot.num.table <- cbind(rep(1, 8), m)
bot.num.table <- rbind(x, cbind(rep(2, 8), m))
bot.num.table <- apply(bot.num.table, 1, paste, collapse='')
bot.num.table <- as.numeric(bot.num.table)
#
file1.1 <- parse.csv(file.path=file.path1.1, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file2.1 <- parse.csv(file.path=file.path2.1, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file3.1 <- parse.csv(file.path=file.path3.1, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file4.1 <- parse.csv(file.path=file.path4.1, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
#
file1.2 <- parse.csv(file.path=file.path1.2, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file2.2 <- parse.csv(file.path=file.path2.2, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file3.2 <- parse.csv(file.path=file.path3.2, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file4.2 <- parse.csv(file.path=file.path4.2, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
#
file1.3 <- parse.csv(file.path=file.path1.3, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file2.3 <- parse.csv(file.path=file.path2.3, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file3.3 <- parse.csv(file.path=file.path3.3, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
file4.3 <- parse.csv(file.path=file.path4.3, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=FALSE)
#

