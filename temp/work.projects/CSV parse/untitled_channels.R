parse.csv <- function (file.path=file.path, var1, var2, var3, var4, var5, var6, var7, profit=profit, draw=draw, sort=FALSE, var.names=TRUE) {
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
    bot.num.table <- cbind(rep(0, nrow(m)), m)
    bot.num.table <- rbind(bot.num.table, cbind(rep(1, nrow(m)), m))
    bot.num.table <- rbind(bot.num.table, cbind(rep(2, nrow(m)), m))
    bot.num.table <- apply(bot.num.table, 1, paste, collapse='')
    bot.num.table <- cbind(seq(1, length(bot.num.table)), bot.num.table)
    return(bot.num.table)
}
#
bot.num.table <- BotBinNumGeneration(3)
#
BotNumSet <- function (file, bot.num.table) {
    file.temp <- file
    file.temp$var1 <- NULL
    file.temp$var2 <- NULL
    file.temp$var3 <- NULL
    file.temp$profit <- NULL
    file.temp$draw <- NULL
    file$var0 <- apply(file.temp, 1, paste, collapse='')   
    for (i in 1:nrow(bot.num.table)) {
        n <- which(file$var0 == bot.num.table[[i,2]])
        file$var0[n] <- bot.num.table[[i,1]]
    } 
    return (file$var0)
}
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
file1.1$var0 <- BotNumSet(file1.1, bot.num.table)
file2.1$var0 <- BotNumSet(file2.1, bot.num.table)
file3.1$var0 <- BotNumSet(file3.1, bot.num.table)
file4.1$var0 <- BotNumSet(file4.1, bot.num.table)
#
file1.2$var0 <- BotNumSet(file1.2, bot.num.table)
file2.2$var0 <- BotNumSet(file2.2, bot.num.table)
file3.2$var0 <- BotNumSet(file3.2, bot.num.table)
file4.2$var0 <- BotNumSet(file4.2, bot.num.table)
#
file1.3$var0 <- BotNumSet(file1.3, bot.num.table)
file2.3$var0 <- BotNumSet(file2.3, bot.num.table)
file3.3$var0 <- BotNumSet(file3.3, bot.num.table)
file4.3$var0 <- BotNumSet(file4.3, bot.num.table)
#
DuplicatedRowFilter <- function (file) {
    file.temp <- file
    file.temp$profit <- NULL
    file.temp$draw <- NULL
    file.temp$var0 <- NULL
    file.temp$var0 <- apply(file.temp, 1, paste, collapse='')
    file <- file[which(duplicated(file.temp$var0) == FALSE), ]
    return (file)
}
#
file1.1 <- DuplicatedRowFilter(file1.1)
file2.1 <- DuplicatedRowFilter(file2.1)
file3.1 <- DuplicatedRowFilter(file3.1)
file4.1 <- DuplicatedRowFilter(file4.1)
#
file1.2 <- DuplicatedRowFilter(file1.2)
file2.2 <- DuplicatedRowFilter(file2.2)
file3.2 <- DuplicatedRowFilter(file3.2)
file4.2 <- DuplicatedRowFilter(file4.2)
#
file1.3 <- DuplicatedRowFilter(file1.3)
file2.3 <- DuplicatedRowFilter(file2.3)
file3.3 <- DuplicatedRowFilter(file3.3)
file4.3 <- DuplicatedRowFilter(file4.3)
#
ProfitNorm <- function (file, m) {
    file$profit.norm <- (file$profit*12) / (abs(file$draw)*m)
    return (file$profit.norm)
}
# 
file1.1$profit.norm <- ProfitNorm(file1.1, m = 26)
file2.1$profit.norm <- ProfitNorm(file2.1, m = 9)
file3.1$profit.norm <- ProfitNorm(file3.1, m = 12)
file4.1$profit.norm <- ProfitNorm(file4.1, m = 4)
#
file1.2$profit.norm <- ProfitNorm(file1.2, m = 26)
file2.2$profit.norm <- ProfitNorm(file2.2, m = 9)
file3.2$profit.norm <- ProfitNorm(file3.2, m = 12)
file4.2$profit.norm <- ProfitNorm(file4.2, m = 4)
#
file1.3$profit.norm <- ProfitNorm(file1.3, m = 26)
file2.3$profit.norm <- ProfitNorm(file2.3, m = 9)
file3.3$profit.norm <- ProfitNorm(file3.3, m = 12)
file4.3$profit.norm <- ProfitNorm(file4.3, m = 4)
#
file1.1 <- file1.1[which(file1.1$profit.norm > 0),]
file2.1 <- file2.1[which(file2.1$profit.norm > 0),]
file3.1 <- file3.1[which(file3.1$profit.norm > 0),]
file4.1 <- file3.1[which(file4.1$profit.norm > 0),]
#
file1.2 <- file1.2[which(file1.2$profit.norm > 0),]
file2.2 <- file2.2[which(file2.2$profit.norm > 0),]
file3.2 <- file3.2[which(file3.2$profit.norm > 0),]
file4.2 <- file4.2[which(file4.2$profit.norm > 0),]
#
file1.3 <- file1.3[which(file1.3$profit.norm > 0),]
file2.3 <- file2.3[which(file2.3$profit.norm > 0),]
file3.3 <- file3.3[which(file3.3$profit.norm > 0),]
file4.3 <- file4.3[which(file4.3$profit.norm > 0),]
#
mycolors <-  rainbow(30, start=0.3, end=0.95)
#
p1.1 <- plot_ly(x=file1.1[[1]], y=file1.1[[2]], z=file1.1[[3]], size=file1.1[[10]], 
                 type="scatter3d", mode="markers", color=file1.1[[11]], 
                 colors=mycolors)
    p2.1 <- plot_ly(x=file2.1[[1]], y=file2.1[[2]], z=file2.1[[3]], size=as.numeric(file2.1[[10]]), 
                   type="scatter3d", mode="markers", color=file2.1[[11]], 
                     colors=mycolors)

        p3.1 <- plot_ly(x=file3.1[[1]], y=file3.1[[2]], z=file3.1[[3]], size=as.numeric(file3.1[[10]]), 
                   type="scatter3d", mode="markers", color=file3.1[[11]], 
                     colors=mycolors)
p4.1 <- plot_ly(x=file4.1[[1]], y=file4.1[[2]], z=file4.1[[3]], size=as.numeric(file4.1[[10]]), 
                   type="scatter3d", mode="markers", color=file4.1[[11]], 
                     colors=mycolors)

    #
    p1.2 <- plot_ly(x=file1.2[[1]], y=file1.2[[2]], z=file1.2[[3]], size=as.numeric(file1.2[[10]]), 
                   type="scatter3d", mode="markers", color=file1.2[[11]], 
                     colors=mycolors)
        p3.2 <- plot_ly(x=file3.2[[1]], y=file3.2[[2]], z=file3.2[[3]], size=as.numeric(file3.2[[10]]), 
                   type="scatter3d", mode="markers", color=file3.2[[11]], 
                     colors=mycolors)
            p4.2 <- plot_ly(x=file4.2[[1]], y=file4.2[[2]], z=file4.2[[3]], size=as.numeric(file4.2[[10]]), 
                       type="scatter3d", mode="markers", color=file4.2[[11]], 
                         colors=mycolors)

    p1.3 <- plot_ly(x=file1.3[[1]], y=file1.3[[2]], z=file1.3[[3]], size=as.numeric(file1.3[[10]]), 
                   type="scatter3d", mode="markers", color=file1.3[[11]], 
                     colors=mycolors)

    p2.3 <- plot_ly(x=file2.3[[1]], y=file2.3[[2]], z=file2.3[[3]], size=as.numeric(file2.3[[10]]), 
                   type="scatter3d", mode="markers", color=file2.3[[11]], 
                     colors=mycolors)
    p3.3 <- plot_ly(x=file3.3[[1]], y=file3.3[[2]], z=file3.3[[3]], size=as.numeric(file3.3[[10]]), 
                   type="scatter3d", mode="markers", color=file3.3[[11]], 
                     colors=mycolors)
    p4.3 <- plot_ly(x=file4.3[[1]], y=file4.3[[2]], z=file4.3[[3]], size=as.numeric(file4.3[[10]]), 
                   type="scatter3d", mode="markers", color=file4.3[[11]], 
                     colors=mycolors)

BotReit <- function (file, bot.num.table) {
    FirstTime <- TRUE
    for (i in 1:nrow(bot.num.table)) {
        bot.reit <- 0
        bot.sum <- 0
        for (n in 1:nrow(file)){
            if (file$var0[n] == bot.num.table[i,1]) {
                bot.reit <- bot.reit + 1
                bot.sum <- bot.sum + file$profit.norm[n]
            }   
        }
        x <- c(i, bot.reit, bot.sum)
        if (FirstTime == TRUE) {
                table <- x 
                FirstTime <- FALSE
        } else {
            table <- cbind(table, x)
        }        
    }
    table <- t(table)
    return (table)
}
#
br1.1 <- BotReit(file1.1, bot.num.table)
br2.1 <- BotReit(file2.1, bot.num.table)
br3.1 <- BotReit(file3.1, bot.num.table)
br4.1 <- BotReit(file4.1, bot.num.table)
#
br1.2 <- BotReit(file1.2, bot.num.table)
br2.2 <- BotReit(file2.2, bot.num.table)
br3.2 <- BotReit(file3.2, bot.num.table)
br4.2 <- BotReit(file4.2, bot.num.table)
#
br1.3 <- BotReit(file1.3, bot.num.table)
br2.3 <- BotReit(file2.3, bot.num.table)
br3.3 <- BotReit(file3.3, bot.num.table)
br4.3 <- BotReit(file4.3, bot.num.table)
#
PerReit <- function (file) {
    FirstTime <- TRUE
    for (i in 1:300) {
        bot.reit <- 0
    bot.sum <- 0
        for (n in 1:nrow(file)) {
            if (i == file$var3[n]) {
                bot.reit <- bot.reit + 1
                bot.sum <- bot.sum + file$profit.norm[n]
            }
        }        
        x <- c(i, bot.reit, bot.sum)
        if (FirstTime == TRUE) {
            table <- x 
            FirstTime <- FALSE
        } else {
            table <- cbind(table, x)
        }
    }       
    table <- t(table)
    return (table)
}
#
pr1.1 <- PerReit(file1.1)
pr2.1 <- PerReit(file2.1)
pr3.1 <- PerReit(file3.1)
pr4.1 <- PerReit(file4.1)
#
pr1.2 <- PerReit(file1.2)
pr2.2 <- PerReit(file2.2)
pr3.2 <- PerReit(file3.2)
pr4.2 <- PerReit(file4.2)
#
pr1.3 <- PerReit(file1.3)
pr2.3 <- PerReit(file2.3)
pr3.3 <- PerReit(file3.3)
pr4.3 <- PerReit(file4.3)
#
pbr1.1 <- plot_ly(x=br1.1[,1], y=br1.1[,2], mode="markers", color=br1.1[,3], colors=mycolors)
pbr2.1 <- plot_ly(x=br2.1[,1], y=br2.1[,2], mode="markers", color=br2.1[,3], colors=mycolors)
pbr3.1 <- plot_ly(x=br3.1[,1], y=br3.1[,2], mode="markers", color=br3.1[,3], colors=mycolors)
pbr4.1 <- plot_ly(x=br4.1[,1], y=br4.1[,2], mode="markers", color=br4.1[,3], colors=mycolors)
#
pbr1.2 <- plot_ly(x=br1.2[,1], y=br1.2[,2], mode="markers", color=br1.2[,3], colors=mycolors)
pbr2.2 <- plot_ly(x=br2.2[,1], y=br2.2[,2], mode="markers", color=br2.2[,3], colors=mycolors)
pbr3.2 <- plot_ly(x=br3.2[,1], y=br3.2[,2], mode="markers", color=br3.2[,3], colors=mycolors)
pbr4.2 <- plot_ly(x=br4.2[,1], y=br4.2[,2], mode="markers", color=br4.2[,3], colors=mycolors)
#
pbr1.3 <- plot_ly(x=br1.1[,1], y=br1.2[,2], mode="markers", color=br1.3[,3], colors=mycolors)
pbr2.3 <- plot_ly(x=br2.1[,1], y=br2.2[,2], mode="markers", color=br2.3[,3], colors=mycolors)
pbr3.3 <- plot_ly(x=br3.1[,1], y=br3.2[,2], mode="markers", color=br3.3[,3], colors=mycolors)
pbr4.3 <- plot_ly(x=br4.1[,1], y=br4.2[,2], mode="markers", color=br4.3[,3], colors=mycolors)
#
ppr1.1 <- plot_ly(x=pr1.1[,2], y=pr1.1[,1], mode="markers", color=pr1.1[,3], colors=mycolors)
ppr2.1 <- plot_ly(x=pr2.1[,2], y=pr2.1[,1], mode="markers", color=pr2.1[,3], colors=mycolors)
ppr3.1 <- plot_ly(x=pr3.1[,2], y=pr3.1[,1], mode="markers", color=pr3.1[,3], colors=mycolors)
ppr4.1 <- plot_ly(x=pr4.1[,2], y=pr4.1[,1], mode="markers", color=pr4.1[,3], colors=mycolors)
#
ppr1.2 <- plot_ly(x=pr1.1[,2], y=pr1.2[,1], mode="markers", color=pr1.2[,3], colors=mycolors)
ppr2.2 <- plot_ly(x=pr2.1[,2], y=pr2.2[,1], mode="markers", color=pr2.2[,3], colors=mycolors)
ppr3.2 <- plot_ly(x=pr3.1[,2], y=pr3.2[,1], mode="markers", color=pr3.2[,3], colors=mycolors)
ppr4.2 <- plot_ly(x=pr4.1[,2], y=pr4.2[,1], mode="markers", color=pr4.2[,3], colors=mycolors)
#
ppr1.3 <- plot_ly(x=pr1.1[,2], y=pr1.3[,1], mode="markers", color=pr1.3[,3], colors=mycolors)
ppr2.3 <- plot_ly(x=pr2.1[,2], y=pr2.3[,1], mode="markers", color=pr2.3[,3], colors=mycolors)
ppr3.3 <- plot_ly(x=pr3.1[,2], y=pr3.3[,1], mode="markers", color=pr3.3[,3], colors=mycolors)
ppr4.3 <- plot_ly(x=pr4.1[,2], y=pr4.3[,1], mode="markers", color=pr4.3[,3], colors=mycolors)
#
plot1 <- subplot(pbr1.1, pbr2.1, pbr3.1, pbr4.1, ppr1.1, ppr2.1, ppr3.1, ppr4.1, nrows=2)
plot2 <- subplot(pbr1.2, pbr2.2, pbr3.2, pbr4.2, ppr1.2, ppr2.2, ppr3.2, ppr4.2, nrows=2)
plot3 <- subplot(pbr1.3, pbr2.3, pbr3.3, pbr4.3, ppr1.3, ppr2.3, ppr3.3, ppr4.3, nrows=2)

