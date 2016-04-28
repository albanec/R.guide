var1 <- 2
var2 <- 12
profit	<- 6
file1.m <- 26
file2.m <- 9
file3.m <- 12
file4.m <- 4
file.path1 <- "/home/rs-evgeni/temp/t2/11-13.csv"
file.path2 <- "/home/rs-evgeni/temp/t2/14.csv"
file.path3 <- "/home/rs-evgeni/temp/t2/15.csv"
file.path4 <- "/home/rs-evgeni/temp/t2/16.csv"
#
file1 <- parse.csv(file.path=file.path1, var1=2, var2=12, profit=profit, sort=FALSE, var.names=FALSE)
file2 <- parse.csv(file.path=file.path2, var1=2, var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
file3 <- parse.csv(file.path=file.path3, var1=2, var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
file4 <- parse.csv(file.path=file.path4, var1=2, var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
#
file1[,4] <- rep(seq(1,18), nrow(file1)/18)
file2[,4] <- rep(seq(1,18), nrow(file2)/18)
file3[,4] <- rep(seq(1,18), nrow(file3)/18)
file4[,4] <- rep(seq(1,18), nrow(file4)/18)
#
file1 <- file1[which(file1[, 3] > 0),]
file2 <- file2[which(file2[, 3] > 0),]
file3 <- file3[which(file3[, 3] > 0),]
file4 <- file4[which(file4[, 3] > 0),]
#
file1[, 5] <- file1[, 3]*12 / abs(file1[, 2])*file1.m
file2[, 5] <- file2[, 3]*12 / abs(file2[, 2])*file2.m
file3[, 5] <- file3[, 3]*12 / abs(file3[, 2])*file3.m
file4[, 5] <- file4[, 3]*12 / abs(file4[, 2])*file4.m
#
mycolors <-  rainbow(30, start=0.3, end=0.95)
p1 <- plot_ly(x=file1[[4]], y=file1[[1]], mode="markers", color=file1[[5]], colors=mycolors) 
p2 <- plot_ly(x=file2[[4]], y=file2[[1]], mode="markers", color=file2[[5]], colors=mycolors) 
p3 <- plot_ly(x=file3[[4]], y=file3[[1]], mode="markers", color=file3[[5]], colors=mycolors) 
p4 <- plot_ly(x=file4[[4]], y=file4[[1]], mode="markers", color=file4[[5]], colors=mycolors) 
p <- subplot(p1, p2, p3, p4, nrows = 1)
#
BotReit <- function (data, n) {
	FirstTime <- TRUE
	for (i in seq(1, n)) {
		bot.reit <- 0
		bot.profit <- 0
		for (q in seq(1, nrow(data))) {
			if (data[q, 4] == i) {
				bot.reit <- as.numeric(bot.reit + 1)
				bot.profit <- as.numeric(bot.profit + data[q, 5])
			}
		}
		reit.row <- c(i, bot.reit, bot.profit)
		if (FirstTime == TRUE) {
			reit.table <- reit.row
			FirstTime <- FALSE
		} else {
			reit.table <- rbind(reit.table, reit.row)
		}
	}
	return(reit.table)
}
PerReit <- function (data, n) {
	FirstTime <- TRUE
	for (i in seq(1, n)) {
		per.reit <- 0
		per.profit <- 0
		for (q in seq(1, nrow(data))) {
			if (data[q, 1] == i) {
				per.reit <- as.numeric(per.reit + 1)
				per.profit <- as.numeric(per.profit + data[q, 5])
			}
		}
		reit.row <- c(i, per.reit, per.profit)
		if (FirstTime == TRUE) {
			reit.table <- reit.row
			FirstTime <- FALSE
		} else {
			reit.table <- rbind(reit.table, reit.row)
		}
	}
	return(reit.table)
}
#
file1.bot.reit.table <- BotReit(file1, 18)
file2.bot.reit.table <- BotReit(file2, 18)
file3.bot.reit.table <- BotReit(file3, 18)
file4.bot.reit.table <- BotReit(file4, 18)
#
file1.per.reit.table <- PerReit(file1, 130)
file2.per.reit.table <- PerReit(file2, 130)
file3.per.reit.table <- PerReit(file3, 130)
file4.per.reit.table <- PerReit(file4, 130)
#
bot.reit.p1 <- plot_ly(y=file1.bot.reit.table[, 2], x=file1.bot.reit.table[, 1], mode="markers",  color=file1.bot.reit.table[,3], colors=mycolors) 
bot.reit.p2 <- plot_ly(y=file2.bot.reit.table[, 2], x=file2.bot.reit.table[, 1], mode="markers",  color=file2.bot.reit.table[,3], colors=mycolors) 
bot.reit.p3 <- plot_ly(y=file3.bot.reit.table[, 2], x=file3.bot.reit.table[, 1], mode="markers",  color=file3.bot.reit.table[,3], colors=mycolors) 
bot.reit.p4 <- plot_ly(y=file4.bot.reit.table[, 2], x=file4.bot.reit.table[, 1], mode="markers",  color=file4.bot.reit.table[,3], colors=mycolors) 
per.reit.p1 <- plot_ly(y=file1.per.reit.table[, 1], x=file1.per.reit.table[, 2], mode="markers",  color=file1.per.reit.table[,3], colors=mycolors) 
per.reit.p2 <- plot_ly(y=file2.per.reit.table[, 1], x=file2.per.reit.table[, 2], mode="markers",  color=file2.per.reit.table[,3], colors=mycolors) 
per.reit.p3 <- plot_ly(y=file3.per.reit.table[, 1], x=file3.per.reit.table[, 2], mode="markers",  color=file3.per.reit.table[,3], colors=mycolors) 
per.reit.p4 <- plot_ly(y=file4.per.reit.table[, 1], x=file4.per.reit.table[, 2], mode="markers",  color=file4.per.reit.table[,3], colors=mycolors) 
reit.p <- subplot(bot.reit.p1, bot.reit.p2, bot.reit.p3, bot.reit.p4, per.reit.p1, per.reit.p2, per.reit.p3, per.reit.p4, nrows = 2)
#
all.p <- subplot(p1, p2, p3, p4, bot.reit.p1, bot.reit.p2, bot.reit.p3, bot.reit.p4, per.reit.p1, per.reit.p2, per.reit.p3, per.reit.p4, nrows = 3)
#
temp <- c (file1[which.max(file1[, 5]), 5], file2[which.max(file2[, 5]), 5], file3[which.max(file3[, 5]), 5], file4[which.max(file4[, 5]), 5])
file1[nrow(file1)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
file2[nrow(file2)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
file3[nrow(file3)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]]) 
file4[nrow(file4)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
#
file1 <- file1[-nrow(file1), ]
file2 <- file2[-nrow(file2), ]
file3 <- file3[-nrow(file3), ]
file4 <- file4[-nrow(file4), ]
#
temp <- c (mean(file1[, 5]), mean(file2[, 5]), mean(file3[, 5]), mean(file4[, 5]))
file1[nrow(file1)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
file2[nrow(file2)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
file3[nrow(file3)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]]) 
file4[nrow(file4)+1, ] <- c(0, 0, 0, 0, temp[[which.max(temp)]])
p1 <- plot_ly(x=file1[[4]], y=file1[[1]], mode="markers", color=file1[[5]], colors=mycolors) 
p2 <- plot_ly(x=file2[[4]], y=file2[[1]], mode="markers", color=file2[[5]], colors=mycolors) 
p3 <- plot_ly(x=file3[[4]], y=file3[[1]], mode="markers", color=file3[[5]], colors=mycolors) 
p4 <- plot_ly(x=file4[[4]], y=file4[[1]], mode="markers", color=file4[[5]], colors=mycolors) 
p <- subplot(p1, p2, p3, p4, nrows = 1)

temp <- c (mean(file1[, 5]), mean(file2[, 5]), mean(file3[, 5]), mean(file4[, 5]))
file1[nrow(file1)+1, ] <- c(0, 0, 0, 0, mean(temp))
file2[nrow(file2)+1, ] <- c(0, 0, 0, 0, mean(temp))
file3[nrow(file3)+1, ] <- c(0, 0, 0, 0, mean(temp)) 
file4[nrow(file4)+1, ] <- c(0, 0, 0, 0, mean(temp))
p1 <- plot_ly(x=file1[[4]], y=file1[[1]], mode="markers", color=file1[[5]], colors=mycolors) 
p2 <- plot_ly(x=file2[[4]], y=file2[[1]], mode="markers", color=file2[[5]], colors=mycolors) 
p3 <- plot_ly(x=file3[[4]], y=file3[[1]], mode="markers", color=file3[[5]], colors=mycolors) 
p4 <- plot_ly(x=file4[[4]], y=file4[[1]], mode="markers", color=file4[[5]], colors=mycolors) 
p <- subplot(p1, p2, p3, p4, nrows = 1)
