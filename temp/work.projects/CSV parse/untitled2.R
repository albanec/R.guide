	file11 <- parse.csv(file.path=file.path1, var1=2 ,var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
	file22 <- parse.csv(file.path=file.path2, var1=2, var2=12, profit=profit, sort=FALSE, var.names=FALSE)
	file33 <- parse.csv(file.path=file.path3, var1=2,var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
	file44 <- parse.csv(file.path=file.path4, var1=2, var2=12, profit=profit, sort=FALSE, var.names=FALSE)
	#
	file1.temp <- parse.csv(file.path=file.path1, var1, var2, var3, var4, var5, profit=recover, sort=FALSE, var.names=FALSE)
	file2.temp <- parse.csv(file.path=file.path2, var1, var2, var3, var4, var5, profit=recover, sort=FALSE, var.names=FALSE)
	file3.temp <- parse.csv(file.path=file.path3, var1, var2, var3, var4, var5, profit=recover, sort=FALSE, var.names=FALSE)
	file4.temp <- parse.csv(file.path=file.path4, var1, var2, var3, var4, var5, profit=recover, sort=FALSE, var.names=FALSE)
	#
	file1$recovery <- file1.temp$profit
	file2$recovery <- file2.temp$profit
	file3$recovery <- file3.temp$profit
	file4$recovery <- file4.temp$profit
	remove(file1.temp); remove(file2.temp); remove(file3.temp); remove(file4.temp)



	file11[, 5] <- file11[, 3]/abs(file11[, 2])*3

per.score1 <- PerReit(file11, 130)
per.score2 <- PerReit(file22, 130)
per.score3 <- PerReit(file33, 130)
per.score4 <- PerReit(file44, 130)

p111 <- plot_ly(x=per.score1[[2]], y=per.score1[[1]], mode = "markers",color=per.score1[[3]], colors=mycolors)
p222 <- plot_ly(x=per.score2[[2]], y=per.score2[[1]], mode="markers",color=per.score2[[3]], colors=mycolors)
p333 <- plot_ly(x=per.score3[[2]], y=per.score3[[1]], mode="markers",color=per.score3[[3]], colors=mycolors)
p444 <- plot_ly(x=per.score4[[2]], y=per.score4[[1]], mode="markers",color=per.score4[[3]], colors=mycolors)

#
file11 <- parse.csv(file.path=file.path1, var1=2 ,var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
file22 <- parse.csv(file.path=file.path2, var1=2, var2=12, profit=profit, sort=FALSE, var.names=FALSE)
file33 <- parse.csv(file.path=file.path3, var1=2,var2=12,  profit=profit, sort=FALSE, var.names=FALSE)
file44 <- parse.csv(file.path=file.path4, var1=2, var2=12, profit=profit, sort=FALSE, var.names=FALSE)
file11[,4] <- rep(seq(1,18), nrow(file11)/18)
file22[,4] <- rep(seq(1,18), nrow(file22)/18)
file33[,4] <- rep(seq(1,18), nrow(file33)/18)
file44[,4] <- rep(seq(1,18), nrow(file44)/18)
file11 <- file11[which(file1[, 3]>0),]
file22 <- file22[which(file22[, 3]>0),]
file33 <- file33[which(file33[, 3]>0),]
file44 <- file44[which(file44[, 3]>0),]
file11[, 5] <- file11[, 3]/abs(file11[, 2])*3
file22[, 5] <- file22[, 3]/abs(file22[, 2])
file33[, 5] <- file33[, 3]/abs(file33[, 2])
file44[, 5] <- file44[, 3]*3/abs(file44[, 2])