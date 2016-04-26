var1<-2; var2<-3; var3<-4; var4<-5
profit <- 6
recover <- 14
file.path1 <- "/home/evgeni/R/temp/t1/11_13.csv"
file.path2 <- "/home/evgeni/R/temp/t1/14.csv"
file.path3 <- "/home/evgeni/R/temp/t1/15.csv"
file.path4 <- "/home/evgeni/R/temp/t1/16.csv"

# парсинг данных и подготовка
	file1 <- parse.csv(file.path=file.path1, var1, var2, var3, var4, profit=profit, sort=FALSE, var.names=FALSE)
	file2 <- parse.csv(file.path=file.path2, var1, var2, var3, var4, profit=profit, sort=FALSE, var.names=FALSE)
	file3 <- parse.csv(file.path=file.path3, var1, var2, var3, var4, profit=profit, sort=FALSE, var.names=FALSE)
	file4 <- parse.csv(file.path=file.path4, var1, var2, var3, var4, profit=profit, sort=FALSE, var.names=FALSE)
	#
	file1.temp <- parse.csv(file.path=file.path1, var1, var2, var3, var4, profit=recover, sort=FALSE, var.names=FALSE)
	file2.temp <- parse.csv(file.path=file.path2, var1, var2, var3, var4, profit=recover, sort=FALSE, var.names=FALSE)
	file3.temp <- parse.csv(file.path=file.path3, var1, var2, var3, var4, profit=recover, sort=FALSE, var.names=FALSE)
	file4.temp <- parse.csv(file.path=file.path4, var1, var2, var3, var4, profit=recover, sort=FALSE, var.names=FALSE)
	#
	file1$recovery <- file1.temp$profit
	file2$recovery <- file2.temp$profit
	file3$recovery <- file3.temp$profit
	file4$recovery <- file4.temp$profit
	remove(file1.temp); remove(file2.temp); remove(file3.temp); remove(file4.temp)
# нумерация ботов 
	bot.numbers <- function (data, n) {
		for (i in seq(1, nrow(data))) {
			data$b.num <- rep(seq(1:n), nrow(data)/n)
		}
		return (data)
	}
	file1 <- bot.numbers(file1, n=18)
	file2 <- bot.numbers(file2, n=18)
	file3 <- bot.numbers(file3, n=18)
	file4 <- bot.numbers(file4, n=18)
# обработка по equity
	temp.data1 <- compare.df(file1, file2, rec=TRUE, p.diff=TRUE)
	temp.data2 <- compare.df(file2, file3, rec=TRUE, p.diff=TRUE)
	temp.data3 <- compare.df(file3, file4, rec=TRUE, p.diff=TRUE)
	temp.data4 <- compare.df(file1, file4, rec=TRUE, p.diff=TRUE)
	#
	temp.data1 <- quant.file(data=temp.data1, var=7, q.low=0.2, low=TRUE)
	temp.data2 <- quant.file(data=temp.data2, var=7, q.low=0.2, low=TRUE)
	temp.data3 <- quant.file(data=temp.data3, var=7, q.low=0.2, low=TRUE)
	temp.data4 <- quant.file(data=temp.data4, var=7, q.low=0.2, low=TRUE)
	temp.data1 <- temp.data1[order(-temp.data1$profit.sum),]
	temp.data2 <- temp.data2[order(-temp.data2$profit.sum),]
	temp.data3 <- temp.data3[order(-temp.data3$profit.sum),]
	temp.data4 <- temp.data4[order(-temp.data4$profit.sum),]
#
# данные для графиков
	mycolors <-  rainbow(30, start=0.3, end=0.95)
	var1.name <- colnames(temp.data1)[1]
	var2.name <- colnames(temp.data1)[2]
	var3.name <- colnames(temp.data1)[3]
	var4.name <- colnames(temp.data1)[4]
	var5.name <- colnames(temp.data1)[5]
# график (profit без заморочек)mode="markers"
	p1 <- plot_ly(x=temp.data[[2]], y=temp.data[[1]], mode="markers",color=temp.data[[11]], colors=mycolors) %>% 
		layout(title = "Profit без учета recovery", 
			p1, xaxis = list(type = "log"),
    	   	yaxis = list(type = "log"))
        	)
# учет recovery
	temp.data2 <- temp.data
	temp.data1$recovery.diff <- abs(temp.data1$recovery - temp.data1$recovery2)
	temp.data2$recovery.diff <- abs(temp.data2$recovery - temp.data2$recovery2)
	temp.data3$recovery.diff <- abs(temp.data3$recovery - temp.data3$recovery2)
	temp.data4$recovery.diff <- abs(temp.data4$recovery - temp.data4$recovery2)
	# сглаженность данных
	temp.data1 <- quant.file(data=temp.data1, var=7, q.hi=0.9, hi=TRUE)
	temp.data2 <- quant.file(data=temp.data2, var=7, q.hi=0.9, hi=TRUE)
	temp.data3 <- quant.file(data=temp.data3, var=7, q.hi=0.9, hi=TRUE)
	temp.data4 <- quant.file(data=temp.data4, var=7, q.hi=0.9, hi=TRUE)
	# 
	temp.data1 <- quant.file(data=temp.data1, var=7, q.hi=0.9, hi=TRUE, abs=TRUE)
	temp.data2 <- quant.file(data=temp.data2, var=7, q.hi=0.5, hi=TRUE, abs=TRUE)
	temp.data3 <- quant.file(data=temp.data3, var=7, q.hi=0.5, hi=TRUE, abs=TRUE)
	temp.data4 <- quant.file(data=temp.data4, var=7, q.hi=0.5, hi=TRUE, abs=TRUE)
	temp.data1 <- temp.data1[order(-temp.data1$profit.sum),]
	temp.data2 <- temp.data2[order(-temp.data2$profit.sum),]
	temp.data3 <- temp.data3[order(-temp.data3$profit.sum),]
	temp.data4 <- temp.data4[order(-temp.data4$profit.sum),]
# график (с учётом recovery)
	p2 <- plot_ly(x=temp.data[[1]], y=temp.data[[2]], z=temp.data[[3]], type="scatter3d", mode="markers", color=temp.data[[9]], colors=mycolors) %>% 
		layout(title = "Profit без учета recovery", 
			scene = list( 
				xaxis = list(title = "var1.name" ),
       	    	yaxis = list(title = "var2.name" ), 
       	    	zaxis = list(title = "var3.name" )
       	  		) 
        	)
