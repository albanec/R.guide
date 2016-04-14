var1<-2; var2<-3; var3<-4; var4<-5; var5<-6
# парсинг данных и подготовка
	file1 <- parse.csv(file.path=file.path1, var1, var2, var3, var4,var5, profit=profit, sort=FALSE, var.names=FALSE)
	file2 <- parse.csv(file.path=file.path2, var1, var2, var3, var4, var5, profit=profit, sort=FALSE, var.names=FALSE)
	file3 <- parse.csv(file.path=file.path3, var1, var2, var3, var4, var5, profit=profit, sort=FALSE, var.names=FALSE)
	file4 <- parse.csv(file.path=file.path4, var1, var2, var3, var4, var5, profit=profit, sort=FALSE, var.names=FALSE)
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
# обработка по equity
	temp.data1 <- compare.df(file1, file2, rec=TRUE, p.diff=TRUE)
	temp.data2 <- compare.df(file2, file3, rec=TRUE, p.diff=TRUE)
	temp.data3 <- compare.df(file3, file4, rec=TRUE, p.diff=TRUE)
	temp.data4 <- compare.df(file1, file4, rec=TRUE, p.diff=TRUE)
	temp.data1 <- quant.file(data=temp.data1, var=10, q.low=0.2, low=TRUE)
	temp.data2 <- quant.file(data=temp.data2, var=10, q.low=0.2, low=TRUE)
	temp.data3 <- quant.file(data=temp.data3, var=10, q.low=0.2, low=TRUE)
	temp.data4 <- quant.file(data=temp.data4, var=10, q.low=0.2, low=TRUE)
	temp.data1 <- temp.data1[order(-temp.data1$profit.sum),]
	temp.data2 <- temp.data2[order(-temp.data2$profit.sum),]
	temp.data3 <- temp.data3[order(-temp.data3$profit.sum),]
	temp.data4 <- temp.data4[order(-temp.data4$profit.sum),]
temp.t1 <- nrow(temp.data1) 
		temp.frame1 <- rep(NA, temp.t1)
		temp.frame1 <- data.frame(temp.frame1)
		temp.frame1$var2 <- temp.data1$var2
		temp.frame1$var3 <- temp.data1$var3
		temp.frame1$var4 <- temp.data1$var4
		temp.frame1$var5 <- temp.data1$var5
		temp.frame1 <- temp.frame1[-1]
		temp.frame1$var0 <- apply(temp.frame1, 1, paste, collapse='')
		temp.frame1$var0  <- as.numeric(temp.frame1$var0)
		temp.data1$var0 <- temp.frame1$var0
		
	temp.t2 <- nrow(temp.data2) 
		temp.frame2 <- rep(NA, temp.t2)
		temp.frame2 <- data.frame(temp.frame2)
		temp.frame2$var2 <- temp.data2$var2
		temp.frame2$var3 <- temp.data2$var3
		temp.frame2$var4 <- temp.data2$var4
		temp.frame2$var5 <- temp.data2$var5
		temp.frame2 <- temp.frame2[-1]
		temp.frame2$var0 <- apply(temp.frame2, 1, paste, collapse='')
		temp.frame2$var0  <- as.numeric(temp.frame2$var0)
		temp.data2$var0 <- temp.frame2$var0
		remove(temp.frame2)
temp.t1 <- nrow(temp.data3) 
		temp.frame1 <- rep(NA, temp.t1)
		temp.frame1 <- data.frame(temp.frame1)
		temp.frame1$var2 <- temp.data3$var2
		temp.frame1$var3 <- temp.data3$var3
		temp.frame1$var4 <- temp.data3$var4
		temp.frame1$var5 <- temp.data3$var5
		temp.frame1 <- temp.frame1[-1]
		temp.frame1$var0 <- apply(temp.frame1, 1, paste, collapse='')
		temp.frame1$var0  <- as.numeric(temp.frame1$var0)
		temp.data3$var0 <- temp.frame1$var0
		remove(temp.frame1)
temp.t1 <- nrow(temp.data4) 
		temp.frame1 <- rep(NA, temp.t1)
		temp.frame1 <- data.frame(temp.frame1)
		temp.frame1$var2 <- temp.data4$var2
		temp.frame1$var3 <- temp.data4$var3
		temp.frame1$var4 <- temp.data4$var4
		temp.frame1$var5 <- temp.data4$var5
		temp.frame1 <- temp.frame1[-1]
		temp.frame1$var0 <- apply(temp.frame1, 1, paste, collapse='')
		temp.frame1$var0  <- as.numeric(temp.frame1$var0)
		temp.data4$var0 <- temp.frame1$var0
		remove(temp.frame1)
#
# данные для графиков
	mycolors <-  rainbow(30, start=0.3, end=0.95)
	var1.name <- colnames(temp.data1)[1]
	var2.name <- colnames(temp.data1)[2]
	var3.name <- colnames(temp.data1)[3]
	var4.name <- colnames(temp.data1)[4]
	var5.name <- colnames(temp.data1)[5]
# график (profit без заморочек)mode="markers"
	p1 <- plot_ly(x=temp.data[[12]], y=temp.data[[1]], mode="markers",color=temp.data[[11]], colors=mycolors) %>% 
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
	temp.data1 <- quant.file(data=temp.data1, var=10, q.hi=0.9, hi=TRUE)
	temp.data2 <- quant.file(data=temp.data2, var=10, q.hi=0.9, hi=TRUE)
	temp.data3 <- quant.file(data=temp.data3, var=10, q.hi=0.9, hi=TRUE)
	temp.data4 <- quant.file(data=temp.data4, var=10, q.hi=0.9, hi=TRUE)
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
