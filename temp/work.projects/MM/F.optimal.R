
# функция генерации рандомного ряда Бернули (вектор сделок)
X <- rbinom(X.length, 1, X.win.rate)

# функция генерации ряда сделок (с max вставками убытков/прибылей)
pos.generate <- function (X.length, X.win.rate, X.win.length, X.loose.length) {
		# генерация ряда Бернули
		X <- rbinom(X.length, 1, X.win.rate)
		# цикл проверки на вставки win/loose
		for ( i in 1:(X.length-1) ) {
	  		# сравнение значений
	  			if (X[i] - X[i+1] == 0) {
	  			# для win сделок
	  				if (X[i] > 0) {
	  					for (y in 1:X.win.length) {
	  						ifelse (is.na(X[i+y]), break, 0)
	  						if ( (X[i]-X[i+y]) != 0 ) break
	  							else {
	  								if(y==X.win.length) {
	  									X[i+X.win.length] <- 0
	  								}	  							
	  							} 
	  					} 
	  				} 
	  				# для loose сделок
	  					else {
	  						for (y in 1:X.loose.length) {
	  							ifelse (is.na(X[i+y]), break, 0)
	  							if ( (X[i]-X[i+y]) != 0 ) break
	  								else {
	  									if(y==X.loose.length) {
	  										X[i+X.loose.length] <- 1
	  									}
	  								} 
	  						}
	  					}
	  				} 
		}	
	return(X)
}

# функция генерации ряда сделок (с max вставками убытков/прибылей и сохранением требуемого win.rate)
pos.generate.true <- function (X.length, X.win.rate, X.win.length, X.loose.length) {
		repeat {
			# генерация ряда Бернули
			X <- rbinom(X.length, 1, X.win.rate)
			# цикл проверки на вставки win/loose
			n.win <- 0
			for ( i in 1:(X.length-1) ) {
	  				# для win сделок
	  					if (X[i] > 0) {
	  						n.win <- n.win + 1
	  						for (y in 1:X.win.length) {
	  							ifelse (is.na(X[i+y]), break, 0)
	  							if ( (X[i]-X[i+y]) != 0 ) break
	  								else {
	  									if(y==X.win.length) {
	  										X[i+X.win.length] <- 0
	  									}	  							
	  								} 
	  						}	 
	  					} 
	  					# для loose сделок
	  						else {
	  							for (y in 1:X.loose.length) {
	  								ifelse (is.na(X[i+y]), break, 0)
	  								if ( (X[i]-X[i+y]) != 0 ) break
	  									else {
	  										if(y==X.loose.length) {
	  											X[i+X.loose.length] <- 1
	  										}
	  									} 
	  							}
	  						}
			}
			temp.win.rate <- n.win / X.length
			if (temp.win.rate == X.win.rate) break 
		}
	return(X)
}

# функция вычисления профита от сгенерированной выше последовательности сделок при заданном уровне риска и win profit'а
F.calc <- function (X, risk, X.win.profit, balance ) {
	# генерация ряда сделок
			n <- length(X) 
		# рабочий фрейм
			b <- rep(NA, n)
			b[1] <- balance
			df = data.frame(pos = X, balance = b)	
		# функция расчёта баланса
			for (i in 2:n) {
				df$balance[1] <- balance
				df$balance[i] <- ifelse( df$pos[i-1] > 0, df$balance[i-1] * (1 + X.win.profit*risk),
									df$balance[i-1] * (1 - risk)
								)	
			}		
	profit <- df$balance[[n]]
	return ( profit )
}

# функция вычисляет профит от ряда сделок при разных уровнях риска (возвращает матрицу значений "риск<->профит")
F.calc2 <- function(X, X.win.profit, balance, r.hi) {
	f <- seq(from=0.001, to=r.hi, by=0.001)
	df.risk = data.frame(f)
	n <- nrow(df.risk)
	for (i in 1:n) {
		df.risk$profit[i] <- F.calc (X, risk = df.risk$f[i], X.win.profit, balance )
	}
	return (df.risk)	
}

# функция вычисляет оптимальный риск для заданной последовательности сделок (возвращает оптимальный риск и профит при нём)
F.calc3 <- function(X, X.win.profit, balance, r.hi) {
			f <- seq(from=0.001, to=r.hi, by=0.001)
			df.risk = data.frame(f)
			n <- nrow(df.risk)
			for (i in 1:n) {
				df.risk$profit[i] <- F.calc( X, risk = df.risk$f[i], X.win.profit, balance )
			}
			y <- which.max(df.risk$profit)
			return ( c(df.risk$f[y], df.risk$profit[y]) )	
	}

# итоговая функция тестирования; проводит "rep" тестов, для каждого из которых генерирует уникальную последовательность сделок 
# и вычисляет оптимальный риск (возвращает матрицу значений "оптимальный риск <-> профит" от каждого теста)
full.F.test <- function (rep, X.length, X.win.rate, X.win.profit, balance, r.hi, X.win.length, X.loose.length) {
	z <- rep(NA, rep)
	t <- data.frame(z)
	for (i in 1:rep) {
		X <- pos.generate.true(X.length, X.win.rate, X.win.length, X.loose.length)
		y <- F.calc3(X, X.win.profit, balance, r.hi)		
		t$risk[i] <- y[1]
		t$profit[i] <- y[2]
	}
	t$z <- NULL
	return (t)
}

