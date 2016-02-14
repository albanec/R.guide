
# функция генерации рандомного ряда Бернули (вектор сделок)
X <- rbinom(X.length, 1, X.win.rate)

# функция вычисления профита от сгенерированной выше последовательности сделок при заданном уровне риска и win profit'а
F.calc <- function (X, risk, win.profit, balance ) {
	# генерация ряда сделок
			n <- length(X) 
		# рабочий фрейм
			b <- rep(NA, n)
			b[1] <- balance
			df = data.frame(pos = X, balance = b)	
		# функция расчёта баланса
			for (i in 2:n) {
				df$balance[1] <- balance
				df$balance[i] <- ifelse( df$pos[i-1] > 0, df$balance[i-1] * (1 + win.profit*risk),
									df$balance[i-1] * (1 - risk)
								)	
			}		
	profit <- df$balance[[n]]
	return ( profit )
}

# функция вычисляет профит от ряда сделок при разных уровнях риска (возвращает матрицу значений "риск<->профит")
F.calc2 <- function(X, win.profit, balance, r.hi) {
	f <- seq(from=0.01, to=r.hi, by=0.01)
	df.risk = data.frame(f)
	n <- nrow(df.risk)
	for (i in 1:n) {
		df.risk$profit[i] <- F.calc (X, risk = df.risk$f[i], win.profit, balance )
	}
	return (df.risk)	
}

# функция вычисляет оптимальный риск для заданной последовательности сделок (возвращает оптимальный риск и профит при нём)
F.calc3 <- function(X, win.profit, balance, r.hi) {
			f <- seq(from=0.01, to=r.hi, by=0.01)
			df.risk = data.frame(f)
			n <- nrow(df.risk)
			for (i in 1:n) {
				df.risk$profit[i] <- F.calc( X, risk = df.risk$f[i], win.profit, balance )
			}
			y <- which.max(df.risk$profit)
			return ( c(df.risk$f[y], df.risk$profit[y]) )	
	}

# итоговая функция тестирования; проводит "rep" тестов, для каждого из которых генерирует уникальную последовательность сделок 
# и вычисляет оптимальный риск (возвращает матрицу значений "оптимальный риск <-> профит" от каждого теста)
full.F.test <- function (rep, X.length, X.win.rate, win.profit, balance, r.hi) {
	z <- rep(NA, rep)
	t <- data.frame(z)
	for (i in 1:rep) {
		X <- rbinom(X.length, 1, X.win.rate)
		y <- F.calc3(X, win.profit, balance, r.hi)		
		t$risk[i] <- y[1]
		t$profit[i] <- y[2]
	}
	t$z <- NULL
	return (t)
}

