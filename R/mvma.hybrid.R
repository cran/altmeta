mvma.hybrid <- function(ys, vars, method = "reml", tol = 1E-10){
	if(method != "ml" & method != "reml") stop("method must be either reml or ml.")
	ys <- as.matrix(ys)
	vars <- as.matrix(vars)
	n <- dim(ys)[1]
	p <- dim(ys)[2]
	if(p < 2) stop("please directly use the univariate model.")

	idx.na <- numeric(0)
	for(i in 1:n){
		if(all(is.na(ys[i,]))) idx.na <- c(idx.na, i)
	}
	if(length(idx.na) > 0){
		ys <- ys[-idx.na,]
		vars <- vars[-idx.na,]
		n <- dim(ys)[1]
	}

	idx.obs <- vector("list", n)
	X <- vector("list", n)
	ys.tilde <- vector("list", n)
	vars.tilde <- vector("list", n)
	for(i in 1:n){
		idx.obs[[i]] <- which(!is.na(ys[i,]))
		X[[i]] <- diag(p)[idx.obs[[i]],]
		if(length(idx.obs[[i]]) == 1) X[[i]] <- matrix(X[[i]], nrow = 1, ncol = p)
		ys.tilde[[i]] <- ys[i, idx.obs[[i]]]
		vars.tilde[[i]] <- vars[i, idx.obs[[i]]]
	}

	target <- function(x){
		mu <- x[1:p]
		tau2 <- x[(p + 1):(2*p)]
		thetas <- matrix(NA, nrow = p - 1, ncol = p - 1)
		for(i in 1:(p - 1)){
			for(j in 1:i){
				if(i == j) thetas[i, j] <- x[2*p + (i - 1)*i/2 + j]
				if(i != j) thetas[i, j] <- thetas[j, i] <- x[2*p + (i - 1)*i/2 + j]
			}
		}
		# Cholesky decomposition: all theta's are between 0 and pi
		L <- matrix(0, nrow = p, ncol = p)
		L[1, 1] <- 1
		for(i in 2:p){
			for(j in 1:i){
				if(j == 1) L[i, j] <- cos(thetas[i - 1, 1])
				if(j >= 2 & j <= i - 1) L[i, j] <- prod(sin(thetas[i - 1, 1:(j - 1)]))*cos(thetas[i - 1, j])
				if(j == i) L[i, j] <- prod(sin(thetas[i - 1, 1:(i - 1)]))
			}
		}

		R <- L%*%t(L)

		out1 <- 0
		out2 <- matrix(0, p, p)
		for(i in 1:n){
			var.temp <- vars.tilde[[i]] + tau2[idx.obs[[i]]]
			y.temp <- ys.tilde[[i]]
			y_mu_D_half <- (y.temp - mu[idx.obs[[i]]])*sqrt(1/var.temp)
			y_mu_D_half <- as.matrix(y_mu_D_half)
			logdet <- determinant(as.matrix(R[idx.obs[[i]], idx.obs[[i]]]), logarithm = TRUE)
			if(logdet$sign <= 0) logdet <- -Inf else logdet <- logdet$modulus
			temp <- logdet+ sum(log(var.temp)) + as.numeric(t(y_mu_D_half)%*%solve(R[idx.obs[[i]], idx.obs[[i]]])%*%y_mu_D_half)
			out1 <- out1 + temp
			if(method == "reml"){
				if(length(idx.obs[[i]]) == 1){
					var.temp <- as.numeric(var.temp)
					out2 <- out2 + t(X[[i]])%*%X[[i]]/var.temp
				}else{
					half.inv <- as.matrix(diag(1/sqrt(var.temp)))
					out2 <- out2 + t(X[[i]])%*%half.inv%*%solve(R[idx.obs[[i]], idx.obs[[i]]])%*%half.inv%*%X[[i]]
				}
			}
		}
		if(method == "ml") out <- as.numeric(out1)
		if(method == "reml"){
			logdet2 <- determinant(as.matrix(out2), logarithm = TRUE)
			if(logdet2$sign <= 0) logdet2 <- -Inf else logdet2 <- logdet2$modulus
			out <- out1 + logdet2
		}
		return(out)
	}

	pi <- 3.1415926
	optim.out <- optim(par = c(rep(0, p), rep(1, p), rep(pi/2, (p - 1)*p/2)), fn = target, lower = c(rep(-Inf, p), rep(10^(-6), p), rep(0 + 0.01, (p - 1)*p/2)), upper = c(rep(Inf, p), rep(Inf, p), rep(pi - 0.01, (p - 1)*p/2)), method = "L-BFGS-B", control = list(factr = tol))
	pars <- optim.out$par

	mu.est <- pars[1:p]
	tau2.est <- pars[(p + 1):(2*p)]

	thetas.est <- matrix(NA, nrow = p - 1, ncol = p - 1)
	for(i in 1:(p - 1)){
		for(j in 1:i){
			if(i == j) thetas.est[i, j] <- pars[2*p + (i - 1)*i/2 + j]
			if(i != j) thetas.est[i, j] <- thetas.est[j, i] <- pars[2*p + (i - 1)*i/2 + j]
		}
	}

	L.est <- matrix(0, nrow = p, ncol = p)
	L.est[1, 1] <- 1
	for(i in 2:p){
		for(j in 1:i){
			if(j == 1) L.est[i, j] <- cos(thetas.est[i - 1, 1])
			if(j >= 2 & j <= i - 1) L.est[i, j] <- prod(sin(thetas.est[i - 1, 1:(j - 1)]))*cos(thetas.est[i - 1, j])
			if(j == i) L.est[i, j] <- prod(sin(thetas.est[i - 1, 1:(i - 1)]))
		}
	}

	mar.R <- L.est%*%t(L.est)

	temp <- matrix(0, p, p)
	for(i in 1:n){
		var.temp <- vars.tilde[[i]] + tau2.est[idx.obs[[i]]]
		if(length(idx.obs[[i]]) == 1){
			var.temp <- as.numeric(var.temp)
			temp <- temp + t(X[[i]])%*%X[[i]]/var.temp
		}else{
			half.inv <- as.matrix(diag(1/sqrt(var.temp)))
			temp <- temp + t(X[[i]])%*%half.inv%*%solve(mar.R[idx.obs[[i]], idx.obs[[i]]])%*%half.inv%*%X[[i]]
		}
	}
	mu.cov <- solve(temp)

	out <- list(mu.est = mu.est, tau2.est = tau2.est, mar.R = mar.R, mu.cov = mu.cov, method = paste("Random-effects model using ", method, sep =""))
	return(out)
}