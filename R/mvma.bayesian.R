mvma.bayesian <- function(ys, covs, n.adapt = 1000, n.chains = 3, n.burnin = 10000, n.iter = 10000, n.thin = 1, data.name = NULL, traceplot = FALSE, coda = FALSE){
	ys <- as.matrix(ys)
	n <- dim(ys)[1]
	m <- dim(ys)[2]
	if(m < 2) stop("please directly use the univariate model.")

	seed <- sample(1000000, 1)

	idx.na <- numeric(0)
	for(i in 1:n){
		if(all(is.na(ys[i,]))) idx.na <- c(idx.na, i)
	}
	if(length(idx.na) > 0){
		ys <- ys[-idx.na,]
		covs <- covs[-idx.na]
		n <- dim(ys)[1]
	}

	y <- NULL
	n.rf <- NULL
	rf.id <- NULL
	for(i in 1:n){
		y <- c(y, ys[i,][!is.na(ys[i,])])
		n.rf <- c(n.rf, sum(!is.na(ys[i,])))
		rf.id <- c(rf.id, which(!is.na(ys[i,])))
	}
	len <- length(y)
	idx.low <- cumsum(c(1, n.rf[-n]))
	idx.upp <- cumsum(n.rf)

	S <- matrix(0, len, len)
	for(i in 1:n){
		S[idx.low[i]:idx.upp[i], idx.low[i]:idx.upp[i]] <- covs[[i]][rf.id[idx.low[i]:idx.upp[i]], rf.id[idx.low[i]:idx.upp[i]]]
	}

	modelstring<-"
	model{
		for(i in 1:n){
			y[idx.low[i]:idx.upp[i]] ~ dmnorm(mu[rf.id[idx.low[i]:idx.upp[i]]], inverse(S[idx.low[i]:idx.upp[i], idx.low[i]:idx.upp[i]] + Tau[rf.id[idx.low[i]:idx.upp[i]], rf.id[idx.low[i]:idx.upp[i]]]))
		}
		for(i in 1:m){
			for(j in 1:m){
				Tau[i, j] <- tau[i]*tau[j]*R[i, j]
			}
		}
		for(i in 1:m){
			mu[i] ~ dnorm(0, 0.001)
			tau[i] ~ dunif(0.001, 10)
		}
	
		R[1:m, 1:m] <- L[1:m, 1:m]%*%t(L[1:m, 1:m])
	
		L[1, 1] <- 1
		for(j in 2:m){
			L[1, j] <- 0
		}
		for(i in 2:(m - 1)){
			L[i, 1] <- cos(theta[i - 1, 1])
			for(j in 2:i){
				L[i, j] <- ifelse(j < i, prod(sin(theta[i - 1, 1:(j - 1)]))*cos(theta[i - 1, j]), prod(sin(theta[i - 1, 1:(i - 1)])))
			}
			for(j in (i + 1):m){
				L[i, j] <- 0
			}
		}
		L[m, 1] <- cos(theta[m - 1, 1])
		for(j in 2:(m - 1)){
			L[m, j] <- prod(sin(theta[m - 1, 1:(j - 1)]))*cos(theta[m - 1, j])
		}
		L[m, m] <- prod(sin(theta[m - 1, 1:(m - 1)]))
	
		for(i in 1:(m - 1)){
			for(j in 1:(m - 1)){
				theta[i, j] ~ dunif(0.01, 3.1415926 - 0.01)
			}
		}
	}"

	data.jags <- list(y = y, S = S, n = n, m = m, rf.id = rf.id, idx.low = idx.low, idx.upp = idx.upp)

	init.jags <- vector("list", n.chains)
	set.seed(seed)
	init.seeds <- sample(1:100000, n.chains)
	for(i in 1:n.chains){
		init.jags[[i]] <- list(mu = rep(0, m), tau = rep(1, m), theta = matrix(3.1415926/2, m - 1, m - 1),
			.RNG.name = "base::Wichmann-Hill", .RNG.seed = init.seeds[i])
	}

	set.seed(seed)
	jags.m <- tryCatch.W.E(jags.model(file = textConnection(modelstring), data = data.jags, inits = init.jags,
		n.chains = n.chains, n.adapt = n.adapt))
	warn.adapt<-jags.m$warning
	jags.m<-jags.m$value
	if(is(warn.adapt,"warning")) cat("Adaptation incomplete; users may increase n.adapt.\n")
	update(jags.m, n.iter = n.burnin)
	jags.out <- coda.samples(model = jags.m, variable.names = c("mu", "tau", "R", "theta"),
		n.iter = n.iter, thin = n.thin)
	smry <- summary(jags.out)
	smry <- cbind(smry$statistics[,c("Mean", "SD")], smry$quantiles[,c("2.5%", "50%", "97.5%")])
	if(is.null(data.name)){
		trace.prefix <- "M_Traceplot_"
		coda.prefix <- "M_Coda_mu_"
	}else{
		trace.prefix <- paste(data.name, "_M_Traceplot_", sep = "")
		coda.prefix <- paste(data.name, "_M_Coda_mu_", sep = "")
	}

	conv.out <- gelman.diag(jags.out, multivariate = FALSE)
	conv.out <- conv.out$psrf

        out <- list(smry = smry, conv = conv.out)

	if(traceplot){
		for(i in 1:m){
			png(paste(trace.prefix, "mu[", i , "]", ".png",sep=""), res = 600, height = 8.5, width = 11, units = "in")
			par(mfrow = c(length(jags.out),1))
			for(j in 1:length(jags.out)){
				temp <- as.vector(jags.out[[j]][,paste("mu[", i, "]", sep = "")])
				plot(temp, type = "l", col = "red", ylab = paste("mu[", i, "]", sep = ""), xlab = "Iterations", main = paste("Chain",j))
			}
			dev.off()
			png(paste(trace.prefix, "tau[", i, "]", ".png",sep=""), res = 600, height = 8.5, width = 11, units = "in")
			par(mfrow = c(length(jags.out),1))
			for(j in 1:length(jags.out)){
				temp <- as.vector(jags.out[[j]][,paste("tau[", i, "]", sep = "")])
				plot(temp, type = "l", col = "red", ylab = paste("tau[", i, "]", sep = ""), xlab = "Iterations", main = paste("Chain",j))
			}
			dev.off()
		}
	}

	if(coda){
		for(i in 1:m){
			temp <- NULL
			for(j in 1:n.chains){
				temp <- c(temp, as.vector(jags.out[[j]][,paste("mu[",i,"]",sep = "")]))
			}
			write.table(matrix(temp, nrow = length(temp), ncol = 1),
				paste(coda.prefix, i, ".txt", sep = ""), row.names = FALSE, col.names = FALSE)
		}
	}

	return(out)
}