mvma <- function(ys, covs, method = "reml", tol = 1E-10){
	if(missing(ys)) stop("the argument ys is missing.")
	if(missing(covs)) stop("the argument covs is missing.")
	if(method != "fe" & method != "ml" & method != "reml") stop("method must be fe, ml, or reml.")
	if(method == "fe") out <- mvma.fe(ys = ys, covs = covs, tol = tol)
	if(method != "fe") out <- mvma.re(ys = ys, covs = covs, method = method, tol = tol)
	return(out)
}