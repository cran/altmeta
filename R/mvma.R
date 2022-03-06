mvma <- function(ys, covs, data, method = "reml", tol = 1E-10){
  if(missing(ys)) stop("the argument ys is missing.")
  if(missing(covs)) stop("the argument covs is missing.")
  if(!missing(data)){
    ys <- eval(substitute(ys), data, parent.frame())
    covs <- eval(substitute(covs), data, parent.frame())
  }
  if(method != "fe" & method != "ml" & method != "reml") stop("method must be fe, ml, or reml.")
  if(method == "fe") out <- mvma.fe(ys = ys, covs = covs, tol = tol)
  if(method != "fe") out <- mvma.re(ys = ys, covs = covs, method = method, tol = tol)
  return(out)
}