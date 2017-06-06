seq.function <-
function(scalefun, from, to, length.out, digits=NULL){
  x <- seq(from=scalefun(from), to=scalefun(to), length.out=length.out)
  y <- numeric(length.out)
  # Handle limiting case where from==to, to avert a uniroot error
  if(from == to){
    y1 <- uniroot(function(y) scaled(y)-x[1], c(from-1,from+1))$root
    return(rep(y1, length.out))
  }
  for(i in seq(length(y)))
    y[i] <- uniroot(function(y) scaled(y)-x[i], sort(c(from, to)))$root
  if(!is.null(digits))
    y <- round(y, digits=digits)
  y
}
