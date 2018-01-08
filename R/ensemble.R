ensemble <- function(K=10, ...){
  pb <- txtProgressBar(max=K, style=3)
  df <- de.sim(...)
  MTDi.ensemble <<- list(MTDi.last)
  de.ensemble <<- list(de.last)
  setTxtProgressBar(pb, 1)
  for(k in 2:K){
    df <- rbind(df, de.sim(...))
    MTDi.ensemble[[k]] <<- list(MTDi.last)
    de.ensemble[[k]] <<- de.last
    setTxtProgressBar(pb, k)
  }
  cat('\n') # finish with progress bar before interactive printing
  df
}
