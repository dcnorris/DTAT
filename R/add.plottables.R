add.plottables <- function(de){
  de <- upData(de
               ,cohort = factor((id-1) %/% 3 + 1)
               ,x = 1 + (id-1) %% 3 + 3*(period-1)
               ,pch = c(111,120)[1+dlt] # ASCII 111='o' 120='x'
               ,print = FALSE
  )
  # Find carried-forward doses and substitute '.' for 'o'
  # TODO: Construct an *index* into de that finds all '.'
  de <- arrange(de, id, period)
  ix <- with(de, which(id==lag(id) & dose==lag(dose)))
  de$pch[ix] <- 20 # a smaller filled circle
  # Find and splay overlapping symbols
  for(x in unique(de$x)){
    for(dose in unique(de$dose)){
      ix <- which(de$x==x & de$dose==dose)
      n <- length(ix)
      ord <- order(de$id[ix]) # Ensure left-right splay preserves id-order
      if(n > 1)
        de$x[ix] <- de$x[ix] + seq(from=-0.12, to=0.12, length.out=n)[ord]
    }
  }
  de
}
