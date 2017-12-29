# TODO: Support display of *absolute* dose [mg] on right-hand axis?
# TODO: Consider supporting *animation* by period and within-period events.
OXplot <- function(de, ylim=NULL){
  esc.stop <- min(Inf, attr(de,'stop.esc'))
  de <- add.plottables(de)
  if(is.null(ylim)){
    ylim <- range(de$dose) + c(-0.5, 0.5)
  }
  ox.col = brewer.pal(4,"Dark2")
  xYplot(dose ~ x, group=cohort, data=de
         , par.settings=list(layout.heights=list(bottom.padding=1.75, right.padding=-1))
         , scales = list(x=list(limits=c(0.5, 3*max(de$period)+0.5),
                                at=seq(1,max(de$id)),
                                rot=60, cex=0.6,
                                col=rep(ox.col,each=3))
                         ,y=list(limits=ylim,
                                 at=seq(from=max(1,ceil(ylim[1])),
                                        to=floor(ylim[2] - 0.001))
                         )
         )
         , xlab = "Patient number"
         , ylab = "Dose level"
         , cex = ifelse(de$pch==20, 0.8, 1.5) # make carry-forward dots smaller
         , pch = de$pch
         , col = ox.col[1 + (as.integer(de$cohort) - 1) %% length(ox.col)]
         , panel = function(x, y, ..., pch, col, cex, subscripts, darkline=esc.stop){
           panel.xyplot(x, y, pch=pch[subscripts], col=col[subscripts], cex=cex[subscripts])
           for(v in seq(from=3.5, to=3*((max(x)+2) %/% 3) - 2.5, by=3)){
             if((v - 0.5) == 3*esc.stop)
               panel.abline(v=v, col='black', lty=2)
             else
               panel.refline(v=v)
           }
         }
  )
}
