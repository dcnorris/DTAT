# TODO: Support display of *absolute* dose [mg] on right-hand axis?
OXplot <- function(de, tox.pending=FALSE, periods=max(de$period), top.dose=max(de$dose),
                   ox.col=brewer.pal(4,"Dark2")){
  esc.stop <- min(Inf, attr(de,'stop.esc'))
  de <- add.plottables(de, tox.pending)
  if(top.dose < max(de$dose)){
    top.dose <- max(de$dose)
    warning(paste("Increasing top.dose to", top.dose, "to avoid lopping top off OXplot."))
  }
  ylim <- c(0.5, top.dose+0.5)
  xYplot(dose ~ x, groups='cohort', data=de
         , par.settings=list(layout.heights=list(bottom.padding=1.75, right.padding=-1))
         , scales = list(x=list(limits=c(0.5, 3*periods+0.5),
                                at=seq(1,max(de$id)),
                                rot=60, cex=0.6,
                                col=rep(ox.col,each=3))
                         ,y=list(limits=ylim,
                                 at=seq(from=max(1,ceiling(ylim[1])),
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
           for(v in 3*(1:periods) + 0.5){
               if((v - 0.5) == 3*esc.stop)
               panel.abline(v=v, col='black', lty=2)
             else
               panel.refline(v=v)
           }
         }
  )
}
