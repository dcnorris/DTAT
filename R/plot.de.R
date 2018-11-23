plot.de <- function(x, viewer=c("internal","external","browser"), ...){
  r2d3::r2d3(data=x,
             script=system.file("htmlwidgets/lib/main.js", package="DTAT"),
             d3_version = 4, container = "div",
             dependencies = system.file(paste("htmlwidgets/lib",
                                              c("margins.js", "exx.js",
                                                "ox-plot.js", "ds-plot.js"),
                                              sep="/"), package="DTAT"),
             viewer=viewer,
             ...)
}
