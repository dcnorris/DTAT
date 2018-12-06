as_d3_data.de <- function(x, ...){
  # Provided a multi-period 'de' object as returned by de.sim(testing=TRUE),
  # this function assembles a data list suitable for passing in r2d3(data=).
  mtd <- attr(x,'mtd')
  data <- list(mtd = data.frame(id=1:length(mtd), mtd=mtd),
               doses = attr(x,'doses'),
               dunit = attr(x,'dunit'),
               trial = x[[length(x)]],
               mtd_quantiles = attr(x,'mtd_quantiles'),
               ds = lapply(x, function(x){
                 dsc <- as.data.frame(ds.curve(x))
                 dsc$dose <- seq(nrow(dsc))
                 # TODO: Delegate the following data 'tweak'
                 #       to the visualization code, since the
                 #       need for it arises purely from
                 #       visualization considerations:
                 dsc <- dsc[c(1,1:nrow(dsc)),] # duplicate dose=1 row
                 dsc$dose[1] <- 0.5
                 dsc
               })
  )
  data$ds <- rbindlist(data$ds, idcol="period")
  # NB: We use a *generic* transformation to JSON,
  #     and not r2d3's special default approach
  #     designed to reduce size of data transmitted:
  jsonlite::toJSON(data)
}
