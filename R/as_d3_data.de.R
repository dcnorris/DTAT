#' Transform the 'de' object to JSON data for D3 visualization.
#' 
#' The returned object is a JSON representation of a JavaScript object having
#' components 'mtd', 'trial' and 'ds' -- the last of which stands for
#' 'dose-survival'.
#' 
#' The (unhelpful) default behavior of function \code{r2d3} is to effect this
#' transformation so as to reduce the size of transmitted data, then restore
#' the original form upon receipt using JavaScript method
#' \code{HTMLWidgets.dataframeToD3()}. See
#' \url{https://rstudio.github.io/r2d3/articles/data_conversion.html}.
#' 
#' @param x The 'de' object to be transformed
#' @param \dots Unused; included to match signature of generic method
#' @return The returned value is a JSON string corresponding to a list of data
#' frames: \item{mtd}{Data frame with columns c('id','mtd')} \item{trial}{Data
#' frame with 'id', 'period', 'dose', 'dlt'.)} \item{ds}{Data frame with
#' columns 'period','surv','lower','upper','dose'.}
#' @author David C. Norris
#' @keywords manip dynamic
#' @examples
#' 
#' library(DTAT)
#' de <- de.sim(testing=TRUE)
#' data <- as_d3_data(de)
#' ## Read 'data' back into a list and check a few things
#' check <- jsonlite::fromJSON(data)
#' stopifnot(max(abs(attr(de,'mtd') - check$mtd)) < 1e-4)
#' stopifnot(max(abs(attr(de,'doses') - check$doses)) < 1e-4)
#' stopifnot(max(abs(attr(de,'mtd_quantiles') - check$mtd_quantiles)) < 1e-4)
#' stopifnot(all(last(de) - check$trial == 0))
#' ## TODO: Add a further check on the check$ds data.frame
#' 
#' @importFrom r2d3 as_d3_data
#' @export as_d3_data
#' @importFrom data.table rbindlist
#' @export
as_d3_data.de <- function(x, ...){
  # Provided a multi-period 'de' object as returned by de.sim(testing=TRUE),
  # this function assembles a data list suitable for passing in r2d3(data=).
  data <- list(mtd = attr(x,'mtd'),
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
