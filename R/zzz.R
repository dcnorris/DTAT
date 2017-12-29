.onLoad <- function(libname, pkgname){
  # Set options that control the 3+3/PC algorithms
  options(ds.conf.level = 0.8
          , dose.drop.threshold = 0.8
          , stop.esc.under = 1/3
          , undo.esc.under = 1/4
  )
}