# Examine the dose-survival curve (with confidence band)
ds.curve <- function(de, ...){
  # 2. Get survfit
  fit <- dose.survfit(de, ...)
  surv  <- approx(fit$time, fit$surv , method="constant", xout=1:max(fit$time))$y
  lower <- approx(fit$time, fit$lower, method="constant", xout=1:max(fit$time))$y
  upper <- approx(fit$time, fit$upper, method="constant", xout=1:max(fit$time))$y
  list(surv=surv, lower=lower, upper=upper)
}
