Pr <-
function(alpha, beta=NULL, argmax=FALSE){ # Note this requires alpha > 0.5
  if (is.null(beta)) { # Then return max over beta of Pr(alpha,beta)
    opt <- optimize(function(beta) Pr(alpha, beta), interval=c(0.0, 1000), maximum=TRUE)
    if (argmax)
      c(argmax=opt$maximum, max=opt$objective)
    else
      opt$objective
  } else { # Define Pr(alpha,beta)
    Q <- Rgamma(alpha - 0.5, beta, lower=FALSE) # Regularized gamma function
    0.5 * sqrt(beta)*gamma(alpha - 0.5)/gamma(alpha) * Q
  }
}
