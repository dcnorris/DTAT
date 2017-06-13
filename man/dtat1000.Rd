\name{dtat1000}
\alias{dtat1000}
\docType{data}
\title{
Precomputed neutrophil-guided chemotherapy dose titration for 1000 simulated subjects.
}
\description{
This dataset is provided to support fast reproduction of a forthcoming pharmacoeconomic
paper that includes examination of the empirical distribution of MTDi in N=1000 simulated
subjects.
}
\usage{data("dtat1000")}
\format{
  A data frame showing end-of-cycle state of neutrophil-guided dose titration for 1000 simulated
  subjects, across 10 cycles of chemotherapy.
  \describe{
    \item{\code{cycle}}{Cycle number 1..10}
    \item{\code{id}}{Subject identifiers; an ordered factor with levels \code{id1} < \dots < \code{id1000}}
    \item{\code{Cc}}{Central-compartment drug concentration}
    \item{\code{Cp}}{Peripheral-compartment drug concentration}
    \item{\code{Prol}}{Progenitor cells in proliferating compartment of Friberg et al (2002) model}
    \item{\code{Tx.1}}{Transit compartment 1}
    \item{\code{Tx.2}}{Transit compartment 1}
    \item{\code{Tx.3}}{Transit compartment 1}
    \item{\code{Circ}}{Concentration (cells/mm^3) of circulating neutrophils}
    \item{\code{dose}}{Dose of 1-hour infusion administered this cycle}
    \item{\code{CircMin}}{Neutrophil nadir (cells/mm^3)}
    \item{\code{tNadir}}{Time (days) of neutrophil nadir}
    \item{\code{scaled.dose}}{Fourth root of dose}
    \item{\code{time}}{Time (weeks) of dose administration}
  }
}
\details{
Running the examples interactively, you can verify the reproducibility of this dataset.
(That demo is included in a \code{donttest} block to spare the CRAN servers.)
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dtat1000)
# 1. Extract the N final doses, assuming convergence by the tenth course
MTD_i <- with(dtat1000, dose[time==27])
MTD_i <- MTD_i[MTD_i < 5000] # Exclude few outliers
# 2. Do a kernel density plot
library(rms)
hist <- histogram(~MTD_i, breaks=c(0,100,200,300,400,600,900,1500,2500,4000,5000)
                  , xlab=expression(MTD[i]))
approx <- data.frame(mtd_i=seq(0, 5000, 10))
approx <- upData(approx,
                 gamma = dgamma(mtd_i, shape=1.75, scale=200))
dist <- xyplot(gamma ~ mtd_i, data=approx, type='l', col='black', lwd=2)
library(grid)
hist + dist
grid.text(expression(MTD[i] \%~\%
                     paste("Gamma(", alpha==1.75, ", ", beta==1/200,")"))
         , x=unit(0.5,"npc")
         , y=unit(0.75,"npc")
         )
\donttest{
# Demonstrate exact reproduction of this data set (the titration takes many minutes!)
set.seed(2016)
OFmod <- Onoue.Friberg()
pkpd <- OFmod$pkpd
inits_fac <- OFmod$inits_fac
params.default <- c(Circ0=5050, kTR=4/89.3, gamma=0.163, Emax=83.9, EC50=7.17*0.808,
                    CL=32.6, Q=5.34, Vc=5.77, Vp=11.0, sigma=0.05, dose=50, duration=1.0
                    )
pop <- data.frame(id = integer()
                  ,Circ0= numeric()
                  ,gamma= numeric()
                  ,Emax = numeric()
                  ,EC50 = numeric()
                  ,CL = numeric()
                  ,Q  = numeric()
                  ,Vc = numeric()
                  ,Vp = numeric()
                  ,kTR = numeric()
                  )
for(id in 1:1000){
  pop[id,'id'] <- id
  pop[id,-1] <- rprior(pkpd, params=params.default)[colnames(pop)[-1],]
}

pop <- upData(pop
             ,MTT = 4/kTR
             ,units = c(Circ0="cells/mm^3"
                       ,MTT="hours"
                       ,kTR="1/hour"
                       ,CL="L/h"
                       ,Q="L/h"
                       ,Vc="L"
                       ,Vp="L"
                       )
             ,print=FALSE
             )

N <- nrow(pop)
# This titration may take an hour to run ...
chemo <- doChemo(doserange = 100,
                 adapt.dosing=newton.raphson(dose1 = 100,
                                             omega = 0.75,
                                             slope1 = -2.0,
                                             slopeU = -0.2)
)

dtat1k <- upData(chemo$course
                , time = 3*(cycle-1)
                , labels = c(time="Time")
                , units = c(time="weeks")
                , print = FALSE)

stopifnot(identical(dtat1k, dtat1000))
} # end 'donttest'
}
\keyword{datasets}
