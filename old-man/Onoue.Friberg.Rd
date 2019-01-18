\name{Onoue.Friberg}
\alias{Onoue.Friberg}
\title{
POMP PK/PD model for docetaxel, combining Onoue et al (2016) with Friberg et al (2002)
}
\description{
This function produces a POMP model combining docetaxel pharmacokinetics (PK) drawn from
Table 2 of Onoue et al (2016) with myelosuppression dynamics drawn from Friberg et al (2002).
This model enables simulation of neutrophil-guided dose titration of docetaxel, as done in
Norris (2017).
}
\usage{
Onoue.Friberg(N, cycle.length.days = 21,
              data=data.frame(time=c(seq(0.0, 1.95, 0.05), 
                                     seq(2.0, cycle.length.days*24, 1.0)),
                              y=NA),
              delta.t = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
Size of simulated population.
}
  \item{cycle.length.days}{
Duration (in days) of chemotherapy cycle to be simulated.
}
  \item{data}{
Passed through as the \code{data} argument of the \code{pomp} constructor.
}
  \item{delta.t}{
Time-step (in hours) of pomp's \code{euler.sim} plug-in.
}
}
\value{
No value is produced; the function sets global variables in package environment \code{DTAT::sim}.
}
\references{
1. Onoue H, Yano I, Tanaka A, Itohara K, Hanai A, Ishiguro H, et al. Significant effect of age on docetaxel pharmacokinetics in Japanese female breast cancer patients by using the population modeling approach. Eur J Clin Pharmacol. 2016 Jun;72(6):703-10. doi:10.1007/s00228-016-2031-3.

2. Friberg LE, Henningsson A, Maas H, Nguyen L, Karlsson MO. Model of chemotherapy-induced myelosuppression with parameter consistency across drugs. J Clin Oncol. 2002 Dec 15;20(24):4713-21. doi:10.1200/JCO.2002.02.140.

3. Norris DC. Dose Titration Algorithm Tuning (DTAT) should supersede 'the' Maximum Tolerated Dose (MTD) in oncology dose-finding trials. F1000Research. 2017;6:112. doi:10.12688/f1000research.10624.3. \url{https://f1000research.com/articles/6-112/v3}

}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pomp}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}