Onoue.Friberg <-
function(cycle.length.days=21,
         data=data.frame(time=c(seq(0.0, 1.95, 0.05), # q3min for 2h, 
                                seq(2.0, cycle.length.days*24, 1.0)), # then hourly until Tmax
                         y=NA),
         delta.t=0.1){
  # Implement a lognormal measurement model
  pkpd.rmeas <- "
  y = rlnorm(log(Circ), sigma);
"
  pkpd.dmeas <- "
  lik = dlnorm(y, log(Circ), sigma, give_log);
"
  pkpd.rprior <- "
  // From Friberg et al 2002 (Table 4, row 1), taking sdlog ~= CV
  Circ0 = rlnorm(log(5050), 0.42);
  double MTT = rlnorm(log(89.3), 0.16);
  kTR = 4/MTT;
  gamma = rlnorm(log(0.163), 0.039);
  Emax  = rlnorm(log(83.9), 0.33);
  double dtx_mm = 0.808; // molar mass (g/mM) of docetaxel
  EC50  = rlnorm(log(7.17*dtx_mm), 0.50);
  // PK params from 2-compartment docetaxel model of Onoue et al (2016)
  CL = rlnorm(log(32.6), 0.295);
  Q  = rlnorm(log(5.34), 0.551);
  Vc = rlnorm(log(5.77), 0.1);
  Vp = rlnorm(log(11.0), 0.598);
"
  pkpd.dprior <- "
  // Parameter #4 setting 'log=1' returns log-density
  lik  = dlnorm(Circ0, log(5050), 0.42, 1);
  double MTT = 4/kTR;
  lik += dlnorm(MTT, log(89.3), 0.16, 1);
  lik += dlnorm(gamma, log(0.163), 0.039, 1);
  lik += dlnorm(Emax, log(83.9), 0.33, 1);
  double dtx_mm = 0.808; // molar mass (g/mM) of docetaxel
  lik += dlnorm(EC50, log(7.17*dtx_mm), 0.50, 1);
  lik += dlnorm(CL, log(32.6), 0.295, 1);
  lik += dlnorm(Q,  log(5.34), 0.551, 1);
  lik += dlnorm(Vc, log(5.77), 0.1, 1);
  lik += dlnorm(Vp, log(11.0), 0.598, 1);
  if (give_log != 1) lik = exp(lik);
"
  pkpd.skel <- "
  double c2p = Q*( Cc - Cp ); // central-to-peripheral flux
  DCc = (dose/duration)*(t < duration ? 1.0 : 0.0)/Vc - (CL/Vc)*Cc - c2p/Vc;
  DCp = c2p/Vp;
  // Myelosuppression model (Emax model, then dynamics per eqs 3-7 from Friberg et al 2002
  double Edrug = Emax*Cc/(Cc + EC50); // classic 'Emax model'
  DProl = (1-Edrug) * Prol * kTR * pow((Circ0 / Circ), gamma)  -  kTR * Prol;
  DTx_1 = kTR * (Prol - Tx_1);
  DTx_2 = kTR * (Tx_1 - Tx_2);
  DTx_3 = kTR * (Tx_2 - Tx_3);
  DCirc = kTR * (Tx_3 - Circ);
  // We implement nadir-finding by integrating CircMin and tNadir into the state:
  int initialHump = CircMin == Circ_0; // Circ may overshoot Circ.0 initially
  int dropToNadir = DCirc < 0.0 && Circ < Circ_0 && (t-tNadir) < 1.0; // falling segment Circ.0-->nadir
  DCirc_0 = 0.0; // this pseudo-state merely remembers initial value
  DCircMin = dropToNadir ? DCirc : 0.0;
  DtNadir  = initialHump || dropToNadir ? 1.0 : 0.0;
"
  pkpd.step <- "
  double c2p = Q*( Cc - Cp ); // central-to-peripheral flux
  Cc += dt*( (dose/duration)*(t < duration ? 1.0 : 0.0)/Vc - (CL/Vc)*Cc - c2p/Vc );
  Cp += dt * c2p/Vp;
  // Myelosuppression model (Emax model, then dynamics per eqs 3-7 from Friberg et al 2002
  double Edrug = Emax*Cc/(Cc + EC50); // classic 'Emax model'
  Prol += dt*( (1-Edrug) * Prol * kTR * pow((Circ0 / Circ), gamma)  -  kTR * Prol );
  Tx_1 += dt * kTR * (Prol - Tx_1);
  Tx_2 += dt * kTR * (Tx_1 - Tx_2);
  Tx_3 += dt * kTR * (Tx_2 - Tx_3);
  double _DCirc = kTR * (Tx_3 - Circ);
  // We implement nadir-finding by integrating CircMin and tNadir into the state:
  int initialHump = CircMin == Circ_0; // Circ may overshoot Circ.0 initially
  int dropToNadir = _DCirc < 0.0 && Circ < Circ_0 && (t-tNadir) < 1.0; // falling segment Circ.0-->nadir
  Circ += dt * _DCirc;
  CircMin += dt*( dropToNadir ? _DCirc : 0.0 );
  tNadir  += dt*( initialHump || dropToNadir ? 1.0 : 0.0 );
"
  pkpd.txform <- "
  TCirc0 = log(Circ0);
  TkTR = log(kTR);
  TEmax = log(Emax);
  TEC50 = log(EC50);
  TCL = log(CL);
  TQ = log(Q);
  TVc = log(Vc);
  TVp = log(Vp);
  Tsigma = log(sigma);
  Tdose = log(dose);
  Tduration = log(duration);
"
  pkpd.txback <- "
  TCirc0 = exp(Circ0);
  TkTR = exp(kTR);
  TEmax = exp(Emax);
  TEC50 = exp(EC50);
  TCL = exp(CL);
  TQ = exp(Q);
  TVc = exp(Vc);
  TVp = exp(Vp);
  Tsigma = exp(sigma);
  Tdose = exp(dose);
  Tduration = exp(duration);
"
  #Tmax <- cycle.length.days*24 # solve for full 21 days of 3-week cycle
  #df <- data.frame(time=c(seq(0.0, 1.95, 0.05), # q3min for 2h, 
  #                        seq(2.0, Tmax, 1.0)), # then hourly until Tmax
  #                 y=NA)
  # This higher-order function returns an initializer yielding given 'start.state'
  inits_fac <- function(start.state=NULL){
    if(is.null(start.state)){
      inits <- function(params, t0, ...){
        params <- as.list(params)
        state <- c(Cc = 0.0
                   ,Cp = 0.0
                   ,Prol = params$Circ0
                   ,Tx.1 = params$Circ0
                   ,Tx.2 = params$Circ0
                   ,Tx.3 = params$Circ0
                   ,Circ = params$Circ0
                   ,Circ.0 = params$Circ0
                   ,CircMin = params$Circ0
                   ,tNadir = t0
        )
        state
      }
    } else {
      inits <- function(params, t0, ...){
        state <- start.state
        state['Circ.0'] <- unname(state['Circ'])
        state['CircMin'] <- unname(state['Circ'])
        state['tNadir'] <- t0
        state
      }
    }
    inits
  }
  pkpd <- pomp(data = data
               , times="time", t0=0
               , skeleton=vectorfield(Csnippet(pkpd.skel))
               , rprocess = euler.sim(step.fun = Csnippet(pkpd.step), delta.t = delta.t)
               , rmeasure = Csnippet(pkpd.rmeas)
               , dmeasure = Csnippet(pkpd.dmeas)
               , initializer = inits_fac()
               , rprior = Csnippet(pkpd.rprior)
               , dprior = Csnippet(pkpd.dprior)
               , statenames=c("Cc", "Cp", "Prol", "Tx.1", "Tx.2", "Tx.3", "Circ","Circ.0","CircMin","tNadir")
               , paramnames=c("Circ0","kTR","gamma","Emax","EC50","CL","Q","Vc","Vp"
                              ,"sigma","dose","duration"
               )
               , toEstimationScale = Csnippet(pkpd.txform)
               , fromEstimationScale = Csnippet(pkpd.txback)
  )
  list(pkpd=pkpd, inits_fac=inits_fac)
}