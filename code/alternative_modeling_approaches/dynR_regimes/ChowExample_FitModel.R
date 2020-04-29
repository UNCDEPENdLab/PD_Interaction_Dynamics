setwd("/Users/apple/Dropbox/Miami_Sim")
setwd("C:\\Users\\admin_mxc681\\Dropbox\\Meng Chen\\Miami_Sim")
setwd("C:\\Users\\mxc681\\Dropbox\\Meng Chen\\Miami_Sim")


#------------------------------------------------------------------------------
# Load packages

require(mvtnorm)
require(Matrix)
require(dynr)
options(scipen=999,digits=5)

np=10;nt=70

# measurement
# this is the factor loadings matrix, Lambda in SEM notation or C in OpenMx notation
meas <- prep.measurement(
  values.load=matrix(c(1,0.8,0.8,0,0,0,0,0,0,1,0.8,0.8,0,0,0,0,0,0,0,0,0,0,0,0), ncol=4,byrow=F), # starting values and fixed values
  params.load=matrix(c('fixed','lp1','lp2', 'fixed','fixed','fixed',
                       'fixed','fixed','fixed','fixed','ln1','ln2',
                       'fixed','fixed','fixed','fixed','fixed','fixed',
                       'fixed','fixed','fixed','fixed','fixed','fixed'), ncol=4,byrow=F),
  state.names=c("Pa","Na","bPN","bNP"),
  obs.names=c("V1","V2","V3","V4","V5","V6")) # parameter numbers or indication that parameter is fixed
# Look
meas


# initial covariances and latent state values
# These initialize the recursive algorithm (extended Kalman filter) that dynr uses
# These are x0 and P0 in OpenMx notation
initial <- prep.initial(
  values.inistate=c(0, 0,-.1,-.1),
  params.inistate=c('fixed', 'fixed','betapn0','betanp0'), #initial position is free parameter 5, initial slope is fixed at 1
  values.inicov=diag(c(1,1,.1,.1),4),
  params.inicov=diag(c('fixed', 'fixed','fixed','fixed'),4)) #initial covariance is fixed to a diagonal matrix of 1s.


# define the differential equation
# dynamics <- prep.linearDynamics(
# 	values.dyn=matrix(c(0, -0.1, 1, -0.2), 2, 2),
# 	params.dyn=matrix(c('fixed', 1, 'fixed', 2), 2, 2), #uses parameters 1 and 2
# 	time="continuous")


model=""
  
if(model="True"){
formula=list(
  list(Pa~arp*Pa+bPN*Na,
       Na~arn*Na+bNP*Pa,
       bPN~ betapn0, 
	#bPN~ (1-betapn1)*betapn0+betapn1*bPN,
       bNP~ (1-betanp1)*betanp0+betanp1*bNP))

# observation and dynamic noise components
# the latent noise is the dynamic noise, Psi in SEM notation or Q in OpenMx notation
# the observed noise is the measurement noise, Theta in SEM notation or R in OpenMx notation
ecov <- prep.noise(
  values.latent=diag(c(0.05,0.05,0.05,0.05),4), params.latent=diag(c('zpa', 'zna','zpn','znp'),4), 
  values.observed=diag(c(0.1,0.1,0.1,0.1,0.1,0.1),6), params.observed=diag(c('vep1','vep2','vep3','ven1','ven2','ven3'),6)
)
ecov

dynm<-prep.formulaDynamics(formula=formula,startval=c(arp=.5,arn=.1,
                                                      betapn0=-0.1,
                                                      #  betapn1=0.1,
                                                      betanp0=-0.1,
                                                      betanp1=0.6),
                           isContinuousTime=F)

Temp.true<-c()
Track.true<-c()
for(run in 1:5){
  data.sim<-read.table(paste0("DFAdat",run,".txt"))
  #    data<-read.table(paste0("TVdfan10T70",run,".csv"),sep=",",na.strings = "NaN",
  #                      col.names =c("V1","V2","V3","V4","V5","V6"))
  
  data.sim$id<-rep(1:np,each=nt)
  data.sim$timepoint<-rep(1:nt,np)
  
  
  data <- dynr.data(data.sim, id="id", time="timepoint", observed=c("V1","V2","V3","V4","V5","V6"))  
  model <- dynr.model(dynamics=dynm, measurement=meas, noise=ecov, initial=initial, 
                      data=data,outfile="cooked1")#,
  # transform = trans)
  #model@ub[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(1.5,5))
  #model@lb[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(-1.5,5))
  
  #Check
  #model@dynamics@jacobian
  
  #define a transformation function for those pesky variances
  # Estimate free parameters
  res <- dynr.cook(model,verbose=FALSE)
  temp<-round(coef(res),3)
  track<-try(res@ bad.standard.errors)
  Track.true<-rbind(Track,track)
  Temp.true<-rbind(Temp,temp)
}

}
if(model="Time-Inv"){
  
  initial.inv <- prep.initial(
    values.inistate=c(0, 0,-.1,-.1),
    params.inistate=c('fixed', 'fixed','betapn0','betanp0'), #initial position is free parameter 5, initial slope is fixed at 1
    values.inicov=diag(c(1,1,.001,.001),4),
    params.inicov=diag(c('fixed', 'fixed','fixed','fixed'),4)) #initial covariance is fixed to a diagonal matrix of 1s.
  
  formula.inv=list(
    list(Pa~arp*Pa+bPN*Na,
         Na~arn*Na+bNP*Pa,
         bPN~ betapn0, 
         bNP~ betanp0))
  
  ecov.inv <- prep.noise(
    values.latent=diag(c(0.05,0.05,0,0),4), params.latent=diag(c('zpa', 'zna','fixed','fixed'),4), 
    values.observed=diag(c(0.1,0.1,0.1,0.1,0.1,0.1),6), params.observed=diag(c('vep1','vep2','vep3','ven1','ven2','ven3'),6)
  )
  ecov
  
  dynm.inv<-prep.formulaDynamics(formula=formula.inv,startval=c(arp=.5,arn=.1,
                                                        betapn0=-0.1,
                                                        betanp0=-0.1),
                             isContinuousTime=F)
  
  Temp.inv<-c()
  Track.inv<-c()
  for(run in 1:5){
    data.sim<-read.table(paste0("DFAdat",run,".txt"))
    #    data<-read.table(paste0("TVdfan10T70",run,".csv"),sep=",",na.strings = "NaN",
    #                      col.names =c("V1","V2","V3","V4","V5","V6"))
    
    data.sim$id<-rep(1:np,each=nt)
    data.sim$timepoint<-rep(1:nt,np)
    
    data <- dynr.data(data.sim, id="id", time="timepoint", observed=c("V1","V2","V3","V4","V5","V6"))  
    model <- dynr.model(dynamics=dynm.inv, measurement=meas, noise=ecov.inv, initial=initial.inv, 
                        data=data,outfile="cooked2")#,
    # transform = trans)
    #model@ub[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(1.5,5))
    #model@lb[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(-1.5,5))
    
    #Check
    #model@dynamics@jacobian
    
    #define a transformation function for those pesky variances
    # Estimate free parameters
    res <- dynr.cook(model,verbose=FALSE)
    temp<-round(coef(res),3)
    track<-try(res@ bad.standard.errors)
    Track.inv<-rbind(Track,track)
    Temp.inv<-rbind(Temp,temp)
  }
  
  
}

if(model="Discrete-Change"){
  formula=list(
    list(Pa~arp*Pa+bPN*Na,
         Na~arn*Na+bNP*Pa,
         bPN~ betapn0, 
         bNP~ ind*betanp0))
  
  ecov <- prep.noise(
    values.latent=diag(c(0.05,0.05,0.05,0.05),4), params.latent=diag(c('zpa', 'zna','zpn','znp'),4), 
    values.observed=diag(c(0.1,0.1,0.1,0.1,0.1,0.1),6), params.observed=diag(c('vep1','vep2','vep3','ven1','ven2','ven3'),6)
  )
  ecov
  
  dynm<-prep.formulaDynamics(formula=formula,startval=c(arp=.5,arn=.1,
                                                        betapn0=-0.1,
                                                        betanp0=0.1),
                             isContinuousTime=F)
  
  Temp.discrete<-c()
  Track.discrete<-c()
  for(run in 1:5){
    data.sim<-read.table(paste0("DFAdat",run,".txt"))
    #    data<-read.table(paste0("TVdfan10T70",run,".csv"),sep=",",na.strings = "NaN",
    #                      col.names =c("V1","V2","V3","V4","V5","V6"))
    
    data.sim$id<-rep(1:np,each=nt)
    data.sim$timepoint<-rep(1:nt,np)
    
    data.sim$ind<- 1
    data.sim$ind[data.sim$timepoint<=round(70/3)]<- -1
    data.sim$ind[data.sim$timepoint>=round(2*70/3)]<- -1
    
    data <- dynr.data(data.sim, id="id", time="timepoint", observed=c("V1","V2","V3","V4","V5","V6"),covariates="ind")  
    model <- dynr.model(dynamics=dynm, measurement=meas, noise=ecov, initial=initial, 
                        data=data,outfile="cooked")#,
    # transform = trans)
    #model@ub[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(1.5,5))
    #model@lb[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(-1.5,5))
    
    #Check
    #model@dynamics@jacobian
    
    #define a transformation function for those pesky variances
    # Estimate free parameters
    res <- dynr.cook(model,verbose=FALSE)
    temp<-round(coef(res),3)
    track<-try(res@ bad.standard.errors)
    Track.discrete<-rbind(Track,track)
    Temp.discrete<-rbind(Temp,temp)
  }
  

  
}







# Prepare for cooking
# put all the recipes together

Temp<-c()
Track<-c()
for(run in 1:5){
  data.sim<-read.table(paste0("DFAdat",run,".txt"))
#    data<-read.table(paste0("TVdfan10T70",run,".csv"),sep=",",na.strings = "NaN",
#                      col.names =c("V1","V2","V3","V4","V5","V6"))
  
  data.sim$id<-rep(1:np,each=nt)
  data.sim$timepoint<-rep(1:nt,np)

  data.sim$ind<- 1
  data.sim$ind[data.sim$timepoint<=round(70/3)]<- -1
  data.sim$ind[data.sim$timepoint>=round(2*70/3)]<- -1

  data <- dynr.data(data.sim, id="id", time="timepoint", observed=c("V1","V2","V3","V4","V5","V6"),covariates="ind")  
  model <- dynr.model(dynamics=dynm, measurement=meas, noise=ecov, initial=initial, 
                    data=data,outfile="cooked")#,
                   # transform = trans)
#model@ub[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(1.5,5))
#model@lb[model$param.names  %in% c("arp","arn","betapn0","betanp0","betanp1")]<-c(rep(-1.5,5))

#Check
#model@dynamics@jacobian

#define a transformation function for those pesky variances
# Estimate free parameters
res <- dynr.cook(model,verbose=FALSE)
temp<-round(coef(res),3)
track<-try(res@ bad.standard.errors)
Track<-rbind(Track,track)
Temp<-rbind(Temp,temp)
}

apply(Temp,2,mean)
model$param.names
#b11 = .54;
#b22 = .01;
#b120 = -.18;
#b121 = 0;
#b210 = -.26;
#b211 = .91;

# Q process noise/factor covariance matrix
#-------------------------------------------
#  Q=diag([.09 .03 0 .05]);

# R measurement error covariance matrix
#--------------------------------------------
#  R=diag([.11 .15 .06 .22 .18 .04]);


# S factor loading matrix
#--------------------------
#  S=[1 0 0 0;
#     .75 0 0 0;
#     .84 0 0 0;
#     0 1 0 0;
#     0 1.49 0 0;
#     0 1.80 0 0];

# Examine results
summary(res)

#------------------------------------------------------------------------------
# some miscellaneous nice functions

# print recipes in forms that will look nice in LaTeX
printex(meas)

printex(ecov)


# get the estimated parameters from a cooked model/data combo
coef(res)


# get the log likelihood, AIC, and BIC from a cooked model/data combo
logLik(res)
AIC(res)
BIC(res)


# compare true parameters to estimated ones
trueParams <- c(-.3, -.7, 2.2, 1.5, 0)
data.frame(name=c('Spring', 'Damping', 'DynVar', 'MeasVar', 'IniPos'), true=trueParams, estim=coef(res))


# compare estimated smoothed latent states to true
# simulated ones
sm <- data.frame(t(res@eta_smooth_final))
cor(sm, t(tx)[-1,])


#------------------------------------------------------------------------------
# End


