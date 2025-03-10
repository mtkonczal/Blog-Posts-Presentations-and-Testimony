library(tidyverse)

# Code from here:
# https://macrosimulation.org/a_new_keynesian_3_equation_model

#Clear the environment
rm(list=ls(all=TRUE))

# Set number of periods
Q=50

# Set number of scenarios
S=3

# Set period in which shock/shift will occur
s=5

# Create (S x Q)-matrices that will contain the simulated data
y=matrix(data=0,nrow=S,ncol=Q) # Income/output
p=matrix(data=0,nrow=S,ncol=Q) # Inflation rate
r=matrix(data=0,nrow=S,ncol=Q) # Real interest rate
rs=matrix(data=0,nrow=S,ncol=Q) # Stabilising interest rate

# Set constant parameter values
a1=0.3  # Sensitivity of inflation with respect to output gap 
a2=0.7  # Sensitivity of output with respect to interest rate
b=1     # Sensitivity of central bank to inflation gap
a3=(a1*(1/(b*a2) + a2))^(-1)

# Set parameter values for different scenarios
A=matrix(data=10,nrow=S,ncol=Q) # autonomous spending
pt=matrix(data=2,nrow=S,ncol=Q) # Inflation target
ye=matrix(data=5,nrow=S,ncol=Q) # Potential output
vt=matrix(data=0,nrow=S,ncol=Q) # Supply shocks

vt[1,s:s]=1  # scenario 1: Reversal Supply Shock
vt[1,(s+1):(s+1)]=-1  # scenario 1: Reversal Supply Shock
vt[2,s:s]=1  # scenario 1: Reversal Supply Shock
vt[3,s:s]=1  # scenario 1: Reversal Supply Shock
vt[3,(s+1):(s+1)]=-1  # scenario 1: Reversal Supply Shock


# Initialise endogenous variables at equilibrium values
y[,1]=ye[,1]
p[,1]=pt[,1]
rs[,1]=(A[,1] - ye[,1])/a1 
r[,1]=rs[,1]

# Simulate the model by looping over Q time periods for S different scenarios
for (i in 1:S){
  
  for (t in 2:Q){
    
    #(1) IS curve
    y[i,t] = A[i,t] - a1*r[i,t-1]
    
    #(2) Phillips Curve
    p[i,t] = p[i,t-1] +a2*(y[i,t]-ye[i,t]) + vt[i,t]
    
    #(3) Stabilising interest rate
    rs[i,t] = (A[i,t] - ye[i,t])/a1
    
    #(4) Monetary policy rule, solved for r
    if(i == 3)
      {a3 = 0}
    
    r[i,t] = rs[i,t] + a3*(p[i,t]-pt[i,t])
    
  } # close time loop
}   # close scenarios loop


### Plot results

### Plots
# Set maximum period for plots
Tmax=15

par(mfrow = c(4, 3))
# Output under different scenarios
plot(vt[1, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Supply Shock", ylim=range(-1:1))
title(main="Scenario 1: Supply Shock and Reversal", cex=0.8 ,line=2)
#lines(y[2, 1:(Tmax+1)],lty=2, lwd=2)
#lines(y[3, 1:(Tmax+1)],lty=3, lwd=2)
#legend("bottomright", legend=c("1: aggregate demand boost", "2: rise inflation target", "3: rise potential output"), lty=1:3, cex=0.8, bty = "n", y.intersp=0.8)
plot(vt[2, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Supply Shock", ylim=range(-1:1))
title(main="Scenario 2: Supply Shock Level", sub="Supply Shock", cex=0.8 ,line=2)
plot(vt[3, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Supply Shock", ylim=range(-1:1))
title(main="Scenario 3: Supply Shock and Reversal,\nNo Fed Response", cex=0.8 ,line=2)

plot(r[1, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=c(14,20)) 
title(main="Fed's Interest Rates")
plot(r[2, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=c(14,20)) 
plot(r[3, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=c(14,20)) 

plot(y[1, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=range(y[1, 1:Tmax],y[3, 1:(Tmax)])) 
title(main="Output")
#lines(y[2, 1:(Tmax+1)],lty=2, lwd=2)
#lines(y[3, 1:(Tmax+1)],lty=3, lwd=2)
#legend("bottomright", legend=c("1: aggregate demand boost", "2: rise inflation target", "3: rise potential output"), lty=1:3, cex=0.8, bty = "n", y.intersp=0.8)
plot(y[2, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=range(y[1, 1:Tmax],y[3, 1:(Tmax)])) 
plot(y[3, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab="Output", ylim=range(y[1, 1:Tmax],y[3, 1:(Tmax)])) 

# Inflation under different scenarios
plot(p[1, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab=expression(pi), ylim=range(p[1, 2:Tmax],p[3, 2:(Tmax)])) 
title(main="Inflation", cex=0.8 ,line=2)
plot(p[2, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab=expression(pi), ylim=range(p[1, 2:Tmax],p[3, 2:(Tmax)])) 
title(sub="Time period", cex=0.8 ,line=2)
plot(p[3, 1:(Tmax+1)],type="l", col=1, lwd=2, lty=1, xlab="", xlim=range(2:(Tmax)), ylab=expression(pi), ylim=range(p[1, 2:Tmax],p[3, 2:(Tmax)])) 

#lines(p[2, 1:(Tmax+1)],lty=2, lwd=2)
#lines(p[3, 1:(Tmax+1)],lty=3, lwd=2)
#legend("bottomright", legend=c("1: aggregate demand boost", "2: rise inflation target", "3: rise potential output"), lty=1:3, cex=0.8, bty = "n", y.intersp=0.8)