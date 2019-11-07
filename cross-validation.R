library(splines)
library(MASS)
data(mcycle); attach(mcycle)
P = 31 #number of splines + 1
knots <- seq(min(times),max(times),by = 
               (max(times)-min(times))/P)
#remove the knot associated with the min
#and max time 
knots = knots[-1]; knots = knots[-31];
X = bs(times,knots=knots)
#compute Omega
Omega <- diag(ncol(X))*2
for (i in 2:ncol(X)){
  Omega[i,i-1] = -1; Omega[i-1,i] = -1; 
}

#our possible lambdas
lambda = seq(0.1,10.0,by=0.25)
#our Predicted Squared Errors
PSE = rep(0,length(lambda))
for (j in 1:length(lambda)){
  for(i in 1:length(accel)){
    tX = X[-i,]
    tB = solve(t(tX)%*%tX+lambda[j]*Omega)%*%t(tX)%*%accel[-i]
    Y_pred = X[i,,drop=F]%*%tB
    PSE[j] = PSE[j]+1/length(accel)*(accel[i]-Y_pred)^2
  }
}
plot(lambda,PSE,type='l',xlab="Lambda",lwd=2,
     ylab="Predicted Squared Error")

B = solve(t(X)%*%X+1*Omega)%*%t(X)%*%accel
nY = X%*%B
plot(times,accel,xlab="Time (ms)"
     ,ylab = "Acceleration", pch=16,
     , col='red')
lines(times,nY,lwd=2)

plot(times,nY,xlab="Time",Y=)