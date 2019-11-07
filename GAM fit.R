library(readr)
fev_data <- read_table2("./fev_data.txt")
#load the mgcv package for the 
#gam function
library(mgcv)
age = fev_data$age.yrs
sex = fev_data$ind.Male
fev = fev_data$fev.L
fit.model <- gam(fev ~ sex + s(age,bs='ps'))

summary(fit.model)
nage = seq(3,18,by=0.5)
nsex = rep(1,length(nage)) #female
fsex = rep(0,length(nage))

#Predict  males and females
predict.m <- predict(fit.model,
                     newdata=data.frame(age=nage,sex=nsex,length=length(nsex)),
                     ,type="response")
predict.f <- predict(fit.model,
                     newdata=data.frame(age=nage,sex=fsex,length=length(fsex)),
                     ,type="response")
jage = jitter(jage)
plot(jage,fev,ylab = "FEV 1",
              xlab="Age",pch=1)
points(jage[sex==0],fev[sex==0],col=2,pch=1)
points(jage[sex==1],fev[sex==1],col=4,pch=1)
lines(nage,predict.m,lwd=2,col=4)
lines(nage,predict.f,lwd=2,col=2)
#GAM on more than one parameter
age = fev_data$age.yrs
sex = fev_data$ind.Male
height = fev_data$ht.in
smoke = fev_data$ind.Smoke
fev = fev_data$fev.L
fit.model <- gam(fev ~ sex + smoke 
                           + s(age,bs='ps')
                           + s(height,bs='ps'))
                          
lm.fit<- lm(fev~ sex + smoke + age + height)
summary(fit.model)