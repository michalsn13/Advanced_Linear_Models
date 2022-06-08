#ANALIZA DANYCH
#Zadanie 1
dane=read.csv('lista_1.csv')
dane=subset(dane,select=colnames(dane)[2:4])
#Zadanie 2
boxplot(numeracy ~ success,dane,main="Numeracy results vs Graduation success",col=c("red","blue"),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
cor(dane$numeracy,dane$success)
#Zadanie 3
boxplot(anxiety ~ success,dane,main="Anxiety levels vs Graduation success",col=c("red","blue"),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
cor(dane$anxiety,dane$success)
#Zadanie 4
model1=glm(success~numeracy+anxiety,dane,family = "binomial")
model1$coefficients
model1$fitted.values
summary(model1)
predict.glm(object=model1,newdata=data.frame(numeracy=10,anxiety=13),type = "response")
z=14.238581+    0.577352*10+   (-1.384069)*13
sigmoid=exp(z)/(1+exp(z))
sigmoid
library('pROC')
plot(roc(response=dane$success, predictor=model1$fitted.values,direction="auto"),
     col="black", lwd=5, main="ROC curve: success ~ anxiety+numeracy",,legacy.axes = TRUE,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
#Zadanie 5
#probit
model2=glm(success~numeracy+anxiety,dane,family = binomial(link="probit"))
model2$coefficients
summary(model2)
predict.glm(object=model2,newdata=data.frame(numeracy=10,anxiety=13),type = "response")
lines(roc(response=dane$success, predictor=model2$fitted.values,direction="auto"),
     col="red", lwd=4, main="ROC curve: success ~ anxiety+numeracy",legacy.axes = TRUE)
#cauchit
model3=glm(success~numeracy+anxiety,dane,family = binomial(link="cauchit"))
model3$coefficients
summary(model3)
predict.glm(object=model3,newdata=data.frame(numeracy=10,anxiety=13),type = "response")
lines(roc(response=dane$success, predictor=model3$fitted.values,direction="auto"),
      col="blue", lwd=3, main="ROC curve: success ~ anxiety+numeracy",legacy.axes = TRUE)
#cloglog
model4=glm(success~numeracy+anxiety,dane,family = binomial(link="cloglog"))
model4$coefficients
summary(model4)
predict.glm(object=model4,newdata=data.frame(numeracy=10,anxiety=13),type = "response")
lines(roc(response=dane$success, predictor=model4$fitted.values,direction="auto"),
      col="green", lwd=2, main="ROC curve: success ~ anxiety+numeracy",legacy.axes = TRUE)
legend(x = 0.4,y=0.55,legend=c("logit","probit","cauchit","cloglog"),col = c("black","red","blue","green"),lty = c(1,1,1,1))
#Zadanie 6
#a)
p=model1$fitted.values
S=diag(p*(1-p))
X=matrix(data=c(rep(1,length(dane$numeracy)),as.numeric(dane$numeracy),as.numeric(dane$anxiety)),ncol=3)
J=t(X)%*%S%*%X
sqrt(diag(solve(J)))
summary(model1)
summary(model1)$coefficients[,"Std. Error"]
model1
#b)
alpha=0.05
summary(model1)
t=68.029-28.286
1-pchisq(t,df=3-1)
#c)
library(generalhoslem)
logitgof(dane$success,fitted(model1))
#d)
par(mfrow=c(1,1))
p_values=data.frame(matrix(ncol=3,nrow=0))
colnames(p_values)=c("β0","β1","β2")
epsilons=10^seq(1,-9,-1)
for (epsilon in epsilons){
  model=glm(success~numeracy+anxiety,dane,family = "binomial",epsilon=epsilon)
  from_glm=summary(model)$coefficients[,"Pr(>|z|)"]
  p_values[nrow(p_values)+1,]=c(from_glm)
}
plot(log10(epsilons),p_values$"β0","b",pch=19,xlab="log_10(epsilon)",ylab="p-value",main="p-values of parameters estimators for different epsilons",ylim=c(0,0.09),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(log10(epsilons),p_values$"β1","b",pch=19,col="red")
lines(log10(epsilons),p_values$"β2","b",pch=19,col="blue")
abline(h = 0.05,col="green",lty=2)
legend(x = -3.5,y=0.092,legend=c("β0","β1","β2"),col = c("black","red","blue"),pch=c(19,19),lwd=c(2,2))
epsilons[which.min(p_values$ß2)]
################################################################################
#SYMULACJE
#Zadanie 1
x=matrix(rnorm(400*3,0,sqrt(1/400)),nrow = 400)
x=data.frame(x)
z=3*(x$X1+x$X2+x$X3)
x$result=1/(1+exp(-z))
p=x$result
S=diag(p*(1-p))
X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=400)
J=t(X)%*%S%*%X
solve(J)
b1=c()
b2=c()
b3=c()
cov_matrix=matrix(rep(0,3^2),ncol=3)
for (i in 1:1000){
  x=matrix(rnorm(400*3,0,sqrt(1/400)),nrow = 400)
  x=data.frame(x)
  z=3*(x$X1+x$X2+x$X3)
  x$result=1/(1+exp(-z))
  x$result=rbinom(n=400,size = 1,prob = x$result) #threshold?
  #model=glm(result~X1+X2+X3-1,x,family="binomial")
  model=glm(result~X1+X2+X3-1,x,family="binomial")
  b1=c(b1,as.numeric(model$coefficients[1]))
  b2=c(b2,as.numeric(model$coefficients[2]))
  b3=c(b3,as.numeric(model$coefficients[3]))
  p=model$fitted.values
  S=diag(p*(1-p))
  X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=400)
  J=t(X)%*%S%*%X
  cov_matrix=cov_matrix+solve(J)
}
cov_matrix/1000
par(mfrow=c(1,3),oma = c(0,0,2,0))
hist(b1,main=paste("β1:μ=",round(mean(b1),4),"σ=",round(sd(b1),4)),freq=F,ylim=c(0,0.2),xlab="β1",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[1]),from = -10,to=10,add=T,col="red")
hist(b2,main=paste("β2:μ=",round(mean(b2),4),"σ=",round(sd(b2),4)),freq=F,ylim=c(0,0.2),xlab="β2",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[2]),from = -10,to=10,add=T,col="red")
hist(b3,main=paste("β3:μ=",round(mean(b3),4),"σ=",round(sd(b3),4)),freq=F,ylim=c(0,0.2),xlab="β3",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[3]),from = -10,to=10,add=T,col="red")
mtext("Rozkład estymatorów parametrów w regresji logistycznej", outer = TRUE, cex = 1.48)
mean(b1)-3
mean(b2)-3
mean(b3)-3

#Zadanie 2
#Wpływ n
x=matrix(rnorm(100*3,0,sqrt(1/100)),nrow = 100)
x=data.frame(x)
z=3*(x$X1+x$X2+x$X3)
x$result=1/(1+exp(-z))
p=x$result
S=diag(p*(1-p))
X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=100)
J=t(X)%*%S%*%X
solve(J)
b1=c()
b2=c()
b3=c()
cov_matrix=matrix(rep(0,3^2),ncol=3)
for (i in 1:1000){
  x=matrix(rnorm(100*3,0,sqrt(1/100)),nrow = 100)
  x=data.frame(x)
  z=3*(x$X1+x$X2+x$X3)
  x$result=1/(1+exp(-z))
  x$result=rbinom(n=100,size = 1,prob = x$result) #threshold?
  model=glm(result~X1+X2+X3-1,x,family="binomial")
  b1=c(b1,as.numeric(model$coefficients[1]))
  b2=c(b2,as.numeric(model$coefficients[2]))
  b3=c(b3,as.numeric(model$coefficients[3]))
  p=model$fitted.values
  S=diag(p*(1-p))
  X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=100)
  J=t(X)%*%S%*%X
  cov_matrix=cov_matrix+solve(J)
}
cov_matrix/1000
par(mfrow=c(1,3),oma = c(0,0,2,0))
hist(b1,main=paste("β1:μ=",round(mean(b1),4),"σ=",round(sd(b1),4)),freq=F,ylim=c(0,0.2),xlab="β1",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[1]),from = -10,to=10,add=T,col="red")
hist(b2,main=paste("β2:μ=",round(mean(b2),4),"σ=",round(sd(b2),4)),freq=F,ylim=c(0,0.2),xlab="β2",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[2]),from = -10,to=10,add=T,col="red")
hist(b3,main=paste("β3:μ=",round(mean(b3),4),"σ=",round(sd(b3),4)),freq=F,ylim=c(0,0.2),xlab="β3",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[3]),from = -10,to=10,add=T,col="red")
mtext("Rozkład estymatorów parametrów w regresji logistycznej", outer = TRUE, cex = 1.48)
mean(b1)-3
mean(b2)-3
mean(b3)-3

#Zadanie 3
library(MASS)
x=matrix(mvrnorm(n=400,mu=rep(0,3),Sigma= 1/400*(matrix(rep(0.3,3^2),nrow=3)+0.7*diag(3))),nrow=400)
x=data.frame(x)
z=3*(x$X1+x$X2+x$X3)
x$result=1/(1+exp(-z))
p=x$result
S=diag(p*(1-p))
X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=400)
J=t(X)%*%S%*%X
J
solve(J)
b1=c()
b2=c()
b3=c()
cov_matrix=matrix(rep(0,3^2),ncol=3)
for (i in 1:1000){
  x=matrix(rnorm(400*3,0,sqrt(1/400)),nrow = 400)
  x=data.frame(x)
  z=3*(x$X1+x$X2+x$X3)
  x$result=1/(1+exp(-z))
  x$result=rbinom(n=400,size = 1,prob = x$result) #threshold?
  #model=glm(result~X1+X2+X3-1,x,family="binomial")
  model=glm(result~X1+X2+X3-1,x,family="binomial")
  b1=c(b1,as.numeric(model$coefficients[1]))
  b2=c(b2,as.numeric(model$coefficients[2]))
  b3=c(b3,as.numeric(model$coefficients[3]))
  p=model$fitted.values
  S=diag(p*(1-p))
  X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3)),ncol=3,nrow=400)
  J=t(X)%*%S%*%X
  cov_matrix=cov_matrix+solve(J)
}
cov_matrix/1000
par(mfrow=c(1,3),oma = c(0,0,2,0))
hist(b1,main=paste("β1:μ=",round(mean(b1),4),"σ=",round(sd(b1),4)),freq=F,ylim=c(0,0.2),xlab="β1",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[1]),from = -10,to=10,add=T,col="red")
hist(b2,main=paste("β2:μ=",round(mean(b2),4),"σ=",round(sd(b2),4)),freq=F,ylim=c(0,0.2),xlab="β2",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[2]),from = -10,to=10,add=T,col="red")
hist(b3,main=paste("β3:μ=",round(mean(b3),4),"σ=",round(sd(b3),4)),freq=F,ylim=c(0,0.2),xlab="β3",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[3]),from = -10,to=10,add=T,col="red")
mtext("Rozkład estymatorów parametrów w regresji logistycznej", outer = TRUE, cex = 1.48)
mean(b1)-3
mean(b2)-3
mean(b3)-3

#Zadanie 4
#Wpływ features
x=matrix(rnorm(400*20,0,sqrt(1/400)),nrow = 400)
x=data.frame(x)
z=3*(x$X1+x$X2+x$X3)
x$result=1/(1+exp(-z))
p=x$result
S=diag(p*(1-p))
X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3),as.numeric(x$X4),as.numeric(x$X5),as.numeric(x$X6),as.numeric(x$X7),as.numeric(x$X8),as.numeric(x$X9),as.numeric(x$X10),as.numeric(x$X11),as.numeric(x$X12),as.numeric(x$X13),as.numeric(x$X14),as.numeric(x$X15),as.numeric(x$X16),as.numeric(x$X17),as.numeric(x$X18),as.numeric(x$X19),as.numeric(x$X20)),ncol=20,nrow=400)
J=t(X)%*%S%*%X
diag(J)
diag(solve(J))
b1=c()
b2=c()
b3=c()
cov_matrix=matrix(rep(0,20^2),ncol=20)
for (i in 1:1000){
  x=matrix(rnorm(400*20,0,sqrt(1/400)),nrow = 400)
  x=data.frame(x)
  z=3*(x$X1+x$X2+x$X3)
  x$result=1/(1+exp(-z))
  x$result=rbinom(n=400,size = 1,prob = x$result) #threshold?
  model=glm(result~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20-1,x,family="binomial")
  b1=c(b1,as.numeric(model$coefficients[1]))
  b2=c(b2,as.numeric(model$coefficients[2]))
  b3=c(b3,as.numeric(model$coefficients[3]))
  p=model$fitted.values
  S=diag(p*(1-p))
  X=matrix(data=c(as.numeric(x$X1),as.numeric(x$X2),as.numeric(x$X3),as.numeric(x$X4),as.numeric(x$X5),as.numeric(x$X6),as.numeric(x$X7),as.numeric(x$X8),as.numeric(x$X9),as.numeric(x$X10),as.numeric(x$X11),as.numeric(x$X12),as.numeric(x$X13),as.numeric(x$X14),as.numeric(x$X15),as.numeric(x$X16),as.numeric(x$X17),as.numeric(x$X18),as.numeric(x$X19),as.numeric(x$X20)),ncol=20,nrow=400)
  J=t(X)%*%S%*%X
  cov_matrix=cov_matrix+solve(J)
}
diag(cov_matrix/1000)
par(mfrow=c(1,3),oma = c(0,0,2,0))
hist(b1,main=paste("β1:μ=",round(mean(b1),4),"σ=",round(sd(b1),4)),freq=F,ylim=c(0,0.2),xlab="β1",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[1]),from = -10,to=10,add=T,col="red")
hist(b2,main=paste("β2:μ=",round(mean(b2),4),"σ=",round(sd(b2),4)),freq=F,ylim=c(0,0.2),xlab="β2",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[2]),from = -10,to=10,add=T,col="red")
hist(b3,main=paste("β3:μ=",round(mean(b3),4),"σ=",round(sd(b3),4)),freq=F,ylim=c(0,0.2),xlab="β3",xlim=c(-10,10),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=3,sd=sds[3]),from = -10,to=10,add=T,col="red")
mtext("Rozkład estymatorów parametrów w regresji logistycznej", outer = TRUE, cex = 1.48)
mean(b1)-3
mean(b2)-3
mean(b3)-3
summary(model)
