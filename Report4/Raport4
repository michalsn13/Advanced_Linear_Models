#Zadanie 1
library(mvtnorm)
library(nlme)
library(xtable)
set.seed(100)
n=20
k=3
p=4
N=n*k
X=matrix(rnorm(N*(p-1),0,1/sqrt(N)),nrow=N,ncol=p-1)
beta=c(rep(3,2),rep(0,p-1-2))
gamma=2
rho=0.3
Sigma=gamma^2*(matrix(rep(rho,k*k),nrow=k,ncol=k)+(1-rho)*diag(k))
Y=matrix(nrow=n,ncol=k)
for (i in 0:(n-1)){
  Y[(i+1),1:k]=rmvnorm(n = 1,mean = t(X[(1+i*k):((i+1)*k),1:(p-1)]%*%beta),sigma = Sigma)
}
data=data.frame(y=c(t(Y)),id=rep(1:n,each=k),time=rep(1:k,n),X=X)
model=gls(y~X,data = data,
correlation = corCompSymm(form =∼ 1|id),
weights = varIdent(form =∼ 1),method="REML")
summary(model)
Sigma_dash=getVarCov(model)
Sigma_dash
num=0
den=0
for (i in 1:(N/k)){
  index=i*k
  X_person=cbind(rep(1,k),X[(index-k+1):index,])
  num=num+t(X_person)%*%solve(Sigma_dash)%*%Y[i,]
  den=den+t(X_person)%*%solve(Sigma_dash)%*%X_person
}
#estymator beta
beta_dash=solve(den)%*%num
beta_dash
model$coefficients
#norma supremum różnic między estymatorami Beta
max(abs(beta_dash-model$coefficients))
#estymator cov_beta
cov_beta_dash=solve(den)
cov_beta_dash
vcov(model)
#norma supremum różnic między estymatorami macierzy kowariancji Beta
max(abs(cov_beta_dash-vcov(model)))
#estymator rho
rho_dash=coef(model$modelStruct$corStruct)
rho_dash
#norma różnicy między rho, a jego estymatorem
abs(rho-rho_dash)
#estymator gamma
gamma_dash=model$sigma
gamma_dash
#norma różnicy między gamma, a jego estymatorem
abs(gamma-gamma_dash)

#Zadanie 2=-6
####zmienne do zmiany (być może trzeba będzie xlim i ylim na histogramach)
n=20
k=3
p=4
method="ML"
###
N=n*k
X=matrix(rnorm(N*(p-1),0,1/sqrt(N)),nrow=N,ncol=p-1)
beta=c(rep(3,2),rep(0,p-1-2))
gamma=2
rho=0.3
Sigma=gamma^2*(matrix(rep(rho,k*k),nrow=k,ncol=k)+(1-rho)*diag(k))

betas=data.frame(matrix(nrow=0,ncol=4))
colnames(betas)=c("b0","b1","b2","b3")
rhos=c()
gammas=c()
for (rep in 1:500){
  Y=matrix(nrow=n,ncol=k)
  for (i in 0:(n-1)){
    Y[(i+1),1:k]=rmvnorm(n = 1,mean = t(X[(1+i*k):((i+1)*k),1:(p-1)]%*%beta),sigma = Sigma)
  }
  data=data.frame(y=c(t(Y)),id=rep(1:n,each=k),time=rep(1:k,n),X=X)
  model=gls(y~X,data = data,
            correlation = corCompSymm(form =∼ 1|id),
            weights = varIdent(form =∼ 1),method=method)
  betas[rep,]=model$coefficients
  tryCatch({rhos=c(rhos,intervals(model)$corStruct["Rho", "est."])},error=function(e){})
  gammas=c(gammas,model$sigma)

}
den=0
for (i in 1:(N/k)){
  index=i*k
  X_person=cbind(rep(1,k),X[(index-k+1):index,])
  den=den+t(X_person)%*%solve(Sigma)%*%X_person
}
cov_beta=solve(den)
hist(betas$b0,main=paste("Histogram estymatorów β0\nŚrednia=",round(mean(betas$b0),4)," Odch.std.=",round(sd(betas$b0),4)),col="blue",xlim=c(-1.5,1.5),ylim=c(0,1.2),xlab="estymator β0", freq=F,breaks=15,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=0,sd=sqrt(diag(cov_beta)[1])),col="red",add=T,lwd=2)
hist(betas$b1,main=paste("Histogram estymatorów β1\nŚrednia=",round(mean(betas$b1),4)," Odch.std.=",round(sd(betas$b1),4)),col="blue",xlim=c(min(betas$b1)-2,max(betas$b1)+2),ylim=c(0,0.25),xlab="estymator β1", freq=F,breaks=15,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean=beta[1],sd=sqrt(diag(cov_beta)[2])),col="red",add=T,lwd=2)
apply(betas,2,mean)-c(0,beta)
max(abs(apply(betas,2,mean)-c(0,beta)))
hist(rhos,main=paste("Histogram estymatorów korelacji błędów ρ\nŚrednia=",round(mean(rhos),4)," Odch.std.=",round(sd(rhos),4)),col="blue",xlim=c(-0.5,1.2),ylim=c(0,2.75),xlab="estymator ρ", freq=F,breaks=15,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mean(rhos)-rho
hist(gammas,main=paste("Histogram estymatorów wariancji błędów γ\nŚrednia=",round(mean(gammas),4)," Odch.std.=",round(sd(gammas),4)),col="blue",xlim=c(1,3),ylim=c(0,2.75),xlab="estymator γ",freq=F,breaks=15,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
mean(gammas)-gamma


