#SYMULACJE
library('MASS')
b=c(3,3)
chi=c()
alpha=c()
X=matrix(rnorm(1000*2,0,1/sqrt(1000)),ncol=2)
n=X%*%b
for (i in 1:10000){
  if (i%%100==0){
    print(i)
  }
  Y=rpois(1000,lambda=exp(n))
  model_nb=glm.nb(Y~X-1)
  model_pois=glm(Y~X-1,family='poisson')
  chi=c(chi,-2*as.numeric((logLik(model_pois)-logLik(model_nb))))
  alpha=c(alpha,1/model_nb$theta)
}

par(mfrow=c(1,2))
hist(chi,breaks=500,freq=F,main="Histogram statystyki X^2 ",xlab="X^2",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlim=c(-0.1,5),ylim=c(0,12))
curve(0.5*dchisq(x,1),add=T,col="red",lwd=2,xlim=c(-0.1,5))
hist(chi,breaks=500,freq=F,main="Histogram statystyki X^2: zbliżenie ",xlab="X^2",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlim=c(-0.1,5),ylim=c(0,5))
curve(0.5*dchisq(x,1),add=T,col="red",lwd=2,xlim=c(-0.1,5))
par(mfrow=c(1,1))
qqplot(x=qchisq(ppoints(5000), df = 1),y = chi,xlab="Kwantyle teoretyczne rozkładu Chi^2 z 1 stopniem swobody",ylab="Kwantyle empiryczne statystyki X^2",xlim=c(0,16),ylim=c(0,16),cex.axis=1.5, cex.main=1.5, cex.sub=1.5,main="Wykres kwantylowo-kwantylowy statystyki X^2")

par(mfrow=c(1,2))
hist(alpha,breaks=50,freq=F,main="Histogram estymatora parametru α",xlab="estymator α",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean = 0,sd=quantile(alpha,0.75)/qnorm(0.75)),add=T,col="red",lwd=2)
hist(alpha,breaks=50,freq=F,main="Histogram estymatora parametru α: zbliżenie",xlab="estymator α",ylim=c(0,15),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
curve(dnorm(x,mean = 0,sd=quantile(alpha,0.75)/qnorm(0.75)),add=T,col="red",lwd=2)
par(mfrow=c(1,1))
qqplot(x=qnorm(ppoints(5000), 0,quantile(alpha,0.75)/qnorm(0.75)),y = alpha,xlab="Kwantyle teoretyczne rozkładu normalnego",ylab="Kwantyle empiryczne estymatora α",cex.axis=1.5, cex.main=1.5, cex.sub=1.5,main="Wykres kwantylowo-kwantylowy estymatora α")
#ANALIZA DANYCH
dane=read.csv('DebTrivedi.csv')
dane=dane[,c(2,7,8,9,14,16,19)]
par(mfrow=c(1,1))
#Histogram
hist(dane$ofp,freq=F,breaks=100,main=paste("Histogram liczby wizyt w gabinecie lekarskim\nŚrednia=",round(mean(dane$ofp),2),"; Wariancja=",round(sd(dane$ofp)^2,2),sep=""),cex.axis=1.5, cex.main=1.5, cex.sub=1.5,xlab="Liczba wizyt w gabinecie lekarskim")
qqplot(qnbinom(ppoints(nrow(dane)),size = )dane$ofp)
dane$logofp=log(dane$ofp+0.5)
library('ggplot2')
library('plyr')
library('xtable')
#Histogram wizyty w szpitalu
dane$hospg=as.character(dane$hosp)
dane$hospg[dane$hosp>2]="3 lub więcej"
xtable(count(dane,'hosp'))
xtable(count(dane,'hospg'))
ggplot(dane,aes(hospg,y=logofp,group=hospg))+
  geom_violin(aes(fill=hospg))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od liczby wizyt w szpitalu")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 25),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")
#Histogram stan zdrowia
xtable(count(dane,'health'))
ggplot(dane,aes(health,y=logofp,group=health))+
  geom_violin(aes(fill=health))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od stanu zdrowia pacjenta")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 25),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")#Histogram choroby
dane$numchrong=as.character(dane$numchron)
dane$numchrong[dane$numchron>2]="3 lub więcej"
xtable(count(dane,'numchron'))
xtable(count(dane,'numchrong'))
ggplot(dane,aes(numchrong,y=logofp,group=numchrong))+
  geom_violin(aes(fill=numchrong))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od liczby chorób przewlekłych pacjenta")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 20),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")#Histogram choroby
#Histogram płeć
count(dane,'gender')
ggplot(dane,aes(gender,y=logofp,group=gender))+
  geom_violin(aes(fill=gender))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od płci pacjenta")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 25),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")#Histogram choroby
#Histogram edukacja
dane$schoolg[dane$school<6]="0) kindergarden"
dane$schoolg[dane$school>=6  & dane$school<9]="1) elementary school"
dane$schoolg[dane$school>=9  & dane$school<12]="2) junior high school"
dane$schoolg[dane$school>=12  & dane$school<16]="3) senior high school"
dane$schoolg[dane$school>=16 & dane$school<18]="4) bachelors"
dane$schoolg[dane$school==18]="5) masters"
count(dane,'school')
count(dane,'schoolg')
ggplot(dane,aes(schoolg,y=logofp,group=schoolg))+
  geom_violin(aes(fill=schoolg))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od poziomu ukończonej edukacji pacjenta")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 20),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")#Histogram choroby
#Histogram ubezpieczenie
count(dane,'privins')
ggplot(dane,aes(privins,y=logofp,group=privins))+
  geom_violin(aes(fill=privins))+
  geom_boxplot(width=0.1,color="black", fill="blue")+
  xlab("log(Liczba wizyt w gabinecie lekarskim)")+
  ggtitle("Histogram log(liczba wizyt w gabinecie lekarskim) w zależności od posiadania dodatkowego ubezpieczenia pacjenta")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 18),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))+
  theme(legend.position = "none")#Histogram choroby

#Modele
dane$schoolg2[dane$school<6]="no education"
dane$schoolg2[dane$school>=6  & dane$school<18]="some education"
dane$schoolg2[dane$school==18]="full education"
library('pscl')
#PR
model_p=glm(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,family="poisson")
summary(model_p)
as.numeric(2*length(model_p$coefficients)*log(nrow(dane))-2*logLik(model_p))
model_p2=glm(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane,family="poisson")
model_p3=glm(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane,family="poisson")
model_p4=glm(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane,family="poisson")
model_p5=glm(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,family="poisson")
anova(model_p2,model_p,test='Chisq') #reduced school
as.numeric(2*length(model_p2$coefficients)*log(nrow(dane))-2*logLik(model_p2))
anova(model_p3,model_p,test='Chisq') #no gender
as.numeric(2*length(model_p3$coefficients)*log(nrow(dane))-2*logLik(model_p3))
anova(model_p4,model_p,test='Chisq') #no privins
as.numeric(2*length(model_p4$coefficients)*log(nrow(dane))-2*logLik(model_p4))
anova(model_p5,model_p,test='Chisq') #no numchron
as.numeric(2*length(model_p5$coefficients)*log(nrow(dane))-2*logLik(model_p5))
winner=model_p
summary(winner) #best one
length(winner$coefficients)
as.numeric(2*length(winner$coefficients)-2*logLik(winner))
as.numeric(2*length(winner$coefficients)*log(nrow(dane))-2*logLik(winner))
sum(dpois(x = 0,lambda = predict.glm(winner,type="response")))/nrow(dane)
#NBR
model_nb=glm.nb(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane)
summary(model_nb)
as.numeric(2*length(model_nb$coefficients)*log(nrow(dane))-2*logLik(model_nb))
model_nb2=glm.nb(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane)
model_nb3=glm.nb(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane)
model_nb4=glm.nb(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane)
model_nb5=glm.nb(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane)
anova(model_nb2,model_nb,test='Chisq') #reduced school
as.numeric(2*length(model_nb2$coefficients)*log(nrow(dane))-2*logLik(model_nb2))
anova(model_nb3,model_nb,test='Chisq') #no gender
as.numeric(2*length(model_nb3$coefficients)*log(nrow(dane))-2*logLik(model_nb3))
anova(model_nb4,model_nb,test='Chisq') #no privins
as.numeric(2*length(model_nb4$coefficients)*log(nrow(dane))-2*logLik(model_nb4))
anova(model_nb5,model_nb,test='Chisq') #no numchron
as.numeric(2*length(model_nb5$coefficients)*log(nrow(dane))-2*logLik(model_nb5))
winner=model_nb2
summary(winner) #best one
1/1.2253
length(winner$coefficients)
as.numeric(2*length(winner$coefficients)-2*logLik(winner))
as.numeric(2*length(winner$coefficients)*log(nrow(dane))-2*logLik(winner))
sum(dnbinom(x = 0,size =winner$theta,mu = predict.glm(winner,type="response")))/nrow(dane)
#ZIPR
library('lmtest')
model_zipr=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
summary(model_zipr)
as.numeric(2*length(model_zipr$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zipr))
model_zipr2=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane,dist="poisson")
model_zipr3=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
model_zipr4=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane,dist="poisson")
model_zipr5=zeroinfl(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
lrtest(model_zipr2,model_zipr) #reduced school
as.numeric(2*length(model_zipr2$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zipr2))
lrtest(model_zipr3,model_zipr) #no gender
as.numeric(2*length(model_zipr3$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zipr3))
lrtest(model_zipr4,model_zipr) #no privins
as.numeric(2*length(model_zipr4$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zipr4))
lrtest(model_zipr5,model_zipr) #no numchron
as.numeric(2*length(model_zipr5$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zipr5))
winner=model_zipr
summary(winner) #best one
length(winner$coefficients[[1]])
as.numeric(2*length(winner$coefficients[[1]])-2*logLik(winner))
as.numeric(2*length(winner$coefficients[[1]])*log(nrow(dane))-2*logLik(winner))
sum(predict(winner,type="prob")[,1])/nrow(dane)
#ZINBR
model_zinbr=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="negbin")
summary(model_zinbr)
as.numeric(2*length(model_zinbr$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zinbr))
model_zinbr2=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane,dist="negbin")
model_zinbr3=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane,dist="negbin")
model_zinbr4=zeroinfl(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane,dist="negbin")
model_zinbr5=zeroinfl(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="negbin")
lrtest(model_zinbr2,model_zinbr) #reduced school
as.numeric(2*length(model_zinbr2$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zinbr2))
lrtest(model_zinbr3,model_zinbr) #no gender
as.numeric(2*length(model_zinbr3$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zinbr3))
lrtest(model_zinbr4,model_zinbr) #no privins
as.numeric(2*length(model_zinbr4$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zinbr4))
lrtest(model_zinbr5,model_zinbr) #no numchron
as.numeric(2*length(model_zinbr5$coefficients[[1]])*log(nrow(dane))-2*logLik(model_zinbr5))
winner=model_zinbr2
summary(winner) #best one
1/1.5312 
length(winner$coefficients[[1]])
as.numeric(2*length(winner$coefficients[[1]])-2*logLik(winner))
as.numeric(2*length(winner$coefficients[[1]])*log(nrow(dane))-2*logLik(winner))
sum(predict(winner,type="prob")[,1])/nrow(dane)
#Hurdle PR
model_hurdle_p=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
summary(model_hurdle_p)
as.numeric(2*length(model_hurdle_p$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_p))
model_hurdle_p2=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane,dist="poisson")
model_hurdle_p3=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
model_hurdle_p4=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane,dist="poisson")
model_hurdle_p5=hurdle(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="poisson")
lrtest(model_hurdle_p2,model_hurdle_p) #reduced school
as.numeric(2*length(model_hurdle_p2$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_p2))
lrtest(model_hurdle_p3,model_hurdle_p) #no gender
as.numeric(2*length(model_hurdle_p3$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_p3))
lrtest(model_hurdle_p4,model_hurdle_p) #no privins
as.numeric(2*length(model_hurdle_p4$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_p4))
lrtest(model_hurdle_p5,model_hurdle_p) #no numchron
as.numeric(2*length(model_hurdle_p5$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_p5))
winner=model_hurdle_p
summary(winner) #best one
length(winner$coefficients[[1]])
as.numeric(2*length(winner$coefficients[[1]])-2*logLik(winner))
as.numeric(2*length(winner$coefficients[[1]])*log(nrow(dane))-2*logLik(winner))
sum(predict(winner,type="prob")[,1])/nrow(dane)
#Hurdle NBR
model_hurdle_nb=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist = "negbin")
summary(model_hurdle_nb)
as.numeric(2*length(model_hurdle_nb$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_nb))
model_hurdle_nb2=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg2)+factor(privins)-1,dane,dist="negbin")
model_hurdle_nb3=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(schoolg)+factor(privins)-1,dane,dist="negbin")
model_hurdle_nb4=hurdle(ofp~factor(hospg)+factor(health)+factor(numchrong)+factor(gender)+factor(schoolg)-1,dane,dist="negbin")
model_hurdle_nb5=hurdle(ofp~factor(hospg)+factor(health)+factor(gender)+factor(schoolg)+factor(privins)-1,dane,dist="negbin")
lrtest(model_hurdle_nb2,model_hurdle_nb) #reduced school
as.numeric(2*length(model_hurdle_nb2$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_nb2))
lrtest(model_hurdle_nb3,model_hurdle_nb) #no gender
as.numeric(2*length(model_hurdle_nb3$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_nb3))
lrtest(model_hurdle_nb4,model_hurdle_nb) #no privins
as.numeric(2*length(model_hurdle_nb4$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_nb4))
lrtest(model_hurdle_nb5,model_hurdle_nb) #no numchron
as.numeric(2*length(model_hurdle_nb5$coefficients[[1]])*log(nrow(dane))-2*logLik(model_hurdle_nb5))
winner=model_hurdle_nb2
summary(winner) #best one
1/1.4146
length(winner$coefficients[[1]])
as.numeric(2*length(winner$coefficients[[1]])-2*logLik(winner))
as.numeric(2*length(winner$coefficients[[1]])*log(nrow(dane))-2*logLik(winner))
sum(predict(winner,type="prob")[,1])/nrow(dane)
## real
sum(dane$ofp==0)/nrow(dane)
