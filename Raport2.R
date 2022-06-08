dane=read.csv('sklep')
dane=subset(dane,select=colnames(dane)[c(2,3,5,4)])
library('ggplot2')
qplot(hour,no.klients, facets = ~as.logical(events),col = day, data = dane)+geom_point(size=3)+
  scale_color_manual(values=c("#ff0000", "#ff8000","#ffff00","#00ff00","#00ffff","#0000ff", "#000057"))+
  labs(x="Hour",y="Number of clients",col="Day")+
  ggtitle("Number of clients served depending on different characteristics")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"),legend.text =element_text(size=15))
  
qplot(hour,no.klients, facets= ~day, data = dane)+
  labs(x="Hour",y="Number of clients")+
  ggtitle("Number of clients served depending on different characteristics")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"))
for (item in Map(list,c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),seq(1,7))){
  day=as.character(item[1])
  number=as.numeric(item[2])
  dane$day_num[dane$day == day] <- number
}
dane$day <- factor(dane$day,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
dane$day_num=as.numeric(dane$day_num)
ggplot(dane,aes(day,no.klients,fill=day))+
  geom_violin()+
  geom_boxplot(width=0.1,col="black",fill="white")+
  labs(x="Day",y="Number of clients")+
  ggtitle("Number of clients served depending on week day")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"))+
  theme(legend.position = "none") 
ggplot(dane,aes(hour,no.klients,group=hour))+
  geom_violin(aes(fill=hour))+
  geom_boxplot(width=0.1,col="black",fill="white")+
  labs(x="Hour",y="Number of clients")+
  ggtitle("Number of clients served depending on the hour")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=10,color="black"),axis.text.y = element_text(size=15,color="black"))+
  theme(legend.position = "none")
ggplot(dane,aes(as.logical(events),no.klients,group=as.logical(events)))+
  geom_violin(aes(fill=as.logical(events)))+
  geom_boxplot(width=0.1,col="black",fill="white")+
  labs(x="Sport event occurance",y="Number of clients")+
  ggtitle("Number of clients served depending on sport event occurance")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=15,color="black"),axis.text.y = element_text(size=15,color="black"))+
  theme(legend.position = "none") 
#Zadanie 3
model=glm(no.klients~factor(day)+factor(events)+factor(hour)+factor(day):factor(hour)+factor(day):factor(events)+factor(hour):factor(events)+factor(day):factor(hour):factor(events),dane,family="poisson",)
summary(model)
rows=rownames(summary(model)$coefficients)
length(rows)
sum(grepl("events",rows,fixed=T))
p=summary(model)$coefficients[,"Pr(>|z|)"]
sum(p<0.05)
p_events=p[grepl("events",rows,fixed=T)]
sum(p_events<0.05)
rows[grepl("events",rows,fixed=T)][p_events<0.05]
###
model2=glm(no.klients~factor(day)+factor(hour)+factor(day):factor(hour),dane,family="poisson")
anova(model,model2,test = "Chisq")
summary(model2)
rows=rownames(summary(model2)$coefficients)
length(rows)
p=summary(model2)$coefficients[,"Pr(>|z|)"]
cols=as.data.frame(do.call(rbind, strsplit(names(p),split=":")))
colnames(cols)=c("day","hour")
cols$day[grepl("hour",cols$day,fixed=T)]="factor(day)None"
cols$hour[grepl("day",cols$hour,fixed=T)]="factor(hour)None"
cols$day <- factor(cols$day, levels=unique(cols$day))
cols$hour <- factor(cols$hour, levels=unique(cols$hour),)
cols$p=as.numeric(p)
ggplot(cols, aes(day, hour, fill= p)) + 
  geom_tile()+
  labs(x="Day",y="Hour",fill="p-value")+
  ggtitle("Model's parameters p-values (with and without interactions)")+
  theme_light()+
  theme(plot.title=element_text(hjust=0.5,size = 30),axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16),axis.text.x = element_text(size=10,color="black"),axis.text.y = element_text(size=10,color="black"))
#Zadanie 4
dane$work_day=(dane$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
dane$hour_interval[dane$hour<12]=1
dane$hour_interval[dane$hour>=12 & dane$hour<16]=2
dane$hour_interval[dane$hour>=16 & dane$hour<20]=3
dane$hour_interval[dane$hour>=20 & dane$hour<24]=4
model3=glm(no.klients~factor(work_day)+factor(hour_interval)+factor(work_day):factor(hour_interval),dane,family="poisson",)
summary(model3)
rows=rownames(summary(model3)$coefficients)
length(rows)
anova(model,model3,test = "Chisq")
#Zadanie 5
betas=as.numeric(model3$coefficients)
f<-function(a,b){
  return(round(sum(dane$no.klients[dane$work_day==a & dane$hour_interval==b]),4))
}
f2<-function(a,b){
  return(round(mean(dane$no.klients[dane$work_day==a & dane$hour_interval==b]),4))
}
table=data.frame(matrix(c(mapply(f,a=T,b=seq(1,4)),mapply(f,a=F,b=seq(1,4)),
                          mapply(f2,a=T,b=seq(1,4)),mapply(f2,a=F,b=seq(1,4)),
                          c("a","a","a","a","a","a","a","a"),
                          round(c(betas[1]+betas[2],betas[1]+betas[2]+betas[3]+betas[6],betas[1]+betas[2]+betas[4]+betas[7],betas[1]+betas[2]+betas[5]+betas[8],betas[1],betas[1]+betas[3],betas[1]+betas[4],betas[1]+betas[5]),4)),ncol=8,byrow=T,))
colnames(table)=c("R 8-12","R 12-16","R 16-20","R 20-23","W 8-12","W 12-16","W 16-20","W 20-23")
table[nrow(table)+1,]=round(exp(as.numeric(table[nrow(table),])),4)
rownames(table)=c("³¹czna liczba klientów","œrednia liczba klientów","postaæ predyktora liniowego","wartoœæ predyktora liniowego","wartoœæ predykowana")
library(xtable)
xtable(table)
#Zadanie 6
A=matrix(c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0),nrow=3,byrow=T)
A
summary(model3)
X=matrix(data=c(rep(1,nrow(dane)),dane$work_day,dane$hour_interval==2,dane$hour_interval==3,dane$hour_interval==4,(dane$work_day & dane$hour_interval==2),(dane$work_day & dane$hour_interval==3),(dane$work_day & dane$hour_interval==4)),byrow=F,nrow=nrow(dane))
S=diag(dane$no.klients)
J=t(X)%*%S%*%X
Sigma=solve(J)
W=t(A%*%betas)%*%solve(A%*%Sigma%*%t(A))%*%(A%*%betas)
W
1-pchisq(W,df=3)
#Zadanie 7
lambdas=as.numeric(table[nrow(table),])
table2=data.frame(matrix(c(qpois(0.5,lambdas),qpois(0.75,lambdas),qpois(0.9,lambdas)),nrow=3,byrow=T))
colnames(table2)=colnames(table)
rownames(table2)=c("50% quantile","75% quantile","90% quantile")
xtable(table2)
