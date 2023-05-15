rape_cases<-read.csv("crime against women.csv")
View(rape_cases)
mean(rape_cases$Rape)
median(rape_cases$Rape)
mode(rape_cases$Rape)

dim(rape_cases)
nrow(rape_cases)
ncol(rape_cases)
str(rape_cases)
summary(rape_cases)
sd(rape_cases$Rape)
sd(rape_cases$Rape)
sd(rape_cases$Kidnapping.and.Abduction)
sd(rape_cases$Dowry.Deaths)
sd(rape_cases$Assault.on.women.with.intent.to.outrage.her.modesty)
sd(rape_cases$Insult.to.modesty.of.Women)
sd(rape_cases$Cruelty.by.Husband.or.his.Relatives)
var(rape_cases$Rape)
var(rape_cases$Kidnapping.and.Abduction)
var(rape_cases$Dowry.Deaths)
var(rape_cases$Assault.on.women.with.intent.to.outrage.her.modesty)
var(rape_cases$Insult.to.modesty.of.Women)
var(rape_cases$Cruelty.by.Husband.or.his.Relatives)
cov(rape_cases$Rape,rape_cases$Cruelty.by.Husband.or.his.Relatives,rape_cases$Dowry.Deaths,na.rm=T)
cor(rape_cases$Dowry.Deaths,rape_cases$Cruelty.by.Husband.or.his.Relatives)
cor(rape_cases$Cruelty.by.Husband.or.his.Relatives,rape_cases$Insult.to.modesty.of.Women)
cor(rape_cases$Rape,rape_cases$Kidnapping.and.Abduction)
cor(rape_cases$Rape,rape_cases$Assault.on.women.with.intent.to.outrage.her.modesty)
boxplot(rape_cases$Rape)


#removing the columns and normalising the data
cases=subset(rape_cases,select=-c(STATE.UT,DISTRICT))
cases
log_scale=log(cases$Rape)
summary(log_scale)
plot(density(cases$Rape))
min(log_scale,na.rm=TRUE)
library(moments)
skewness(cases)
hist(cases$Rape)
#splitting the data
train<-sample(1:nrow(rape_cases),0.7*nrow(rape_cases))
trains<-rape_cases[train,]
test<-rape_cases[-train,]
dim(trains)
dim(test)
#LINEAR REGRESSION MODEL

reg<-lm(Rape~Assault.on.women.with.intent.to.outrage.her.modesty,data=trains)
summary(reg)
coefficients(reg)
plot(reg)
#x=24
as.numeric(coefficients(reg)[ 10.0961634 ]+coefficients(reg)[ 0.4213397 ]*24)
#or
func=data.frame(Rape=24)
func
predict(reg,func)
#
train2<-sample(1:nrow(rape_cases),0.7*nrow(rape_cases))
trains2<-rape_cases[train2,]
test2<-rape_cases[-train2,]
dim(trains2)
dim(test2)
reg2<-lm(Rape~Kidnapping.and.Abduction,data=trains2 )
summary(reg2)
coefficients(reg2)

as.numeric(coefficients(reg2)[18.1204801]+coefficients(reg2)[0.5805244 ]*30)
plot(reg2)


