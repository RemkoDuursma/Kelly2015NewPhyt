setwd("P:/JK Australia Manuscripts")
EUCBIOMASS<-read.csv("Gas_exchange.csv",sep=",", header=TRUE)
names(EUCBIOMASS)
str(EUCBIOMASS)
EUCBIOMASS$H2O<-factor(EUCBIOMASS$H2O,levels=c("D","ND"))
EUCBIOMASS$CO2<-factor(EUCBIOMASS$CO2,levels=c("ELEVATED","AMBIENT"))
EUCBIOMASS$M<-factor(EUCBIOMASS$M,levels=c("A","B"))
EUCBIOMASS$SP<-factor(EUCBIOMASS$SP,levels=c("POP","PIL"))
library(nlme)
#Linear mixed effects model

#A
with(EUCBIOMASS,plot(A~ST))
EUCA.lme<-lme(A~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCA.lme)
EUCA<-anova(EUCA.lme)
EUCA
#Testing normality of residuals
qqnorm(resid(EUCA.lme))
qqline(resid(EUCA.lme))
shapiro.test(resid(EUCA.lme))
with(EUCBIOMASS,tapply(A,list(M,ST),mean))

#GS
EUCGS.lme<-lme(log(gs)~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCGS.lme)
EUCGS<-anova(EUCGS.lme)
EUCGS
#Testing normality of residuals
qqnorm(resid(EUCGS.lme))
qqline(resid(EUCGS.lme))
shapiro.test(resid(EUCGS.lme))
with(EUCBIOMASS,tapply(gs,list(M,ST),mean))

#CI
EUCCI.lme<-lme(Ci~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCCI.lme)
EUCCI<-anova(EUCCI.lme)
EUCCI
#Testing normality of residuals
qqnorm(resid(EUCCI.lme))
qqline(resid(EUCCI.lme))
shapiro.test(resid(EUCCI.lme))
with(EUCBIOMASS,tapply(Ci,list(M,ST),mean))

#G1
EUCG1.lme<-lme(log(g1)~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCG1.lme)
EUCG1<-anova(EUCG1.lme)
EUCG1
#Testing normality of residuals
qqnorm(resid(EUCG1.lme))
qqline(resid(EUCG1.lme))
shapiro.test(resid(EUCG1.lme))
with(EUCBIOMASS,tapply(g1,list(M,ST),mean))

#VPD
EUCVPD.lme<-lme(log(VPD)~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCVPD.lme)
EUCVPD<-anova(EUCVPD.lme)
EUCVPD
#Testing normality of residuals
qqnorm(resid(EUCVPD.lme))
qqline(resid(EUCVPD.lme))
shapiro.test(resid(EUCVPD.lme))
with(EUCBIOMASS,tapply(VPD,list(M,ST),mean))

#CI_CA
EUCCI_CA.lme<-lme(Ci_Ca~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCCI_CA.lme)
EUCCI_CA<-anova(EUCCI_CA.lme)
EUCCI_CA
#Testing normality of residuals
qqnorm(resid(EUCCI_CA.lme))
qqline(resid(EUCCI_CA.lme))
shapiro.test(resid(EUCCI_CA.lme))
with(EUCBIOMASS,tapply(Ci_Ca,list(M,ST),mean))

#ITE
EUCITE.lme<-lme(log(ITE)~SP*CO2*H2O*M,random=~1|GH/R,data=EUCBIOMASS)
summary(EUCITE.lme)
EUCITE<-anova(EUCITE.lme)
EUCITE
#Testing normality of residuals
qqnorm(resid(EUCITE.lme))
qqline(resid(EUCITE.lme))
shapiro.test(resid(EUCITE.lme))
with(EUCBIOMASS,tapply(ITE,list(M,ST),mean))



