
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
a<-with(PIL,cbind(aJuly,aOctober))
aerror<-with(PIL,cbind(aerror1,aerror2))
library(gplots)
windows(60,70)
#A
#bottom,left,top,right
par(las=2) 
par(mar=c(2.5,5,1,1)) 
par(mfrow=c(6,2))
with(PIL,
     barplot2(a,  beside=T,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("","","","","","","",""),
	 
	          xlab="",ylab="", cex.lab=1.25, cex.axis=1.25,
			  plot.ci=TRUE,
         ci.u=a+aerror,ci.l=a-aerror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,36),tcl=0))
		axis(2,tcl=-0.5,cex.axis=1)
		
		title(main="Eucalyptus pilularis",  font.main=4,cex.main=1.4)
	text(0.5,33.5, "(a)", pos=4,cex=1,font=2)	
par(las=3)	
mtext(side = 2, text =expression(bold(A[sat])), line = 3.75,font=2, cex=0.8)
mtext(side = 2, text =expression(bold((mu~mol~m^-2~s^-1))),
font=2,cex=0.75, line = 2.5)	
par(las=1)		
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
a<-with(POP,cbind(aJuly,aOctober))
aerror<-with(POP,cbind(aerror1,aerror2))
#bottom,left,top,right
	par(mar=c(2.5,1,1,5))
with(POP,
     barplot2(a,  beside=T,axis.lty=1,space=c(0.05,0.8),axes=FALSE,yaxt="n",
	 names.arg=c("","","","","","","",""),
	 
	          xlab="",ylab="",plot.ci=TRUE,
         ci.u=a+aerror,ci.l=a-aerror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,36),tcl=0))
		axis(4,labels=FALSE,tcl=-0.5)
		title(main="Eucalyptus populnea",  font.main=4,cex.main=1.4)
par(las=1)	
axis(4,labels=TRUE,tck=-0.01)

setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
gs<-with(PIL,cbind(gJuly,gOctober))
gserror<-with(PIL,cbind(gerror1,gerror2))	
#bottom,left,top,right
	par(mar=c(2.5,5,1,1)) 
	
#GS
with(PIL,
     barplot2(gs,  beside=T,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("","","","","","","",""),
         xlab="",ylab="",plot.ci=TRUE,
         cex.lab=1.25,ci.u=gs+gserror,ci.l=gs-gserror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,0.3),tcl=0))
		axis(2,tcl=-0.5,cex.axis=1,at=seq(0,0.3,by=0.1),labels=seq(0,0.3,by=0.1))
	text(0.5,0.28, "(b)", pos=4,cex=1,font=2)
par(las=3)	
mtext(side = 2, text =expression(bold(g[s])), line = 3.75,font=2, cex=0.8)
mtext(side = 2, text =expression(bold((mol~m^-2~s^-1))),
font=2,cex=0.75, line = 2.5)	
par(las=1)
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
gs<-with(POP,cbind(gJuly,gOctober))
gserror<-with(POP,cbind(gerror1,gerror2))
#bottom,left,top,right
par(mar=c(2.5,1,1,5))
with(POP,
     barplot2(gs,  beside=T,axis.lty=1,space=c(0.05,0.8),axes=FALSE,
	 names.arg=c("","","","","","","",""),
         xlab="",ylab="",plot.ci=TRUE,
         ci.u=gs+gserror,ci.l=gs-gserror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,0.6),tcl=0))
		axis(4,labels=TRUE,tcl=-0.5,cex.axis=1)

setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
pilci1<-with(PIL,cbind(pilci,pilcio))
pilcierror1<-with(PIL,cbind(pilcierror1,pilcioerror2))
#bottom,left,top,right
	par(mar=c(2.5,5,1,1)) 
	
	setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
g1<-with(PIL,cbind(g1July,g1October))
g1error<-with(PIL,cbind(g1error1,g1error2))	
#bottom,left,top,right
	par(mar=c(2.5,5,1,1)) 
	
	
#G1
with(PIL,
     barplot2(g1,  beside=T,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("","","","","","","",""),
         xlab="",ylab=expression(bold(g[1])),plot.ci=TRUE,
         cex.lab=1.25,ci.u=g1+g1error,ci.l=g1-g1error,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,3.5),tcl=0))
		axis(2,tcl=-0.5,cex.axis=1,at=seq(0,3.5,by=1),labels=seq(0,3.5,by=1))
	text(0.5,3.25, "(c)", pos=4,cex=1,font=2)	
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
g1<-with(POP,cbind(g1July,g1October))
gserror<-with(POP,cbind(g1error1,g1error2))
#bottom,left,top,right
par(mar=c(2.5,1,1,5))
with(POP,
     barplot2(g1,  beside=T,axis.lty=1,space=c(0.05,0.8),axes=FALSE,
	 names.arg=c("","","","","","","",""),
         xlab="",ylab="",plot.ci=TRUE,
         ci.u=g1+g1error,ci.l=g1-g1error,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,7.0),tcl=0))
		axis(4,labels=TRUE,tcl=-0.5,cex.axis=1)

PIL<-read.csv("pil.csv",sep=",", header=TRUE)
v<-with(PIL,cbind(vJuly,vOctober))
verror<-with(PIL,cbind(verror1,verror2))	
#bottom,left,top,right
	par(mar=c(2.5,5,1,1)) 
	
	
#VPD
with(PIL,
     barplot2(v,  beside=T,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("","","","","","","",""),
         xlab="",ylab=expression(bold(VPD~~(kPa))),plot.ci=TRUE,
         cex.lab=1.25,ci.u=v+verror,ci.l=v-verror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,2.0),tcl=0))
		axis(2,tcl=-0.5,cex.axis=1)
	text(0.5,1.8, "(d)", pos=4,cex=1,font=2)	
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
g1<-with(POP,cbind(vJuly,vOctober))
gserror<-with(POP,cbind(verror1,verror2))
#bottom,left,top,right
par(mar=c(2.5,1,1,5))
with(POP,
     barplot2(v,  beside=T,axis.lty=1,space=c(0.05,0.8),axes=FALSE,
	 names.arg=c("","","","","","","",""),
         xlab="",ylab="",plot.ci=TRUE,
         ci.u=v+verror,ci.l=v-verror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,2.0),tcl=0))
		axis(4,labels=TRUE,tcl=-0.5,cex.axis=1)
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
pilci1<-with(PIL,cbind(pilci,pilcio))
pilcierror1<-with(PIL,cbind(pilcierror1,pilcioerror2))
#bottom,left,top,right
	par(mar=c(2.5,5,1,1)) 
	
	#CICA

with(PIL,
     barplot2(pilci1,  beside=T,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("","","","","","","",""),
         xlab="",ylab=expression(bold(C[i]/C[a])),plot.ci=TRUE,
         cex.lab=1.4,ci.u=pilci1+pilcierror1,ci.l=pilci1-pilcierror1,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,1),tcl=0))
		axis(2,tcl=-0.5,cex.axis=1)
	text(0.5,0.9, "(e)", pos=4,cex=1,font=2)	
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
ci<-with(POP,cbind(ciju,cij))
cierror<-with(POP,cbind(cijuerror1,cijerror2))
#bottom,left,top,right
par(mar=c(2.5,1,1,5))
with(POP,
     barplot2(ci,  beside=TRUE,axis.lty=1,space=c(0.05,0.8),axes=FALSE,
	 names.arg=c("","","","","","","",""),
         xlab="",ylab="",
		 plot.ci=TRUE,
         ci.u=ci+cierror,ci.l=ci-cierror,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,1),tcl=0))
		axis(4,labels=TRUE,tcl=-0.5)
		
			
	
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
ite<-with(PIL,cbind(July,October))
error<-with(PIL,cbind(error1,error2))		
	par(mar=c(4,5,1,1)) 
#ITE
par(las=3)
with(PIL,
     barplot2(ite,  beside=TRUE,axis.lty=1,space=c(0.05,0.8),yaxt="n",
	 names.arg=c("380","700","380","700","380","700","380","700"),cex.name=1.1,
         font.lab=2,
		 
		 plot.ci=TRUE,
         ci.u=ite+error,ci.l=ite-error,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,20)))
par(las=1)		
axis(2,tcl=-0.5,cex.axis=1,at=seq(0,20,by=5),labels=seq(0,20,by=5))
 
names<-"July"
name<-"October"
par(las=3)
mtext(side = 2, text =expression(bold("ITE")), line = 3.75,font=2, cex=0.8)
mtext(side = 2, text =expression(bold((mu * mol ~ CO[2] ~ mmol~H[2]*O))),
font=2,cex=0.75, line = 2.5)
text(0.5,18.0, "(f)", pos=4,cex=1.25,font=2)
par(las=1)
mtext(names,1,2.75,adj=0.225,font=2, cex= 0.75)
mtext(name,1,2.75,adj=0.85,font=2, cex= 0.75)
par(las=3)
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
ite<-with(POP,cbind(July,October))
error<-with(POP,cbind(error1,error2))
par(mar=c(4,1,1,5))
par(las=3)
with(POP,
     barplot2(ite,  beside=TRUE,axis.lty=1,space=c(0.05,0.8),axes=FALSE,
	 names.arg=c("380","700","380","700","380","700","380","700"),cex.name=1.1,
         ylab="",
		 plot.ci=TRUE,
         ci.u=ite+error,ci.l=ite-error,col=c("blue","red",
	     "white","white","blue","red",
		 "white","white"),border=c("black","black","blue",
		"red","black","black","blue","red"),ylim=c(0,20)))
		par(las=1)
		axis(4,labels=TRUE,tc1=-0.05)
names<-"July"
name<-"January"
par(las=1)
mtext(names,1,2.75,adj=0.225,font=2, cex= 0.75)
mtext(name,1,2.75,adj=0.85,font=2, cex= 0.75)
# this is the one you paste in word
 dev.copy2eps(file="figA_GS_CICA_G1_VPD_ITE.eps")
	