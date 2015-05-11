adderrorbars <- function(x,y,SE,direction,barlen=0.075,...){
    if(direction=="up")arrows(x0=x, x1=x, y0=y, y1=y + SE, code=2, 
angle=90, length=barlen,...)
    if(direction=="down")arrows(x0=x, x1=x, y0=y, y1=y - SE, code=2, 
angle=90, length=barlen,...)
    if(direction=="left")arrows(x0=x, x1=x-SE, y0=y, y1=y, code=2, 
angle=90, length=barlen,...)
    if(direction=="right")arrows(x0=x, x1=x+SE, y0=y, y1=y, code=2, 
angle=90, length=barlen,...)

if(direction=="updown"){
    arrows(x0=x, x1=x, y0=y, y1=y + SE, code=2, 
angle=90, length=barlen,...)
    arrows(x0=x, x1=x, y0=y, y1=y - SE, code=2, 
angle=90, length=barlen,...)
}

}
windows(8,7)
#PIL BIO
par(mfrow=c(3,2))
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(mar=c(2,5,2,2)) 
par(las=3)


bp <- with(PIL, barplot(BIO,col=c("blue","red","white","white"),yaxt="n",
border=c("black","black","blue","red"),ylim=c(0,3000),

ylab="Total plant mass (g)",font.lab=2,cex.lab=1.5))
with(PIL,adderrorbars(x=bp, y=BIO, SE=BIOE,direction="updown"))
axis(1,labels=FALSE,tcl=0,cex.axis=1.5)
axis(2,labels=TRUE,tcl=-0.5,font=1,cex.axis=1.5)
title(main="Eucalyptus pilularis",  font.main=4,cex.main=1.5)
text(0.05,2850, "(a)", pos=4,cex=1.5,font=2)

#POP BIO
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(mar=c(2,2,2,5)) 
par(las=3)

bp <- with(POP, barplot(BIO,col=c("blue","red","white","white"),axes=FALSE,
border=c("black","black","blue","red"),ylim=c(0,1300)
))
with(POP,adderrorbars(x=bp, y=BIO, SE=BIOE,direction="updown"))
axis(4,labels=TRUE,tcl=-0.5,font=1,cex.axis=1.5)
axis(1,labels=FALSE,tcl=0,cex.axis=1.5)
title(main="Eucalyptus populnea",  font.main=4,cex.main=1.5)

#PIL TRANS
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(las=3)
par(mar=c(2,5,2,2)) 

bp <- with(PIL, barplot(TRA,col=c("blue","red","white","white"),yaxt="n",
border=c("black","black","blue","red"),ylim=c(0,360),
ylab="Total transpiration (l)",font.lab=2,cex.lab=1.5))
with(PIL,adderrorbars(x=bp, y=TRA, SE=TRAE,direction="updown"))
axis(1,labels=FALSE,tcl=0,cex.axis=1.5)
axis(2,labels=TRUE,font=1,cex.axis=1.5)
text(0.05,330, "(b)", pos=4,cex=1.5,font=2)
#POP TRANS
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
POP<-read.csv("pop.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(mar=c(2,2,2,5))

bp <- with(POP, barplot(TRA,col=c("blue","red","white","white"),axes=FALSE,
border=c("black","black","blue","red"),ylim=c(0,360)))
with(POP,adderrorbars(x=bp, y=TRA, SE=TRAE,direction="updown"))
axis(4,labels=FALSE,tcl=-0.5,font=1,cex.axis=1.5)
axis(1,labels=FALSE,tcl=0,cex.axis=1.5)

#PIL WUE
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
PIL<-read.csv("pil.csv",sep=",", header=TRUE)
par(las=1)

#bottom,left,top and right
par(mar=c(4.5,5,2,2)) 
bp <- with(PIL, barplot(WUE,col=c("blue","red","white","white"),yaxt="n",
border=c("black","black","blue","red"),
ylim=c(0,8),font.lab=2,
 names.arg=c("380","700","380","700"),cex.names=1.5))
with(PIL,adderrorbars(x=bp, y=WUE, SE=WUEE,direction="updown"))
axis(1,labels=FALSE,tcl=0,cex.axis=1.5)
axis(2,labels=TRUE,font=1,cex.axis=1.5)
par(las=3)
mtext(side = 2, text =expression(bold("WUE")), line = 4,font=2, cex=0.9)
mtext(side = 2, text =expression(bold((mmol ~ C ~ mol^-1 ~ H[2]*O))), line=2.5,
font=2,cex=0.8)
par(las=1)
names<-"Well-watered"
name<-"Droughted"
mtext(names,1,3,adj=0.15,font=2, cex= 0.9)
mtext(name,1,3,adj=0.85,font=2, cex= 0.9)
text(0.05,7, "(c)", pos=4,cex=1.5,font=2)
#bottom,left,top and right
par(mar=c(4.5,2,2,5)) 
par(las=1)

#POP WUE
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
POP<-read.csv("pop.csv",sep=",", header=TRUE)

bp <- with(POP, barplot(WUE,col=c("blue","red","white","white"),axes=FALSE,
border=c("black","black","blue","red"),ylim=c(0,8),
names.arg=c("380","700","380","700"),cex.name=1.5
))

with(POP,adderrorbars(x=bp, y=WUE, SE=WUEE,direction="updown"))

axis(4,labels=FALSE,tcl=-0.5,font=1,cex.axis=1)
axis(1,labels=FALSE,tcl=0,cex.axis=1)
names<-"Well-watered"
name<-"Droughted"
mtext(names,1,3,adj=0.15,font=2, cex= 0.9)
mtext(name,1,3,adj=0.85,font=2, cex= 0.9)
# this is the one you paste in word
 dev.copy2eps(file="figBIO_TRANS_WUE.eps")