setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
PILBIOMASS<-read.csv("PILTRANSAA.csv",sep=",", header=TRUE)
names(PILBIOMASS)
str(PILBIOMASS)

windows(width=8, height=4) #, pointsize=18)
par(xaxs="i",yaxs="i")

par(las=2) 
par(mar=c(4.5,4.5,1,1)) 
par(xaxs="i",yaxs="i")
par(mfrow=c(1,2), cex.lab=1)
#PIL
par(las=1)
with(PILBIOMASS,plot(E[ST=="PILAD"]~D[ST=="PILAD"],col="blue",pch=1,
ylim=range(0,1.1*max(E)),xlim=range(0,1.05*max(D)),
ylab="",xlab=expression(bold(Day))))
title(main="Eucalyptus pilularis",  font.main=4,cex.main=1)

with(PILBIOMASS,arrows(D[ST=="PILAD"],
 ESE[ST=="PILAD"], D[ST=="PILAD"], LSE[ST=="PILAD"]
 , length = .035, angle = 90, code = 3,col="blue")) 
#or
       
with(PILBIOMASS,points(E[ST=="PILAND"]~D[ST=="PILAND"],col="blue",pch=16))
with(PILBIOMASS,arrows(D[ST=="PILAND"],
 ESE[ST=="PILAND"], D[ST=="PILAND"], LSE[ST=="PILAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(PILBIOMASS,points(E[ST=="PILED"]~D[ST=="PILED"],col="red",pch=1))
with(PILBIOMASS,arrows(D[ST=="PILED"],
 ESE[ST=="PILED"], D[ST=="PILED"], LSE[ST=="PILED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(PILBIOMASS,points(E[ST=="PILEND"]~D[ST=="PILEND"],col="red",pch=16))
with(PILBIOMASS,arrows(D[ST=="PILEND"],
 ESE[ST=="PILEND"], D[ST=="PILEND"], LSE[ST=="PILEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 2, text =expression(bold(Transpiration~~(l~week^-1))), line = 2.5,font=2, cex=1.0)
legend("topleft",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
cex=0.75,bty="n",
 pch = c(16,1,16,1), col=c("blue","blue","red","red"), #xjust = .5, yjust = .5,
              )
par(las=1) 
setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL BIOMASS")
POPBIOMASS<-read.csv("POPTRANSAA.csv",sep=",", header=TRUE)
names(POPBIOMASS)
str(POPBIOMASS)


#POP
#bottom,left,top,right
par(xaxs="i",yaxs="i")
par(las=2) 
par(mar=c(4.5,1,1,4.5)) 
par(las=1)
with(POPBIOMASS,plot(E[ST=="POPAD"]~D[ST=="POPAD"],col="blue",pch=1,yaxt="n",
ylim=c(0, 1.1*max(E)),xlim=c(0,1.05*max(D)),
ylab="",xlab=expression(bold(Day))))
title(main="Eucalyptus populnea",  font.main=4,cex.main=1)
with(POPBIOMASS,arrows(D[ST=="POPAD"],
 ESE[ST=="POPAD"], D[ST=="POPAD"], LSE[ST=="POPAD"]
 , length = .035, angle = 90, code = 3,col="blue")) 
 axis(4,labels=TRUE,tcl=-0.5,cex.axis=1)
#or
       
with(POPBIOMASS,points(E[ST=="POPAND"]~D[ST=="POPAND"],col="blue",pch=16))
with(POPBIOMASS,arrows(D[ST=="POPAND"],
 ESE[ST=="POPAND"], D[ST=="POPAND"], LSE[ST=="POPAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(POPBIOMASS,points(E[ST=="POPED"]~D[ST=="POPED"],col="red",pch=1))
with(POPBIOMASS,arrows(D[ST=="POPED"],
 ESE[ST=="POPED"], D[ST=="POPED"], LSE[ST=="POPED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(POPBIOMASS,points(E[ST=="POPEND"]~D[ST=="POPEND"],col="red",pch=16))
with(POPBIOMASS,arrows(D[ST=="POPEND"],
 ESE[ST=="POPEND"], D[ST=="POPEND"], LSE[ST=="POPEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
 
 # looks great on screeen / printer
 dev.copy2pdf(file="somname.pdf")
 # looks great printed, or after printing MS to PDF
 # this is the one you paste in word
 dev.copy2eps(file="fig19.eps")
 
 
 