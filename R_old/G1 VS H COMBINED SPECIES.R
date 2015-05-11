setwd("C:/Documents and Settings/Jeffrey Kelly/Desktop/EUC DATA/EUC OVERALL GAS EXCHANGE")
PILBIOMASS<-read.csv("EUC_GS_OVERALL_BBM.csv",sep=",", header=TRUE)
names(PILBIOMASS)
str(PILBIOMASS)


par(xaxs="i",yaxs="i")
windows(width=8, height=4) #, pointsize=18)
#A
#bottom,left,top,right
par(las=2) 
par(mar=c(4.5,4.5,1,1)) 
par(xaxs="i",yaxs="i")
par(mfrow=c(1,1),cex.lab=1)
#PIL
par(las=1)
with(PILBIOMASS,plot(G1[ST=="PILAD"]~H[ST=="PILAD"],col="blue",pch=1,
ylim=c(0,1.1*max(G1)),xlim=c(0,1.05*max(H)),
ylab="",xlab=expression(bold(Height~(m))),cex.lab=1.25))
title(main="",  font.main=4,cex.main=1.1)
     
with(PILBIOMASS,points(G1[ST=="PILAND"]~H[ST=="PILAND"],col="blue",pch=16))

with(PILBIOMASS,points(G1[ST=="PILED"]~H[ST=="PILED"],col="red",pch=1))

with(PILBIOMASS,points(G1[ST=="PILEND"]~H[ST=="PILEND"],col="red",pch=16))

par(las=3)
mtext(side = 2, text =expression(bold(g[1])), line = 3,font=2, cex=1.25)


#POP
#bottom,left,top,right

with(PILBIOMASS,points(G1[ST=="POPAD"]~H[ST=="POPAD"],col="blue",pch=2))
savefont<-par(font=4)

legend(2.125,12,  c("Eucalyptus pilularis","Eucalyptus populnea"),
cex=0.9,bty="n", 
 pch = c(1,2) )
par(savefont) #xjust = .5, yjust = .5,
#  savefont <- par(font=3)
#legend("topright", legend=c('Label 1', 'Label 2'), pch=1:2)
#par(savefont)
            
              
 #axis(4,labels=TRUE,tcl=-0.5,cex.axis=1)
#or
       
with(PILBIOMASS,points(G1[ST=="POPAND"]~H[ST=="POPAND"],col="blue",pch=17))

 
with(PILBIOMASS,points(G1[ST=="POPED"]~H[ST=="POPED"],col="red",pch=2))


with(PILBIOMASS,points(G1[ST=="POPEND"]~H[ST=="POPEND"],col="red",pch=17))
# this is the one you paste in word
 dev.copy2eps(file="figg1.eps")
 