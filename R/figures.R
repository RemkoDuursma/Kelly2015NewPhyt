

figure1 <- function(){
  
  par(mar=c(5,5,1,1), cex.lab=1.1)
  
  # Generate Figure 1 for Jeff's paper
  ACi <- function(Ci) {
    Jmax <- 96
    Vcmax <- 60
    Rd <- 1
    Gstar <- 42
    Km <- 730
    Ac <- Vcmax * (Ci - Gstar)/(Ci + Km) - Rd
    Aj <- Jmax/4 * (Ci - Gstar)/(Ci + 2*Gstar) - Rd
    A <- min(Ac, Aj)
    return(A)
  }
  
  #make vector
  Ci <- seq(25,800,by=25)
  Jmax <- 96
  Vcmax <- 60
  Rd <- 1
  Gstar <- 42
  Km <- 730
  Ac <- Vcmax * (Ci - Gstar)/(Ci + Km) - Rd
  Aj <- Jmax/4 * (Ci - Gstar)/(Ci + 2*Gstar) - Rd
  A <- pmin(Ac, Aj)
  
  #make individual points
  cirat1 <- 0.6
  spp1aca <- ACi(cirat1*380)
  spp1eca <- ACi(cirat1*700)
  resp1 <- spp1eca/spp1aca
  
  cirat2 <- 0.8
  spp2aca <- ACi(cirat2*380)
  spp2eca <- ACi(cirat2*700)
  resp2 <- spp2eca/spp2aca
  
  #make plot
  plot(Ci,A, type="l",ylim=c(0,20),ylab = expression(A~(mu*mol~m^-2~s^-1)),
       xlab=expression(C[i]~(mu*mol~mol^-1)))
  points(cirat1*380,spp1aca,pch=1,cex=1.5)
  points(cirat1*700,spp1eca,pch=1,cex=1.5)
  points(cirat2*380,spp2aca,pch=2,cex=1.5)
  points(cirat2*700,spp2eca,pch=2,cex=1.5)
  
  connect1x <- c(cirat1*380,cirat1*700,cirat1*700)
  connect1y <- c(spp1aca,spp1aca,spp1eca)
  points(connect1x,connect1y,type="l",lty=2)
  
  connect2x <- c(cirat2*380,cirat2*700,cirat2*700)
  connect2y <- c(spp2aca,spp2aca,spp2eca)
  points(connect2x,connect2y,type="l",lty=3)
  
}



figure2 <- function(df){
  
  par(xaxs="i", yaxs="i")
  palette(alpha(c("forestgreen","darkorange"),0.5))
  pchs <- c(19, 17)
  with(df, plot(Tmax/10, MAP, 
                xlim=c(20,40), ylim=c(0,2500),
                xlab=expression("Maximum temperature of warmest month"~(degree*C)),
                ylab="Mean annual precipitation (mm)",
                pch=pchs[species], col=species))
  
  legend("topright", c("E. pilularis","E. populnea"), text.font=3,
         pch=pchs, col=alpha(palette(),0.9), bty='n')
  
}


figure3 <- function(PILBIOMASS, POPBIOMASS){
  
  par(xaxs="i",yaxs="i", las=1, mar=c(4.5,4.5,1,1), 
      mfrow=c(1,2), cex.lab=1)
  
  
  palette(c("blue","blue","red","red"))
  pchs <- c(1,16,1,16)
  with(PILBIOMASS, {
    plot(D, E, col=ST,pch=pchs[ST],
         ylim=c(0,30),
         xlim=c(0,200),
         xlab="Day",
         ylab=expression(Transpiration~~(l~week^-1)))
    arrows(D,ESE, D, LSE, length = .035, angle = 90, code = 3,col=ST)
  })
  title(main="Eucalyptus pilularis",  font.main=4, cex.main=1)
  legend("topleft",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
         cex=0.75,bty="n",
         pch = c(16,1,16,1), col=c("blue","blue","red","red"))

  pchs <- c(2,17,2,17)
  with(POPBIOMASS, {
    plot(D, E, col=ST,pch=pchs[ST],
         ylim=c(0,15),
         xlim=c(0,250),
         xlab="Day",
         ylab=expression(Transpiration~~(l~week^-1)))
    arrows(D,ESE, D, LSE, length = .035, angle = 90, code = 3,col=ST)
  })
  title(main="Eucalyptus populnea",  font.main=4, cex.main=1)
  
}



figure4 <- function(df){
  
  # linear regressions
  lm1 <- lm(g1 ~ H, data=subset(df, Species == "PIL"))
  lm2 <- lm(g1 ~ H, data=subset(df, Species == "POP"))
  
  
  par(xaxs="i",yaxs="i", las=1, mar=c(4.5,4.5,1,1), 
      mfrow=c(1,2), cex.lab=1)
  
  palette(c("blue","red"))
  pchs <- c(16,1,16,1)
  
  plotpanel <- function(sp, ...){
    with(subset(df, Species == sp), {
      plot(H, g1, col=CO2,pch=pchs[H2O],
           xlab="Height (m)",
           ylab=expression(g[1]),
           panel.first= predline(lm(g1 ~ H, data=subset(df, Species == sp))),
           ...
      )
    })
  }
  
  pvalr2 <- function(lmobj){
    legend("topright", legend=c(as.expression(bquote(italic(p) == .(formatPval(glance(lmobj)$p.value)))),
                                as.expression(bquote(R^2 == .(round(glance(lmobj)$r.squared,2))))
    ), bty='n')
  }
  
  plotpanel("PIL", ylim=c(0,6), xlim=c(0,4))
  pvalr2(lm1)
  legend("topleft",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
         cex=0.75,bty="n",
         pch = c(16,1,16,1), col=c("blue","blue","red","red"))
  title(main="Eucalyptus pilularis",  font.main=4, cex.main=1)
  
  pchs <- c(17,2,17,2)
  
  plotpanel("POP", ylim=c(0,12), xlim=c(0,2))
  pvalr2(lm2)
  title(main="Eucalyptus populnea",  font.main=4, cex.main=1)
  
  
  
}






figure5 <- function(spotagg, fpil, fpop){
  
  par(mfrow=c(1,2),cex.main=0.9, mar=c(4.5, 4.5, 1, 1))
  
  with(subset(spotagg, Water_treatment == "ND"), 
       plot(Ci.mean, Photo.mean, col=c("blue","red")[CO2_treatment],
            xlab=expression(C[i]~~(ppm)),
            main="Field capacity",
            ylab=expression(A[net]~~(mu*mol~m^-2~s^-1)),
            xlim=c(50,500), ylim=c(0,30),
            pch=c(19,17)[species], 
            panel.first={
              
              if(exists("bpil")){
                ci <- getci(bpil)
                addpoly(ci$Ci, ci$ucl, ci$lcl)
                ci <- getci(bpop)
                addpoly(ci$Ci, ci$ucl, ci$lcl)
              }
              
              adderrorbars(Ci.mean, Photo.mean, Ci.se, c("right","left"),
                           col=c("blue","red")[CO2_treatment])
              adderrorbars(Ci.mean, Photo.mean, Photo.se, c("up","down"),
                           col=c("blue","red")[CO2_treatment])
            }
       ))
  
  l <- legend("topleft", c("E. pilularis", "E.populnea"), bty='n',
         pch=c(19,17), lty=c(1,5))
  legend(l$rect$left+l$rect$w, l$rect$top, 
         c(expression(Ambient~C[a]),
                          expression(Elevated~C[a])
                          ), fill=c("blue","red"), bty='n')
  
  addaciline(fpil)
  addaciline(fpop, lty=5)
  
  
  with(subset(spotagg, Water_treatment == "D"), 
       plot(Ci.mean, Photo.mean, col=c("blue","red")[CO2_treatment],
            xlab=expression(C[i]~~(ppm)),
            ylab=expression(A[net]~~(mu*mol~m^-2~s^-1)),
            xlim=c(50,500), ylim=c(0,30),
            main="50% Field capacity",
            pch=c(21,24)[species], bg="white",
            panel.first={
              
              if(exists("bpil")){
                ci <- getci(bpil)
                addpoly(ci$Ci, ci$ucl, ci$lcl)
                ci <- getci(bpop)
                addpoly(ci$Ci, ci$ucl, ci$lcl)
              }
              
              adderrorbars(Ci.mean, Photo.mean, Ci.se, c("right","left"),
                           col=c("blue","red")[CO2_treatment])
              adderrorbars(Ci.mean, Photo.mean, Photo.se, c("up","down"),
                           col=c("blue","red")[CO2_treatment])
            }
       ))
  
  addaciline(fpil)
  addaciline(fpop, lty=5)
}



figure6 <- function(df){
  
  
  par(mar=c(5,5,1,1), cex.lab=1.1, xaxs="i", yaxs="i")
  
  pchs <- c(1,19,1,19,2,17,2,17)
  palette(c("blue","red"))
  
  lm1 <- lm(wue ~ ite, data=subset(df, Species == "PIL"))
  lm2 <- lm(wue ~ ite, data=subset(df, Species == "POP"))
  
  with(df, plot(ite, wue, 
                xlim=c(0,30),
                ylim=c(0,10),
                pch=pchs[ST],
                col=CO2,
                xlab=expression(ITE~~(mu*mol~CO[2]~mmol~H[2]*O^-1)),
                ylab=expression(WUE~~(mu*mol~CO[2]~mmol~H[2]*O^-1)),
                panel.first={
                  predline(lm1, lwd=1)
                  predline(lm2, lwd=1, lty=5)
                }
                ))
  
  l <- legend("bottomright", 
              legend=c(expression(aC[a]-W,aC[a]-D,eC[a]-W,eC[a]-D)),
              bty="n",pch=c(17,2,17,2),col=c("blue","blue","red","red"),
              cex=0.8,pt.cex=1,
              title=expression(italic(Eucalyptus~populnea)))
              
  legend(l$rect$left, l$rect$top + l$rect$h, legend=c(expression(aC[a]-W,aC[a]-D,eC[a]-W,eC[a]-D)),
         bty="n",pch=c(16,1,16,1),col=c("blue","blue","red","red"),
         cex=0.8,pt.cex=1,
         title=expression(italic(Eucalyptus~pilularis)))
        
  
}

  
  
  
  
  

