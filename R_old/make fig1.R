
# Generate Figure 1 for Jeff's paper

ACi <- function(Ci) {
  Jmax <- 96
  Vcmax <- 60
  Rd <- 1
  Gstar <- 42
  Km <- 730
  Ac <- Vcmax * (i - Gstar)/(i + Km) - Rd
  Aj <- Jmax/4 * (i - Gstar)/(i + 2*Gstar) - Rd
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