

gh <- list()
for(k in 2:5){
  
  csvs <- dir(paste0("data/met/gh",k), full.names=TRUE, recursive=TRUE)
  
  dat <- lapply(csvs,read.csv,skip=5)
  
  dat <- lapply(dat, function(x)x[,!grepl("X[.][0-9]{1,2}", names(x))])
  dat <- lapply(dat, function(x)x[,names(x) != "X"])
  
  dat <- do.call(rbind, dat)
  names(dat)[1] <- "DateTime"
  dat$DateTime <- as.POSIXct(as.character(dat$DateTime), format="%d/%m/%Y %H:%M")
  assign(paste0("gh",k),dat)
}

write.csv(gh2, "output/data/gh2_met.csv", row.names=FALSE)
write.csv(gh3, "output/data/gh3_met.csv", row.names=FALSE)
write.csv(gh4, "output/data/gh4_met.csv", row.names=FALSE)
write.csv(gh5, "output/data/gh5_met.csv", row.names=FALSE)

