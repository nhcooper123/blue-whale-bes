setwd("/Users/trueman/Desktop/R scripts/migrate.files")
library(raster)


##########read resident whale model files with the trophic attenuation d13C values
setwd("/Users/trueman/Desktop/R scripts/migrate.files/Model.files/blue.res")

resNor<-read.csv("Norway.Res.TL.csv", header=TRUE)
resIre<-read.csv("Ireland.Res.TL.csv", header=TRUE)
resCan<-read.csv("Canaries.Res.TL.csv", header=TRUE)
resMidAtl<-read.csv("Atl.Res.TL.csv", header=TRUE)
resCV<-read.csv("CV.TL.csv", header=TRUE)


#remove samples with NAs
resNor<-na.omit(resNor)
resIre<-na.omit(resIre)
resCan<-na.omit(resCan)
resMidAtl<-na.omit(resMidAtl)
resCV<-na.omit(resCV)



#plot loess fits to the 30 resident simulated whales for each region



setEPS()
postscript("resident.eps", width=10, height=5)
par(new=FALSE)
par(mfrow=c(2,3))



#norway resident

#get series length
inds<-length(unique(resNor$Rep))

#run loess on day number
lo<-loess(resNor$TL4[resNor$Rep==1]+6~resNor$Day.No[resNor$Rep==1], span=0.1)

# plot blank
plot(x=resNor$Day.No[resNor$Rep==1], y=resNor$TL4[resNor$Rep==1], ylim=c(-23,-12), main="Norwegian Sea" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
#add loess line
lines(resNor$Day.No[resNor$Rep==1],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
# run loess and plot for each of the 30 simulations 

for(w in 2:inds){
	lo<-loess(resNor$TL4[resNor$Rep==w]+6~resNor$Day.No[resNor$Rep==w], span=0.1)
lines(resNor$Day.No[resNor$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)


#Ireland resident
inds<-length(unique(resIre$Rep))
lo<-loess(resIre$TL1[resNor$Rep==1]+6~resIre$Day.No[resIre$Rep==1], span=0.1)
plot(x=resIre$Day.No[resIre$Rep==1], y=resIre$TL1[resIre$Rep==1], ylim=c(-23,-12), main="West Ireland" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resIre$Day.No[resIre$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resIre$TL1[resIre$Rep==w]+6~resIre$Day.No[resIre$Rep==w], span=0.1)
lines(resIre$Day.No[resIre$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)


#Canary resident
inds<-length(unique(resCan$Rep))
lo<-loess(resCan$d13C[resNor$Rep==1]+6~resCan$Day.No[resCan$Rep==1], span=0.1)
plot(x=resCan$Day.No[resCan$Rep==1], y=resCan$d13C[resCan$Rep==1], ylim=c(-23,-12), main="Canaries" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resCan$Day.No[resCan$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resCan$d13C[resCan$Rep==w]+6~resCan$Day.No[resCan$Rep==w], span=0.1)
lines(resCan$Day.No[resCan$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)



#Mid Atl resident
inds<-length(unique(resMidAtl$Rep))
lo<-loess(resMidAtl$d13C[resMidAtl$Rep==1]+6~resMidAtl$Day.No[resMidAtl$Rep==1], span=0.1)
plot(x=resMidAtl$Day.No[resMidAtl$Rep==1], y=resMidAtl$d13C[resMidAtl$Rep==1], ylim=c(-23,-12), main="M.Atlantic" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resMidAtl$Day.No[resMidAtl$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resMidAtl$d13C[resMidAtl$Rep==w]+6~resMidAtl$Day.No[resMidAtl$Rep==w], span=0.1)
lines(resMidAtl$Day.No[resMidAtl$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)


#Cape.V resident - note here the day.no column is labelled as count - didn't round to renaming...
inds<-length(unique(resCV$Rep))
lo<-loess(resCV$TL4[resCV$Rep==1]+6~resCV$count[resCV$Rep==1], span=0.1)
plot(x=resCV$count[resCV$Rep==1], y=resCV$TL4[resCV$Rep==1], ylim=c(-23,-12), main="Cape Verde" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resCV$count[resCV$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resCV$TL4[resCV$Rep==w]+6~resCV$count[resCV$Rep==w], span=0.1)
lines(resCV$count[resCV$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)
dev.off()
