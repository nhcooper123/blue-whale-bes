setwd("/Users/trueman/Desktop/R scripts/migrate.files")
library(raster)
library(gstat)
library(sp)
library(maps)
library(mapdata)



##plot model output annual average baseline images
Atl_carbon   <- raster("data/Atl_Annual_d13C.grd")
Atl_nitrogen <- raster("data/Atl_Annual_d15N.grd")

image(Atl_carbon, col=colsrev)
image(Atl_nitrogen, col=colsrev)


##########read resident whale model files

resNor    <- read.csv("data/Norway.csv", header = TRUE)
resIre    <- read.csv("data/west.Ireland.csv", header = TRUE)
resCan    <- read.csv("data/Canaries.csv", header = TRUE)
resMidAtl <- read.csv("data/Mid_Atl.csv", header = TRUE)





#plot loess fits to the 30 resident simulated whales for each region



setEPS()
postscript("resident.eps", width=10, height=5)

par(mfrow=c(2,2))




#norway resident
inds<-length(unique(resNor$Rep))
lo<-loess(resNor$d13C[resNor$Rep==1]+6~resNor$Day.No[resNor$Rep==1], span=0.1)
plot(x=resNor$Day.No[resNor$Rep==1], y=resNor$d13C[resNor$Rep==1], ylim=c(-26,-9), main="Norwegian Sea" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resNor$Day.No[resNor$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resNor$d13C[resNor$Rep==w]+6~resNor$Day.No[resNor$Rep==w], span=0.1)
lines(resNor$Day.No[resNor$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)



#Ireland resident
inds<-length(unique(resIre$Rep))
lo<-loess(resIre$d13C[resNor$Rep==1]+6~resIre$Day.No[resIre$Rep==1], span=0.1)
plot(x=resIre$Day.No[resIre$Rep==1], y=resIre$d13C[resIre$Rep==1], ylim=c(-26,-9), main="West Ireland" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resIre$Day.No[resIre$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resIre$d13C[resIre$Rep==w]+6~resIre$Day.No[resIre$Rep==w], span=0.1)
lines(resIre$Day.No[resIre$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)


#Canary resident
inds<-length(unique(resCan$Rep))
lo<-loess(resCan$d13C[resNor$Rep==1]+6~resCan$Day.No[resCan$Rep==1], span=0.1)
plot(x=resCan$Day.No[resCan$Rep==1], y=resCan$d13C[resCan$Rep==1], ylim=c(-26,-9), main="Canaries" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
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
plot(x=resMidAtl$Day.No[resMidAtl$Rep==1], y=resMidAtl$d13C[resMidAtl$Rep==1], ylim=c(-26,-9), main="M.Atlantic" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(resMidAtl$Day.No[resMidAtl$Rep==1],predict(lo), col="red", lwd=0.2)
par(new=TRUE)
for(w in 2:inds){
	lo<-loess(resMidAtl$d13C[resMidAtl$Rep==w]+6~resMidAtl$Day.No[resMidAtl$Rep==w], span=0.1)
lines(resMidAtl$Day.No[resMidAtl$Rep==w],predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)




#######PLOT out the best match whale simulation
resTrack <- read.csv("data/record_migrate5.csv", header=TRUE)



#plot the loess summaries
postscript("preg.loess.eps", height=4, width=6)

par(mfrow=c(1,1))


days<-seq(from=1, to=(6*365+60))
inds<-length(unique(resTrack$Rep))
lo<-loess(resTrack$d13C[resTrack$Rep==1]+6~days, span=0.05)
days2<-seq(from=1, to=2250)
plot(x=days, y=resTrack$d13C[resTrack$Rep==1], ylim=c(-26,-9), main="" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(days2,predict(lo), col="red", lwd=0.2)
par(new=TRUE)


for(w in 2:inds){
	lo<-loess(resTrack$d13C[resTrack$Rep==w]+6~days, span=0.05)
	days3<-seq(from=1, to=length(predict(lo)))

lines(days3,predict(lo), col="red", lwd=0.2)

par(new=TRUE)
}
par(new=FALSE)

dev.off()




###plot out the movement model map tracks
tst<-as.matrix(rep(1, length(10400)))
full<-extent(-80,50,0,80)
tplate<-raster(nrows=(130),ncols=(80),ext=(full),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

colz<-colorRampPalette(c("light grey", "green", "pink","red"))(6)
colz2<-colorRampPalette(c("red", "orange", "light grey"))(6)
colzF<-c(colz,colz2)

par(mfrow=c(1,1))
set EPS()
postscript("preg.map.eps", height=5, width=4)

plot(x=NA, y=NA, xlim=c(-80,50), ylim=c(0, 80), xlab="Day No", ylab="d13C")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"grey40")


jLat<-jitter(resTrack$Lat, factor=2)
jLon<-jitter(resTrack$Lon, factor=1)
points(jLon, jLat, col=colzF[resTrack$Month], pch=21, cex=0.1)
map('world', col="black", fill=TRUE, add =TRUE, lwd=0.25)


dev.off()




###plot out loess only for best fit (whale 43 in this run)
lo<-loess(resTrack$d13C[resTrack$Rep==43]+6~days, span=0.05)

plot(x=days, y=resTrack$d13C[resTrack$Rep==43], ylim=c(-26,-9), main="" , type="p", cex=0, col="red", xlab="Day.No", ylab="d13C")
lines(days2,predict(lo), col="red", lwd=0.5)



###plot out best fit map (whale 43 in this run)

plot(x=NA, y=NA, xlim=c(-80,50), ylim=c(0, 80), xlab="Day No", ylab="d13C")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"grey40")

resTrackB<-resTrack[resTrack$Rep==43,]
jLat<-jitter(resTrackB$Lat, factor=2)
jLon<-jitter(resTrackB$Lon, factor=1)
points(jLon, jLat, col=colzF[resTrack$Month], pch=21, cex=0.1)
map('world', col="black", fill=TRUE, add =TRUE, lwd=0.25)



