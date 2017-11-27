library(plyr)
library(maps)
library(mapdata)
library(R.utils)
library(raster)
library(RColorBrewer)

#read in the measured whale data
setwd("/Users/trueman/Desktop/R scripts/Whale")
data<-read.csv("KC.NHM.fullB.csv", header=TRUE)
colz<-colorRampPalette(c("brown", "cadet blue", "pink"))(12)
colz<-seq(1:6)
fin<-data[data$Spp=="fin",]
minke<-data[data$Spp=="minke",]
sei<-data[data$Spp=="sei",]
blue<-data[data$Spp=="blue",]


##HERE fix day numbers reflecting monthly samples (for loess sampling - fixed to have same no.s)
predict.days<-seq(from=30, to=(8*365)+100, by=31)
blue$Day.sim<-rev(predict.days)
plot(y=blue$d13C, x=blue$Day.sim, type="l")
par(new=TRUE)
plot(y=blue$d15N, x=blue$Day.sim, type="l", axes=FALSE, col="red")



##### files to account for trophic level time integrations
rm(list=ls())
#Read in environmental values
setwd("/Users/trueman/Desktop/R scripts/migrate.files/Model.files/")

#plkd13c values
d13Cjan<-raster("d13C_Jan.gri")
d13Cfeb<-raster("d13C_Feb.gri") 
d13Cmar<-raster("d13C_Mar.gri")
d13Capr<-raster("d13C_Apr.gri")
d13Cmay<-raster("d13C_May.gri")
d13Cjun<-raster("d13C_Jun.gri")
d13Cjul<-raster("d13C_Jul.gri")
d13Caug<-raster("d13C_Aug.gri")
d13Csep<-raster("d13C_Sep.gri")
d13Coct<-raster("d13C_Oct.gri")
d13Cnov<-raster("d13C_Nov.gri")
d13Cdec<-raster("d13C_Dec.gri")

TL1<-stack(d13Cjan, d13Cfeb, d13Cmar, d13Capr, d13Cmay, d13Cjun, d13Cjul, d13Caug, d13Csep, d13Coct, d13Cnov, d13Cdec)



#d13C of different trophic levels
TL2 <- stack("Trolev2_d13C.grd")
TL3 <- stack("Trolev3_d13C.grd")
TL4 <- stack("Trolev4_d13C.grd")


e.Atl<-extent(-60, 30, 20, 70)
Atl.extTL1<-crop(TL1, e.Atl)
Atl.extTL2<-crop(TL2, e.Atl)
Atl.extTL3<-crop(TL3, e.Atl)
Atl.extTL4<-crop(TL4, e.Atl)


tm1<-c(median(Atl.extTL1$layer.1, na.rm=TRUE), median(Atl.extTL1$layer.2, na.rm=TRUE), median(Atl.extTL1$layer.3, na.rm=TRUE), median(Atl.extTL1$layer.4, na.rm=TRUE), median(Atl.extTL1$layer.5, na.rm=TRUE), median(Atl.extTL1$layer.6, na.rm=TRUE), median(Atl.extTL1$layer.7, na.rm=TRUE), median(Atl.extTL1$layer.8, na.rm=TRUE), median(Atl.extTL1$layer.9, na.rm=TRUE), median(Atl.extTL1$layer.10, na.rm=TRUE), median(Atl.extTL1$layer.11, na.rm=TRUE), median(Atl.extTL1$layer.12, na.rm=TRUE))

tm2<-c(median(Atl.extTL2$layer.1, na.rm=TRUE), median(Atl.extTL2$layer.2, na.rm=TRUE), median(Atl.extTL2$layer.3, na.rm=TRUE), median(Atl.extTL2$layer.4, na.rm=TRUE), median(Atl.extTL2$layer.5, na.rm=TRUE), median(Atl.extTL2$layer.6, na.rm=TRUE), median(Atl.extTL2$layer.7, na.rm=TRUE), median(Atl.extTL2$layer.8, na.rm=TRUE), median(Atl.extTL2$layer.9, na.rm=TRUE), median(Atl.extTL2$layer.10, na.rm=TRUE), median(Atl.extTL2$layer.11, na.rm=TRUE), median(Atl.extTL2$layer.12, na.rm=TRUE))

tm3<-c(median(Atl.extTL3$layer.1, na.rm=TRUE), median(Atl.extTL3$layer.2, na.rm=TRUE), median(Atl.extTL3$layer.3, na.rm=TRUE), median(Atl.extTL3$layer.4, na.rm=TRUE), median(Atl.extTL3$layer.5, na.rm=TRUE), median(Atl.extTL3$layer.6, na.rm=TRUE), median(Atl.extTL3$layer.7, na.rm=TRUE), median(Atl.extTL3$layer.8, na.rm=TRUE), median(Atl.extTL3$layer.9, na.rm=TRUE), median(Atl.extTL3$layer.10, na.rm=TRUE), median(Atl.extTL3$layer.11, na.rm=TRUE), median(Atl.extTL3$layer.12, na.rm=TRUE))

tm4<-c(median(Atl.extTL4$layer.1, na.rm=TRUE), median(Atl.extTL4$layer.2, na.rm=TRUE), median(Atl.extTL4$layer.3, na.rm=TRUE), median(Atl.extTL4$layer.4, na.rm=TRUE), median(Atl.extTL4$layer.5, na.rm=TRUE), median(Atl.extTL4$layer.6, na.rm=TRUE), median(Atl.extTL4$layer.7, na.rm=TRUE), median(Atl.extTL4$layer.8, na.rm=TRUE), median(Atl.extTL4$layer.9, na.rm=TRUE), median(Atl.extTL4$layer.10, na.rm=TRUE), median(Atl.extTL4$layer.11, na.rm=TRUE), median(Atl.extTL4$layer.12, na.rm=TRUE))

plot(x=1:12, y=tm1, type="l", col="black", ylim=c(-26, -23))
par(new=TRUE)
plot(x=1:12, y=tm2, type="l", col="red", ylim=c(-26, -23), xlab="", ylab="", axes=FALSE)
par(new=TRUE)
plot(x=1:12, y=tm3, type="l", col="blue", ylim=c(-26, -23), xlab="", ylab="", axes=FALSE)
par(new=TRUE)
plot(x=1:12, y=tm4, type="l", col="green", ylim=c(-26, -23), xlab="", ylab="", axes=FALSE)
par(new=FALSE)



#read in combined model files (BIIIIG)
setwd("/Users/trueman/Desktop/R scripts/migrate.files/Model.files")
resTrack<-read.csv("model.sims.csv", header=TRUE)
# cut out last rep if it didn't work

resTrack<-resTrack[-c(3213051:3213211),]


##HERE fix day numbers reflecting monthly samples (for loess sampling)

lengthS=length(resTrack$d13C[resTrack$Rep==1])
mo_no<-lengthS/30+1
sampleD<-rnorm(mo_no,30,1)

sampledays<-as.integer(cumsum(sampleD))




#extract model values only for days of sample - as defined above (cound change to an average if wanted)
test<-ddply(resTrack, "Rep", function(x) {
	newSeries<-x[x$count2%in%sampledays,]
})

test2<-ddply(test, "Rep", function(x) {
	newSeries<-x[x$count2<3021,]
})



# here add the diff d13C isoscape values only for the sampled tracks

for(i in 1:nrow(test2)){
  mon.no <- test2$Month[i]
  test2$TL1[i] <- raster::extract(x=TL1[[mon.no]], y=test2[i , c("Lon", "Lat")])
  test2$TL2[i] <- raster::extract(x=TL2[[mon.no]], y=test2[i , c("Lon", "Lat")])
  test2$TL3[i] <- raster::extract(x=TL3[[mon.no]], y=test2[i , c("Lon", "Lat")])
  test2$TL4[i] <- raster::extract(x=TL4[[mon.no]], y=test2[i , c("Lon", "Lat")])
  }





#run loess through series and predict for same days as measured

test3<-ddply(test2, "Rep", function(x){
	lo<-predict(loess(x$d13C~x$count2, span=0.05))[1:97]
	
})

test3TL1<-ddply(test2, "Rep", function(x){
	lo<-predict(loess(x$TL1~x$count2, span=0.05))[1:97]
	
})
test3TL2<-ddply(test2, "Rep", function(x){
	lo<-predict(loess(x$TL2~x$count2, span=0.05))[1:97]
	
})
test3TL3<-ddply(test2, "Rep", function(x){
	lo<-predict(loess(x$TL3~x$count2, span=0.05))[1:97]
	
})
test3TL4<-ddply(test2, "Rep", function(x){
	lo<-predict(loess(x$TL4~x$count2, span=0.05))[1:97]
	
})









test3<-test3TL4


test3<-t(test3)
colnames(test3)<-1:length(unique(resTrack$Rep))-1
test3<-as.data.frame(test3[-1,])


# link to the measured data to allow regression comparison
test3$Day<-test2$count2[test2$Rep==1][1:97]
test3$Blue<-rev(blue$d13C)

# run regressions for each model simulation and save r square values
r2<-vector()
for(run in 1:length(unique(resTrack$Rep))){
	testX<-lm(test3[,run]~test3$Blue)
	r2[run]<-summary(lm(test3[,run]~test3$Blue))$adj.r.squared

}

which.max(r2)
order(r2)

# histogram of r2
setEPS()
postscript("hist.r2.eps", width=10, height=7)
hist(r2, main="")
dev.off()

#######PLOT BASIC BEST FITS
par(mar=c(5, 4, 4, 6) + 0.1)
plot(x<-test3$Day, y=test3$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=1.5)
par(new=TRUE)
plot(x<-test3$Day, y=test3[,which.max(r2)], col="black", type="l", axes=FALSE, xlab="", ylab="")
axis(4, at=seq(from=-35, to=-15, by=0.5))
mtext("modelled d13C", side=4, line=3)
par(new=FALSE)

# if you to plot a different run
plot(x<-test3$Day, y=test3$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=2)
par(new=TRUE)
plot(x<-test3$Day, y=test3[,910], col="black", type="l", axes=FALSE, xlab="", ylab="")
axis(4, at=seq(from=-35, to=-15, by=0.5))
mtext("modelled d13C", side=4, line=3)
par(new=FALSE)


setEPS()
postscript("top100.line.eps", width=10, height=7)

#plot top 100 models
plot(x<-test3$Day, y=test3$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=2, ylim=c(-20, -16.5))
par(new=TRUE)
for(p in 959:1059){
plot(x<-test3$Day, y=test3[,p]+4, col="dark grey", type="l", axes=FALSE, xlab="", ylab="", lwd=0.2, ylim=c(-27, -13))
par(new=TRUE)	
	
}
axis(4, at=seq(from=-27, to=-13, by=0.5))
mtext("modelled d13C", side=4, line=3)

par(new=TRUE)
plot(x<-test3$Day, y=test3$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=2, ylim=c(-20, -16.5))

dev.off()

###plot out best fit map (whale 43 in this run)

plot(x=NA, y=NA, xlim=c(-80,50), ylim=c(0, 80), xlab="", ylab="", axes=FALSE)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"grey40")
colz<-colorRampPalette(c("light grey", "green", "pink","red"))(6)
colz2<-colorRampPalette(c("red", "orange", "light grey"))(6)
colzF<-c(colz,colz2)
colzY<-colorRampPalette(c("white", "light blue", "purple"))(3020)

colzB<-brewer.pal(9,"RdBu")
colzB2<-rev(colorRampPalette(colzB) (3020))

#resTrackX<-resTrack[resTrack$Rep==unique(test$Rep)[which.max(r2)],]
resTrackX<-resTrack[resTrack$Rep==unique(test2$Rep)[order(r2)[487]],]

jLat<-jitter(resTrackX$Lat, factor=2)
jLon<-jitter(resTrackX$Lon, factor=1)
points(jLon, jLat, bg=colzY[resTrackX$count2], pch=21, col="black", cex=1, lwd=0.5)
map('world', col="black", fill=TRUE, add =TRUE, lwd=0.25)




setEPS()
postscript("top100.move.eps", width=10, height=7)

plot(x=NA, y=NA, xlim=c(-80,50), ylim=c(0, 80), xlab="", ylab="", axes=FALSE)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"grey90")

for(p in 959:1058){
resTrackX<-resTrack[resTrack$Rep==unique(test2$Rep)[order(r2)[p]],]
jLat<-jitter(resTrackX$Lat, factor=3)
jLon<-jitter(resTrackX$Lon, factor=2)
points(jLon, jLat, bg=colzB2[resTrackX$count2], pch=21, col="black", cex=0.3, lwd=0.05)

par(new=TRUE)	
	
}
map('world', col="black", fill=TRUE, add =TRUE, lwd=0.25)


dev.off()



####Analyses




# extract top X% best fit models for whole series
cut<-0.1
limit<-length(r2)-length(r2)*cut

bottom<-0.1
limitB<-length(r2)*bottom

topX<-ddply(resTrack, "Rep", function(x) {
	newSeries<-x[x$Rep%in%unique(resTrack$Rep)[order(r2)[limit:length(r2)]],]
})

bottomY<-ddply(resTrack, "Rep", function(x) {
	newSeries<-x[x$Rep%in%unique(resTrack$Rep)[order(r2)[1:limitB]],]
})



# what is the max lat for each of the top X models?
max.lat<-ddply(topX, "Rep", function(x) {
	newSeries<-max(x$Lat)
	
	})

# what is the max lat for each of the bottom Y models?
max.lat.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-max(x$Lat)
	
	})


# what is the mean lat for each of the top X models?
mean.lat<-ddply(topX, "Rep", function(x) {
	newSeries<-median(x$Lat)
	
	})

# what is the mean lat for each of the bottom Y models?
mean.lat.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-median(x$Lat)
	
	})


# what is the std.dev of lat for each of the top X models?
sd.lat<-ddply(topX, "Rep", function(x) {
	newSeries<-sd(x$Lat)
	
	})

# what is the std.dev of lat for each of the bottom Y models?
sd.lat.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-sd(x$Lat)
	
	})
	
	#######LON
	# what is the min lon for each of the top X models?
min.lon<-ddply(topX, "Rep", function(x) {
	newSeries<-min(x$Lon)
	
	})

# what is the min lon for each of the bottom Y models?
min.lon.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-min(x$Lon)
	
	})


# what is the mean lon for each of the top X models?
mean.lon<-ddply(topX, "Rep", function(x) {
	newSeries<-median(x$Lon)
	
	})

# what is the mean lon for each of the bottom Y models?
mean.lon.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-median(x$Lon)
	
	})


# what is the std.dev of lon for each of the top X models?
sd.lon<-ddply(topX, "Rep", function(x) {
	newSeries<-sd(x$Lon)
	
	})

# what is the std.dev of lon for each of the bottom Y models?
sd.lon.bot<-ddply(bottomY, "Rep", function(x) {
	newSeries<-sd(x$Lon)
	
	})

	
	
	
par(mfrow=c(2,3))
boxplot(max.lat$V1, max.lat.bot$V1, main="max.lat", col=c("grey","light blue"))
boxplot(mean.lat$V1, mean.lat.bot$V1, main="mean.lat", col=c("grey","light blue"))
boxplot(sd.lat$V1, sd.lat.bot$V1, main="sd.lat", col=c("grey","light blue"))
boxplot(min.lon$V1, min.lon.bot$V1, main="min.lon", col=c("grey","light blue"))
boxplot(mean.lon$V1, mean.lon.bot$V1, main="mean.lon", col=c("grey","light blue"))
boxplot(sd.lon$V1, sd.lon.bot$V1, main="sd.lon", col=c("grey","light blue"))


setEPS()
postscript("maxlat.box.eps", width=10, height=7)

par(mfrow=c(1,2))
boxplot(max.lat$V1, max.lat.bot$V1, main="max.lat", col=c("grey","light blue"))
boxplot(sd.lat$V1, sd.lat.bot$V1, main="sd.lat", col=c("grey","light blue"))

dev.off()
#prop >70N
prop70<-(length(which(max.lat$V1>69)))/length(max.lat$V1)

#prop >65N
prop65<-(length(which(max.lat$V1>64)))/length(max.lat$V1)




# what is the max lat for all models?
max.lat.ALL<-ddply(resTrack, "Rep", function(x) {
	newSeries<-max(x$Lat)
	
	})

#prop >70N
prop70.All<-(length(which(max.lat.ALL$V1>69)))/length(max.lat.ALL$V1)

#prop >65N
prop65.All<-(length(which(max.lat.ALL$V1>64)))/length(max.lat.ALL$V1)



#prop >70N [bottom]
prop70.bot<-(length(which(max.lat.bot$V1>69)))/length(max.lat.bot$V1)

#prop >65N [bottom]
prop65.bot<-(length(which(max.lat.bot$V1>64)))/length(max.lat.bot$V1)

# compare the proportion of models reaching Barents Sea
prop70
prop70.All
prop70.bot


###plot out top and bottom 10% models as kernal desnisty surface

library(spatstat)

setEPS()
postscript("top100kds.eps", width=6, height=10)

par(mfrow=c(1,1))
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE)

jLat<-jitter(topX$Lat, factor=2)
jLon<-jitter(topX$Lon, factor=1)

pts<-ppp(jLon, jLat, window=owin(c(-80,55), c(-10, 85)))
plot(density(pts), add=TRUE)

points(jLon, jLat, bg=colzY[topX$count2], pch=19, col="grey", cex=0.05, lwd=0.01)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.01)
dev.off()


par(mfrow=c(1,1))
###plot out bottom 10% models as kernal desnisty surface
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE)

jLat<-jitter(bottomY$Lat, factor=2)
jLon<-jitter(bottomY$Lon, factor=1)

ptsB<-ppp(jLon, jLat, window=owin(c(-80,55), c(-10, 85)))
plot(density(ptsB), add=TRUE)

points(jLon, jLat, bg=colzY[topX$count2], pch=19, col="grey", cex=0.05, lwd=0.01)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)



par(mfrow=c(1,2))
par(mar=c(3, 3, 3, 2))

plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE, main="Top 10%")
plot(density(pts), add=TRUE)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)

plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE,  main="Bottom 10%")
plot(density(ptsB), add=TRUE)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)




##########################################################################


# extract the mid region only (behaviour 2) day 1000 - day 2500
mid<-ddply(resTrack, "Rep", function(x) {
	newSeries<-x[x$count2%in%seq(from=1000, to=2500, by=1),]
})


lengthS.mid=1500
mo_no.mid<-lengthS.mid/30-1
sampleD.mid<-rnorm(mo_no.mid,30,1)

sampledays.mid<-as.integer(cumsum(sampleD.mid))+1000-30


#extract only values for days of sample
mid.test<-ddply(mid, "Rep", function(x) {
	newSeries<-x[x$count2%in%sampledays.mid,]
})


#run loess through series and predict for same days as measured
mid.test3<-ddply(mid.test, "Rep", function(x){
	lo<-predict(loess(x$d13C~x$count2, span=0.2))[1:48]
	
})



mid.test3<-t(mid.test3)
colnames(mid.test3)<-seq(from=1, to=length(unique(mid.test$Rep)), by=1)
mid.test3<-as.data.frame(mid.test3[-1,])

mid.test3$Day<-mid.test$count2[mid.test$Rep==1][1:48]

mid.test3$Blue<-rev(blue$d13C[blue$Day.sim<2500 & blue$Day.sim>1000])


#remove runs with missing data
mid.test4<-mid.test3[, -which(colMeans(is.na(mid.test3)) > 0.5)]



mid.r2<-vector()
for(run in 3:dim(mid.test4)[2]-2){
	mid.r2[run]<-summary(lm(mid.test4[,run]~mid.test4$Blue))$adj.r.squared

}
which.max(mid.r2)
order(mid.r2)



####plot top regresion 
par(mfrow=c(1,2))
par(mar=c(5, 4, 4, 5))
plot(x<-mid.test4$Day, y=mid.test4$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=1.5)
par(new=TRUE)

plot(x<-mid.test4$Day, y=mid.test4[,which.max(mid.r2)], col="black", type="l", axes=FALSE, xlab="", ylab="")
axis(4, at=seq(from=-26, to=-20, by=0.5))
mtext("modelled d13C", side=4, line=3)
par(new=FALSE)


par(mfrow=c(1,2))
par(mar=c(5, 4, 4, 5))
plot(x<-mid.test4$Day, y=mid.test4$Blue, col="red", type="l", ylab="measured d13C", xlab="day no", col.axis="red", lwd=1.5)
par(new=TRUE)

plot(x<-mid.test4$Day, y=mid.test4[,102], col="black", type="l", axes=FALSE, xlab="", ylab="")
axis(4, at=seq(from=-26, to=-20, by=0.5))
mtext("modelled d13C", side=4, line=3)
par(new=FALSE)



par(mar=c(3, 2,2, 3))

plot(x=NA, y=NA, xlim=c(-80,50), ylim=c(0, 80), xlab="", ylab="", axes=FALSE)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
"grey40")


colzY<-colorRampPalette(c("white", "light blue", "purple"))(2500)

resTrackX<-mid[mid$Rep==unique(mid$Rep[which.max(mid.r2)]),]
resTrackX<-mid[mid$Rep==unique(mid$Rep)[order(mid.r2)[102]],]


jLat<-jitter(resTrackX$Lat, factor=2)
jLon<-jitter(resTrackX$Lon, factor=1)
points(jLon, jLat, bg=colzY[resTrackX$count2], pch=21, col="black", cex=1, lwd=0.5)
map('world', col="black", fill=TRUE, add =TRUE, lwd=0.25)





#########repeat analysis just for the mid part












# extract top X% best fit models for whole series
cut<-0.1
mid.limit<-length(mid.r2)-length(mid.r2)*cut

bottom<-0.1
mid.limitB<-length(mid.r2)*bottom

mid.topX<-ddply(mid, "Rep", function(x) {
	newSeries<-x[x$Rep%in%unique(mid$Rep)[order(mid.r2)[mid.limit:length(mid.r2)]],]
})

mid.bottomY<-ddply(mid, "Rep", function(x) {
	newSeries<-x[x$Rep%in%unique(mid$Rep)[order(mid.r2)[1:mid.limitB]],]
})



# what is the max lat/lon for each of the top X models?
mid.max.lat<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-max(x$Lat)
	})

# what is the max lat for each of the bottom Y models?
mid.max.lat.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-max(x$Lat)
	
	})


# what is the mean lat for each of the top X models?
mid.mean.lat<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-median(x$Lat)
	
	})

# what is the mean lat for each of the bottom Y models?
mid.mean.lat.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-median(x$Lat)
	
	})


# what is the std.dev of lat for each of the top X models?
mid.sd.lat<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-sd(x$Lat)
	
	})

# what is the std.dev of lat for each of the bottom Y models?
mid.sd.lat.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-sd(x$Lat)
	
	})
	
	
	# what is the max lat for all models?
mid.max.lat.ALL<-ddply(mid, "Rep", function(x) {
	newSeries<-max(x$Lat)
	
	})

	
	
	
	
	
	
	
	# what is the max lat/lon for each of the top X models?
mid.min.lon<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-min(x$Lon)
	})

# what is the max lat for each of the bottom Y models?
mid.min.lon.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-min(x$Lon)
	
	})


# what is the mean lat for each of the top X models?
mid.mean.lon<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-median(x$Lon)
	
	})

# what is the mean lat for each of the bottom Y models?
mid.mean.lon.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-median(x$Lon)
	
	})


# what is the std.dev of lat for each of the top X models?
mid.sd.lon<-ddply(mid.topX, "Rep", function(x) {
	newSeries<-sd(x$Lon)
	
	})

# what is the std.dev of lat for each of the bottom Y models?
mid.sd.lon.bot<-ddply(mid.bottomY, "Rep", function(x) {
	newSeries<-sd(x$Lon)
	
	})
	
	
	# what is the min lat for all models?
mid.min.lat.ALL<-ddply(mid, "Rep", function(x) {
	newSeries<-max(x$Lin)
	
	})

	
	
	
	
	
par(mfrow=c(2,3))
boxplot(mid.max.lat$V1, mid.max.lat.bot$V1, main="max.lat", col=c("grey","light blue"))
boxplot(mid.mean.lat$V1, mid.mean.lat.bot$V1, main="mean.lat", col=c("grey","light blue"))
boxplot(mid.sd.lat$V1, mid.sd.lat.bot$V1, main="sd.lat", col=c("grey","light blue"))
boxplot(mid.min.lon$V1, mid.min.lon.bot$V1, main="min.lon", col=c("grey","light blue"))
boxplot(mid.mean.lon$V1, mid.mean.lon.bot$V1, main="mean.lon", col=c("grey","light blue"))
boxplot(mid.sd.lon$V1, mid.sd.lon.bot$V1, main="sd.lon", col=c("grey","light blue"))





#prop >70N
mid.prop70<-(length(which(mid.max.lat$V1>69)))/length(mid.max.lat$V1)

#prop >65N
mid.prop65<-(length(which(mid.max.lat$V1>64)))/length(mid.max.lat$V1)


#prop >70N
mid.prop70.All<-(length(which(mid.max.lat.ALL$V1>69)))/length(mid.max.lat.ALL$V1)

#prop >65N
mid.prop65.All<-(length(which(mid.max.lat.ALL$V1>64)))/length(mid.max.lat.ALL$V1)



#prop >70N [bottom]
mid.prop70.bot<-(length(which(mid.max.lat.bot$V1>69)))/length(mid.max.lat.bot$V1)

#prop >65N [bottom]
mid.prop65.bot<-(length(which(mid.max.lat.bot$V1>64)))/length(mid.max.lat.bot$V1)

# compare the proportion of models reaching Barents Sea
mid.prop70
mid.prop70.All
mid.prop70.bot


###plot out top and bottom 10% models as kernal desnisty surface


par(mfrow=c(1,1))
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE)

jLat<-jitter(mid.topX$Lat, factor=2)
jLon<-jitter(mid.topX$Lon, factor=1)

pts<-ppp(jLon, jLat, window=owin(c(-80,55), c(-10, 85)))
plot(density(pts), add=TRUE)

points(jLon, jLat, bg=colzY[topX$count2], pch=19, col="grey", cex=0.05, lwd=0.01)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)



par(mfrow=c(1,1))
###plot out bottom 10% models as kernal desnisty surface
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE)

jLat<-jitter(mid.bottomY$Lat, factor=2)
jLon<-jitter(mid.bottomY$Lon, factor=1)

ptsB<-ppp(jLon, jLat, window=owin(c(-80,55), c(-10, 85)))
plot(density(ptsB), add=TRUE)

points(jLon, jLat, bg=colzY[topX$count2], pch=19, col="grey", cex=0.05, lwd=0.1)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)



par(mfrow=c(1,2))
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE, main="mid phase: top 10%")
plot(density(pts), add=TRUE)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)

plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE, main="mid phase: bottom 10%")
plot(density(ptsB), add=TRUE)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.25)









