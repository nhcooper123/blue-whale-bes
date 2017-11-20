# Author: Clive Trueman
# About: script to extract top 10% and bottom 10% of models from 
# simulations and extract latitude and longitude info from them
# Tidied by Natalie Cooper Oct 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(plyr)
# note if using dplyr later you'll need to detach plyr
library(maps)
library(mapdata)
library(R.utils)

# Read in the measured whale data and select blue whale
whale_isos <- read_csv("data/raw-whale-isotope-data.csv")
blue <- filter(whale_isos, whale_ID == "KC7")

# Fix day numbers reflecting monthly samples 
# (for loess sampling - fixed to have same no.s)
predict.days <- seq(from = 30, to = (8 * 365) + 100, by = 31)
blue$Day.sim <- rev(predict.days)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Read in model simulations output
# Takes a bit of time
resTrack <- read_csv("data/Model.sims.csv")

# Remove rep that didn't work
resTrack<-resTrack[-c(3213051:3213211),]

# Fix day numbers reflecting monthly samples (for loess sampling)
lengthS <- length(resTrack$d13C[resTrack$Rep == 1])
mo_no <- lengthS / 30 + 1
sampleD <- rnorm(mo_no, 30, 1)
sampledays <- as.integer(cumsum(sampleD))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Extract model values only for days of sample - 
# as defined above (could change to an average if wanted)
test <- ddply(resTrack, "Rep", function(x) {
	newSeries <- x[x$count2%in%sampledays, ]
})

test2 <- ddply(test, "Rep", function(x) {
	newSeries <- x[x$count2 < 3021, ]
})

# Run loess through series and predict for same days as measured
test3 <- ddply(test2, "Rep", function(x){
	lo <- predict(loess(x$d13C ~ x$count2, span = 0.05))[1:97]
})

# Transpose
test3 <- t(test3)

# Add column names
colnames(test3) <- 1:length(unique(resTrack$Rep)) - 1
test3 <- as.data.frame(test3[-1, ])

# Link to the measured data to allow regression comparison
test3$Day <- test2$count2[test2$Rep == 1][1:97]
test3$Blue <- rev(blue$d13C)

# Run regressions for each model simulation and save r square values
r2 <- vector()

for(run in 1:length(unique(resTrack$Rep))){
	testX <- lm(test3[, run] ~ test3$Blue)
	r2[run] <- summary(lm(test3[, run] ~ test3$Blue))$adj.r.squared
}

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# extract top 10% and bottom 10% best fit models for whole series
# write out for figures 
cut <- 0.1
limit <- length(r2) - length(r2) * cut
top100 <- test3[, order(r2)[limit:length(r2)] + 1]

bottom <- 0.1
limitB <- length(r2) * bottom
bottom100 <- test3[, order(r2)[1:limitB] + 1]

# Add blue whale data back in
top100$blue <- rev(blue$d13C)
top100$day <- test2$count2[test2$Rep == 1][1:97]

bottom100$blue <- rev(blue$d13C)
bottom100$day <- test2$count2[test2$Rep == 1][1:97]

# Write to file
write.csv(file = "data/top100models.csv", top100, quote = FALSE)
write.csv(file = "data/bottom100models.csv", bottom100, quote = FALSE)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Extract latitude and longtiudes from top and bottom 10% of models

topX <- ddply(resTrack, "Rep", function(x) {
	newSeries <- x[x$Rep%in%unique(resTrack$Rep)[order(r2)[limit:length(r2)]], ]
})

bottomY <- ddply(resTrack, "Rep", function(x) {
	newSeries <- x[x$Rep%in%unique(resTrack$Rep)[order(r2)[1:limitB]],]
})

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# what is the max lat for each of the top X models?
max.lat <- ddply(topX, "Rep", function(x) {
	newSeries <- max(x$Lat)
})

# what is the max lat for each of the bottom Y models?
max.lat.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- max(x$Lat)
})

# what is the mean lat for each of the top X models?
mean.lat <- ddply(topX, "Rep", function(x) {
	newSeries <- median(x$Lat)
	})

# what is the mean lat for each of the bottom Y models?
mean.lat.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- median(x$Lat)
	})

# what is the std.dev of lat for each of the top X models?
sd.lat <- ddply(topX, "Rep", function(x) {
	newSeries <- sd(x$Lat)
})

# what is the std.dev of lat for each of the bottom Y models?
sd.lat.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- sd(x$Lat)
})

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Longitude

# what is the min lon for each of the top X models?
min.lon <- ddply(topX, "Rep", function(x) {
	newSeries <- min(x$Lon)
})

# what is the min lon for each of the bottom Y models?
min.lon.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- min(x$Lon)
})

# what is the mean lon for each of the top X models?
mean.lon <- ddply(topX, "Rep", function(x) {
	newSeries <- median(x$Lon)
})

# what is the mean lon for each of the bottom Y models?
mean.lon.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- median(x$Lon)
})

# what is the std.dev of lon for each of the top X models?
sd.lon <- ddply(topX, "Rep", function(x) {
	newSeries <- sd(x$Lon)
})

# what is the std.dev of lon for each of the bottom Y models?
sd.lon.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- sd(x$Lon)
})

	
	
	



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

par(mfrow=c(1,1))
plot(x=NA, y=NA, xlim=c(-80,55), ylim=c(-10, 85), xlab="", ylab="", axes=FALSE)

jLat<-jitter(topX$Lat, factor=2)
jLon<-jitter(topX$Lon, factor=1)

pts<-ppp(jLon, jLat, window=owin(c(-80,55), c(-10, 85)))
plot(density(pts), add=TRUE)

points(jLon, jLat, bg=colzY[topX$count2], pch=19, col="grey", cex=0.05, lwd=0.01)
map('world', col="grey", fill=TRUE, add =TRUE, lwd=0.01)



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









