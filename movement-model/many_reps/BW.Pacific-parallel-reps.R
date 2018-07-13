# Whale migration model


#how many times to run the simulation (run a couple of reps first to check!)
rep.no=1000

# duration of simulation (days)
tot.day<- 365*4

# setwd("/Users/trueman/Desktop/R scripts/migrate.files/Model.files")


# run the Lat.Lon code to get the km conversion first.
par.old<-par(mar = c(0, 0, 0, 0))
par(mfrow=c(1,1))
par<-par.old
# Degree to km function

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
	
rad <- pi/180
a1 <- lat1 * rad
a2 <- long1 * rad
b1 <- lat2 * rad
b2 <- long2 * rad
dlon <- b2 - a2
dlat <- b1 - a1
a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
c <- 2 * atan2(sqrt(a), sqrt(1 - a))
R <- 6378.145
d <- R * c
return(d)
}

# array for distance vectors
Lat.Lon<-array(data=0, dim=c(180, 360,2))

	#longitude distance varies with lat
	Lt=vector(length=179)
	count=1
	
	for(Lat in 89:-90){
		x<-earth.dist(-180,Lat, -179,Lat)
		Lt[count]<-x
		count=count+1
		}

Lat.Lon[1:180,1,2]<-0
	Lat.Lon[1:180,2,2]<-Lt
	for(Lon in 3:359){
	Lat.Lon[1:180,Lon,2]<-Lat.Lon[,Lon-1,2]+Lat.Lon[,2,2]
	}
	
		#latitude distance constant at 111km

	Lt2=rep(0,179)
	count=2
		for(Lat in -89:89){

		Lt2[count]<-Lt2[count-1]+111
		count=count+1
		}

Lat.Lon[,,1]<-Lt2


Lon2<-Lat.Lon[,1:359,2]/Lat.Lon[,359,2]*360



# Fin Whale migration sub-model

# install.packages(c("R.matlab", "maps", "mapdata", "scales", "TTR", "maptools", "raster"))
library(R.matlab)
library(maps)
library(mapdata)
library(scales)
library(TTR)
require(maptools)
require(raster) 
library(parallel)

llCRS = CRS("+proj=longlat +ellps=WGS84")

# link to monthly isoscape layers (Atlantic only initially to save computing time)
# migration model working on 1 day steps, isoscape on monthly averages

# monthy layers of d13CPOM, PLK biomass and SST as raster stack / coordinate layers rather than matrix/dataframe?
# convert lat-lon to km

full<-extent(-180,180,-90,90)
tplate2<-raster(ncols=(360),nrows=(180),ext=(full),crs=llCRS)


Bathy<-raster("Bathy_raster.gri")

#build raster stacks for model

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

Cmaps<-stack(d13Cjan, d13Cfeb, d13Cmar, d13Capr, d13Cmay, d13Cjun, d13Cjul, d13Caug, d13Csep, d13Coct, d13Cnov, d13Cdec)


SSTjan<-raster("SST_Jan.gri")
SSTfeb<-raster("SST_Feb.gri") 
SSTmar<-raster("SST_Mar.gri")
SSTapr<-raster("SST_Apr.gri")
SSTmay<-raster("SST_May.gri")
SSTjun<-raster("SST_Jun.gri")
SSTjul<-raster("SST_Jul.gri")
SSTaug<-raster("SST_Aug.gri")
SSTsep<-raster("SST_Sep.gri")
SSToct<-raster("SST_Oct.gri")
SSTnov<-raster("SST_Nov.gri")
SSTdec<-raster("SST_Dec.gri")

SSTs<-stack(SSTjan, SSTfeb, SSTmar, SSTapr, SSTmay, SSTjun, SSTjul, SSTaug, SSTsep, SSToct, SSTnov, SSTdec)

PlkBjan<-raster("PlkB_Jan.gri")
PlkBfeb<-raster("PlkB_Feb.gri") 
PlkBmar<-raster("PlkB_Mar.gri")
PlkBapr<-raster("PlkB_Apr.gri")
PlkBmay<-raster("PlkB_May.gri")
PlkBjun<-raster("PlkB_Jun.gri")
PlkBjul<-raster("PlkB_Jul.gri")
PlkBaug<-raster("PlkB_Aug.gri")
PlkBsep<-raster("PlkB_Sep.gri")
PlkBoct<-raster("PlkB_Oct.gri")
PlkBnov<-raster("PlkB_Nov.gri")
PlkBdec<-raster("PlkB_Dec.gri")

PlkBs<-stack(PlkBjan, PlkBfeb, PlkBmar, PlkBapr, PlkBmay, PlkBjun, PlkBjul, PlkBaug, PlkBsep, PlkBoct, PlkBnov, PlkBdec)

# Master variables used to move animals in each time step (t):
# Probability of moving - - Prob.move
# Distance moved - -  - - - Dist.move
# Direction moved- - - - -  Dir.move
Prob.move<-0
Dist.move<-0
Dir.move<-NA

# threshold min Plk biomass for staying
Pk_thresh<-50

# threshold min SST for breeding
SSTB_thresh<-20

# temp range
max.temp <-25
min.temp<- 3

# Behavioural states: Forage (F), Migrate north (MN), Migrate south (MS),Calve (C)

#  add month nos for expecetd behavioural states (Can change to environment+physiology trigger later)
BS<-NA


SST.prev<-NA

# tst<-as.matrix(rep(1, length(10400)))
 #full<-extent(-80,50,0,80)
 #tplate<-raster(nrows=(130),ncols=(80),ext=(full),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# set up split screen and adding points to both panels
 par(bg="black")
 par(col.lab="black")
 par(col.axis="black")

 pars<-c('plt', 'usr')
 par(mfrow=c(1,2))

 par1<-c(list(mfg=c(1,1), par=pars))
 par2<-c(list(mfg=c(1,2), par=pars))
 par(mar=c(3,3,2,6))

# testP<-raster(tst, template=tplate)
# image(testP, col="grey95")

 plot(x=NA, y=NA, xlim=c(-150,-70), ylim=c(-10, 60), xlab="Day No", ylab="d13C")
 rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey40")

 map('world',xlim=c(-150,-70),ylim=c(-10,60), col="black", fill=TRUE, add =TRUE, lwd=0.25)
 par(new=TRUE)


# calendar day number of start
day.no.i<-1


# Current location 
Lon.i<- -115
Lat.i<- 20

Lon.now<-Lon.i
Lat.now<-Lat.i


# plot baleen isotope in second split screen
par(mar=c(3,0,2,2))
 par(col.lab="white")
 par(col.axis="white")
 xt<-seq(from=day.no.i, to=day.no.i+tot.day, by=10)
 yt=seq(from=-30, to=-15, by=1)
 plot(x=NA, y=NA, xlim=c(day.no.i, day.no.i+tot.day), ylim=c(-30, -15), axes=FALSE, ann=FALSE)
 axis(1,labels=xt, at=xt, col="white", line=1)
 axis(2,labels=yt, at=yt, col="white", line=1)
 rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey40")
 par(new=TRUE)



#set up vectors for rns nos
N.migrate<-rep(NA, rep.no)
S.migrate<-rep(NA, rep.no)
Forage<-rep(NA, rep.no)

# resident or migrant? here 50% probable migrant
migYN<-rbinom(rep.no,1,0.5)



# maximum daily distance and std dev (km by behaviour state)
max.dist.kmF<-rnorm(rep.no,15,5)
max.dist.kmMS<-rnorm(rep.no,70,20)
max.dist.kmMN<-max.dist.kmMS

dist.SDF<-25
dist.SDMS<-30
dist.SDMN<-50

# set up movement direction variation
F<-matrix(data=NA, nrow=rep.no, ncol=8, byrow=TRUE)
F[,1]<-rnorm(rep.no,15,5)
F[,2]<-rnorm(rep.no,15,5)
F[,3]<-rnorm(rep.no,15,5)
F[,4]<-rnorm(rep.no,15,5)
F[,5]<-rnorm(rep.no,15,5)
F[,6]<-rnorm(rep.no,15,5)
F[,7]<-rnorm(rep.no,15,5)
F[,8]<-rnorm(rep.no,15,5)


MS<-matrix(data=NA, nrow=rep.no, ncol=8, byrow=TRUE)
MS[,1]<-rnorm(rep.no,7.5,5)
MS[,2]<-rnorm(rep.no,7.5,5)
MS[,3]<-rnorm(rep.no,7.5,5)
MS[,4]<-rnorm(rep.no,15,5)
MS[,5]<-rnorm(rep.no,45,20)
MS[,6]<-rnorm(rep.no,45,20)
MS[,7]<-rnorm(rep.no,75,40)
MS[,8]<-rnorm(rep.no,100,40)


#create more symmetrical migrations - but increase variance
MN<-matrix(data=NA, nrow=rep.no, ncol=8, byrow=TRUE)

MN[1:rep.no,]<-rev(MS[1:rep.no,])
#MN<-c(rnorm(1,100,5), rnorm(1,75,8), rnorm(1,25,8), rnorm(1,25,3), rnorm(1,15,5), rnorm(1,7.5,2), rnorm(1,7.5,2), rnorm(1,7.5,2))


#set up progress bar
# pb = txtProgressBar(min = 0, max = rep.no, initial = 1) 

# ******************************************
# source the whale simulation function
source("whale_sim_fun.R")
# ******************************************


# Now loop over each iteration in parallel using parallel mclapply
whale_reps <- mclapply(1:rep.no, whale_sim_fun, 
                       mc.cores = 8)

save(whale_reps, file = "big_whale_run.rda", compress = "xz")

# AJ - i have to convert this list to a matrix for output in Clive's format
# There is code in the helpfile for ?parallel::mclapply

# write final file
#write.csv(Whale, file="PacifStest.csv")

