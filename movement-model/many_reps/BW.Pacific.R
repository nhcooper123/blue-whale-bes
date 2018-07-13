# Whale migration model


#how many times to run the simulation (run a couple of reps first to check!)
rep.no=10 #1000

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

# duration of simulation (days)
tot.day<- 5 #365*4


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





# table for storing data
Whale<-matrix(NA, nrow<-rep.no*tot.day, ncol=14)
Whale<-as.data.frame(Whale)
colnames(Whale)<-c("Day.No", "Lat", "Lon", "km.Lat", "km.Lon","SST", "Plk", "d13C", "direction", "distance", "B.state", "Depth", "Rep", "Month")
as.factor(Whale[,7])
count=0

#set up progress bar
# pb = txtProgressBar(min = 0, max = rep.no, initial = 1) 


for (reps in 1:rep.no){

# setTxtProgressBar(pb,reps)

#HERE set up random variations in whale behavioural variables - fixed for individual whale but vary between reps


if(migYN[reps]<1){
N.migrate<-c(13,14)
S.migrate<-c(110,111)
Forage<-c(1:12)
} else

{
N.migrate<-c(3,4,5,6)
S.migrate<-c(9,10,11,12)
Forage<-c(1,2,7,8)
}



#set up coordinates in matrix form
Lat.sample<-90-Lat.i
Lon.sample<-180+Lon.i

km_dist<-Lat.Lon[Lat.sample,Lon.sample,]
Loc.array<-c(Lon.sample, Lat.sample)

km_dist.Move<-vector(length=2)

day.no<-day.no.i
# WRITE WHALE DAY NO, LAT, LON, KM HERE
Whale[1,1:5]<-c(day.no,Lat.i, Lon.i, km_dist[1], km_dist[2])
####################################################


# begin loop for timestep
for (t in 1:tot.day){
count=count+1

# stop day no going beyond 365	
	if(day.no >365) {day.no = 1}

# which month are you in? 


	if (day.no >0 & day.no <32){mon.no =1} else 
	if (day.no >31 & day.no <61){mon.no =2} else 
	if (day.no >60 & day.no <93) {mon.no =3} else 
	if (day.no >92 & day.no <123){mon.no =4} else 
	if (day.no >122 & day.no <155){mon.no =5} else 
	if (day.no >154 & day.no <186) {mon.no =6} else 
	if (day.no >185 & day.no <218) {mon.no =7} else 
	if (day.no >217 & day.no <250) {mon.no =8} else 
	if (day.no >249 & day.no <281) {mon.no =9} else 
	if (day.no >280 & day.no <313) {mon.no =10} else 
	if (day.no >312 & day.no <343) {mon.no =11} else  {mon.no =12}

	
#previous month - allow zooplankton response.
P.month<-mon.no-1
if (P.month==0) {P.month<-12}	
	
	#read in correct month layer for d13C, biomass and SST

	d13C<-subset(Cmaps,P.month)
	SST<-subset(SSTs,mon.no)
	PlkB<-subset(PlkBs,P.month)

# work out day number within rep

		if(reps==1){
			day.sim<-count
		}else
		{
		countR<-tot.day*(reps-1)
		day.sim<-count-countR
		}
		
		# define behavioural state - here based on month only
		# split two behavour times based on count number
		
		
		
		if(mon.no %in% N.migrate){BS<-"MN"}else
		if(mon.no %in% S.migrate){BS<-"MS"}else
		BS<-"F"
		

# set up adjacent degree vectors

	Lon.Adj<-c((Lon.sample-1), Lon.sample, (Lon.sample+1), (Lon.sample-1), (Lon.sample+1),(Lon.sample-1), Lon.sample, (Lon.sample+1)) 
	
	Lat.Adj<-c((Lat.sample-1), (Lat.sample-1), (Lat.sample-1), Lat.sample, Lat.sample, (Lat.sample+1), (Lat.sample+1), (Lat.sample+1)) 

		# sense depth in each adjacent square
		Depth.NW<-Bathy[Lat.Adj[1],Lon.Adj[1]]
		Depth.N<-Bathy[Lat.Adj[2],Lon.Adj[2]]
		Depth.NE<-Bathy[Lat.Adj[3],Lon.Adj[3]]
		Depth.W<-Bathy[Lat.Adj[4],Lon.Adj[4]]
		Depth.E<-Bathy[Lat.Adj[5],Lon.Adj[5]]
		Depth.SW<-Bathy[Lat.Adj[6],Lon.Adj[6]]
		Depth.S<-Bathy[Lat.Adj[7],Lon.Adj[7]]
		Depth.SE<-Bathy[Lat.Adj[8],Lon.Adj[8]]
	
	DepthV<-c(Depth.NW, Depth.N, Depth.NE, Depth.W, Depth.E, Depth.SW, Depth.S, Depth.SE)

	Depth.t<-Bathy[Loc.array[2],Loc.array[1]]
		
		

	#plankton abundance in current location
	Plk.t<-PlkB[Loc.array[2],Loc.array[1]]
	
	#SST in current location
	SST.t<-SST[Loc.array[2],Loc.array[1]]

	#d13C in current location
	d13C.t<-d13C[Loc.array[2],Loc.array[1]]
	
	
		#Depth in current location

	Depth.t<-Bathy[Loc.array[2],Loc.array[1]]

# do not migrate north if water is colder than 5 degrees (change BS to forage)
if(is.na(SST.t)==TRUE){BS=BS}else
if(BS=="MN" & SST.t<5){BS<-"F"}else
# do not migrate south if water is warmer than 25 degrees (change BS to calve or forage)
if(BS=="MS" & SST.t>25){BS<-"F"}

###############WRITE CONDITIONS TO WHALE FOR DAY = T: HERE. UPDATE MOVE LOCATIONS AT END OF LOOP FOR T+1
Whale[count,6:8]<-c(SST.t, Plk.t, d13C.t)
Whale[count,12]<-Depth.t


##### read in the adjacent vectors
# Depth
# SST
# plk

	ord<-c("NW", "N", "NE", "W", "E","SW", "S", "SE")
	MDir<-c(1,2,3,4,5,6,7,8)
	
				# sense plankton abundance in each adjacent square
	Plk.NW<-PlkB[Lat.Adj[1],Lon.Adj[1]]
	Plk.N<-PlkB[Lat.Adj[2],Lon.Adj[2]]
	Plk.NE<-PlkB[Lat.Adj[3],Lon.Adj[3]]
	Plk.W<-PlkB[Lat.Adj[4],Lon.Adj[4]]
	Plk.E<-PlkB[Lat.Adj[5],Lon.Adj[5]]
	Plk.SW<-PlkB[Lat.Adj[6],Lon.Adj[6]]
	Plk.S<-PlkB[Lat.Adj[7],Lon.Adj[7]]
	Plk.SE<-PlkB[Lat.Adj[8],Lon.Adj[8]]
	
		plk.tot<-c(Plk.NW, Plk.N, Plk.NE, Plk.W, Plk.E,Plk.SW, Plk.S, Plk.SE)

			# sense SST in each adjacent square
	SST.NW<-SST[Lat.Adj[1],Lon.Adj[1]]
	SST.N<-SST[Lat.Adj[2],Lon.Adj[2]]
	SST.NE<-SST[Lat.Adj[3],Lon.Adj[3]]
	SST.W<-SST[Lat.Adj[4],Lon.Adj[4]]
	SST.E<-SST[Lat.Adj[5],Lon.Adj[5]]
	SST.SW<-SST[Lat.Adj[6],Lon.Adj[6]]
	SST.S<-SST[Lat.Adj[7],Lon.Adj[7]]
	SST.SE<-SST[Lat.Adj[8],Lon.Adj[8]]
	
		SST.tot<-c(SST.NW, SST.N, SST.NE, SST.W, SST.E,SST.SW, SST.S, SST.SE)

			
			adjacent<-as.data.frame(cbind(ord, Lon.Adj, Lat.Adj, MDir), col.names=c("ord", "Lon", "Lat", "Dir"))

	# are you going to move? here setting a very simple binary probability for each behaviour state 	
	
	if(BS=="F"){
		
		ifelse(Plk.t<Pk_thresh, Prob.move <- 0.9, Prob.move <- 0.3)
		if(is.na(SST.t)==TRUE){Prob.move<-1} else
		if(SST.t<=min.temp){Prob.move<-1} else
		if(SST.t>=max.temp){Prob.move<-1}

     MoveYN<-rbinom(1,1,Prob.move)
	} else 
	
	if(BS=="MN"){
		
		ifelse(Plk.t>(Pk_thresh), Prob.move <- 0.5, Prob.move <- 0.9)
		if(is.na(SST.t)==TRUE){Prob.move<-1} else
		if(SST.t<=(min.temp+2)){Prob.move<-0.1} else
		if(SST.t>=(max.temp-5)){Prob.move<-1} else
        {Prob.move <- 0.9}
        
     MoveYN<-rbinom(1,1,Prob.move)
	} else 
	
	if(BS=="MS"){
		ifelse(Plk.t>(Pk_thresh), Prob.move <- 0.5, Prob.move <- 0.9)
		if(is.na(SST.t)==TRUE){Prob.move<-1} else
		if (SST.t>SSTB_thresh & SST.t<max.temp){Prob.move<-0.3} else

		{Prob.move <- 0.9}

     MoveYN<-rbinom(1,1,Prob.move)
	} 





	# what direction will you move in?	

		# arrange by plk and assign probability 
		adjacent$plk.tot<-plk.tot
		adjacent$SST<-SST.tot
		adjacent$Depth<-DepthV

# apply different movement rules for the behavioural states

    if (BS %in% "F"){
				if(Lon.now<(-160)){adjacent$Prob<-F[reps,]+c(-10,-5,10,-10,10, -10, -5, 10)}else
				adjacent$Prob<-adjacent$Prob<-F[reps,]
				
				if(is.na(SST.t)==TRUE){adjacent$Prob<-adjacent$Prob}else
				if(SST.t>21) {adjacent$Prob<-adjacent$Prob+c(25,20,10,5,5,1,1,1)}else
				if(SST.t<5) {adjacent$Prob<-adjacent$Prob+c(1,1,1,5,5,15,20,25)}
				
				
			adjacent$Prob<-adjacent$Prob+adjacent$plk.tot
	
				for(cold in 1:8){
				if(is.na(adjacent$SST[cold]==TRUE)){adjacent$Prob[cold]<-NA}else
				if(adjacent$SST[cold]<(min.temp+1)){
							adjacent$Prob[which.max(adjacent$SST)]<-adjacent$Prob[which.max(adjacent$SST)]*2} else
				if(adjacent$SST[cold]>(max.temp-1)){
					adjacent$Prob[which.min(adjacent$SST)]<-adjacent$Prob[which.min(adjacent$SST)]*2}
				}




	for(deep in 1:8){
				if(is.na(adjacent$Depth[deep])==TRUE){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>=0){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>-100){
							adjacent$Prob[deep]<-0.01*adjacent$Prob[deep]} else
				if(adjacent$Depth[deep]>-200){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/10} else
				if(adjacent$Depth[deep]>-400){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/2}
				}

#			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){adjacent$Prob<-c(1,1,1,1,1,1,1,1)}

# end the day loop if you're stuck in land (i.e. start the next rep)
			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){break}


	max.dist=1
	ifelse(Plk.t<Pk_thresh, max.dist.km<- rnorm(1,10,25), max.dist.km <- max.dist.kmF[reps,])
	dist.SD<-dist.SDF
} else 






				if(BS=="MS"){
				if(Lon.now<(-160)){adjacent$Prob<-MS[reps,]+c(-50,-25,50,-50,50, -50, -25, 10)}else
				adjacent$Prob<-MS[reps,]

#			if(Lon.now>-5){adjacent$Prob<-c(10,10,10,20,40,40,70,85)}else
#			adjacent$Prob<-c(0.1,0.1,0.1,15,15,30,40,30)
			
								# bit more liklihood for warmer water
	
			for(cold in 1:8){
				if(is.na(adjacent$SST[cold]==TRUE)){adjacent$Prob[cold]<-NA}else
					if(adjacent$SST[cold]<-which.max(adjacent$SST)) {
							adjacent$Prob[cold]<-adjacent$Prob[cold]+7}
		

					# bit more liklihood for higher plk
		

				if(is.na(adjacent$plk.tot[cold])==TRUE){adjacent$Prob[cold]<-NA} else
					if(adjacent$plk.tot[cold]<- which.max(adjacent$plk.tot)) {
							adjacent$Prob[cold]<-adjacent$Prob[cold]+10}
				}

			
			# sense if potential cell is shelf (-150m depth)- if so reduce prob 

	for(deep in 1:8){
				if(is.na(adjacent$Depth[deep])==TRUE){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>=0){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>-150){
							adjacent$Prob[deep]<-0.0001} else
				if(adjacent$Depth[deep]>-250){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/10} else
				if(adjacent$Depth[deep]>-500){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/2}
				}
				
				# give max liklihood relative to depth if there is land adjacent

		if(NA %in% adjacent$SST){adjacent$Prob<-(adjacent$Depth*-1)}
		adjacent$Prob[adjacent$Prob<0]<-0

					max.dist=2

			max.dist.km=max.dist.kmMS[reps]
			dist.SD<-dist.SDMS
			
#			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){adjacent$Prob<-c(1,1,1,1,1,1,1,1)}

# end the loop if you're stuck in land (i.e. start the next rep)
			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){break}
}else

		
		
		



		if(BS=="MN"){
					if(Lon.now <(-160)){adjacent$Prob<-MN[reps,]+c(-50,-25,50,-50,50, -50, -25, 10)} else
					adjacent$Prob<-MN[reps,]
					
			#		if(Lon.now >-40){adjacent$Prob<-c(40,70,85,20,40,10,10,10)} else
			#		adjacent$Prob<-c(30,40,30,15,15,0.1,0.1,0.1)

		# bit more liklihood for higher plk
		
			for(cold in 1:8){
				if(is.na(adjacent$plk.tot[cold])==TRUE){adjacent$Prob[cold]<-NA} else
					if(adjacent$plk.tot[cold]<- which.max(adjacent$plk.tot)) {
							adjacent$Prob[cold]<-adjacent$Prob[cold]+10}
				}

# sense if you're off the slope (-400m depth)- if so reduce prob 

	for(deep in 1:8){
				if(is.na(adjacent$Depth[deep])==TRUE){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>=0){adjacent$Prob[deep]<-NA} else
				if(adjacent$Depth[deep]>-150){
							adjacent$Prob[deep]<-0.0001} else
				if(adjacent$Depth[deep]>-250){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/10} else
				if(adjacent$Depth[deep]>-500){
							adjacent$Prob[deep]<-adjacent$Prob[deep]/2}
				}
				
				# give max liklihood relative to depth if there is land adjacent

		if(NA %in% adjacent$SST){adjacent$Prob<-(adjacent$Depth*-1)}
		adjacent$Prob[adjacent$Prob<0]<-0

		max.dist=2
		max.dist.km=max.dist.kmMN[reps]
		dist.SD<-dist.SDMN
#			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){adjacent$Prob<-c(1,1,1,1,1,1,1,1)}

# end the loop if you're stuck in land (i.e. start the next rep)
			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){break}

		 } 





		# how far will you move (in degree cells) currently a random poisson (degree) or norm (km)
		
		distance<-rpois(1,2)
		if(distance>max.dist) {distance=max.dist}
		
		distance.km<-rnorm(1, max.dist.km/2, dist.SD)
		distance.km[distance.km<0]<-0

		if(distance.km>max.dist.km) {distance.km=max.dist.km}
		
Move.Prob<-adjacent$Prob
Move.Prob[is.na(Move.Prob)]<-0.0001
Move.Prob[Move.Prob==0]<-0.0001


	#function to move = direction move and dist move
	direction.choice<-as.numeric(sample(adjacent$MDir, 1, prob=Move.Prob, replace=FALSE))
	

# calculate new location in km from reference cell (lat=90N, lon=180W) - but noting that dist from refernce in longitude will vary with lat - so need to calculte wrt proportional distance of total circumference at old and new lats.


Lat_current<-90-as.integer(km_dist[1]/111)
Lat_current_sample<-as.integer(km_dist[1]/111)

# calculate km_dist longitude [2] as proportion of globe at that latitude [1]
km_current_prop<-(km_dist[2]/Lat.Lon[Lat_current_sample,359,2])
		
			km.Lt.Move<-c(km_dist[1]-(sin(45)*distance.km), km_dist[1]-distance.km, km_dist[1]-(sin(45)*distance.km), km_dist[1], km_dist[1],km_dist[1]+(sin(45)*distance.km), km_dist[1]+distance.km, km_dist[1]+(sin(45)*distance.km)) 

Lat_line_next<-as.integer(km.Lt.Move/111)

# calculate current km_dist longitude [2] as proportion of globe at the potential new latitudes [1]
km_next_lat<-(km_current_prop*Lat.Lon[Lat_line_next,359,2])


			km.Ln.Move<-c(km_next_lat[1]-(sin(45)*distance.km), km_next_lat[2], km_next_lat[3]+(sin(45)*distance.km), km_next_lat[4]-distance.km, km_next_lat[5]+distance.km, km_next_lat[6]-(sin(45)*distance.km), km_next_lat[7], km_next_lat[8]+(sin(45)*distance.km)) 

km_next_prop<-(km.Ln.Move/Lat.Lon[Lat_line_next,359,2])

Lat_next<-90-Lat_line_next
Lon_next<--180+as.integer(km_next_prop*360)

km_dist.Move[1]<-km.Lt.Move[direction.choice]
km_dist.Move[2]<-km.Ln.Move[direction.choice]

Lat_new<-Lat_next[direction.choice]
Lon_new<-Lon_next[direction.choice]
Lat.sample.new<-90-Lat_new
Lon.sample.new<-180+Lon_new

## set the new locations - and set location to previous if surrounded by land

		if(MoveYN==0) {
			Lon.sample.new=Lon.sample
			Lat.sample.new=Lat.sample
			km_dist<-km_dist

			} else
	km_dist<-km_dist.Move
	
if(Whale$Depth[t]>0){
	Lon.new=Whale$Lon[count-2]
	Lat.new=Whale$Lat[count-2]
	Lon.sample.new=180+Lon.new
	Lat.sample.new=90-Lat.new
	km_dist<-c(Whale$km.Lat[count-2], Whale$km.Lon[count-2])
	
}	
	
Loc.array<-c(Lon.sample.new, Lat.sample.new)


# JUST WRITE THE MOVEMENT DATA HERE

	
	# save the information
Whale[count,9:11]<-c(ord[direction.choice], distance.km, BS)
Whale[count,13]<-reps
Whale[count,14]<-mon.no
	
	day.no=day.no+1
	if(BS=="F"){cols<-"green"} else
	if(BS=="MN"){cols<-"blue"} else
	cols<-"red"
	
#  	par(par1)
# 	par(mar=c(3,3,2,6))
# 	plot(x=NA, y=NA, xlim=c(-150,-70), ylim=c(-10, 60), xlab="", ylab="", axes=FALSE)
# 	points(Whale$Lon[count-1], Whale$Lat[count-1], pch=21, cex=1, lwd=0.1, bg=cols, col="black")
#  	map('world',xlim=c(-150,-70),ylim=c(-10,60), col="black", fill=TRUE, add =TRUE, lwd=0.25)
# 
# 	par(par2)
# 	par(mar=c(3,0,2,2))
# 	plot(x=NA, y=NA, xlim=c(day.no.i, day.no.i+tot.day), ylim=c(-30, -15), xlab="", ylab="", axes=FALSE)
# 
# 	points(day.no.i-1+t, Whale$d13C[count], pch=21, cex=1, lwd=0.1, bg=cols, col="black")
# 	
#safe if SST.t<-NA

Lat.now<-90-Lat.sample.new
Lon.now=-180+Lon.sample.new
Lat.sample<-Lat.sample.new
Lon.sample<-Lon.sample.new

SST.prev<-Whale$SST[count-1]
	Whale[count,1:5]<-c(day.no,Lat.now, Lon.now, km_dist[1], km_dist[2])
# end time step loop t
}
#end reps loop
print(reps)
}
#close progress bar
#close(pb)
# add a column of day.nos to the final file (helpful for later analysis)
Whale$count<-rep(seq(from=1, to=t, by=1),reps)

# write final file
write.csv(Whale, file="PacifStest.csv")

