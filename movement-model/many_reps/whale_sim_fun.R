whale_sim_fun <- function(rep_id){
  
  
  # table for storing data
  Whale<-matrix(NA, nrow=tot.day, ncol=14)
  Whale<-as.data.frame(Whale)
  colnames(Whale)<-c("Day.No", "Lat", "Lon", "km.Lat", "km.Lon","SST", "Plk", "d13C", "direction", "distance", "B.state", "Depth", "Rep", "Month")
  as.factor(Whale[,7])
  count=0
  
  
  # setTxtProgressBar(pb,reps)
  
  #HERE set up random variations in whale behavioural variables - fixed for individual whale but vary between reps
  
  
  if(migYN[rep_id]<1){
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
    # AJ - i dont know what this chunk of code is doing.
    if(rep_id==1){
      day.sim<-count
    }else
    {
      countR<-tot.day*(rep_id-1)
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
      if(Lon.now<(-160)){adjacent$Prob<-F[rep_id,]+c(-10,-5,10,-10,10, -10, -5, 10)}else
        adjacent$Prob<-adjacent$Prob<-F[rep_id,]
      
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
      ifelse(Plk.t<Pk_thresh, max.dist.km<- rnorm(1,10,25), max.dist.km <- max.dist.kmF[rep_id,])
      dist.SD<-dist.SDF
    } else 
      
      
      
      
      
      
      if(BS=="MS"){
        if(Lon.now<(-160)){adjacent$Prob<-MS[rep_id,]+c(-50,-25,50,-50,50, -50, -25, 10)}else
          adjacent$Prob<-MS[rep_id,]
        
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
        
        max.dist.km=max.dist.kmMS[rep_id]
        dist.SD<-dist.SDMS
        
        #			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){adjacent$Prob<-c(1,1,1,1,1,1,1,1)}
        
        # end the loop if you're stuck in land (i.e. start the next rep)
        if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){break}
      }else
        
        
        
        
        
        
        
        if(BS=="MN"){
          if(Lon.now <(-160)){adjacent$Prob<-MN[rep_id,]+c(-50,-25,50,-50,50, -50, -25, 10)} else
            adjacent$Prob<-MN[rep_id,]
          
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
          max.dist.km=max.dist.kmMN[rep_id]
          dist.SD<-dist.SDMN
          #			if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){adjacent$Prob<-c(1,1,1,1,1,1,1,1)}
          
          # end the loop if you're stuck in land (i.e. start the next rep)
          if (is.na(sum(adjacent$Prob, na.rm=TRUE)==TRUE)){break}
          
        } 
    
    
    
    
    
    # how far will you move (in degree cells) currently a random poisson (degree) or norm (km)
    
    distance<-rpois(1,2)
    if(distance>max.dist) {distance=max.dist}
    
    # truncated normal distribution defines length of the 
    # movement vector.
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
    Whale[count,13]<-rep_id # record rep id here
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
  #end rep_id loop
  
  # add in counts for each iteration
  Whale$count<-rep(seq(from=1, to=t, by=1))
  
  return(Whale)
} # end whale_sim_fun
