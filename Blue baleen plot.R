library(TSA)

# setwd("/Users/trueman/Desktop/R scripts/Whale")
data<-read.csv("data/KC.NHM.full.csv", header=TRUE)
par(mfrow=c(1,1))
colz<-colorRampPalette(c("brown", "cadet blue", "pink"))(12)
colz<-seq(1:6)
fin<-data[data$Spp=="fin",]
minke<-data[data$Spp=="minke",]
sei<-data[data$Spp=="sei",]
blue<-data[data$Spp=="blue",]


#BLUE
#setEPS()
#postscript("blue.eps")
par(mfrow=c(1,1))
plot(x=rev(blue$Samp.No[blue$Whale=="KC7"]), y=blue$d13C[blue$Whale=="KC7"], 
     type="o", xlim=c(80,0), ylim=c(-20, -16.5), xlab="increment no", 
     ylab=expression(paste(delta^{13},"C")), pch=21, cex=1.5, lwd=0.8, bg=colz[1])
text(5,-20, "most recent")
text(80,-20, "oldest")
text(5, -16.75, "Whale KC1")

par(new=TRUE)
plot(x=rev(blue$Samp.No[blue$Whale=="KC7"]), y=blue$d15N[blue$Whale=="KC7"], 
     type="o", xlim=c(80,0), ylim=c(5, 13),  axes=F, xlab="", ylab="", 
     pch=21, cex=1.5, lwd=0.8, bg="grey")
axis(side=4)
mtext(side=4, line=3, expression(paste(delta^{15},"N")))
#dev.off()
#par(new=FALSE)





## fourier analysis of blue whale - frequency is in cm of baleen
## fourier for phase 1
blueP1<-blue[blue$Samp.No<(80-25),]

par(mfrow=c(1,1))
p=periodogram(blueP1$d15N)
#plot(p)
dd<-data.frame(freq=p$freq, spec=p$spec)
order=dd[order(-dd$spec),]
top2 = head(order, 2)
top2
distance=1/top2$f

text(0.4, 4, paste(distance[1],"cm"))
#text(0.4, 3.5, paste(distance[2], "cm")


## autocorrelation analysis of phase 1 d15N, and cross-correlation of phase 1 d15N and d13C values

acf(blue$d15N, lag.max=30, plot=TRUE)
ccf(blue$d13C, blue$d15N, lag.max=30, plot=TRUE)
