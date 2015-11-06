#R CMD BATCH ../bin/prettyPlot.R
library(MASS)  # in case it is not already loaded 
library(RColorBrewer)
library(fields)
k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
d <-read.table("meanRankSpeedData.tsv", header=T)
dr<-read.table( "rawRankSpeedData.tsv", header=T)
#reg1  <- lm(d$speedRank~d$accuracyRank)
#reg1c <- coefficients(reg1)

plotMe <- function() {
       
op<-par(mfrow=c(1,2),cex=1.1,las=2)



# plot(c(0,1),c(1,0), xlim=c(1,0),ylim=c(1,0),xlab="normalised accuracy rank", ylab="normalised speed rank",type="l",lty=2,lwd=3)

# #DIAGONALS
# #lines(c(0,1),c(0,1),lty=2)
# #lines(c(0,1),c(1,0),lty=2, lwd=3)

# #BOX
# lines(c(0,0),c(0,1),lty=1,lwd=2)#left
# lines(c(0,1),c(1,1),lty=1,lwd=2)#top
# lines(c(1,1),c(1,0),lty=1,lwd=2)#right
# lines(c(1,0),c(0,0),lty=1,lwd=2)#bottom

# for(id in unique(dr$testId)){       
#        regr1  <- lm(dr$normSpeedRank[ dr$testId == id]~dr$normAccuracyRank[ dr$testId == id])
#        regr1c <- coefficients(regr1)
#        if( regr1c[1]+regr1c[2] > 0 ){
# 	   lines(c(0,1),regr1c[1]+regr1c[2]*c(0,1), lwd=5)
#        }
#        else{#y negative when x = 0
#             #y = mx + c = 0
# 	    #  =>     x = -c/m
# 	   lines(c(0, -1*regr1c[1]/regr1c[2] ),  c(regr1c[1], 0), lwd=5)	
# 	   cat("Y<0!: ",id, "\n")
#        }
# }

#smoothScatter(dr$accuracyRank, dr$speedRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), pch=19, cex=.8, xlab="normalised accuracy rank", ylab="normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2)) 
#lines(lowess(dr$accuracyRank, dr$speedRank, f = .2), col = 2, lwd=5)
#boxit()

######################################################################
#sumRanks	accuracyRank	speedRank	method	numTests	yearPublished	IF	H5	cites	hindex	mindex
cites   <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$cites),    method = "spearman")
IF      <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$IF),       method = "spearman")
H5      <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$H5),       method = "spearman")
hindex  <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$hindex),   method = "spearman")
mindex  <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$mindex),   method = "spearman")
relAge  <-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$relAge),   method = "spearman")
relCites<-cor.test(d$accuracyRank+d$speedRank, as.numeric(d$relCites), method = "spearman")

barplot(t(c(hindex$estimate, mindex$estimate, relAge$estimate, H5$estimate, cites$estimate, relCites$estimate, IF$estimate)), names=c("corresAuth\nH-index", "corresAuth\nM-index", 'relativeAge', 'journalH5', 'totalCites', 'relativeCites', 'journalIF'), ylab="Spearman's rho",ylim=c(-0.5,0.5), main="Affect size of prestige measures\non speed+accuracy ranks")
lines(c(-100,100),c(0,0))

par(mar = c(5,4,4,5) + .1)
smoothScatter(d$accuracyRank, d$speedRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.8, xlab="mean normalised accuracy rank", ylab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n") #nrpoints=.3*length(d$speedRank)
th <- as.numeric(quantile(as.numeric(d$IF), probs=0.75,na.rm=T))
points(d$accuracyRank[as.numeric(d$IF) > th], d$speedRank[as.numeric(d$IF) > th], pch='*',cex=1.5)
th <- as.numeric(quantile(as.numeric(d$hindex), probs=0.75,na.rm=T))
points(d$accuracyRank[d$hindex > th], d$speedRank[d$hindex > th], pch='o',cex=1.0)
th <- as.numeric(quantile(as.numeric(d$cites), probs=0.75,na.rm=T))
points(d$accuracyRank[d$cites > th], d$speedRank[d$cites > th], pch='x',cex=1.0)
text(1.0, 1.1, "* = hi profile journal; o = hi profile author; x = hi cited",  pos=4, cex=0.5)
axis(1,at=(0:5)/5)
axis(2,at=(0:5)/5)

#abline(reg1, lwd=5)
#regression
#lines(c(0,1),reg1c[1]+reg1c[2]*c(0,1), lwd=5)
#xr<-0.2; text(xr, reg1c[1]+reg1c[2]*xr, "regression",pos=3, cex=0.8)
#lowess
lines(lowess(d$accuracyRank, d$speedRank, f = .2), col = 2, lwd=5)
#text(0.2, 0.4, "lowess curve",pos=3, cex=0.8, col = 2)

boxit()

}

fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  my.cols <- rev(brewer.pal(k, "RdYlBu"))

  image.plot(xm,ym,z, col = my.cols, legend.only = T, add =F)
}

boxit <- function(){

#DIAGONAL
lines(c(0,1),c(1,0),lty=2, lwd=3)
#BOX
lines(c(0,0),c(0,1),lty=1,lwd=2)#left
lines(c(0,1),c(1,1),lty=1,lwd=2)#top
lines(c(1,1),c(1,0),lty=1,lwd=2)#right
lines(c(1,0),c(0,0),lty=1,lwd=2)#bottom

text(-0.2,-0.2, "fast+accurate",  pos=2, cex=0.5)
text(-0.2, 1.2, "slow+accurate",  pos=2, cex=0.5)
text( 1.2, 1.2, "slow+inaccurate",pos=4, cex=0.5)
text( 1.2,-0.2, "fast+inaccurate",pos=4, cex=0.5)

}

pdf(file=    "../figures/Figure1.pdf", width = 10, height = 5)
plotMe()
dev.off()

png(filename="../figures/Figure1.png", width = 720, height = 360)
plotMe()
dev.off()

######################################################################
#PLAYING WITH P VALUES:

keys<-unique(dr$testId)
i<-1;
p_values <- rep(1, length(keys))
weights  <- rep(0, length(keys))

for(id in keys){       
       len<-length(dr$accuracyRank[ dr$testId == id])       
       spear<-cor.test(dr$accuracyRank[ dr$testId == id], dr$speedRank[ dr$testId == id], method = "spearman", alternative = "less")
       cat(spear$estimate, "\t", spear$p.value, "\t", len, "\t", id, "\n")
       p_values[i] <- spear$p.value
       
       methods<-dr$method[dr$testId == id]
       for (meth in methods){
       	   numOverlaps<-d$numTests[d$method == meth]; 
	   weights[i]<-weights[i]+numOverlaps
       }

       i<-i+1;
}

#transform weights -- higher weights have less overlap with other rankings/benchmarks:
weights<-1000/(weights+1)												                                                                                  

library(metap)

sumz(p_values, weights = weights)
#sumz =  -0.0356562 p =  0.5142218 

meanp(p_values)
#z =  -1.629963  p =  0.9484453

#library("CombinePValue")

#selfcontained.test(p_values,weight=weights,p_permu=NA)

#competitive.test(Pvalue=p_values,Weight=weights)



