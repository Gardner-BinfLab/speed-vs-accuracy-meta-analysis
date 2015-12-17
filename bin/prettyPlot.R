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

######################################################################
plotMe <- function(r) {
       
relCitesA<-cor.test(1-d$accuracyRank, as.numeric(d$relCites), method = "spearman")

hindexA  <-cor.test(1-d$accuracyRank, as.numeric(d$hindex),   method = "spearman")
mindexA  <-cor.test(1-d$accuracyRank, as.numeric(d$mindex),   method = "spearman")
H5A      <-cor.test(1-d$accuracyRank, as.numeric(d$H5),       method = "spearman")
relAgeA  <-cor.test(1-d$accuracyRank, as.numeric(d$relAge),   method = "spearman")
speedA   <-cor.test(1-d$accuracyRank, as.numeric(d$speedRank),method = "spearman")
citesA   <-cor.test(1-d$accuracyRank, as.numeric(d$cites),    method = "spearman")
IFA      <-cor.test(1-d$accuracyRank, as.numeric(d$IF),       method = "spearman")

op<-par(mfrow=c(1,2),cex=1.1,las=2)
barplot(t(c(hindexA$estimate, mindexA$estimate, H5A$estimate, relAgeA$estimate, speedA$estimate, citesA$estimate, IFA$estimate)), names=c("H-index", "M-index", 'journal.H5', "relative\nage", "speed", "#citations", 'journal.IF'), ylab="Spearman's rho",ylim=c(-0.1,0.1),main="Correlations with accuracy")
lines(c(-100,100),c(0,0))

par(mar = c(5,4,4,5) + .1)
smoothScatter(d$speedRank, d$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="mean normalised accuracy rank", xlab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n"
) 
lines(lowess(d$speedRank, d$accuracyRank, f = .2), col = 2, lwd=5)
axis(1,at=(0:5)/5)
axis(2,at=(0:5)/5)
th <- as.numeric(quantile(as.numeric(d$IF), probs=0.75,na.rm=T))
points(d$speedRank[as.numeric(d$IF) > th], d$accuracyRank[as.numeric(d$IF) > th], pch='*',cex=1.5)
th <- as.numeric(quantile(as.numeric(d$hindex), probs=0.75,na.rm=T))
points(d$speedRank[d$hindex > th], d$accuracyRank[d$hindex > th], pch='o',cex=1.0)
th <- as.numeric(quantile(as.numeric(d$cites), probs=0.75,na.rm=T))
points(d$speedRank[d$cites > th], d$accuracyRank[d$cites > th], pch='x',cex=1.0)
text(1.0, 1.1, "* = hi profile journal; o = hi profile author; x = hi cited",  pos=4, cex=0.5)

boxit2()

}

######################################################################
plotMeOld <- function(r) {
       
if(r[1]>0 & r[2]>0){
	op<-par(mfrow=c(2,2),cex=1.1,las=2)
}
else{
	op<-par(mfrow=c(1,2),cex=1.1,las=2)
}

if( r[1]>0 ){
par(mar = c(5,4,4,5) + .1)
smoothScatter(d$accuracyRank, d$speedRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.95, xlab="mean normalised accuracy rank", ylab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n") #nrpoints=.3*length(d$speedRank)
th <- as.numeric(quantile(as.numeric(d$IF), probs=0.75,na.rm=T))
points(d$accuracyRank[as.numeric(d$IF) > th], d$speedRank[as.numeric(d$IF) > th], pch='*',cex=1.5)
th <- as.numeric(quantile(as.numeric(d$hindex), probs=0.75,na.rm=T))
points(d$accuracyRank[d$hindex > th], d$speedRank[d$hindex > th], pch='o',cex=1.0)
th <- as.numeric(quantile(as.numeric(d$cites), probs=0.75,na.rm=T))
points(d$accuracyRank[d$cites > th], d$speedRank[d$cites > th], pch='x',cex=1.0)
text(1.0, 1.1, "* = hi profile journal; o = hi profile author; x = hi cited",  pos=4, cex=0.5)
axis(1,at=(0:5)/5)
axis(2,at=(0:5)/5)
lines(lowess(d$accuracyRank, d$speedRank, f = .2), col = 2, lwd=5)

boxit()


######################################################################
#sumRanks	accuracyRank	speedRank	method	numTests	yearPublished	IF	H5	cites	hindex	mindex
cites   <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$cites),    method = "spearman")
IF      <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$IF),       method = "spearman")
H5      <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$H5),       method = "spearman")
hindex  <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$hindex),   method = "spearman")
mindex  <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$mindex),   method = "spearman")
relAge  <-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$relAge),   method = "spearman")
relCites<-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$relCites), method = "spearman")

#cites per year
aac <- ((as.numeric(d$yearPublished)>0) & (as.numeric(d$cites) > 0))
citesPerYear<-cor.test(2-d$accuracyRank-d$speedRank, as.numeric(d$cites[aac])/(1+2015-as.numeric(d$yearPublished)), method = "spearman")

barplot(t(c(hindex$estimate, H5$estimate, mindex$estimate, relAge$estimate, cites$estimate, IF$estimate)), names=c("H-index", 'journal.H5', "M-index", "relative\nage", "#citations", 'journal.IF'), ylab="Spearman's rho",ylim=c(-0.1,0.1), main="Correlations with speed+accuracy")
lines(c(-100,100),c(0,0))

}

if( r[2]>0 ){

######################################################################
sPa<-d$accuracyRank+d$speedRank
par(mar = c(6,4,4,5) + .1, cex=1.1)

hIQ <-quantile(as.numeric(d$hindex), probs=c(0,0.25,0.5,0.75,1),na.rm=T)
hiHI   <-sPa[as.numeric(d$hindex)   > hIQ[4]]
hqrHI  <-sPa[ (as.numeric(d$hindex) <=hIQ[4]) & (as.numeric(d$hindex) > hIQ[3]) ]
lqrHI  <-sPa[ (as.numeric(d$hindex) <=hIQ[3]) & (as.numeric(d$hindex) > hIQ[2]) ]
loHI   <-sPa[  as.numeric(d$hindex) <=hIQ[2] ]
boxplot(loHI,lqrHI,hqrHI,hiHI,names=c("low\nH-index","lower\ninter-quartile\nH-index","higher\ninter-quartile\nH-index","high\nH-index"), ylab="speed+accuracy rank", ylim=c(2.0,0.0), main="H-index",notch = T, varwidth = T)

otHI   <-sPa[  as.numeric(d$hindex) > hIQ[2] ]
wilcox.test(loHI,otHI,alternative="l")
     t.test(loHI,otHI,alternative="l")

ifIQ<-quantile(as.numeric(d$IF), probs=c(0,0.25,0.5,0.75,1),na.rm=T)
hiIF   <-sPa[as.numeric(d$IF)   > ifIQ[4]]
hqrIF  <-sPa[ (as.numeric(d$IF) <=ifIQ[4]) & (as.numeric(d$IF) > ifIQ[3]) ]
lqrIF  <-sPa[ (as.numeric(d$IF) <=ifIQ[3]) & (as.numeric(d$IF) > ifIQ[2]) ]
loIF   <-sPa[  as.numeric(d$IF) <=ifIQ[2] ]
boxplot(loIF,lqrIF,hqrIF,hiIF,names=c("low IF","lower\ninter-quartile\nIF","higher\ninter-quartile\nIF","high IF"), ylab="speed+accuracy rank", ylim=c(2.0,0.0), main="Impact factor",notch = T, varwidth = T)

otIF   <-sPa[as.numeric(d$IF)   <= ifIQ[4]]
wilcox.test(hiIF,otIF,alternative="l")
     t.test(hiIF,otIF,alternative="l")
     }


}

######################################################################
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  my.cols <- rev(brewer.pal(k, "RdYlBu"))

  image.plot(xm,ym,z, col = my.cols, legend.only = T, add =F)
}

######################################################################
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

######################################################################
boxit2 <- function(){

#DIAGONAL
lines(c(0,1),c(1,0),lty=2, lwd=3)
#BOX
lines(c(0,0),c(0,1),lty=1,lwd=2)#left
lines(c(0,1),c(1,1),lty=1,lwd=2)#top
lines(c(1,1),c(1,0),lty=1,lwd=2)#right
lines(c(1,0),c(0,0),lty=1,lwd=2)#bottom

text(-0.2,-0.2, "fast+accurate",  pos=2, cex=0.5)
text(-0.2, 1.2, "fast+inaccurate",  pos=2, cex=0.5)
text( 1.2, 1.2, "slow+inaccurate",pos=4, cex=0.5)
text( 1.2,-0.2, "slow+accurate",pos=4, cex=0.5)

}

######################################################################

pdf(file=    "../figures/Figure1.pdf", width = 11,  height = 5)
plotMe( c(1,1) )
dev.off()

png(filename="../figures/Figure1.png", width = 730, height = 360)
plotMe( c(1,1) )
dev.off()

######################################################################

# #ENRICHMENT:
# accQs<-quantile(as.numeric(d$accuracyRank), probs=c(0.25,0.5,0.75),na.rm=T)
# spdQs<-quantile(as.numeric(d$speedRank), probs=c(0.25,0.5,0.75),na.rm=T)

# tt<-length(d$sumRanks)
# fa<-length(d$sumRanks[ d$speedRank<spdQs[1] & d$accuracyRank<accQs[1] ]) #fast & accurate
# sa<-length(d$sumRanks[ d$speedRank>spdQs[3] & d$accuracyRank<accQs[1] ]) #slow & accurate
# fi<-length(d$sumRanks[ d$speedRank<spdQs[1] & d$accuracyRank>accQs[3] ]) #fast & inaccurate
# si<-length(d$sumRanks[ d$speedRank>spdQs[3] & d$accuracyRank>accQs[3] ]) #slow & inaccurate

# fast<-length(d$sumRanks[ d$speedRank<spdQs[1] ])
# slow<-length(d$sumRanks[ d$speedRank>spdQs[3] ])

# speedAcc <-
# matrix(c(fa, sa, fi, si),
#             nrow = 2,
#             dimnames =
#             list(c("fast", "slow"),
#             c("accurate", "inaccurate")))
# speedAcc
# fisher.test(speedAcc, alternative = "less")

# inAccvNot <-
# matrix(c(fast-fi, slow-si, fi, si),
#             nrow = 2,
#             dimnames =
#             list(c("fast", "slow"),
#             c("!inaccurate", "inaccurate")))
# inAccvNot
# fisher.test(inAccvNot, alternative = "l")


######################################################################
#TESTING IF SPEED inv. ACCURACY IS TRUE:

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

######################################################################

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









