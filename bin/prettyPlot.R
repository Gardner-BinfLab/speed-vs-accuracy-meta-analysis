#!/usr/bin/Rscript

#R CMD BATCH ../bin/prettyPlot.R

library(MASS)  # in case it is not already loaded 
library(RColorBrewer)
library(fields)
library(gplots)

k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
d <-read.table("meanRankSpeedData.tsv", header=T)
dr<-read.table( "rawRankSpeedData.tsv", header=T)
reg1  <- lm(d$speedRank~d$accuracyRank)
#reg1c <- coefficients(reg1)
summary(reg1)

regM <- lm(accuracyRank ~ speedRank + IF + H5 + cites + hindex + mindex + relAge + relCites, data=d)
summary(regM)

######################################################################
plotMe <- function(r) {
       
op<-par(mfrow=c(1,1),cex=1.1,las=2)

par(mar = c(5,4,4,5) + .1)
smoothScatter(d$speedRank, d$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="mean normalised accuracy rank", xlab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Speed") 
lines(lowess(d$speedRank, d$accuracyRank, f = .2), col = 2, lwd=5)
axis(1,at=(0:5)/5)
axis(2,at=(0:5)/5)
boxit()

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
text(-0.2, 1.2, "fast+inaccurate",  pos=2, cex=0.5)
text( 1.2, 1.2, "slow+inaccurate",pos=4, cex=0.5)
text( 1.2,-0.2, "slow+accurate",pos=4, cex=0.5)

}

######################################################################

pdf(file=    "../figures/smoothScatter-speed-vs-accuracy.pdf", width = 5.5,  height = 5)
plotMe( )
dev.off()

######################################################################
pdf(file=    "../figures/smoothScatters.pdf", width = 11,  height = 5)
op<-par(mfrow=c(1,2),cex=1.1,las=2,mar = c(5,4,4,5) + .1)
smoothScatter(log10(as.numeric(d$IF)), d$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="JIF",xlim=c(log10(0.4),log10(54)),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. JIF") 
notNA <- !is.na(d$IF)
lines(lowess(log10(d$IF[notNA]), d$accuracyRank[notNA], f = .2), col = 2, lwd=5)
tcks<-c(0.5,1,2.5,5,10,25,50); axis(1,at=log10(tcks), tcks)
axis(2,at=(0:5)/5)

text(log10(2.576),  1.25, "BMC Bioinf.",    pos=4, srt=90, cex=0.75)
text(log10(4.333),  1.25, "JMB",            pos=4, srt=90, cex=0.75)
text(log10(4.981),  1.25, "Bioinformatics", pos=4, srt=90, cex=0.75)
text(log10(9.112),  1.25, "NAR",            pos=4, srt=90, cex=0.75)
text(log10(14.630), 1.25, "Genome res.",    pos=4, srt=90, cex=0.75)
text(log10(32.072), 1.25, "Nature methods", pos=4, srt=90, cex=0.75)

smoothScatter(log10(as.numeric(d$cites)), d$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="Cites",xlim=c(0,5),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Cites") 
notNA <- !is.na(d$cites)
lines(lowess(log10(d$cites[notNA]), d$accuracyRank[notNA], f = .2), col = 2, lwd=5)
axis(1,at=0:5, c(1,10,100,"1,000","10,000","100,000"))
axis(2,at=(0:5)/5)

#smoothScatter(dr$speedRank, dr$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="Speed",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="raw accuracy vs. raw speed") 
#lines(lowess(d$speedRank, d$accuracyRank, f = .2), col = 2, lwd=5)
#axis(1,at=(0:5)/5)
#axis(2,at=(0:5)/5)

#smoothScatter(d$speedRank,   d$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="mean normalised accuracy rank", xlab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="mean accuracy vs. mean speed") 
#lines(lowess(d$speedRank, d$accuracyRank, f = .2), col = 2, lwd=5)
#axis(1,at=(0:5)/5)
#axis(2,at=(0:5)/5)

dev.off()

######################################################################


pdf(file=    "../figures/supplementary-figures-small.pdf", width = 8,  height = 15)
op<-par(mfrow=c(4,2),cex=1.0,las=1)
hist(d$accuracyRank, breaks=50, xlab="Accuracy",main="")
hist(d$speedRank, breaks=50, xlab="Speed",main="")
hist(log10(d$cites), breaks=30, xlab="Cites",main="",xaxt = "n",xlim=c(0,5))
axis(1,at=0:5, c(10^(0:2),"1,000","10,000","100,000"))
hist(log10(d$IF), breaks=25, xlab="JIF",main="",xaxt = "n",xlim=c(-0.3,1.7))
tcks<-c(0.5,1,2.5,5,10,25,50); axis(1,at=log10(tcks), tcks)
hist(log10(d$H5), breaks=30, xlab="JH5",main="",xaxt = "n",xlim=c(1,2.7))
tcks<-c(10,25,50,100,250,500); axis(1,at=log10(tcks), tcks)
hist(log10(d$hindex), breaks=30, xlab="H-index",main="",xaxt = "n",xlim=c(0.6,2.2))
tcks<-c(1,5,10,25,50,100,150); axis(1,at=log10(tcks), tcks)
hist(d$mindex, breaks=30, xlab="M-index",main="",xlim=c(0,10)) #,xaxt = "n")
#axis(1,at=log10(c(0.5,1,2,3,4,5,10)), c(0.5,1,2,3,4,5,10))
hist(d$relAge, breaks=30, xlab="Relative age",main="")
dev.off()


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

##

#transform weights -- higher weights have less overlap with other rankings/benchmarks:
weights<-1000/(weights+1)
library(metap)

sumz(p_values, weights = weights)
#sumz =  0.2372642 p =  0.4062259

meanp(p_values)
#z =  -0.2294146  p =  0.5907266 

library("CombinePValue")
#selfcontained.test(p_values,weight=weights,p_permu=NA)
#competitive.test(Pvalue=p_values,Weight=weights)
#$`significance level for combining pvalues`
#[1] 0.58728

######################################################################
#PLOT WORD SCORES:
wS<-read.table("wordScores.tsv", header=T, row.names=4)

N=20
pdf(file=    "../figures/wordScores.pdf", width = 16,  height = 8)
op<-par(cex=2.0,las=2,mar = c(7,4,4,2) + .1)
barplot(as.numeric( c(head(wS,n=N)[,1], NA, NA, NA, tail(wS,n=N)[,1]) ), names= c( row.names(head(wS,n=N)), '.', '.', '.', row.names(tail(wS,n=N)) ), ylab="word score (bits)",main="head & tail word scores",ylim=c(-10,7))
dev.off()

######################################################################
#All vs All Spearman Heatmap

dNames <- c("IF", "H5", "relAge",       "yearPublished", "cites", "relCites",       "mindex",  "hindex",  "accuracyRank",  "speedRank" )
pNames <- c("JIF", "JH5", "Rel. age", "Year",          "Cites", "Rel. cites", "M-index", "H-index", "Accuracy",      "Speed")
pvalMatrix<-matrix(1, length(dNames), length(dNames))
rhoMatrix <-matrix(0, length(dNames), length(dNames))
sigMatrix <-matrix("",length(dNames), length(dNames))

colnames(pvalMatrix)<-pNames
rownames(pvalMatrix)<-pNames
colnames(rhoMatrix) <-pNames
rownames(rhoMatrix) <-pNames
colnames(sigMatrix) <-pNames
rownames(sigMatrix) <-pNames

for(i in 1:length(dNames)){
      for(j in 1:length(dNames)){
	    spear<-cor.test(d[,dNames[i] == colnames(d)], d[,dNames[j] == colnames(d)], method = "spearman", exact = T)   #, alternative = "less")
	    pvalMatrix[i,j] <- spear$p.value 
	    rhoMatrix[i,j]  <- spear$estimate
	    if(spear$p.value < 0.05){
		sigMatrix[i,j]  <- "X"
	    }
      }
}


pdf(file=    "../figures/spearmanHeatmap.pdf", width = 7,  height = 6)
par(mar = c(8,4,4,4) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(rhoMatrix, cellnote=sigMatrix,notecex=1.5,notecol="black", col=redblue(40), density.info="none", trace="none", dendrogram=c("column"), symm=F,symkey=T,symbreaks=T, scale="none", key.title = "", srtRow=45, adjRow=c(0, 1), srtCol=45, adjCol=c(1,1), breaks=(-20:20)/20,
margins = c(8, 8), cexRow=1.5, cexCol=1.5)
dev.off()

png(file=    "../figures/spearmanHeatmap.png", width = 420,  height = 360)
par(mar = c(8,4,4,4) + .1, cex=0.5) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(rhoMatrix, cellnote=sigMatrix,notecex=1.5,notecol="black", col=redblue(75), density.info="none", trace="none", dendrogram=c("row"), symm=F,symkey=T,symbreaks=T, scale="none", cexRow=0.95, cexCol=0.95, key.title = "Spearman\'s rho") 
dev.off()


relCitesA<-cor.test(1-d$accuracyRank, as.numeric(d$relCites),     method = "spearman")
hindexA  <-cor.test(1-d$accuracyRank, as.numeric(d$hindex),       method = "spearman")
mindexA  <-cor.test(1-d$accuracyRank, as.numeric(d$mindex),       method = "spearman")
H5A      <-cor.test(1-d$accuracyRank, as.numeric(d$H5),           method = "spearman")
relAgeA  <-cor.test(1-d$accuracyRank, as.numeric(d$relAge),       method = "spearman")
speedA   <-cor.test(1-d$accuracyRank, as.numeric(d$speedRank),    method = "spearman")
citesA   <-cor.test(1-d$accuracyRank, as.numeric(d$cites),        method = "spearman")
IFA      <-cor.test(1-d$accuracyRank, as.numeric(d$IF),           method = "spearman")
yearA    <-cor.test(1-d$accuracyRank, as.numeric(d$yearPublished),method = "spearman")

pdf(file=    "../figures/spearmanBarplot.pdf", width = 5,  height = 3)
op<-par(mfrow=c(1,1),cex=1.0,las=2)
barplot(t(c(mindexA$estimate, hindexA$estimate, relAgeA$estimate, H5A$estimate, speedA$estimate, citesA$estimate, relCitesA$estimate, yearA$estimate, IFA$estimate)), names=c("M-index", "H-index", "Rel. age", 'JH5', "Speed", "Cites", "Rel. cites", "Year", "JIF"), ylab="Spearman's rho",ylim=c(-0.1,0.1),main="Correlates with accuracy rank")
lines(c(-100,100),c(0,0))
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#GENERATE PLOTS FOR THE PERMUTATION TESTS!:

accPerms <-read.table("meanRankAccuracyPerms.tsv", header=T)
spdPerms <-read.table(   "meanRankSpeedPerms.tsv", header=T)

gridRes  <- 10; #10 x 10 grid
numPerms <- max(spdPerms$permutation)+1

library(gplots)
h2dNorm <- hist2d(d$accuracyRank,d$speedRank, show=FALSE, same.scale=TRUE, nbins=gridRes)

sumX  <- 0*h2dNorm$counts
sumXX <- 0*h2dNorm$counts

fastInacc <- vector(mode = "numeric", length = numPerms)
fastAcc   <- vector(mode = "numeric", length = numPerms)
slowInacc <- vector(mode = "numeric", length = numPerms)
slowAcc   <- vector(mode = "numeric", length = numPerms)
midBlock  <- vector(mode = "numeric", length = 4*numPerms)

ii <- 1;
for(i in 0:(numPerms-1)){      
      h2dPerm <- hist2d( accPerms$accuracyRank[ accPerms$permutation == i], spdPerms$speedRank[ spdPerms$permutation == i], show=FALSE, same.scale=TRUE, nbins=gridRes)                 
      sumX  <-  h2dPerm$counts                   + sumX;	
      sumXX <- (h2dPerm$counts * h2dPerm$counts) + sumXX;	

      #corners:
      fastInacc[i] <- h2dPerm$counts[gridRes,1]
      fastAcc[i]   <- h2dPerm$counts[1,1]
      slowInacc[i] <- h2dPerm$counts[gridRes,gridRes]
      slowAcc[i]   <- h2dPerm$counts[1,gridRes]
      
      if(gridRes == 10){
      		 midBlock[ii] <- h2dPerm$counts[5,5]
		 ii <- ii + 1
      		 midBlock[ii] <- h2dPerm$counts[6,6]
		 ii <- ii + 1
      		 midBlock[ii] <- h2dPerm$counts[5,6]
		 ii <- ii + 1
      		 midBlock[ii] <- h2dPerm$counts[6,5]
		 ii <- ii + 1
      }
}

stddevPerms <- sqrt( (numPerms * sumXX - (sumX*sumX)) / (numPerms*(numPerms-1)) )
meanPerms   <- sumX/numPerms

zScores    <- 0*h2dNorm$counts
medRelAges <- 0*h2dNorm$counts
medHindex  <- 0*h2dNorm$counts
breaks <- seq(0,1, length=gridRes+1)

for(i in 1:gridRes){
      for(j in 1:gridRes){
      	    zScores[i,j] <- (h2dNorm$counts[i,j] - meanPerms[i,j])/stddevPerms[i,j]
	    
	    medRelAges[i,j] <- median(d$relAge[ breaks[i] <= d$accuracyRank & d$accuracyRank <= breaks[i+1] & breaks[j] <= d$speedRank & d$speedRank <= breaks[j+1]],na.rm=TRUE)
	    medHindex[i,j]  <- median(d$hindex[ breaks[i] <= d$accuracyRank & d$accuracyRank <= breaks[i+1] & breaks[j] <= d$speedRank & d$speedRank <= breaks[j+1]],na.rm=TRUE)
      }
}

pValues<-pnorm(-abs(zScores))
sigMatrix <-matrix("",gridRes, gridRes)

for(i in 1:gridRes){
      for(j in 1:gridRes){
	    if(pValues[i,j] < 0.05){
		sigMatrix[i,j]  <- "X"
	    }
      }
}

rownames(zScores)<-character(gridRes)
colnames(zScores)<-character(gridRes)


pdf(file=    "../figures/zscores-SpeedVsAccuracyH.pdf", width = 8,  height = 10)
par(mfrow=c(3,3),cex=1.4,las=1)
mx <- max(c(slowAcc,fastAcc,slowInacc,fastInacc))
hist(slowAcc,breaks=0:(mx+1)-0.5, main="",xlab="",ylab="Freq.",ylim=c(0,300),xlim=c(0,10),yaxt="n",xaxt="n")#Slow and accurate
arrows(h2dNorm$counts[1,gridRes],200,h2dNorm$counts[1,gridRes],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,5,10))
axis(2,at=(0:3)*100)
plot(1, type="n", axes=F, xlab="", ylab="")
hist(fastAcc,breaks=0:(mx+1)-0.5, main="",xlab="",ylab="Freq.",ylim=c(0,300),xlim=c(0,10),yaxt="n",xaxt="n") #Fast and accurate
arrows(h2dNorm$counts[1,1],200,h2dNorm$counts[1,1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,5,10))
axis(2,at=(0:3)*100)
plot(1, type="n", axes=F, xlab="", ylab="")
      if(gridRes == 10){
      		 hist(midBlock,breaks=(0:(1+max(midBlock)/2))*2-0.5, main="",xlab="",ylab="Freq.",ylim=c(0,1000),xlim=c(0,21),yaxt="n",xaxt="n") #Medial speed & accuracy
		 arrows(h2dNorm$counts[5,5],750,h2dNorm$counts[5,5],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[6,6],750,h2dNorm$counts[6,6],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[5,6],750,h2dNorm$counts[5,6],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[6,5],750,h2dNorm$counts[6,5],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,10,20))
axis(2,at=c(0,500,1000))
      } else {
		plot(1, type="n", axes=F, xlab="", ylab="")
      }
plot(1, type="n", axes=F, xlab="", ylab="")
hist(slowInacc,breaks=0:(mx+1)-0.5, main="",xlab="",ylab="Freq.",ylim=c(0,300),xlim=c(0,10),yaxt="n",xaxt="n") #Slow and inaccurate
arrows(h2dNorm$counts[gridRes,gridRes],200,h2dNorm$counts[gridRes,gridRes],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,5,10))
axis(2,at=(0:3)*100)
plot(1, type="n", axes=F, xlab="", ylab="")
hist(fastInacc,breaks=0:(mx+1)-0.5, main="",xlab="",ylab="Freq.",ylim=c(0,300),xlim=c(0,10),yaxt="n",xaxt="n") #Fast and inaccurate
arrows(h2dNorm$counts[gridRes,1],200,h2dNorm$counts[gridRes,1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,5,10))
axis(2,at=(0:3)*100)
dev.off()

#colScale <- 20 #redblue(colScale), 
colScale <- 11
pdf(file=    "../figures/zscores-SpeedVsAccuracy.pdf", width = 10,  height = 10)
heatmap.2(zScores[,nrow(zScores):1], cellnote=sigMatrix[,nrow(zScores):1],notecex=2.5,notecol="black", col=brewer.pal(n = colScale, name = "RdBu"), density.info="none", trace="none", dendrogram="none", symm=F,symkey=T,symbreaks=T, breaks=seq(-3.5,3.5,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(2, 2), key.title = "Z", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=2.0)
dev.off()

colScale <- 9
pdf(file=    "../figures/relAge-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medRelAges[,nrow(medRelAges):1], col=rev(brewer.pal(n = colScale, name = "YlGnBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "Relative age", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()


colScale <- 9
pdf(file=    "../figures/hindex-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medHindex[,nrow(medHindex):1], col=brewer.pal(n = colScale, name = "BuGn"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medHindex, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "H-index", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()


heatmap.2(medHindex[,nrow(medHindex):1], col=brewer.pal(n = colScale, name = "BuGn"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=T,symbreaks=T, breaks=seq(0,max(medHindex, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "H-index", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))


######################################################################


# pdf(file=    "../figures/Figure1-b.pdf", width = 10,  height = 5)
# op<-par(cex=1.5,las=2)
# barplot(t(c(mindexA$estimate, hindexA$estimate, relAgeA$estimate, H5A$estimate, speedA$estimate, citesA$estimate, IFA$estimate)), names=c("author.M", "author.H", "relative\nage", 'journal.H5', "speed", "#citations", 'journal.IF'), ylab="Spearman's rho",ylim=c(-0.1,0.1),main="Correlates with accuracy rank")
# lines(c(-100,100),c(0,0))
# dev.off()

# pdf(file=    "../figures/Figure1-a.pdf", width = 10,  height = 8)
# heatmap.2(rhoMatrix, col=redblue(75), density.info="none", trace="none", dendrogram=c("row"), symm=T, symkey=T, key.title = "Spearman\'s rho", key.xlab = "rho", symbreaks=T, scale="none", cexRow=1.1, cexCol=1.1) 
# dev.off()

# pdf(file=    "../figures/Figure1-h1.pdf", width = 5,  height = 5)
# par(cex=1.0,mar=c(4, 3, 4, 2) + 0.1)#mar: c(bottom, left, top, right)
# hist(slowInacc,breaks=0:(mx+1)-0.5, main="Slow and inaccurate\nreal vs permuted",xlab="Number of methods")
# arrows(h2dNorm$counts[gridRes,gridRes],150,h2dNorm$counts[gridRes,gridRes],0,col="red",lwd=2)
# box(which = "figure")
# dev.off()

# system('convert -size 1000x1000 xc:white ../figures/Figure1.pdf
# composite -density 150 -geometry 400x400+0+0   ../figures/Figure1-b.pdf ../figures/Figure1.pdf ../figures/Figure1.pdf
# composite -density 150 -geometry 400x600+0+400 ../figures/Figure1-a.pdf ../figures/Figure1.pdf ../figures/Figure1.pdf
# composite -density 150 -geometry 100x100+600+0 ../figures/Figure1-h1.pdf ../figures/Figure1.pdf ../figures/Figure1.pdf')
# #{size}{+-}x{+-}y


######################################################################
#Comparing method relative ages of the slow+inaccurate and fast+accurate groups:

#top/bottom 9 squares
wilcox.test(d$relAge[d$accuracyRank>=0.8 & d$speedRank>=0.8],d$relAge[d$accuracyRank<=0.3 & d$speedRank<=0.3],alternative="l")

b<-boxplot(d$relAge[d$accuracyRank>=0.8 & d$speedRank>=0.8],
        d$relAge[d$accuracyRank<=0.3 & d$speedRank>=0.8],
        d$relAge[d$accuracyRank>=0.8 & d$speedRank<=0.3],
        d$relAge[d$accuracyRank<=0.3 & d$speedRank<=0.3],
	names=c("","","",""),ylab="Relative age",plot=0)

pdf(file=    "../figures/relAge-speedAcc.pdf", width = 5,  height = 6)
op<-par(las=2,cex=1.2,mfrow=c(2,2))
boxplot(d$relAge[d$accuracyRank>=0.8 & d$speedRank>=0.8],
        d$relAge[d$accuracyRank<=0.3 & d$speedRank>=0.8],
        d$relAge[d$accuracyRank>=0.8 & d$speedRank<=0.3],
        d$relAge[d$accuracyRank<=0.3 & d$speedRank<=0.3],
	ylab="Relative age",names=c("slow+\ninaccurate","slow+\naccurate","fast+\ninaccurate","fast+\naccurate"),ylim=c(0,1.1))
text(1:4,0*(1:4)+1.05,paste("(n=",b$n,")",sep=""),col="red",cex=0.9)

boxplot(d$cites[d$accuracyRank>=0.8 & d$speedRank>=0.8],
        d$cites[d$accuracyRank<=0.3 & d$speedRank>=0.8],
        d$cites[d$accuracyRank>=0.8 & d$speedRank<=0.3],
        d$cites[d$accuracyRank<=0.3 & d$speedRank<=0.3],
	ylab="Citations",names=c("slow+\ninaccurate","slow+\naccurate","fast+\ninaccurate","fast+\naccurate"),ylim=c(0,1000))

boxplot(d$IF[d$accuracyRank>=0.8 & d$speedRank>=0.8],
        d$IF[d$accuracyRank<=0.3 & d$speedRank>=0.8],
        d$IF[d$accuracyRank>=0.8 & d$speedRank<=0.3],
        d$IF[d$accuracyRank<=0.3 & d$speedRank<=0.3],
	ylab="JIF",names=c("slow+\ninaccurate","slow+\naccurate","fast+\ninaccurate","fast+\naccurate"),ylim=c(0,16))

boxplot(d$hindex[d$accuracyRank>=0.8 & d$speedRank>=0.8],
        d$hindex[d$accuracyRank<=0.3 & d$speedRank>=0.8],
        d$hindex[d$accuracyRank>=0.8 & d$speedRank<=0.3],
        d$hindex[d$accuracyRank<=0.3 & d$speedRank<=0.3],
	ylab="H-index",names=c("slow+\ninaccurate","slow+\naccurate","fast+\ninaccurate","fast+\naccurate"),ylim=c(0,80))

dev.off()



