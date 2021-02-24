#!/usr/bin/Rscript

#Rscript ../bin/prettyPlot.R
#R CMD BATCH ../bin/prettyPlot.R


library(MASS)  # in case it is not already loaded 
library(RColorBrewer)
library(fields)
library(gplots)
#library(metap)
#library("CombinePValue")
library("hash")
library("vioplot")

k <- 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
d2005 <-read.table("meanRankSpeedData.tsv",      header=T)
dr2005<-read.table( "rawRankSpeedData2005-2020.tsv", header=T)

#d2015 <-read.table("meanRankSpeedData2015.tsv",      header=T)
#dr2015<-read.table( "rawRankSpeedData2016-2020.tsv", header=T)



######################################################################
#TESTING IF SPEED inv. ACCURACY IS TRUE:
####Regression:
reg1  <- lm(d2005$accuracyRank ~ d2005$speedRank)
summary(reg1)

regM <- lm(accuracyRank ~ speedRank + H5 + citations + hindex + mindex + relAge + relCites + version + commits + contributors, data=d2005, na.action=na.roughfix)
summary(regM)

####PCA:
xx<-cbind(d2005$accuracyRank, d2005$speedRank, d2005$H5, d2005$citations, d2005$hindex, d2005$mindex, d2005$relAge, d2005$relCites,d2005$yearPublished, d2005$version, d2005$commits, d2005$contributors)
colnames(xx)<-c("accuracy","speed","JH5","citations","hindex","mindex","relAge","relCites","age", "version", "commits", "contributors")
pca<-prcomp(na.omit(xx), center = TRUE,scale=TRUE)
pca
summary(pca)

####Random Forest:
# library(randomForest)

# ind <- sample(2,nrow(d2005),replace=TRUE,prob=c(0.7,0.3))
# trainData <- d2005[ind==1,c(2,3,6:14)]
# testData  <- d2005[ind==2,c(2,3,6:14)]
# acc.rf <- randomForest(accuracyRank ~ ., data=trainData,ntree=1000,proximity=TRUE, na.action=na.roughfix)
# round(importance(acc.rf,type=1), 2)
# par(mfrow=c(1,1))
# predTest <-predict(acc.rf,testData)
# plot( testData$accuracyRank,predTest)
# reg1  <- lm(predTest ~ testData$accuracyRank)
# abline(reg1,col="red")
# corTest<-cor.test( testData$accuracyRank,predTest)
# text(0.05,0.4,paste("R2: ", round(corTest$estimate,2), "\np-value: ", round(corTest$p.value,2), "\nN: ", length(predTest)), col="red", pos=4)

# varImpPlot(acc.rf,type=1)

######################################################################
#FIGURE 1 A & B
#All vs All Spearman Heatmap

dNames <- c("H5", "relAge",    "yearPublished", "citations", "relCites",   "mindex",  "hindex",  "accuracyRank",  "speedRank", "version", "commits", "contributors" )
pNames <- c("JH5", "Rel. age", "Year",          "Citations", "Rel. cites", "M-index", "H-index", "Accuracy",      "Speed", "version", "commits", "contributors")
pvalMatrix<-matrix(1, length(dNames), length(dNames))
rhoMatrix <-matrix(0, length(dNames), length(dNames))
sigMatrix <-matrix("",length(dNames), length(dNames))

colnames(pvalMatrix)    <-pNames
rownames(pvalMatrix)    <-pNames
colnames(rhoMatrix)     <-pNames
rownames(rhoMatrix)     <-pNames
colnames(sigMatrix)     <-pNames
rownames(sigMatrix)     <-pNames
sigCount     <- 0
sigCount2015 <- 0
for(i in 1:length(dNames)){
      for(j in 1:length(dNames)){
	    spear<-cor.test(d2005[,dNames[i] == colnames(d2005)], d2005[,dNames[j] == colnames(d2005)], method = "spearman", exact = T) 
	    pvalMatrix[i,j] <- spear$p.value 
	    rhoMatrix[i,j]  <- spear$estimate
	    if(spear$p.value < 0.05){
		sigMatrix[i,j]  <- "X"
                sigCount <- sigCount + 1
	    }

      }
}

#FIGURE 1A
pdf(file=    "../figures/spearmanHeatmap.pdf", width = 7,  height = 6)
par(mar = c(8,4,4,4) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(rhoMatrix, cellnote=sigMatrix,notecex=1.5,notecol="black", col=rev(redblue(40)), density.info="none", trace="none", dendrogram=c("column"), symm=F,symkey=T,symbreaks=T, scale="none", key.title = "", srtRow=45, adjRow=c(0, 1), srtCol=45, adjCol=c(1,1), breaks=(-20:20)/20,
margins = c(8, 8), cexRow=1.5, cexCol=1.5,font=2)
dev.off()

############
#BARPLOT OF SPEARMAN RHO VALUES:

relCitesA     <-cor.test(d2005$accuracyRank, as.numeric(d2005$relCites),     method = "spearman")
hindexA       <-cor.test(d2005$accuracyRank, as.numeric(d2005$hindex),       method = "spearman")
mindexA       <-cor.test(d2005$accuracyRank, as.numeric(d2005$mindex),       method = "spearman")
H5A           <-cor.test(d2005$accuracyRank, as.numeric(d2005$H5),           method = "spearman")
relAgeA       <-cor.test(d2005$accuracyRank, as.numeric(d2005$relAge),       method = "spearman")
speedA        <-cor.test(d2005$accuracyRank, as.numeric(d2005$speedRank),    method = "spearman")
citesA        <-cor.test(d2005$accuracyRank, as.numeric(d2005$citations),    method = "spearman")
yearA         <-cor.test(d2005$accuracyRank, as.numeric(d2005$yearPublished),method = "spearman")
versionA      <-cor.test(d2005$accuracyRank, as.numeric(d2005$version),      method = "spearman")
commitsA      <-cor.test(d2005$accuracyRank, as.numeric(d2005$commits),      method = "spearman")
contributorsA <-cor.test(d2005$accuracyRank, as.numeric(d2005$contributors), method = "spearman")

spearmansA <- c(mindexA$estimate, hindexA$estimate, speedA$estimate, relAgeA$estimate, H5A$estimate, citesA$estimate, relCitesA$estimate, yearA$estimate, versionA$estimate, commitsA$estimate, contributorsA$estimate)
spearmansAP<- c(mindexA$p.value, hindexA$p.value, speedA$p.value, relAgeA$p.value, H5A$p.value, citesA$p.value, relCitesA$p.value, yearA$p.value, versionA$p.value, commitsA$p.value, contributorsA$p.value)
namesA     <- c("M-index", "H-index", "Speed", "Rel. age", 'JH5', "Citations", "Rel. cites", "Year", "Version", "Commits", "Contributors")
ixA <- sort(spearmansA, index.return=T)$ix

#Figure S4A
pdf(file=    "../figures/spearmanBarplot.pdf", width = 5,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(6,4,4,4) + .1)
bp<-barplot(t(spearmansA[ixA]),names=namesA[ixA], ylab="Spearman's rho",ylim=c(-0.01,0.25),main="Correlates with accuracy")
lines(c(-100,100),c(0,0))
for (i in 1:length(spearmansAP)){
    if( spearmansAP[ixA[i]] < 0.005 ){
    	text(bp[i], spearmansA[ixA[i]], '**', col='red', pos=3 )
    }
    else if( spearmansAP[ixA[i]] < 0.05 ){
    	text(bp[i], spearmansA[ixA[i]], '*', col='red', pos=3 )
    }
}
dev.off()

####
relCitesS<-cor.test(d2005$speedRank, as.numeric(d2005$relCites),     method = "spearman")
hindexS  <-cor.test(d2005$speedRank, as.numeric(d2005$hindex),       method = "spearman")
mindexS  <-cor.test(d2005$speedRank, as.numeric(d2005$mindex),       method = "spearman")
H5S      <-cor.test(d2005$speedRank, as.numeric(d2005$H5),           method = "spearman")
relAgeS  <-cor.test(d2005$speedRank, as.numeric(d2005$relAge),       method = "spearman")
accuracyS<-cor.test(d2005$speedRank, as.numeric(d2005$accuracyRank), method = "spearman")
citesS   <-cor.test(d2005$speedRank, as.numeric(d2005$citations),        method = "spearman")
yearS    <-cor.test(d2005$speedRank, as.numeric(d2005$yearPublished),method = "spearman")
versionS      <-cor.test(d2005$speedRank, as.numeric(d2005$version),      method = "spearman")
commitsS      <-cor.test(d2005$speedRank, as.numeric(d2005$commits),      method = "spearman")
contributorsS <-cor.test(d2005$speedRank, as.numeric(d2005$contributors), method = "spearman")

spearmansS <- c(mindexS$estimate, hindexS$estimate, accuracyS$estimate, relAgeS$estimate, H5S$estimate, citesS$estimate, relCitesS$estimate, yearS$estimate, versionS$estimate, commitsS$estimate, contributorsS$estimate)
namesS     <- c("M-index", "H-index", "Accuracy", "Rel. age", 'JH5', "Citations", "Rel. cites", "Year", "version", "commits", "contributors")
ixS <- sort(spearmansS, index.return=T)$ix


#Figure S4B
pdf(file=    "../figures/spearmanBarplotSpeed.pdf", width = 5,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2)
barplot(t(spearmansS[ixS]), names=namesS[ixS], ylab="Spearman's rho",ylim=c(-0.1,0.1),main="Correlates with speed")
lines(c(-100,100),c(0,0))
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#FIGURE 1B
#GENERATE PLOTS FOR THE PERMUTATION TESTS!:

accPerms <-read.table("meanRankAccuracyPerms.tsv", header=T)
spdPerms <-read.table(   "meanRankSpeedPerms.tsv", header=T)

gridRes  <- 10; #10 x 10 grid
numPerms <- max(accPerms$permutation)+1

#library(gplots)
h2dNorm     <- hist2d(d2005$accuracyRank,d2005$speedRank, show=FALSE, same.scale=TRUE, nbins=gridRes)

sumX  <- 0*h2dNorm$counts
sumXX <- 0*h2dNorm$counts


fastInacc <- vector(mode = "numeric", length = 4*numPerms)
fastAcc   <- vector(mode = "numeric", length = 4*numPerms)
slowInacc <- vector(mode = "numeric", length = 4*numPerms)
slowAcc   <- vector(mode = "numeric", length = 4*numPerms)
midBlock  <- vector(mode = "numeric", length = 4*numPerms)

dNames <- c("mindex",  "hindex", "relAge", "H5", "speedRank", "citations", "relCites", "yearPublished", "version", "commits", "contributors")
estimatesHash   <- hash( dNames, 0*(1:length(dNames))+1 )
estimatesCounts <- hash( dNames, 0*(1:length(dNames))+1 )

#counts
ii <- 1;
for(i in 0:(numPerms-1)){      
      h2dPerm <- hist2d( accPerms$accuracyRank[ accPerms$permutation == i], spdPerms$speedRank[ spdPerms$permutation == i], show=FALSE, same.scale=TRUE, nbins=gridRes)                 
      sumX    <- h2dPerm$counts                   + sumX;	
      sumXX   <- (h2dPerm$counts * h2dPerm$counts) + sumXX;	
      
      
      
      if(gridRes == 10){
      		 #Mid block of 4 squares:
      		 midP<-floor(gridRes/2)
      		 midBlock[ii:(ii+3)] <- c(h2dPerm$counts[midP,midP],h2dPerm$counts[midP+1,midP+1],h2dPerm$counts[midP,midP+1],h2dPerm$counts[midP+1,midP])
		 
		 #Corners:
      		 fastInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,gridRes],      h2dPerm$counts[1+1,gridRes],      h2dPerm$counts[1,gridRes-1],      h2dPerm$counts[1+1,gridRes-1])
      		 fastAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,gridRes],h2dPerm$counts[gridRes-1,gridRes],h2dPerm$counts[gridRes,gridRes-1],h2dPerm$counts[gridRes-1,gridRes-1])
      		 slowInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,1],            h2dPerm$counts[1+1,1],            h2dPerm$counts[1,1+1],            h2dPerm$counts[1+1,1+1])
      		 slowAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,1],      h2dPerm$counts[gridRes-1,1],      h2dPerm$counts[gridRes,1+1],      h2dPerm$counts[gridRes-1,1+1])
		 
		 ii <- ii + 4
	}

      for(fname in dNames){
      	       
      	       permCor      <-cor.test(accPerms$accuracyRank[ accPerms$permutation == i], as.numeric(d2005[,colnames(d2005)==fname]), method = "spearman")	       
      	       if(permCor$p.value < 0.05){
		     estimatesHash[[fname]][estimatesCounts[[fname]]]<-permCor$estimate
		     estimatesCounts[[fname]]<-estimatesCounts[[fname]]+1		     
      	       }
       }

}

######################################################################
#barplot with significant rho vals from permuted data shown:

#FIGURE 1B
pdf(file=    "../figures/spearmanBarplot-withPerms.pdf", width = 5,  height = 5)
xVals <- barplot(t(spearmansA[ixA]), plot=F)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(6,4,4,4) + .1)
barplot(t(spearmansA[ixA]),names=namesA[ixA], ylab="Spearman's rho",ylim=c(-0.25,0.25),main="Correlates with accuracy")
lines(c(-100,100),c(0,0))
for(i in 1:length(dNames)){      
      points(estimatesHash[[dNames[ixA[i]]]]*0+xVals[i],estimatesHash[[dNames[ixA[i]]]],pch="x",cex=1.0,col="skyblue2")
}
for (i in 1:length(ixA)){
    if( spearmansAP[ixA[i]] < 0.005 ){
    	text(xVals[i], 0, '**', col='red', pos=1 )
    }
    else if( spearmansAP[ixA[i]] < 0.05 ){
    	text(xVals[i], 0, '*', col='red', pos=1 )
    }
}
dev.off()

#convert figure1.pdf -background white -flatten  figure1.png

######################################################################


stddevPerms <- sqrt( (numPerms * sumXX - (sumX*sumX)) / (numPerms*(numPerms-1)) )
meanPerms   <- sumX/numPerms

zScores        <- 0*h2dNorm$counts

medRelAges <- 0*h2dNorm$counts
medHindex  <- 0*h2dNorm$counts
medCites   <- 0*h2dNorm$counts
medCommits <- 0*h2dNorm$counts
medContribs<- 0*h2dNorm$counts
breaks <- seq(0,1, length=gridRes+1)

for(i in 1:gridRes){
      for(j in 1:gridRes){
      	    zScores[i,j]     <- (h2dNorm$counts[i,j]     - meanPerms[i,j])/stddevPerms[i,j]
	    
	    medRelAges[i,j] <- median(d2005$relAge[ breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]],na.rm=TRUE)
	    medHindex[i,j]  <- median(d2005$hindex[ breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]],na.rm=TRUE)
	    medCites[i,j]   <- median(  log10(d2005$citations[  breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
	    medCommits[i,j]   <- median(  log10(d2005$commits[  breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
	    medContribs[i,j]   <- median(  log10(d2005$contributors[  breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
      }
}

pValues<-pnorm(-abs(zScores))
sigMatrix <-matrix("",gridRes, gridRes)

sigCount     <- 0
for(i in 1:gridRes){
      for(j in 1:gridRes){
	    if(pValues[i,j] < 0.05){
		sigMatrix[i,j]  <- "X"
                sigCount <- sigCount + 1
	    }

      }
}

rownames(zScores)<-character(gridRes)
colnames(zScores)<-character(gridRes)

binom.test(sigCount,    gridRes^2, p = 0.05)

###################################
#Figure 2: histogram inserts 
pdf(file=    "../figures/zscores-SpeedVsAccuracyH.pdf", width = 8,  height = 10)
par(mfrow=c(3,3),cex=1.4,las=1)
mx <- max(c(slowAcc,fastAcc,slowInacc,fastInacc))
hist(slowAcc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="slo&acc",yaxt="n",xaxt="n")#Slow and accurate 
arrows(h2dNorm$counts[gridRes,  1  ],2000,h2dNorm$counts[gridRes,  1],  0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes-1,1  ],2000,h2dNorm$counts[gridRes-1,1],  0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes,  1+1],2000,h2dNorm$counts[gridRes,  1+1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes-1,1+1],2000,h2dNorm$counts[gridRes-1,1+1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,6,12))
axis(2,at=(0:2)*1000)
plot(1, type="n", axes=F, xlab="", ylab="")
hist(fastAcc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="fast&acc",yaxt="n",xaxt="n") #Fast and accurate 
arrows(h2dNorm$counts[gridRes,  gridRes],  2000,h2dNorm$counts[gridRes,  gridRes],  0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes-1,gridRes],  2000,h2dNorm$counts[gridRes-1,gridRes],  0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes,  gridRes-1],2000,h2dNorm$counts[gridRes,  gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[gridRes-1,gridRes-1],2000,h2dNorm$counts[gridRes-1,gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,6,12))
axis(2,at=(0:2)*1000)
plot(1, type="n", axes=F, xlab="", ylab="")
      if(gridRes == 10){
      		 hist(midBlock,breaks=(0:(1+max(midBlock)/2))*2-0.5, xlab="",ylab="Freq.",xlim=c(0,22),ylim=c(0,800),main="medial",yaxt="n",xaxt="n") #Medial speed & accuracy 
		 arrows(h2dNorm$counts[5,5],7000,h2dNorm$counts[5,5],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[6,6],7000,h2dNorm$counts[6,6],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[5,6],7000,h2dNorm$counts[5,6],0,col="red",lwd=2,length=0.1,angle=30)
		 arrows(h2dNorm$counts[6,5],7000,h2dNorm$counts[6,5],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,10,20))
axis(2,at=c(0,5000,10000))
      } else {
		plot(1, type="n", axes=F, xlab="", ylab="")
      }
plot(1, type="n", axes=F, xlab="", ylab="")
hist(slowInacc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="slo&inacc",yaxt="n",xaxt="n") #Slow and inaccurate 
arrows(h2dNorm$counts[1,    1],2000,h2dNorm$counts[1,    1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1+1,  1],2000,h2dNorm$counts[1+1,  1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1,  1+1],2000,h2dNorm$counts[1,  1+1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1+1,1+1],2000,h2dNorm$counts[1+1,1+1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,6,12))
axis(2,at=(0:2)*1000)
plot(1, type="n", axes=F, xlab="", ylab="")
hist(fastInacc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="fast&inacc",yaxt="n",xaxt="n") #Fast and inaccurate
arrows(h2dNorm$counts[1,  gridRes  ],2000,h2dNorm$counts[1,  gridRes  ],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1+1,gridRes  ],2000,h2dNorm$counts[1+1,gridRes  ],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1,  gridRes-1],2000,h2dNorm$counts[1,  gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
arrows(h2dNorm$counts[1+1,gridRes-1],2000,h2dNorm$counts[1+1,gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
axis(1,at=c(0,6,12))
axis(2,at=(0:2)*1000)
dev.off()


#      		 fastInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,gridRes],      h2dPerm$counts[1+1,gridRes],      h2dPerm$counts[1,gridRes-1],      h2dPerm$counts[1+1,gridRes-1])
#      		 fastAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,gridRes],h2dPerm$counts[gridRes-1,gridRes],h2dPerm$counts[gridRes,gridRes-1],h2dPerm$counts[gridRes-1,gridRes-1])
#      		 slowInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,1],            h2dPerm$counts[1+1,1],            h2dPerm$counts[1,1+1],            h2dPerm$counts[1+1,1+1])
#      		 slowAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,1],      h2dPerm$counts[gridRes-1,1],      h2dPerm$counts[gridRes,1+1],      h2dPerm$counts[gridRes-1,1+1])


###################################
#FIGURE 2
write.table(zScores[nrow(zScores):1,], file = "heatmapZ-scores.tsv", sep = "\t", eol = "\n", row.names = FALSE, col.names = FALSE)

#colScale <- 20 #redblue(colScale), 
colScale <- 11
pdf(file=    "../figures/zscores-SpeedVsAccuracy.pdf", width = 10,  height = 10)
heatmap.2(zScores[nrow(zScores):1,],
          cellnote=sigMatrix[nrow(zScores):1,],notecex=2.5,notecol="black", 
          col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="none", trace="none", dendrogram="none", symm=F,symkey=T,symbreaks=T, breaks=seq(-3.5,3.5,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(2, 2), key.title = "Z", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=2.0)
dev.off()


#convert figure2.pdf -background white -flatten  figure2.png

###################################
#SUPPLEMENTARY HEATMAPS:

colScale <- 9
pdf(file=    "../figures/relAge-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medRelAges[nrow(medRelAges):1,], col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "Relative age", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()


###################################

colScale <- 9
pdf(file=    "../figures/hindex-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medHindex[nrow(medHindex):1,], col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medHindex, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "H-index", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()

###################################

colScale <- 9
pdf(file=    "../figures/cites-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medCites[nrow(medCites):1,], col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medCites, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Citations+1)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()


colScale <- 9
pdf(file=    "../figures/commits-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medCommits[nrow(medCommits):1,], col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medCommits, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Commits)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()

colScale <- 9
pdf(file=    "../figures/contributors-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medContribs[nrow(medContribs):1,], col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medContribs, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Contributors)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()




######################################################################
#DISTRIBUTION PLOTS FOR EACH METRIC, FOR THE SUPPLEMENT:

pdf(file=    "../figures/supplementary-figures-small.pdf", width = 9,  height = 9)
op<-par(mfrow=c(4,3),cex=1.0,las=2)
hist(d2005$accuracyRank, breaks=50, xlab="Accuracy",main="",col="plum3")
hist(d2005$speedRank, breaks=50, xlab="Speed",main="",col="plum3")
hist(log10(d2005$H5), breaks=30, xlab="JH5",main="",xaxt = "n",xlim=c(1,2.7),col="plum3")
tcks<-c(10,25,50,100,250,500); axis(1,at=log10(tcks), tcks)
hist(log10(d2005$citations), breaks=30, xlab="Citations",main="",xaxt = "n",xlim=c(0,5),col="plum3")
axis(1,at=0:5, c(10^(0:2),"1,000","10,000","100,000"))
hist(log10(d2005$hindex), breaks=30, xlab="H-index",main="",xaxt = "n",xlim=c(0.6,2.32),col="plum3")
tcks<-c(5,10,50,100,200); axis(1,at=log10(tcks), tcks)
hist(d2005$mindex, breaks=30, xlab="M-index",main="",xlim=c(0,10),col="plum3") 
hist(d2005$relAge, breaks=30, xlab="Relative age",main="",col="plum3")
hist(d2005$relCites, breaks=30, xlab="Relative number of citations",main="",col="plum3")
hist(d2005$yearPublished, breaks=30, xlab="Year Published",main="",col="plum3")
hist(log10(d2005$version), breaks=30, xlab="Version",main="",col="plum3")
hist(log10(d2005$commits), breaks=30, xlab="Commits",main="",xaxt = "n",col="plum3")
axis(1,at=0:4, c(10^(0:4)))
hist(log10(d2005$contributors), breaks=30, xlab="Contributors", main="",xaxt = "n",col="plum3")
tcks<-c(1,10,25,50,100); axis(1,at=log10(tcks), tcks)
dev.off()

pairMat <- cbind(d2005$accuracyRank,d2005$speedRank,log10(d2005$H5),log10(d2005$citations),log10(d2005$hindex),log10(d2005$mindex),d2005$relAge,d2005$relCites,d2005$yearPublished,log10(d2005$version),log10(d2005$commits),log10(d2005$contributors))
colnames(pairMat)<-c("Accuracy","Speed","log10:\nJH5","log10:\nCitations","log10:\nH-index","log10:\nM-index","relAge","relCites","yearPublished", "log10:\nVersion", "log10:\nCommits", "log10:\nContributors")

pdf(file=    "../figures/supplementary-figures-pairs.pdf", width = 9,  height = 9)
pairs(pairMat, main="Scatter plots of each feature", pch='.', col="plum3")
dev.off()

######################################################################
#Are github repo'ed tools more accurate?:
ks.test(d2005$accuracyRank[is.na(d2005$commits)],d2005$accuracyRank[!is.na(d2005$commits)])

######################################################################
#DISTRIBUTION PLOTS FOR EACH METRIC, FOR THE SUPPLEMENT:


######################################################################
#Comparing method relative ages of the slow+inaccurate and fast+accurate groups:

#top/bottom 9 squares
wilcox.test(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],alternative="g")

b<-boxplot(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
        d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
	d2005$relAge[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
	d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
        d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
	names=c("","","","",""),ylab="Relative age",plot=0)


pdf(file=    "../figures/relAge-speedAcc.pdf", width = 7,  height = 7)
op<-par(las=2,cex=1.8,mfrow=c(1,1),mar=c(5, 6.5, 4, 2) + 0.1, mgp=c(3, 1, 0))                 #‘mar’ A numerical vector of the form ‘c(bottom, left, top, right)’ ::: mgp: margin line for the ax
vioplot(na.omit(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8]),
        na.omit(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2]),
	na.omit(d2005$relAge[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6]),
        na.omit(d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8]),
        na.omit(d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2]),col="khaki1", colMed="black", pchMed='|', wex=0.9,
                                        #ylab="Relative age")# ,
        names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"), horizontal=TRUE,
        lwd=1, xlab="Relative age"
        )
dev.off()


######################################################################
std.error <- function(x){
	  return(sd(x)/sqrt(sum(!is.na(x))))
}

jitterPlot <- function(x, pos, col) {
    xp <-  rnorm(length(x), mean = pos, sd = 0.10)
    points(x,        xp,col=col, bg=col, pch=21)
    q  <-  quantile(x, probs<-(0:4)/4, na.rm=TRUE)
    ########!!!!!! https://www.r-bloggers.com/whisker-of-boxplot/
    upperWhisker <- min(max(x, na.rm=TRUE), q[4] + 1.5 * abs(q[4] - q[1]), na.rm=TRUE ) 
    lowerWhisker <- max(min(x, na.rm=TRUE), q[1] - 1.5 * abs(q[4] - q[1]), na.rm=TRUE )
    q  <-  c( lowerWhisker, q[2], median(x, na.rm=TRUE), q[4], upperWhisker)
    w  <-  c(          0.1, 0.25,      0.35, 0.25,          0.1)
    lwd <- c(            1,     2,        3,    2,            1)+1
    for(i in 1:length(q)){
        lines(c(q[i],q[i]),c(pos-w[i],pos+w[i]),lwd=lwd[i])
    }
    se <- std.error(x)
    mn <- mean(x, na.rm=TRUE)
    q  <-  c( mn-sd(x), mn-se, mn, mn+se, mn+sd(x))
    for(i in 1:length(q)){
        lines(c(q[i],q[i]),c(pos-w[i],pos+w[i]),lwd=lwd[i], col='blue')
    }
    
    lines(c(lowerWhisker,upperWhisker),c(pos,pos),lwd=3)
}

par(cex=2,cex.axis=2,cex.lab=2,cex.main=3,las=1,mfrow=c(1,1),mar=c(5, 14, 4, 2) +0.1,mgp=c(3,2,0), bty='l')
plot(NA,NA, ylim=c(0.0,6.0),xlim=c(0,1.0),xlab="", ylab="", main="", yaxt = "n")
af<-na.omit(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8])
text(0.9,0.6,paste("N=",length(af),sep=''),pos=4,cex=1.5)
jitterPlot(af,1,'pink')
as<-na.omit(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2])
text(0.9,1.6,paste("N=",length(as),sep=''),pos=4,cex=1.5)
jitterPlot(as,2,'pink')
mams<-na.omit(d2005$relAge[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6])
text(0.9,2.6,paste("N=",length(mams),sep=''),pos=4,cex=1.5)
jitterPlot(mams,3,'pink')
ifs<-na.omit(d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8])
text(0.9,3.6,paste("N=",length(ifs),sep=''),pos=4,cex=1.5)
jitterPlot(ifs,4,'pink')
is<-na.omit(d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2])
text(0.9,4.6,paste("N=",length(is),sep=''),pos=4,cex=1.5)
jitterPlot(is,5,'pink')
axis(2,c(1,2,3,4,5), c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"))
mtext(expression(paste("Relative Age")), side = 1, line = 5, cex=3)

######################################################################

#darkseagreen2 olivedrab1 olivedrab2

pdf(file=    "../figures/relAge-cites-if-hindex.pdf", width = 5,  height = 5)
op<-par(las=2,cex=1.8,mfrow=c(2,2),mar=c(5, 3.5, 0, 2) + 0.1, mgp=c(2.5, 1, 0))                 #‘mar’ A numerical vector of the form ‘c(bottom, left, top, right)’ ::: mgp: margin line for the axis title, axis labels and axis line
boxplot(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
        d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
	d2005$relAge[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
        d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
        d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
	ylab="Relative age",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,1.1))
#text(1:4,0*(1:4)+1.05,paste("(n=",b$n,")",sep=""),col="red",cex=0.9)
text(1:5,0*(1:5)+1.05,paste("",b$n,"",sep=""),col="red",cex=0.9)
boxplot(log10(d2005$citations[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8]+1),
        log10(d2005$citations[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8]+1),
	log10(d2005$citations[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6]+1),
        log10(d2005$citations[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2]+1),
        log10(d2005$citations[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2]+1),
	ylab="log10(Citations+1)",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(1,5))
boxplot(d2005$IF[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
        d2005$IF[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
	d2005$IF[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
        d2005$IF[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
        d2005$IF[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
	ylab="JIF",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,16))
boxplot(d2005$hindex[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
        d2005$hindex[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
	d2005$hindex[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
        d2005$hindex[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
        d2005$hindex[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
	ylab="H-index",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,80))
dev.off()

######################################################################
#SMOOTH SCATTER PLOTS:

plotMe <- function(r) {
       
op<-par(mfrow=c(1,1),cex=1.1,las=2)

par(mar = c(5,4,4,5) + .1)
smoothScatter(d2005$speedRank, d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="mean normalised accuracy rank", xlab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Speed") 
lines(lowess(d2005$speedRank, d2005$accuracyRank, f = .2), col = 2, lwd=5)
axis(1,at=(0:5)/5)
axis(2,at=(0:5)/5)
boxit()

}

###################################
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  my.cols <- rev(brewer.pal(k, "RdYlBu"))

  image.plot(xm,ym,z, col = my.cols, legend.only = T, add =F)
}

###################################
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

#REMOVE:
pdf(file=    "../figures/smoothScatter-speed-vs-accuracy.pdf", width = 5.5,  height = 5)
plotMe( )
dev.off()

###################################
#REMOVE:
pdf(file=    "../figures/smoothScatters.pdf", width = 11,  height = 5)
op<-par(mfrow=c(1,2),cex=1.1,las=2,mar = c(5,4,4,5) + .1)
smoothScatter(log10(as.numeric(d2005$IF)), d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="JIF",xlim=c(log10(0.4),log10(54)),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. JIF") 
notNA <- !is.na(d2005$IF)
lines(lowess(log10(d2005$IF[notNA]), d2005$accuracyRank[notNA], f = .2), col = 2, lwd=5)
tcks<-c(0.5,1,2.5,5,10,25,50); axis(1,at=log10(tcks), tcks)
axis(2,at=(0:5)/5)

text(log10(2.576),  1.25, "BMC Bioinf.",    pos=4, srt=90, cex=0.75)
text(log10(4.333),  1.25, "JMB",            pos=4, srt=90, cex=0.75)
text(log10(4.981),  1.25, "Bioinformatics", pos=4, srt=90, cex=0.75)
text(log10(9.112),  1.25, "NAR",            pos=4, srt=90, cex=0.75)
text(log10(14.630), 1.25, "Genome res.",    pos=4, srt=90, cex=0.75)
text(log10(32.072), 1.25, "Nature methods", pos=4, srt=90, cex=0.75)

smoothScatter(log10(as.numeric(d2005$citations)), d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="Citations",xlim=c(0,5),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Citations") 
notNA <- !is.na(d2005$citations)
lines(lowess(log10(d2005$citations[notNA]), d2005$accuracyRank[notNA], f = .2), col = 2, lwd=5)
axis(1,at=0:5, c(1,10,100,"1,000","10,000","100,000"))
axis(2,at=(0:5)/5)

dev.off()


######################################################################
#PLOT WORD SCORES:
wS<-read.table("wordScores.tsv", header=T, row.names=4)

N=40
pdf(file=    "../figures/wordScores.pdf", width = 20,  height = 8)
op<-par(cex=2.0,las=2,mar = c(7,4,4,2) + .1)
barplot(as.numeric( c(head(wS[wS$traingFreq>0 & wS$backgroundFreq>0,],n=N)[,1], NA, NA, NA, tail(wS[wS$traingFreq>0 & wS$backgroundFreq>0,],n=N)[,1]) ), names= c( row.names(head(wS[wS$traingFreq>0 & wS$backgroundFreq>0,],n=N)), '.', '.', '.', row.names(tail(wS[wS$traingFreq>0 & wS$backgroundFreq>0,],n=N)) ), ylab="word score (bits)",main="head & tail word scores",ylim=c(-10,7), cex.names=.75)
dev.off()


######################################################################
#REMOVE:
png(file=    "../figures/citesVsAccuracy.png", width = 1000,  height = 1000)
op<-par(mfrow=c(1,1),cex=2.0,las=2)
plot(log10(d2005$citations),d2005$accuracyRank, pch=20, col="cornflowerblue", xlab="log10(# citations)", ylab="Normalised accuracy", main="Accuracy vs # Citations")
abline(lm(d2005$accuracyRank~log10(d2005$citations)), col="red", lwd=3) # regression line (y~x) 
lines(lowess(log10(d2005$citations),d2005$accuracyRank,delta=0.3), col="blue", lwd=3) # lowess line (x,y)
text(3.75, 0.1, "Spearman\'s rho=0.07, p=0.29")
dev.off()

