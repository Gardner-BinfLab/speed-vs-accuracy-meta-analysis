#!/usr/bin/Rscript

#Rscript ../bin/prettyPlot.R
#R CMD BATCH ../bin/prettyPlot.R

library(MASS) 
library(RColorBrewer)
library(fields)
library(gplots)
#library(metap)
#library("CombinePValue")
library("hash")
library("vioplot")
library("randomForest")
library("WebPower")

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

regM <- lm(accuracyRank ~ speedRank + H5 + citations + hindex + mindex + relAge + relCites + version + commits + contributors + issues + issuesFracOpen + pullrequests + forks, data=d2005, na.action=na.roughfix)
summary(regM)

######################################################################
#FIGURE 1 A & B
#All vs All Spearman Heatmap

dNamesALL <- c("H5", "relAge",    "yearPublished", "citations", "relCites",   "mindex",  "hindex",  "accuracyRank",  "speedRank", "version", "commits", "contributors", "issues", "issuesFracOpen", "pullrequests", "forks" )
pNamesALL <- c("JH5", "Rel. age", "Year",          "Citations", "Rel. cites", "M-index", "H-index", "Accuracy",      "Speed",     "Version", "Commits", "Contributors", "Issues", "%Open issues", "Pull req.", "Forks")
pvalMatrix<-matrix(1, length(dNamesALL), length(dNamesALL))
rhoMatrix <-matrix(0, length(dNamesALL), length(dNamesALL))
sigMatrix <-matrix("",length(dNamesALL), length(dNamesALL))

colnames(pvalMatrix)    <-pNamesALL
rownames(pvalMatrix)    <-pNamesALL
colnames(rhoMatrix)     <-pNamesALL
rownames(rhoMatrix)     <-pNamesALL
colnames(sigMatrix)     <-pNamesALL
rownames(sigMatrix)     <-pNamesALL
sigCount     <- 0
sigCount2015 <- 0
N <- length(dNamesALL)
for(i in 1:N){
      for(j in 1:N){
	    spear<-cor.test(d2005[,dNamesALL[i] == colnames(d2005)], d2005[,dNamesALL[j] == colnames(d2005)], method = "spearman", exact = T) 
	    pvalMatrix[i,j] <- p.adjust(spear$p.value,"BH", n=N*(N-1)/2-N)
	    rhoMatrix[i,j]  <- spear$estimate
	    #print(paste(c("P:", round(spear$p.value,3), "P.adj:", round(p.adjust(spear$p.value,"BH", n=N*(N-1)/2-N),3)), sep=""))
	    if(p.adjust(spear$p.value,"BH", n=N*(N-1)/2-N) <= 0.05){ #spear$p.value < 0.05){#n is N choose 2, less the main diagonal (no double counting)
		sigMatrix[i,j]  <- "X"
                sigCount <- sigCount + 1
	    }
      }
}

#FIGURE 1A
pdf(file=    "../figures/spearmanHeatmap.pdf", width = 9,  height = 8)
par(mar = c(8,4,4,4) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(rhoMatrix, cellnote=sigMatrix,notecex=1.5,notecol="black", col=rev(redblue(40)), density.info="none", trace="none", dendrogram=c("column"), symm=F,symkey=T,symbreaks=T, scale="none", key.title = "", srtRow=45, adjRow=c(0, 1), srtCol=45, adjCol=c(1,1), breaks=(-20:20)/20,
margins = c(8, 8), cexRow=1.5, cexCol=1.5,font=2)
dev.off()

############
#BARPLOT OF SPEARMAN RHO VALUES:

relCitesA       <-cor.test(d2005$accuracyRank, as.numeric(d2005$relCites),       method = "spearman")
hindexA         <-cor.test(d2005$accuracyRank, as.numeric(d2005$hindex),         method = "spearman")
mindexA         <-cor.test(d2005$accuracyRank, as.numeric(d2005$mindex),         method = "spearman")
H5A             <-cor.test(d2005$accuracyRank, as.numeric(d2005$H5),             method = "spearman")
relAgeA         <-cor.test(d2005$accuracyRank, as.numeric(d2005$relAge),         method = "spearman")
speedA          <-cor.test(d2005$accuracyRank, as.numeric(d2005$speedRank),      method = "spearman")
citesA          <-cor.test(d2005$accuracyRank, as.numeric(d2005$citations),      method = "spearman")
yearA           <-cor.test(d2005$accuracyRank, as.numeric(d2005$yearPublished),  method = "spearman")
versionA        <-cor.test(d2005$accuracyRank, as.numeric(d2005$version),        method = "spearman")
commitsA        <-cor.test(d2005$accuracyRank, as.numeric(d2005$commits),        method = "spearman")
contributorsA   <-cor.test(d2005$accuracyRank, as.numeric(d2005$contributors),   method = "spearman")
issuesA         <-cor.test(d2005$accuracyRank, as.numeric(d2005$issues),         method = "spearman")
issuesFracOpenA <-cor.test(d2005$accuracyRank, as.numeric(d2005$issuesFracOpen), method = "spearman")
pullrequestsA   <-cor.test(d2005$accuracyRank, as.numeric(d2005$pullrequests),   method = "spearman")
forksA          <-cor.test(d2005$accuracyRank, as.numeric(d2005$forks),          method = "spearman")

spearmansA <- c(mindexA$estimate, hindexA$estimate, speedA$estimate, relAgeA$estimate, H5A$estimate, citesA$estimate, relCitesA$estimate, yearA$estimate, versionA$estimate, commitsA$estimate, contributorsA$estimate, issuesA$estimate, issuesFracOpenA$estimate, pullrequestsA$estimate, forksA$estimate)
spearmansAP<- c( mindexA$p.value,  hindexA$p.value,  speedA$p.value,  relAgeA$p.value,  H5A$p.value,  citesA$p.value,  relCitesA$p.value,  yearA$p.value,  versionA$p.value,  commitsA$p.value,  contributorsA$p.value,  issuesA$p.value,  issuesFracOpenA$p.value,  pullrequestsA$p.value,  forksA$p.value)
namesA     <- c(       "M-index",        "H-index",         "Speed",       "Rel. age",        "JH5",     "Citations",       "Rel. cites",         "Year",         "Version",         "Commits",         "Contributors",        "#Issues",           "%Open issues",            "Pull reqs",         "Forks")
dNamesA    <- c(       "mindex",          "hindex",     "speedRank",         "relAge",         "H5",     "citations",         "relCites","yearPublished",         "version",         "commits",         "contributors",         "issues",         "issuesFracOpen",         "pullrequests",         "forks")
pNamesA    <- c(       "M-index",        "H-index",         "Speed",       "Rel. age",        "JH5",     "Citations",       "Rel. cites",         "Year",         "Version",         "Commits",         "Contributors",        "#Issues",           "%Open issues",            "Pull reqs",         "Forks")

names(spearmansA ) <-  namesA
names(spearmansAP) <-  namesA
#Re-order on Rho:
ixA <- sort(spearmansA, index.return=T)$ix
spearmansA <- spearmansA[ixA]
spearmansAP<- spearmansAP[ixA]
namesA     <-  namesA[ixA]
dNamesA    <- dNamesA[ixA]
pNamesA    <- pNamesA[ixA]

#Print correlations and P-values mentioned in the text: "version number, and numbers of contributors, commits, forks and issues"  
print(spearmansA )
print(spearmansAP)



#Figure S4A
pdf(file=    "../figures/spearmanBarplot.pdf", width = 5,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(7,4,4,4) + .1)
bp<-barplot(t(spearmansA),names=namesA, ylab="Spearman's rho",ylim=c(-0.2,0.3),main="Correlates with accuracy")
lines(c(-100,100),c(0,0))
for (i in 1:length(spearmansAP)){
    pos <- 3
    if(spearmansA[i] < 0){
       pos <- 1
    }
    
    if( spearmansAP[i] < 0.005 ){
    	text(bp[i], spearmansA[i], '**', col='red', pos=pos )
    }
    else if( spearmansAP[i] < 0.05 ){
    	text(bp[i], spearmansA[i], '*', col='red', pos=pos )
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
contributorsS   <-cor.test(d2005$speedRank, as.numeric(d2005$contributors),   method = "spearman")
issuesS         <-cor.test(d2005$speedRank, as.numeric(d2005$issues),         method = "spearman")
issuesFracOpenS <-cor.test(d2005$speedRank, as.numeric(d2005$issuesFracOpen), method = "spearman")
pullrequestsS   <-cor.test(d2005$speedRank, as.numeric(d2005$pullrequests),   method = "spearman")
forksS          <-cor.test(d2005$speedRank, as.numeric(d2005$forks),          method = "spearman")

spearmansS <- c(mindexS$estimate, hindexS$estimate, accuracyS$estimate, relAgeS$estimate, H5S$estimate, citesS$estimate, relCitesS$estimate, yearS$estimate, versionS$estimate, commitsS$estimate, contributorsS$estimate, issuesS$estimate, issuesFracOpenS$estimate, pullrequestsS$estimate, forksS$estimate)
spearmansSP<- c( mindexS$p.value,  hindexS$p.value,  accuracyS$p.value,  relAgeS$p.value,  H5S$p.value,  citesS$p.value,  relCitesS$p.value,  yearS$p.value,  versionS$p.value,  commitsS$p.value,  contributorsS$p.value,  issuesS$p.value,  issuesFracOpenS$p.value,  pullrequestsS$p.value,  forksS$p.value)
namesS     <- c(       "M-index",        "H-index",         "Accuracy",       "Rel. age",         "JH5",     "Citations",       "Rel. cites",         "Year",         "Version",         "Commits",         "Contributors",        "#Issues",           "%Open issues",            "Pull reqs",         "Forks")
names(spearmansS ) <-  namesS
names(spearmansSP) <-  namesS
ixS <- sort(spearmansS, index.return=T)$ix
spearmansS <- spearmansS[ixS]
spearmansSP<- spearmansSP[ixS]
namesS     <- namesS[ixS]

#Figure S4B
pdf(file=    "../figures/spearmanBarplotSpeed.pdf", width = 5,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(7,4,4,4) + .1)
barplot(t(spearmansS), names=namesS, ylab="Spearman's rho",ylim=c(-0.1,0.1),main="Correlates with speed")
lines(c(-100,100),c(0,0))
for (i in 1:length(spearmansSP)){
    pos <- 3
    if(spearmansS[i] < 0){
       pos <- 1
    }
    
    if( spearmansSP[i] < 0.005 ){
    	text(bp[i], spearmansS[i], '**', col='red', pos=pos )
    }
    else if( spearmansSP[i] < 0.05 ){
    	text(bp[i], spearmansS[i], '*', col='red', pos=pos )
    }
}
dev.off()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#GENERATE PLOTS FOR THE PERMUTATION TESTS!:

accPerms <-read.table("meanRankAccuracyPerms.tsv", header=T)
spdPerms <-read.table(   "meanRankSpeedPerms.tsv", header=T)

#Figure 2
gridRes  <- 3; #10 x 10 or 5 x 5 or 3 x 3 grid
numPerms <- max(accPerms$permutation)+1

#library(gplots)
h2dNorm     <- hist2d(d2005$accuracyRank,d2005$speedRank, show=FALSE, same.scale=TRUE, nbins=gridRes)

sumX  <- 0*h2dNorm$counts
sumXX <- 0*h2dNorm$counts

if(gridRes == 10){
     multiplier <- 4
}else if(gridRes == 5){
     multiplier <- 1    
}else if(gridRes == 3){
     multiplier <- 1
}    
fastInacc <- vector(mode = "numeric", length = multiplier*numPerms)
fastAcc   <- vector(mode = "numeric", length = multiplier*numPerms)
slowInacc <- vector(mode = "numeric", length = multiplier*numPerms)
slowAcc   <- vector(mode = "numeric", length = multiplier*numPerms)
midBlock  <- vector(mode = "numeric", length = multiplier*numPerms)


estimatesHash   <- hash( dNamesA, 0*(1:length(dNamesA))+1 )
estimatesCounts <- hash( dNamesA, 0*(1:length(dNamesA))+1 )

#Very slow code block, fills matrices with data from permutation derived values: 
rhoAccMatrix <-matrix(0, numPerms, length(dNamesA))
colnames(rhoAccMatrix) <- pNamesA   #namesA
#counts
ii <- 1;
for(i in 0:(numPerms-1)){      
      h2dPerm <- hist2d( accPerms$accuracyRank[ accPerms$permutation == i], spdPerms$speedRank[ spdPerms$permutation == i], show=FALSE, same.scale=TRUE, nbins=gridRes)                 
      #Fast variance calculation:
      sumX    <- h2dPerm$counts                   + sumX;	
      sumXX   <- (h2dPerm$counts * h2dPerm$counts) + sumXX;	
      
      #for gridRes  <- 10;
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
	}else if(gridRes == 5){
	     	 midP<-3
		 #for gridRes  <- 3;
		 midBlock[ii:(ii+3)] <- c(h2dPerm$counts[midP,midP])
		 #Corners:
      		 fastInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,gridRes])
      		 fastAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,gridRes])
      		 slowInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,1])
      		 slowAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,1])
		 ii <- ii + 1
	}else if(gridRes == 3){
	     	 midP<-2
		 #for gridRes  <- 3;
		 midBlock[ii:(ii+3)] <- c(h2dPerm$counts[midP,midP])
		 #Corners:
      		 fastInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,gridRes])
      		 fastAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,gridRes])
      		 slowInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,1])
      		 slowAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,1])
		 ii <- ii + 1
        }

	j <- 0
      for(fname in dNamesA){
      	      j <- j+1 
      	       permCor      <-cor.test(accPerms$accuracyRank[ accPerms$permutation == i], as.numeric(d2005[,colnames(d2005)==fname]), method = "spearman")
	       rhoAccMatrix[i,j] <- permCor$estimate
      	       if(permCor$p.value < 0.05){
		     estimatesHash[[fname]][estimatesCounts[[fname]]]<-permCor$estimate
		     estimatesCounts[[fname]]<-estimatesCounts[[fname]]+1		     
      	       }
       }
}

######################################################################

#FIGURE 1B: empirical P-values
#empirical P-values ( p = r/n):
spearmansA.emp.pvals <-spearmansA/spearmansA #initialise with 1's 

for (i in 1:length(spearmansA)){
    if(length(rhoAccMatrix[ rhoAccMatrix[,i]>spearmansA[i]  ,i]) < numPerms/2){
       spearmansA.emp.pvals[i] <-   length(rhoAccMatrix[ rhoAccMatrix[,i]>spearmansA[i]  ,i]) / numPerms
    }
    else {
       spearmansA.emp.pvals[i] <-   length(rhoAccMatrix[ rhoAccMatrix[,i]<spearmansA[i]  ,i]) / numPerms
    }
}
#correct for multiple testing:
spearmansA.emp.pvals <- p.adjust(spearmansA.emp.pvals,"BH")




#FIGURE 1B
pdf(file=    "../figures/spearmanBarplot-withPerms-violin.pdf", width = 10,  height = 10)
op<-par(mfrow=c(1,1),cex=1.5,las=2, mar = c(7,4,4,4) + .1)
vioplot(rhoAccMatrix,ylim=c(-0.25,0.25), col="wheat",ylab="Spearman's rho", main="Correlation with accuracy")
points(1:length(dNamesA), spearmansA, col="red", pch="*", cex=2)
for (i in 1:length(spearmansA)){       
    if(spearmansA.emp.pvals[i] <= 0.05){
        points(i, spearmansA[i], col="red", pch=10, cex=2)
    }
}
dev.off()

#convert figure1.pdf -background white -flatten  figure1.png

######################################################################


stddevPerms <- sqrt( (numPerms * sumXX - (sumX*sumX)) / (numPerms*(numPerms-1)) )
meanPerms   <- sumX/numPerms

zScores    <- 0*h2dNorm$counts
medRelAges <- 0*h2dNorm$counts
medHindex  <- 0*h2dNorm$counts
medCites   <- 0*h2dNorm$counts
medCommits <- 0*h2dNorm$counts
medContribs<- 0*h2dNorm$counts
breaks <- seq(0,1, length=gridRes+1)

for(i in 1:gridRes){
      for(j in 1:gridRes){
      	    zScores[i,j]     <- (h2dNorm$counts[i,j]     - meanPerms[i,j])/stddevPerms[i,j]
	    
	    medRelAges[i,j]   <- median(d2005$relAge[                breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]],   na.rm=TRUE)
	    medHindex[i,j]    <- median(d2005$hindex[                breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]],   na.rm=TRUE)
	    medCites[i,j]     <- median(  log10(d2005$citations[     breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
	    medCommits[i,j]   <- median(  log10(d2005$commits[       breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
	    medContribs[i,j]  <- median(  log10(d2005$contributors[  breaks[i] <= d2005$accuracyRank & d2005$accuracyRank <= breaks[i+1] & breaks[j] <= d2005$speedRank & d2005$speedRank <= breaks[j+1]]+1),na.rm=TRUE)
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

#For example, there was an excess of ``slow and inaccurate'' software
#Z=
print(zScores[1,1])
#P=
pnorm(zScores[1,1], mean = 0, sd = 1, lower.tail = F)

binom.test(sigCount,    gridRes^2, p = 0.05)

###################################
#FIGURE 2A
write.table(zScores[nrow(zScores):1,], file = "heatmapZ-scores.tsv", sep = "\t", eol = "\n", row.names = FALSE, col.names = FALSE)

#colScale <- 20 #redblue(colScale), 
colScale <- 11
pdf(file=    "../figures/zscores-SpeedVsAccuracy-gridRes3.pdf", width = 10,  height = 10)
heatmap.2(zScores[nrow(zScores):1,],
          cellnote=sigMatrix[nrow(zScores):1,],notecex=2.5,notecol="black", 
          col=rev(brewer.pal(n = colScale, name = "RdBu")), density.info="none", trace="none", dendrogram="none", symm=F,symkey=T,symbreaks=T, breaks=seq(-3.5,3.5,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(2, 2), key.title = "Z", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=2.0)
dev.off()

#FIGURE 2B
pdf(file=    "../figures/zscores-withPerms-violin.pdf", width = 5,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(7,4,1,1) + .1)
vioplot(
	(midBlock -mean(midBlock ))/sd(midBlock),
	(fastInacc-mean(fastInacc))/sd(fastInacc),
	(fastAcc  -mean(fastAcc  ))/sd(fastAcc),
	(slowAcc  -mean(slowAcc  ))/sd(slowAcc),
	(slowInacc-mean(slowInacc))/sd(slowInacc),
	ylim=c(-6.5,6.5), col="wheat",ylab="Z-score", main="",
        names=c("Med. Speed+\nMed. Accuracy","Fast+\nInaccurate","Fast+\nAccurate","Slow+\nAccurate","Slow+\nInaccurate"),
        lwd=1)
	if(gridRes == 3){
		   points(1:5, c(zScores[midP,midP],zScores[1,gridRes],zScores[gridRes,gridRes],zScores[gridRes,1] , zScores[1,1]), col="red", pch="*", cex=3)
	}
dev.off()

#convert figure2.pdf -background white -flatten  figure2.png

###################################
#SUPPLEMENTARY HEATMAPS:

colScale <- 9
pdf(file=    "../figures/relAge-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
heatmap.2(medRelAges[nrow(medRelAges):1,], col=rev(brewer.pal(n = colScale, name = "YlGnBu")), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "Relative age", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
dev.off()

######################################################################
#DISTRIBUTION PLOTS FOR EACH METRIC, FOR THE SUPPLEMENT:

pdf(file=    "../figures/supplementary-figures-small.pdf", width = 9,  height = 9)
op<-par(mfrow=c(4,4),cex=1.0,las=2)
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
hist(d2005$relCites, breaks=30, xlab="Relative #citations",main="",col="plum3")
hist(d2005$yearPublished, breaks=30, xlab="Year Published",main="",col="plum3")
hist(d2005$version, breaks=30, xlab="Version",main="",col="plum3")
hist(log10(d2005$commits), breaks=30, xlab="Commits",main="",xaxt = "n",col="plum3")
axis(1,at=0:4, c(10^(0:4)))
hist(log10(d2005$contributors), breaks=30, xlab="Contributors", main="",xaxt = "n",col="plum3")
tcks<-c(1,10,25,50,100); axis(1,at=log10(tcks), tcks)
#, "issues", "issuesFracOpen", "pullrequests", "forks"
hist(log10(d2005$issues), breaks=30, xlab="#Issues",main="",col="plum3",xaxt = "n")
axis(1,at=0:3, c(10^(0:3)))
hist(d2005$issuesFracOpen, breaks=30, xlab="%Open Issues",main="",col="plum3")
hist(log10(d2005$pullrequests), breaks=30, xlab="#Pull Requests",main="",col="plum3",xaxt = "n")
axis(1,at=0:3, c(10^(0:3)))
hist(log10(d2005$forks),        breaks=30, xlab="#Forks",        main="",col="plum3",xaxt = "n")
axis(1,at=0:3, c(10^(0:3)))
dev.off()

pdf(file=    "../figures/supplementary-distributions-permuted.pdf", width = 9,  height = 4.5)
op<-par(mfrow=c(1,2),cex=1.0,mar = c(5, 5, 4, 2) + 0.1, las=2) 
hist(accPerms$accuracyRank, breaks=50, xlab="Permuted normalised accuracy ranks",main="",col="plum3",ylab="")
par(las=0)
mtext("Frequency", side=2, line = 4)
par(las=2)
hist(   spdPerms$speedRank, breaks=50, xlab="Permuted normalised speed ranks",   main="",col="plum3",ylab="")
par(las=0)
mtext("Frequency", side=2, line = 4)
dev.off()

pairMat <- cbind(d2005$accuracyRank,d2005$speedRank,log10(d2005$H5),log10(d2005$citations),log10(d2005$hindex),log10(d2005$mindex),d2005$relAge,d2005$relCites,d2005$yearPublished,d2005$version,log10(d2005$commits),log10(d2005$contributors),
log10(d2005$issues), d2005$issuesFracOpen, log10(d2005$pullrequests), log10(d2005$forks))
colnames(pairMat)<-c("Accuracy","Speed","log10:\nJH5","log10:\nCitations","log10:\nH-index","log10:\nM-index","Rel. \nAge","Rel. \nCites","Year \nPublished", "Version", "log10:\nCommits", "log10:\nContributors",
"log10:\nIssues", "% Open \nIssues", "log10:\nPull \nRequests", "log10:\nForks")

pdf(file=    "../figures/supplementary-figures-pairs.pdf", width = 9,  height = 9)
pairs(pairMat, main="Scatter plots of each feature", pch='.', col="plum3")
dev.off()

######################################################################
#Are github repo'ed tools more accurate?:
ks.test(d2005$accuracyRank[is.na(d2005$commits)],d2005$accuracyRank[!is.na(d2005$commits)])


######################################################################
#Comparing method relative ages of the slow+inaccurate and fast+accurate groups:

pdf(file=    "../figures/relAge-speedAcc.pdf", width = 7,  height = 7)
op<-par(las=2,cex=1.8,mfrow=c(1,1),mar=c(5, 6.5, 4, 2) + 0.1, mgp=c(3, 1, 0))                 #‘mar’ A numerical vector of the form ‘c(bottom, left, top, right)’ ::: mgp: margin line for the ax
delta<- 1/gridRes
vioplot(na.omit(d2005$relAge[d2005$accuracyRank>=(1-delta) & d2005$speedRank>=(1-delta)]),
        na.omit(d2005$relAge[d2005$accuracyRank>=(1-delta) & d2005$speedRank<=delta]),
	na.omit(d2005$relAge[(0.5-delta/2)<=d2005$accuracyRank & d2005$accuracyRank<=(0.5+delta/2) & (0.5-delta/2)<=d2005$speedRank & d2005$speedRank<=(0.5+delta/2)]),
        na.omit(d2005$relAge[d2005$accuracyRank<=delta & d2005$speedRank>=(1-delta)]),
        na.omit(d2005$relAge[d2005$accuracyRank<=delta & d2005$speedRank<=delta]),col="khaki1", colMed="black", pchMed='|', wex=0.9,
                                        #ylab="Relative age")# ,
        names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"), horizontal=TRUE,
        lwd=1, xlab="Relative age"
        )
dev.off()

#fast + accurate vs slow + inaccurate
wilcox.test(d2005$relAge[d2005$accuracyRank>=(1-delta) & d2005$speedRank>=(1-delta)],d2005$relAge[d2005$accuracyRank<=(delta) & d2005$speedRank<=(delta)],alternative="g")
#fast + accurate vs everything else
wilcox.test(d2005$relAge[d2005$accuracyRank>=(1-delta) & d2005$speedRank>=(1-delta)],d2005$relAge[d2005$accuracyRank<(1-delta) & d2005$speedRank<(1-delta)],alternative="g")


##############################
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
#    se <- std.error(x)
#    mn <- mean(x, na.rm=TRUE)
#    q  <-  c( mn-sd(x), mn-se, mn, mn+se, mn+sd(x))
#    for(i in 1:length(q)){
#        lines(c(q[i],q[i]),c(pos-w[i],pos+w[i]),lwd=lwd[i], col='blue')
#    }
    
    lines(c(lowerWhisker,upperWhisker),c(pos,pos),lwd=3)
}

pdf(file=    "../figures/relAge-speedAcc-jitterPlot.pdf", width = 15,  height = 15)
par(cex=2,cex.axis=2,cex.lab=2,cex.main=3,las=1,mfrow=c(1,1),mar=c(6, 14, 4, 2) +0.1,mgp=c(3,2,0), bty='l')
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
dev.off()

wilcox.test(af,is,alternative="g")

######################################################################
#REVIEWER REQUESTS:
######################################################################

#Reviewer/Referee 1 
#"it's not clear to me that this study is large enough"

k <- 6
my.cols.p <- rev(brewer.pal(k, "RdYlBu"))

pdf(file=    "../figures/powerCurves.pdf", width = 20,  height = 10)
op<-par(cex=1.25,las=1)
plot(NA,NA,xlim=c(0,1000),ylim=c(0,1),xlab="Sample Size", ylab="Power", main="Power curves, alpha=0.05, correlation coefficient")
for(i in 0:5){
      r<-i/10
      #WebPower:
      res <- wp.correlation(n=seq(20,1000,10),r=r, alternative="two.sided")
      lines(res$n,res$p, lwd=2, col=my.cols.p[i+1])
      points(res$n,res$p, pch='x', col=my.cols.p[i+1])
}
numSamples <- c(499, 464, 461, 424, 402, 234, 232, 206)
for( i in 1:length(numSamples)){
     lines(c(numSamples[i],numSamples[i]), c(0, 1))
}
legend(700, 0.4, paste("r = ", (0:5)/10), fil=my.cols.p)
lines(c(0,1000),c(0.8,0.8),lty=2)
dev.off()

#####################
#Reviewer/Referee 2 requested plots:
#"plot each software package (potentially sized by the number of studies on which a software tool was included)"
pdf(file=    "../figures/numberBenchmarksPerToolBarplot.pdf", width = 100,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(12,4,4,4) + .1)
bp<-barplot(d2005$numTests,names=d2005$method, ylab="Number of benchmarks",ylim=c(0,26),main="Number of benchmarks per software tool")
dev.off()

#Presumably the data are not fully complete (i.e., some scholar profiles couldn't be identified, etc). A table within either the methods or results breaking out the completeness of the records would be helpful.

naFeatureCounts<-matrix(0, length(dNamesALL), 2)
for(i in 1:length(dNamesALL)){      
      naFeatureCounts[i,2] <- sum(is.na(d2005[,dNamesALL[i] == colnames(d2005)]))
      naFeatureCounts[i,1] <- length(d2005[,dNamesALL[i] == colnames(d2005)]) - naFeatureCounts[i,2]      
}
rownames(naFeatureCounts) <- pNamesALL
colnames(naFeatureCounts) <- c("Known", "NA")

ixF <- sort(naFeatureCounts[,1], index.return=T, decreasing=T)$ix

pdf(file=    "../figures/numberRealValueFeaturesBarplot.pdf", width = 10,  height = 5)
op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(7,4,4,4) + .1)
barplot(t(naFeatureCounts[ixF,]), col=c("plum3", "plum1"), xlab = "", ylab = "Count", ylim=c(0,500), xlim = c(0,length(dNamesALL)+6), width = 1, main="Data completeness")
legend("bottomright", 
 legend = c("Real val.", "NA"),
 fill = c("plum3", "plum1"),
 title = "Data")
for(i in 1:length(dNamesALL)){      
      text( i*(length(dNamesALL)+3.25)/length(dNamesALL)-0.5 , 60, paste("N=",naFeatureCounts[ixF[i],1]), srt=90 )
}
dev.off()

#Reviewer 3 requested plots:
#"What does the correlation between commits and citations imply?  Given the correlation between commits and accuracy, how might we interpret the lack of the transitive correlation between citations and accuracy?"

######################################################################



pdf(file=    "../figures/selected-scatter-plots.pdf", width = 10,  height = 15)
op<-par(mfrow=c(3,2),cex=1.5,las=2)
plot(d2005$yearPublished,log10(d2005$citations+1),yaxt = "n",xlab="Year Published", ylab="Number of Citations", pch=20, col="red", xlim=c(1980,2020))
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$citations+1) ~ d2005$yearPublished, data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.yr.cor <- cor.test(d2005$yearPublished, d2005$citations, method = "spearman")
text(1980, 0.25, paste("Spearman's Rho: ", signif(cit.yr.cor$estimate, digits=3), "\nP.value: ", signif(cit.yr.cor$p.value, digits=3)),pos=4)
#############
plot(d2005$yearPublished,log10(d2005$commits+1),yaxt = "n", ylab="Number of Commits", xlab="Year Published", pch=20, col="red", xlim=c(1980,2020))
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$commits+1) ~ d2005$yearPublished, data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.com.cor <- cor.test(d2005$yearPublished, d2005$commits, method = "spearman")
text(1980, 0.5, paste("Spearman's Rho: ", signif(cit.com.cor$estimate, digits=3), "\nP.value: ", signif(cit.com.cor$p.value, digits=3)),pos=4)
#############
plot(log10(d2005$citations+1),log10(d2005$commits+1),xaxt = "n",yaxt = "n", ylab="Number of Commits", xlab="Number of Citations", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$commits+1) ~ log10(d2005$citations+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.com.cor <- cor.test(d2005$citations, d2005$commits, method = "spearman")
text(0.01, 0.5, paste("Spearman's Rho: ", signif(cit.com.cor$estimate, digits=3), "\nP.value: ", signif(cit.com.cor$p.value, digits=3)),pos=4)
#############
plot(NA)
#############
plot(d2005$accuracyRank,log10(d2005$citations+1),yaxt = "n",xlab="Accuracy", ylab="Number of Citations", pch=20, col="red")
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$citations+1) ~ d2005$accuracyRank, data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.yr.cor <- cor.test(d2005$accuracyRank, d2005$citations, method = "spearman")
text(0.01, 0.25, paste("Spearman's Rho: ", signif(cit.yr.cor$estimate, digits=3), "\nP.value: ", signif(cit.yr.cor$p.value, digits=3)),pos=4)
##############
plot(d2005$accuracyRank,log10(d2005$commits+1),yaxt = "n", ylab="Number of Commits", xlab="Accuracy", pch=20, col="red")
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$commits+1) ~ d2005$accuracyRank, data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.com.cor <- cor.test(d2005$accuracyRank, d2005$commits, method = "spearman")
text(0.01, 0.5, paste("Spearman's Rho: ", signif(cit.com.cor$estimate, digits=3), "\nP.value: ", signif(cit.com.cor$p.value, digits=3)),pos=4)
dev.off()







######################################################################

pdf(file=    "../figures/selected-scatter-plots2.pdf", width = 20,  height = 20)
op<-par(mfrow=c(3,3),cex=1.5,las=2)
#####1. cites vs year
plot(log10(d2005$citations+1),d2005$year,xaxt = "n",ylab="Year", xlab="Number of Citations", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(d2005$year ~ log10(d2005$citations+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.yr.cor <- cor.test(d2005$citations, d2005$year, method = "spearman")
text(1.5, 1930, paste("Spearman's Rho: ", signif(cit.yr.cor$estimate, digits=3), "\nP.value: ", signif(cit.yr.cor$p.value, digits=3)),pos=4)
#####2. cites vs forks
plot(log10(d2005$citations+1),log10(d2005$forks+1),xaxt = "n",yaxt = "n", ylab="Number of Forks", xlab="Number of Citations", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
axis(2,at=c(0,log10(2), 1:3), c(0, 10^(0:3)))
abline(lm(log10(d2005$forks+1) ~ log10(d2005$citations+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.fks.cor <- cor.test(d2005$citations, d2005$forks, method = "spearman")
text(1.5, 0.25, paste("Spearman's Rho: ", signif(cit.fks.cor$estimate, digits=3), "\nP.value: ", signif(cit.fks.cor$p.value, digits=3)),pos=4)
#####3. cites vs commits
plot(log10(d2005$citations+1),log10(d2005$commits+1),xaxt = "n",yaxt = "n", ylab="Number of Commits", xlab="Number of Citations", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
axis(2,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(log10(d2005$commits+1) ~ log10(d2005$citations+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
cit.com.cor <- cor.test(d2005$citations, d2005$commits, method = "spearman")
text(1.5, 0.5, paste("Spearman's Rho: ", signif(cit.com.cor$estimate, digits=3), "\nP.value: ", signif(cit.com.cor$p.value, digits=3)),pos=4)
#####
#####4. issues vs accuracy
plot(log10(d2005$issues+1),d2005$accuracyRank,xaxt = "n",ylab="Accuracy", xlab="Number of issues", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(d2005$accuracyRank ~ log10(d2005$issues+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.iss.cor <- cor.test(d2005$issues,d2005$accuracyRank, method = "spearman")
text(1.25, 0.05, paste("Spearman's Rho: ", signif(acc.iss.cor$estimate, digits=3), "\nP.value: ", signif(acc.iss.cor$p.value, digits=3)),pos=4)
#####5. forks vs accuracy
plot(log10(d2005$forks+1),d2005$accuracyRank,xaxt = "n",ylab="Accuracy", xlab="Number of forks", pch=20, col="red",xlim=c(0,3))
axis(1,at=c(0,log10(2), 1:3), c(0, 10^(0:3)))
abline(lm(d2005$accuracyRank ~ log10(d2005$forks+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.fks.cor <- cor.test(d2005$forks,d2005$accuracyRank, method = "spearman")
text(1.0, 0.05, paste("Spearman's Rho: ", signif(acc.fks.cor$estimate, digits=3), "\nP.value: ", signif(acc.fks.cor$p.value, digits=3)),pos=4)
#####6. PRs vs accuracy
plot(log10(d2005$pullrequests+1),d2005$accuracyRank,xaxt = "n",ylab="Accuracy", xlab="Number of pull requests", pch=20, col="red",xlim=c(0,3))
axis(1,at=c(0,log10(2), 1:3), c(0, 10^(0:3)))
abline(lm(d2005$accuracyRank ~ log10(d2005$pullrequests+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.prs.cor <- cor.test(d2005$pullrequests,d2005$accuracyRank, method = "spearman")
text(1.0, 0.05, paste("Spearman's Rho: ", signif(acc.prs.cor$estimate, digits=3), "\nP.value: ", signif(acc.prs.cor$p.value, digits=3)),pos=4)
#####
#####7. commits vs accuracy
plot(log10(d2005$commits+1),d2005$accuracyRank,xaxt = "n",ylab="Accuracy", xlab="Number of commits", pch=20, col="red",xlim=c(0,4))
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(d2005$accuracyRank ~ log10(d2005$commits+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.com.cor <- cor.test(d2005$commits,d2005$accuracyRank, method = "spearman")
text(1.5, 0.05, paste("Spearman's Rho: ", signif(acc.com.cor$estimate, digits=3), "\nP.value: ", signif(acc.com.cor$p.value, digits=3)),pos=4)
#####8. version vs accuracy
plot(log10(d2005$citations+1),d2005$accuracyRank,xaxt = "n",ylab="Accuracy", xlab="Number of citations", pch=20, col="red")
axis(1,at=c(0,log10(2), 1:4), c(0, 10^(0:4)))
abline(lm(d2005$accuracyRank ~ log10(d2005$citations+1), data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.cit.cor <- cor.test(d2005$citations,d2005$accuracyRank, method = "spearman")
text(1.5, 0.05, paste("Spearman's Rho: ", signif(acc.cit.cor$estimate, digits=3), "\nP.value: ", signif(acc.cit.cor$p.value, digits=3)),pos=4)
#####9. speed vs accuracy
plot(d2005$speedRank,d2005$accuracyRank,ylab="Accuracy", xlab="Speed", pch=20, col="red")
abline(lm(d2005$accuracyRank ~ d2005$speedRank, data=d2005, na.action=na.roughfix), lwd=2, col="red" )  #, na.action=na.omit
acc.spd.cor <- cor.test(d2005$speedRank,d2005$accuracyRank, method = "spearman")
text(0.25, 0.05, paste("Spearman's Rho: ", signif(acc.spd.cor$estimate, digits=3), "\nP.value: ", signif(acc.spd.cor$p.value, digits=3)),pos=4)
dev.off()








######################################################################

#darkseagreen2 olivedrab1 olivedrab2

# pdf(file=    "../figures/relAge-cites-if-hindex.pdf", width = 5,  height = 5)
# op<-par(las=2,cex=1.8,mfrow=c(2,2),mar=c(5, 3.5, 0, 2) + 0.1, mgp=c(2.5, 1, 0))                 #‘mar’ A numerical vector of the form ‘c(bottom, left, top, right)’ ::: mgp: margin line for the axis title, axis labels and axis line
# boxplot(d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
#         d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
# 	d2005$relAge[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
#         d2005$relAge[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
#         d2005$relAge[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
# 	ylab="Relative age",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,1.1))
# #text(1:4,0*(1:4)+1.05,paste("(n=",b$n,")",sep=""),col="red",cex=0.9)
# text(1:5,0*(1:5)+1.05,paste("",b$n,"",sep=""),col="red",cex=0.9)
# boxplot(log10(d2005$citations[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8]+1),
#         log10(d2005$citations[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8]+1),
# 	log10(d2005$citations[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6]+1),
#         log10(d2005$citations[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2]+1),
#         log10(d2005$citations[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2]+1),
# 	ylab="log10(Citations+1)",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(1,5))
# boxplot(d2005$IF[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
#         d2005$IF[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
# 	d2005$IF[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
#         d2005$IF[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
#         d2005$IF[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
# 	ylab="JIF",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,16))
# boxplot(d2005$hindex[d2005$accuracyRank>=0.8 & d2005$speedRank>=0.8],
#         d2005$hindex[d2005$accuracyRank<=0.2 & d2005$speedRank>=0.8],
# 	d2005$hindex[0.4<=d2005$accuracyRank & d2005$accuracyRank<=0.6 & 0.4<=d2005$speedRank & d2005$speedRank<=0.6],
#         d2005$hindex[d2005$accuracyRank>=0.8 & d2005$speedRank<=0.2],
#         d2005$hindex[d2005$accuracyRank<=0.2 & d2005$speedRank<=0.2],
# 	ylab="H-index",names=c("fast+\naccurate","slow+\naccurate","med.speed+\nmed.accuracy","fast+\ninaccurate","slow+\ninaccurate"),ylim=c(0,80))
# dev.off()

######################################################################
#SMOOTH SCATTER PLOTS:

# plotMe <- function(r) {
       
# op<-par(mfrow=c(1,1),cex=1.1,las=2)

# par(mar = c(5,4,4,5) + .1)
# smoothScatter(d2005$speedRank, d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="mean normalised accuracy rank", xlab="mean normalised speed rank",xlim=c(1.2,-0.2),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Speed") 
# lines(lowess(d2005$speedRank, d2005$accuracyRank, f = .2), col = 2, lwd=5)
# axis(1,at=(0:5)/5)
# axis(2,at=(0:5)/5)
# boxit()

# }

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
# pdf(file=    "../figures/smoothScatter-speed-vs-accuracy.pdf", width = 5.5,  height = 5)
# plotMe( )
# dev.off()

###################################
#REMOVE:
# pdf(file=    "../figures/smoothScatters.pdf", width = 11,  height = 5)
# op<-par(mfrow=c(1,2),cex=1.1,las=2,mar = c(5,4,4,5) + .1)
# smoothScatter(log10(as.numeric(d2005$IF)), d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="JIF",xlim=c(log10(0.4),log10(54)),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. JIF") 
# notNA <- !is.na(d2005$IF)
# lines(lowess(log10(d2005$IF[notNA]), d2005$accuracyRank[notNA], f = .2), col = 2, lwd=5)
# tcks<-c(0.5,1,2.5,5,10,25,50); axis(1,at=log10(tcks), tcks)
# axis(2,at=(0:5)/5)

# text(log10(2.576),  1.25, "BMC Bioinf.",    pos=4, srt=90, cex=0.75)
# text(log10(4.333),  1.25, "JMB",            pos=4, srt=90, cex=0.75)
# text(log10(4.981),  1.25, "Bioinformatics", pos=4, srt=90, cex=0.75)
# text(log10(9.112),  1.25, "NAR",            pos=4, srt=90, cex=0.75)
# text(log10(14.630), 1.25, "Genome res.",    pos=4, srt=90, cex=0.75)
# text(log10(32.072), 1.25, "Nature methods", pos=4, srt=90, cex=0.75)

# smoothScatter(log10(as.numeric(d2005$citations)), d2005$accuracyRank, nbin=1000, nrpoints=0, colramp=colorRampPalette(my.cols), postPlotHook = fudgeit, pch=19, cex=.85, ylab="Accuracy", xlab="Citations",xlim=c(0,5),ylim=c(1.2,-0.2), xaxt = "n", yaxt = "n",main="Accuracy vs. Citations") 
# notNA <- !is.na(d2005$citations)
# lines(lowess(log10(d2005$citations[notNA]), d2005$accuracyRank[notNA], f = .2), col = 2, lwd=5)
# axis(1,at=0:5, c(1,10,100,"1,000","10,000","100,000"))
# axis(2,at=(0:5)/5)

# dev.off()


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
# png(file=    "../figures/citesVsAccuracy.png", width = 1000,  height = 1000)
# op<-par(mfrow=c(1,1),cex=2.0,las=2)
# plot(log10(d2005$citations),d2005$accuracyRank, pch=20, col="cornflowerblue", xlab="log10(# citations)", ylab="Normalised accuracy", main="Accuracy vs # Citations")
# abline(lm(d2005$accuracyRank~log10(d2005$citations)), col="red", lwd=3) # regression line (y~x) 
# lines(lowess(log10(d2005$citations),d2005$accuracyRank,delta=0.3), col="blue", lwd=3) # lowess line (x,y)
# text(3.75, 0.1, "Spearman\'s rho=0.07, p=0.29")
# dev.off()

####Random Forest:
# library(randomForest)
# ind <- sample(2,nrow(d2005),replace=TRUE,prob=c(0.7,0.3))
# trainData <- d2005[ind==1,c(2,3,6:19)]
# testData  <- d2005[ind==2,c(2,3,6:19)]
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

####PCA:
# xx<-cbind(d2005$accuracyRank, d2005$speedRank, d2005$H5, d2005$citations, d2005$hindex, d2005$mindex, d2005$relAge, d2005$relCites,d2005$yearPublished, d2005$version, d2005$commits, d2005$contributors, d2005$issues, d2005$issuesFracOpen, d2005$pullrequests, d2005$forks)
# colnames(xx)<-c("accuracy","speed","JH5","citations","hindex","mindex","relAge","relCites","age", "version", "commits", "contributors", "issues", "issuesFracOpen", "pullrequests", "forks")
# pca<-prcomp(na.omit(xx), center = TRUE,scale=TRUE)
# pca
# summary(pca)

#barplot with significant rho vals from permuted data shown:

# pdf(file=    "../figures/spearmanBarplot-withPerms.pdf", width = 5,  height = 5)
# xVals <- barplot(t(spearmansA[ixA]), plot=F)
# op<-par(mfrow=c(1,1),cex=1.0,las=2, mar = c(6,4,4,4) + .1)
# barplot(t(spearmansA[ixA]),names=namesA[ixA], ylab="Spearman's rho",ylim=c(-0.25,0.25),main="Correlates with accuracy")
# lines(c(-100,100),c(0,0))
# for(i in 1:length(dNames)){      
#       points(estimatesHash[[dNames[ixA[i]]]]*0+xVals[i],estimatesHash[[dNames[ixA[i]]]],pch="x",cex=1.0,col="skyblue2")
# }
# for (i in 1:length(ixA)){
#     if( spearmansAP[ixA[i]] < 0.005 ){
#     	text(xVals[i], 0, '**', col='red', pos=1 )
#     }
#     else if( spearmansAP[ixA[i]] < 0.05 ){
#     	text(xVals[i], 0, '*', col='red', pos=1 )
#     }
# }
# dev.off()

###################################

# colScale <- 9
# pdf(file=    "../figures/hindex-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
# par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
# heatmap.2(medHindex[nrow(medHindex):1,], col=brewer.pal(n = colScale, name = "BuGn"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medHindex, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "H-index", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
# dev.off()


# colScale <- 9
# pdf(file=    "../figures/cites-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
# par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
# heatmap.2(medCites[nrow(medCites):1,], col=brewer.pal(n = colScale, name = "BuPu"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medCites, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Citations+1)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
# dev.off()

# colScale <- 9
# pdf(file=    "../figures/commits-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
# par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
# heatmap.2(medCommits[nrow(medCommits):1,], col=brewer.pal(n = colScale, name = "BuPu"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medCommits, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Commits+1)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
# dev.off()

# colScale <- 9
# pdf(file=    "../figures/contributors-SpeedVsAccuracy-heatmap.pdf", width = 10,  height = 10)
# par(mar = c(8,4,4,8) + .1) #c(bottom, left, top, right). default: c(5, 4, 4, 2) + 0.1
# heatmap.2(medContribs[nrow(medContribs):1,], col=brewer.pal(n = colScale, name = "BuPu"), density.info="histogram", trace="none", dendrogram="none", symm=F,symkey=F,symbreaks=T, breaks=seq(0,max(medContribs, na.rm = TRUE)+1,length=colScale+1), scale="none", cexRow=1.5, cexCol=1.5, margins = c(8, 8), key.title = "log10(Contributors+1)", Colv=FALSE, Rowv=FALSE, xlab="Speed",ylab="Accuracy", cex=1.0, na.rm=TRUE,na.color=par("bg"))
# dev.off()

######################################################################
#histogram inserts gridRes <- 10
# pdf(file=    "../figures/zscores-SpeedVsAccuracyH.pdf", width = 8,  height = 10)
# par(mfrow=c(3,3),cex=1.4,las=1)
# mx <- max(c(slowAcc,fastAcc,slowInacc,fastInacc))
# hist(slowAcc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="slo&acc",yaxt="n",xaxt="n")#Slow and accurate 
# arrows(h2dNorm$counts[gridRes,  1  ],2000,h2dNorm$counts[gridRes,  1],  0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes-1,1  ],2000,h2dNorm$counts[gridRes-1,1],  0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes,  1+1],2000,h2dNorm$counts[gridRes,  1+1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes-1,1+1],2000,h2dNorm$counts[gridRes-1,1+1],0,col="red",lwd=2,length=0.1,angle=30)
# axis(1,at=c(0,6,12))
# axis(2,at=(0:2)*1000)
# plot(1, type="n", axes=F, xlab="", ylab="")
# hist(fastAcc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="fast&acc",yaxt="n",xaxt="n") #Fast and accurate 
# arrows(h2dNorm$counts[gridRes,  gridRes],  2000,h2dNorm$counts[gridRes,  gridRes],  0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes-1,gridRes],  2000,h2dNorm$counts[gridRes-1,gridRes],  0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes,  gridRes-1],2000,h2dNorm$counts[gridRes,  gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[gridRes-1,gridRes-1],2000,h2dNorm$counts[gridRes-1,gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
# axis(1,at=c(0,6,12))
# axis(2,at=(0:2)*1000)
# plot(1, type="n", axes=F, xlab="", ylab="")
#       if(gridRes == 10){
#       		 hist(midBlock,breaks=(0:(1+max(midBlock)/2))*2-0.5, xlab="",ylab="Freq.",xlim=c(0,22),ylim=c(0,800),main="medial",yaxt="n",xaxt="n") #Medial speed & accuracy 
# 		 arrows(h2dNorm$counts[5,5],7000,h2dNorm$counts[5,5],0,col="red",lwd=2,length=0.1,angle=30)
# 		 arrows(h2dNorm$counts[6,6],7000,h2dNorm$counts[6,6],0,col="red",lwd=2,length=0.1,angle=30)
# 		 arrows(h2dNorm$counts[5,6],7000,h2dNorm$counts[5,6],0,col="red",lwd=2,length=0.1,angle=30)
# 		 arrows(h2dNorm$counts[6,5],7000,h2dNorm$counts[6,5],0,col="red",lwd=2,length=0.1,angle=30)
# axis(1,at=c(0,10,20))
# axis(2,at=c(0,5000,10000))
#       } else {
# 		plot(1, type="n", axes=F, xlab="", ylab="")
#       }
# plot(1, type="n", axes=F, xlab="", ylab="")
# hist(slowInacc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="slo&inacc",yaxt="n",xaxt="n") #Slow and inaccurate 
# arrows(h2dNorm$counts[1,    1],2000,h2dNorm$counts[1,    1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1+1,  1],2000,h2dNorm$counts[1+1,  1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1,  1+1],2000,h2dNorm$counts[1,  1+1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1+1,1+1],2000,h2dNorm$counts[1+1,1+1],0,col="red",lwd=2,length=0.1,angle=30)
# axis(1,at=c(0,6,12))
# axis(2,at=(0:2)*1000)
# plot(1, type="n", axes=F, xlab="", ylab="")
# hist(fastInacc,breaks=0:(mx+1)-0.5, xlab="",ylab="Freq.",xlim=c(0,15),ylim=c(0,800),main="fast&inacc",yaxt="n",xaxt="n") #Fast and inaccurate
# arrows(h2dNorm$counts[1,  gridRes  ],2000,h2dNorm$counts[1,  gridRes  ],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1+1,gridRes  ],2000,h2dNorm$counts[1+1,gridRes  ],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1,  gridRes-1],2000,h2dNorm$counts[1,  gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
# arrows(h2dNorm$counts[1+1,gridRes-1],2000,h2dNorm$counts[1+1,gridRes-1],0,col="red",lwd=2,length=0.1,angle=30)
# axis(1,at=c(0,6,12))
# axis(2,at=(0:2)*1000)
# dev.off()

##############################

#      		 fastInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,gridRes],      h2dPerm$counts[1+1,gridRes],      h2dPerm$counts[1,gridRes-1],      h2dPerm$counts[1+1,gridRes-1])
#      		 fastAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,gridRes],h2dPerm$counts[gridRes-1,gridRes],h2dPerm$counts[gridRes,gridRes-1],h2dPerm$counts[gridRes-1,gridRes-1])
#      		 slowInacc[ii:(ii+3)] <- c(h2dPerm$counts[1,1],            h2dPerm$counts[1+1,1],            h2dPerm$counts[1,1+1],            h2dPerm$counts[1+1,1+1])
#      		 slowAcc[ii:(ii+3)]   <- c(h2dPerm$counts[gridRes,1],      h2dPerm$counts[gridRes-1,1],      h2dPerm$counts[gridRes,1+1],      h2dPerm$counts[gridRes-1,1+1])

##############################

