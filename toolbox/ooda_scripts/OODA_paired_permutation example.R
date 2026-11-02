
###
### Here is a function to calculate the pvalue using the permutation testing method
### To use it, just run the code and then you can call the function with any other data set
### Below the function I put some sample code on how to use it
### Below that I put all the code I used to create the plots we saw yesterday and comments on what exactly it's doing
###
### NOTE: Alot of the code assumes the groups are the same size and the groups are in the same order (i.e. sub 1 group 1 corresponds to sub 1 group 2)
### But I can change that if thats not true for any future data sets you have
###

permutationPvalue <- function(data, numPerms, groupSize, plot=FALSE, title="", labels=c("Group 1", "Group 2")){	
	gstarRest <- estGStar(data[,1:groupSize]) 	
	gstarTask <- estGStar(data[,(groupSize+1):(groupSize*2)])
	gstarDistance <- calcDistance(gstarRest, gstarTask) 
	
	permDistances <- NULL
	for(i in 1:numPerms){ 	
		samps <- sample(1:2, groupSize, replace=TRUE)
		samps <- (samps-1)*groupSize + 1:groupSize
		
		gstar1 <- estGStar(data[,samps])
		gstar2 <- estGStar(data[,-samps])
		
		permDistances <- c(permDistances, calcDistance(gstar1, gstar2))
	}
	Pvalue <- sum(permDistances>gstarDistance)/numPerms 
	
	if(plot){
		data2 <- cbind(data[,1:(groupSize*2)], gstarRest, gstarTask)
		loc <- cmdscale(dist(t(data2), method="binary"), k=2) 
		plot(loc, xlab="MDS 1", ylab="MDS 2", col=c(rep("red", groupSize), rep("blue", groupSize), "red", "blue"), pch=c(rep(16, groupSize*2), 17, 17), 
				cex=c(rep(1, groupSize*2), 2, 2), main=title)
		
		for(i in 1:groupSize)							
			segments(loc[i,1], loc[i,2], loc[i+groupSize,1], loc[i+groupSize,2], lty=3)
		segments(loc[groupSize*2+1,1], loc[groupSize*2+1,2], loc[groupSize*2+2,1], loc[groupSize*2+2,2], lty=3) 	
		legend("bottomright", labels, pch=16, col=c("red", "blue")) 	
	}
	
	return(Pvalue)
}

### Sample Code 1
library(bingat)

data(braingraphs)
numPerms <- 100 					#The number of permutations to run
groupSize <- 19						#How big the groups are
plot <- TRUE						#Plot the MDS plot with lines between the same subjects
title <- "My MDS"					#A title for your plot
labels <- c("Group 1", "Group 2")	#Labels for the legend

pval <- permutationPvalue(braingraphs, numPerms, groupSize, plot, title, labels)
pval

### Sample Code 2 - Your Data
library(bingat)

data <- read.csv("Caterina Data.csv") 
pval <- permutationPvalue(data, 100, 28, TRUE, "Resting vs Task", c("Resting", "Task"))
pval



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Below here is all the code I used to create the plots from the meeting and the code to make the data set from the matrices you sent me ~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###
### This calculates the P-Value and saves all the distances from every permutation test
###
library(bingat)

data <- read.csv("Caterina Data.csv") 
data <- apply(data, 2, full2lt) 		#This line changes the data from a full matrix of ~69k values to just the lower triangle (~31k value)
data <- data[,1:56] 					#Since we are only looking at Rest vs Task for now I pull out the first 56 columns (TaskResid are the last 28)

numPerms <- 1000 						#The number of permutations to run
groupSize <- 28							#How big the groups are

gstarRest <- estGStar(data[,1:28]) 		#Here I get the g-stars for both groups
gstarTask <- estGStar(data[,29:56])
gstarDistance <- calcDistance(gstarRest, gstarTask) 	#And then I calculate the distance between them

permDistances <- NULL 					#This will hold the distances for each permutation test
for(i in 1:numPerms){ 					#This will randomly sample the data into 2 groups 'numPerms' times and each time calculate the distance between the 2 groups g-stars
	samps <- sample(1:2, groupSize, replace=TRUE)
	samps <- (samps-1)*groupSize + 1:groupSize
	
	gstar1 <- estGStar(data[,samps])
	gstar2 <- estGStar(data[,-samps])
	
	permDistances <- c(permDistances, calcDistance(gstar1, gstar2))
}

Pvalue <- sum(permDistances>gstarDistance)/numPerms 	#This calculates the P-value from the permutation test
Pvalue



###
### If you want to plot the differences in distances like the plots we showed
###
hist(permDistances, xlim=c(0, max(permDistances*1.5, gstarDistance*1.5)), ylim=c(0, 400), 
		main="Distance Between G-Stars with Permuting", xlab="") 	#Histogram of the distances
abline(v=gstarDistance, col="blue", lwd=3) 							#Adds the gstar line



###
### If you want an MDS plot with the segments connecting dots from the same person
###
data2 <- cbind(data, gstarRest, gstarTask)
loc <- cmdscale(dist(t(data2), method="binary"), k=2) 	#This calculates the distance between every point and creates 2D coordinates for them
plot(loc, xlab="MDS 1", ylab="MDS 2", col=c(rep("red", groupSize), rep("blue", groupSize), "red", "blue"), pch=c(rep(16, groupSize*2), 17, 17), 
		cex=c(rep(1, groupSize*2), 2, 2), main="MDS of Resting and Task")

for(i in 1:groupSize){ 									#This adds the lines for the normal points
	segments(loc[i,1], loc[i,2], loc[i+groupSize,1], loc[i+groupSize,2], lty=3)
}
segments(loc[groupSize*2+1,1], loc[groupSize*2+1,2], loc[groupSize*2+2,1], loc[groupSize*2+2,2], lty=3) 	#This adds the g-star line
legend("bottomright", c("Resting", "Task"), pch=16, col=c("red", "blue")) 	



###
### The 3 group mds plot with lines
###
gstarTaskResid <- estGStar(data[,57:84]) 	#Need the Task Resid gstar

data3 <- cbind(data, gstarRest, gstarTaskResid)
loc <- cmdscale(dist(t(data3), method="binary"), k=2)
plot(loc, xlab="MDS 1", ylab="MDS 2", col=c(rep("red", groupSize), rep("blue", groupSize), rep("green", groupSize), "red", "blue", "green"), 
		pch=c(rep(16, groupSize*3), 17, 17, 17), cex=c(rep(1, groupSize*3), 2, 2, 2), main="MDS of Resting, Task, and TaskResid")

for(i in 1:groupSize){
	segments(loc[i,1], loc[i,2], loc[i+groupSize,1], loc[i+groupSize,2], lty=3)
	segments(loc[i+groupSize,1], loc[i+groupSize,2], loc[i+groupSize*2,1], loc[i+groupSize*2,2], lty=3)
}
segments(loc[groupSize+1,1], loc[groupSize+1,2], loc[groupSize+2,1], loc[groupSize+2,2], lty=3)
segments(loc[groupSize+2,1], loc[groupSize+2,2], loc[groupSize+3,1], loc[groupSize+3,2], lty=3)
legend("bottomright", c("Resting", "Task", "TaskResid"), pch=16, col=c("red", "blue", "green"))



###
### Code I used to convert what you sent me from 82 files into 1
###
setwd("C:/for_ooda") 						#This just navigates to the data you sent me and sets it as the current directory
files <- list.files() 						#This gets all the files in the current directory (which I set in the last line)
files <- grep("mat_", files, value=TRUE) 	#This pulls out just the 82 matrix files

data <- NULL
for(file in files){ 						#Here I'm opening them 1 at a time and combining them into 'data
	temp <- read.csv(file, header=FALSE)
	temp <- as.vector(as.matrix(temp))
	data <- cbind(data, temp)
}

name <- rep(paste("Subject", 1:28), 3) 		#Next I give them some fancy column names so I know which is which
name <- paste(name, rep(c("Rest", "Task", "TaskResid"), each=28))
colnames(data) <- name

write.csv(data, "Caterina Data.csv", row.names=FALSE) 	#This writes the data to a csv file


