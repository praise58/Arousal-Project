
###
### Here is a function to calculate the pvalue using the permutation testing method
### To use it, just run the code and then you can call the function with any other data set
### Below the function I put some sample code on how to use it
###
### NOTE: Alot of the code assumes the groups are the same size and the groups are in the same order (i.e. sub 1 group 1 corresponds to sub 1 group 2)
### But I can change that if thats not true for any future data sets you have
###

permutationPvalue <- function(data, numPerms, groupSize, plot=FALSE, title="", labels=c("Group 1", "Group 2"), enableMC=FALSE, cores=3){	
	gstarRest <- getMLEandLoglike(data[,1:groupSize])$mleTree
	gstarTask <- getMLEandLoglike(data[,(groupSize+1):(groupSize*2)])$mleTree
	gstarDistance <- sqrt(sum((gstarRest-gstarTask)^2))
	
	if(!enableMC){
		permDistances <- NULL
		for(i in 1:numPerms){ 	
			samps <- sample(1:2, groupSize, replace=TRUE)
			samps <- (samps-1)*groupSize + 1:groupSize
			
			gstar1 <- getMLEandLoglike(data[,samps])$mleTree
			gstar2 <- getMLEandLoglike(data[,-samps])$mleTree
			
			permDistances <- c(permDistances, sqrt(sum((gstar1-gstar2)^2)))
		}
	}else{
		require(doParallel)
		cl <- makeCluster(cores) 
		registerDoParallel(cl)
		
		permDistances <- foreach(icount(numPerms), .combine=cbind, .inorder=FALSE, .multicombine=TRUE, .export=c("getMLEandLoglike")) %dopar%{
			samps <- sample(1:2, groupSize, replace=TRUE)
			samps <- (samps-1)*groupSize + 1:groupSize
			
			gstar1 <- getMLEandLoglike(data[,samps])$mleTree
			gstar2 <- getMLEandLoglike(data[,-samps])$mleTree
			
			ret <- sqrt(sum((gstar1-gstar2)^2))
			ret
		}
		stopCluster(cl) 
	}
	
	Pvalue <- sum(permDistances >= gstarDistance)/numPerms 
	
	if(plot){
		data2 <- cbind(data[,1:(groupSize*2)], gstarRest, gstarTask)
		loc <- cmdscale(dist(t(data2)), k=2) 
		plot(loc, xlab="MDS 1", ylab="MDS 2", col=c(rep("red", groupSize), rep("blue", groupSize), "red", "blue"), pch=c(rep(16, groupSize*2), 17, 17), 
				cex=c(rep(1, groupSize*2), 2, 2), main=title)
		
		for(i in 1:groupSize)							
			segments(loc[i,1], loc[i,2], loc[i+groupSize,1], loc[i+groupSize,2], lty=3)
		segments(loc[groupSize*2+1,1], loc[groupSize*2+1,2], loc[groupSize*2+2,1], loc[groupSize*2+2,2], lty=3) 	
		legend("bottomright", labels, pch=16, col=c("red", "blue")) 	
	}
	
	return(Pvalue)
}

### Code to Run the Test
library(HMPTrees)

data <- read.csv("Caterina Data Raw.csv")

numPerms <- 50
groupSize <- 28
numCores <- 3 #The number of parallel instances to run, it is limited by the number of cores your computer has on Windows.

pval <- permutationPvalue(data, numPerms, groupSize, TRUE, "Resting vs Task", c("Resting", "Task"), TRUE, numCores)





######################
### Test for Speed ###
######################
library(HMPTrees)

data <- read.csv("Caterina Data Raw.csv")

numPerms <- 50
groupSize <- 28
numCores <- 3

ptr <- proc.time()
pval <- permutationPvalue(data, numPerms, groupSize, TRUE, "Resting vs Task", c("Resting", "Task"), FALSE)
seqTime <- proc.time() - ptr

ptr <- proc.time()
pval <- permutationPvalue(data, numPerms, groupSize, TRUE, "Resting vs Task", c("Resting", "Task"), TRUE, numCores)
parTime <- proc.time() - ptr

print(paste("Sequential time: ", seqTime[3], "seconds"))
print(paste("Parallel time: ", parTime[3], "seconds"))





