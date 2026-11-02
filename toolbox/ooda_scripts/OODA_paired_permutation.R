
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
	Pvalue <- sum(permDistances>=gstarDistance)/numPerms 
	
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
	
	return(list(pvalue=Pvalue,gstarG1=gstarRest,gstarG2=gstarTask))
}