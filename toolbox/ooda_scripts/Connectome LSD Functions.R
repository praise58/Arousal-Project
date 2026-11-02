
findBlocks <- function(gstar1, gstar2, blockCuts, labels=NULL, abs=FALSE, useLSD=TRUE){
	### Get the gstar diff
	gsdiff <- unlist(gstar1 - gstar2)
	if(abs)
		gsdiff <- abs(gsdiff)
	
	### Set up vars
	numNodes <- getNumNodes(gsdiff, "adjmatrixlt")
	blkmat <- matrix(0, numNodes, numNodes)
	blockSplits <- c(blockCuts, numNodes)
	if(is.null(labels))
		labels <- paste("Community", 1:(length(blockCuts)))
	
	labelMat <- outer(labels, labels, "paste", sep=" - ")
	labelVec <- labelMat[lower.tri(labelMat, diag=TRUE)]
	labelVecWOrder <- as.data.frame(cbind(label=labelVec, order=1:length(labelVec)), stringsAsFactors=FALSE)
	labelVecWOrder[,2] <- as.numeric(labelVecWOrder[,2])
	
	### Set up block design
	numBlocks <- length(blockSplits) - 1
	blkNum <- 1
	for(i in 2:length(blockSplits)){ # Go over cols
		for(j in i:length(blockSplits)){ # Go over rows
			upper <- blockSplits[j-1] + 1
			lower <- blockSplits[j]
			left <- blockSplits[i-1] + 1
			right <- blockSplits[i]
			blkmat[upper:lower, left:right] <- blkNum
			
			blkNum <- blkNum + 1
		}
	}
	diag(blkmat) <- 0
	blockByEdge <- full2lt(as.vector(blkmat))
	
	### Lsd method setup
	gdiffByBlock <- as.data.frame(cbind(block=blockByEdge, val=gsdiff))
	rownames(gdiffByBlock) <- 1:nrow(gdiffByBlock)
	gdiffByBlock[,1] <- labelVec[gdiffByBlock[,1]]
	
	### Run lsd method
	cnt <- 0
	newRun <- TRUE
	groupInfo <- NULL
	while(nrow(groupInfo) > 1 || newRun){
		newRun <- FALSE
		
		model <- aov(val~block, data=gdiffByBlock)
		
		if(useLSD){
			lsd <- LSD.test(model, "block", p.adj="bonferroni")		
		}else{
			lsd <- SNK.test(model, "block")							# edge cases are more spread
		}
		
		groupInfo <- lsd$group
		groupInfo[,1] <- trimws(as.vector(groupInfo[,1]))
		groupInfo[,3] <- as.vector(groupInfo[,3])
		
		# Get our differences and order them
		diffs <- abs(diff(groupInfo[,2]))
		diffs <- cbind(1:length(diffs), diffs)
		diffs <- diffs[order(diffs[,2]),]
		
		nothingToMerge <- FALSE
		diffLoc <- 1
		while(TRUE){
			# Check we havent run out of things to try to merge
			if(diffLoc > nrow(diffs)){
				nothingToMerge <- TRUE
				break
			}
			
			minDiff <- diffs[diffLoc, 1]
			
			# Pull out the 2 we want to merge
			id1 <- as.character(groupInfo[minDiff, 1])
			id2 <- as.character(groupInfo[minDiff+1, 1])
			id1split <- strsplit(groupInfo[minDiff, 3], "")[[1]]
			id2split <- strsplit(groupInfo[minDiff+1, 3], "")[[1]]
			
			# See if they are in the same group
			if(any(id1split %in% id2split))
				break
			
			diffLoc <- diffLoc + 1
		}
		
		if(nothingToMerge)
			break
		
		newID <- paste(id1, id2, sep=", ")
		gdiffByBlock[which(gdiffByBlock[,1] == id1 | gdiffByBlock[,1] == id2), 1] <- newID
		
		cnt <- cnt + 1
	}
	
	### Relabel our blocks by their new groupings
	groupIds <- lapply(strsplit(groupInfo[,1], ", "), function(x){
				which(labelVecWOrder[,1] %in% x)
			})
	
	### Get scores for every group
	z <- lmer(val~block + (1|block) - 1, data=gdiffByBlock)
	q <- data.frame(summary(z)$coefficients)
	
	### Estimate p-value from t-value
	q$pv <- 2 * (1 - pnorm(abs(q[,3])))
	qGIorder <- q[order(q[,1], decreasing=TRUE),]
	groupInfo <- cbind(groupInfo, score=qGIorder$pv)
	
	return(list(lsdGroups=groupInfo, blockByEdge=blockByEdge, blockCuts=blockSplits, 
					numBlocks=numBlocks, labels=labels, blockIds=groupIds, abs=abs,
					gsdiff=gsdiff))
}

plotHeatmap <- function(blocksRes, location=NULL, fileName="LSD_Heatmap"){
	# Set up a id to name mapping
	blockByEdge2 <- blocksRes$blockByEdge
	for(i in 1:length(blocksRes$blockIds))
		blockByEdge2[blocksRes$blockByEdge %in% blocksRes$blockIds[[i]]] <-  blocksRes$lsdGroups[i,2]    
	diffMat <- vec2mat(blockByEdge2, "adjmatrixlt")
	diag(diffMat) <- NA
	
	# Make our cut lines be between communities
	cuts <- blocksRes$blockCuts + .5
	
	# Create a dummy vector that correctly aligns the labels
	labLocs <- NULL
	for(i in 1:(length(cuts)-1))
		labLocs[i] <- floor(mean(c(cuts[i+1], cuts[i])))
	labs <- rep("", ncol(diffMat))
	labs[labLocs] <- labels
	
	# Set up color functions
	if(blocksRes$abs){
		colfun <- colorRampPalette(c("white", "white", "yellow", "orange", "red"))						
	}else{
		colfun <- colorRampPalette(c("blue", "green", "white", "white", "white", "orange", "red"))						
	}
	
	# Create a global variable to work with heatmap
	varForHeatMapInternalEvalH <<- ncol(diffMat)-cuts+1
	varForHeatMapInternalEvalV <<- cuts
	
	# Check if we should save the file somewhere specific
	saveOut <- FALSE
	if(!is.null(location))
		saveOut <- TRUE
	
	if(saveOut)
		png(paste(location, fileName, ".png", sep=""))
	# Plot
	heatmap.2(diffMat, dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none", symm=TRUE, 
			main="Blocks Selected by LSD", key.title="Correlation",
			add.expr=abline(h=varForHeatMapInternalEvalH, v=varForHeatMapInternalEvalV), 
			key=TRUE, labRow=labs, labCol=labs, cexRow=1, cexCol=1, key.xlab="",
			col=colfun(300)
	)
	if(saveOut)
		dev.off()
}

plotDensity <- function(blocksRes, location=NULL, fileName="LSD_Density"){
	numBlocks <- length(blocksRes$blockIds)
	
	# Set up a id to name mapping
	blockByEdge3 <- blocksRes$blockByEdge
	for(i in 1:numBlocks)
		blockByEdge3[blocksRes$blockByEdge %in% blocksRes$blockIds[[i]]] <-  i
	
	# Calculate density
	den <- vector("list", numBlocks)
	for(i in 1:numBlocks)
		den[[i]] <- density(blocksRes$gsdiff[which(i==blockByEdge3)])
	
	# Set up color functions
	if(blocksRes$abs){
		colfun <- colorRampPalette(c("red", "orange", "yellow", "grey", "grey"))						
	}else{
		colfun <- colorRampPalette(c("red", "orange", "grey", "grey", "grey", "green", "blue"))	
	}
	cols2 <- colfun(numBlocks)
	
	# Set up the x and y lims
	maxXlim <- max(abs(range(blocksRes$gsdiff)))
	maxXlim <- maxXlim * 1.25
	maxYlim <- max(sapply(den, function(x) {max(x$y)}))
	maxYlim <- maxYlim * 1.25
	
	# Check if we should save the file somewhere specific
	saveOut <- FALSE
	if(!is.null(location))
		saveOut <- TRUE
	
	if(saveOut)
		png(paste(location, fileName, ".png", sep=""))
	
	# Create a blank plot
	plot(1, type="n", xlab="Value", ylab="Density", xlim=c(-maxXlim, maxXlim), ylim=c(0, maxYlim), main="Block Group Densities")
	
	# Add each denisty plot
	for(i in 1:numBlocks)
		lines(den[[i]], col=cols2[i], lwd=3)
	
	# Plot the overall density
	den <- density(blocksRes$gsdiff)
	lines(den, col="black", lwd=5)
	
	if(saveOut)
		dev.off()
}

plotBarchart <- function(blocksRes, location=NULL, fileName="LSD_BarChart"){
	colfun <- colorRampPalette(c("red", "orange", "grey", "grey", "grey", "green", "blue"))	
	
	# Hide the Group Labels
	plotData <- blocksRes$lsdGroups[,1:3]
	plotData[,1] <- ""
	
	# Set up the ylim
	maxYlim <- max(abs(range(plotData[,2])))
	maxYlim <- maxYlim * 1.25
	
	# Check if we should save the file somewhere specific
	saveOut <- FALSE
	if(!is.null(location))
		saveOut <- TRUE
	
	if(saveOut)
		png(paste(location, fileName, ".png", sep=""))
	
	bar.group(plotData, las=1, col=colfun(nrow(plotData)), ylim=c(-maxYlim, maxYlim), 
			ylab="Mean Value", xlab="Block", main="Block Grouping by LSD Method")
	
	if(saveOut)
		dev.off()
}

plotHeatmapBB <- function(blocksRes, reorder=TRUE, location=NULL, fileName="LSD_HeatmapByBlock"){
	# Create a matrix to convert map ids to names
	maxblks <- choose(blocksRes$numBlocks, 2) + blocksRes$numBlocks
	bm <- matrix(0, blocksRes$numBlocks, blocksRes$numBlocks)
	bm[lower.tri(bm, TRUE)] <- 1:maxblks
	bm <- t(bm)
	bm[lower.tri(bm, TRUE)] <- 1:maxblks
	
	# Convert names to ids
	for(i in 1:length(blocksRes$blockIds))
		bm[bm %in% blocksRes$blockIds[[i]]] <-  blocksRes$lsdGroups[i,2]
	
	# Set up color functions
	if(blocksRes$abs){
		colfun <- colorRampPalette(c("white", "white", "yellow", "orange", "red"))						
	}else{
		colfun <- colorRampPalette(c("blue", "green", "white", "white", "white", "orange", "red"))						
	}
	
	# Set heatmap to reorder or not based on arguments
	colv <- reorder
	rowv <- reorder
	dendrogram <- "both"
	if(!reorder)
		dendrogram <- "none"
	
	varForHeatMapInternalEval <<- 0:blocksRes$numBlocks+.5
	
	# Check if we should save the file somewhere specific
	saveOut <- FALSE
	if(!is.null(location))
		saveOut <- TRUE
	
	if(saveOut)
		png(paste(location, fileName, ".png", sep=""))
	# Plot
	heatmap.2(bm, trace="none", symm=TRUE, main="Blocks Selected by LSD",
			add.expr=abline(h=varForHeatMapInternalEval, v=varForHeatMapInternalEval), 
			labRow=labels, labCol=labels, cexRow=1, cexCol=1, 
			key.title="Correlation", key=TRUE, key.xlab="", 
			Colv=colv, Rowv=rowv, dendrogram=dendrogram,
			col=colfun(300)
	)
	if(saveOut)
		dev.off()
}

getMLEandLoglike <- function(data, maxSteps=50, weightCols=NULL, delta=10^(-6), weight=NULL){
	if(missing(data))
		stop("A valid data set is required.")
	
	### Fix for anyone using the old weighting argument
	if(is.null(weightCols) && !is.null(weight))
		weightCols <- weight
	
	numSubs <- ncol(data)
	numEdges <- nrow(data)
	
	### If subject weighting we need to redefine numSubs
	if(!is.null(weightCols)) 
		numSubs <- sum(weightCols)
	
	### Work around for adding mse into applys
	dataTemp <- data
	
	### Generate inital g* from mean
	if(is.null(weightCols)){ 	# No weighting
		gstar <- apply(data, 1, mean)
	}else{ 						# Subject weighting
		gstar <- apply(data, 1, function(x){
					(x%*%weightCols)/sum(weightCols)
				})
	}
	
	### Storage matrix for outputs
	fvals <- matrix(0, maxSteps+1, 4)
	colnames(fvals) <- c("f", "deltaf", "tau", "LL")
	
	### f - Sum of squares of each observed connectome to g*
	calcf <- sum(apply(data, 2, function(x, gs) {sqrt(sum((x - gs)^2))}, gs=gstar))
	deltaf <- 1
	tau <- numSubs/calcf
	logLikBase <- lgamma(numEdges/2) - lgamma(numEdges) - numEdges*log(2) - numEdges/2*log(base::pi)
	logLik <- numSubs*(logLikBase + log(tau)) - numSubs	
	
	count <- 1
	fvals[count,] <- c(calcf, deltaf, tau, logLik)
	while((count < maxSteps) && (deltaf > delta)){ 
		### Get an updated distance to every graph from the g*
		mse <- apply(data, 2, function(x, gs) {sqrt(sum((x - gs)^2))}, gs=gstar)
		dataTemp[nrow(data)+1,] <- mse
		
		### Get the num and den for g* calculation
		numBase <- apply(dataTemp[,mse != 0], 2, function(x) {x[1:length(x)-1]/x[length(x)]})
		denBase <- 1/mse[mse !=0]
		if(is.null(weightCols)){	# No weighting
			num <- rowSums(numBase) 
			den <- sum(denBase)
		}else{ 						# Subject weighting
			num <- numBase %*% weightCols
			den <- as.vector(denBase %*% weightCols)
		}
		
		### Update g* and calcf
		gstar <- num/den
		calcfBase <- apply(data, 2, function(x, gs) {sqrt(sum((x - gs)^2))}, gs=gstar)
		if(is.null(weightCols)){	# No weighting
			calcf <- sum(calcfBase)
		}else{ 												# Subject weighting
			calcf <- as.vector(calcfBase %*% weightCols)
		}
		
		### Recalc new stat values
		deltaf <- abs(fvals[count, 1] - calcf)
		tau <- numSubs/calcf
		logLik <- numSubs*(logLikBase + log(tau)) - numSubs	
		
		### Save our values
		count <- count + 1
		fvals[count,] <- c(calcf, deltaf, tau, logLik)
	}
	
	### Trim our return stat values file before returning
	fvals <- fvals[1:count,]
	
	ret <- list(iters=count, Loglik=logLik, tau=tau, mleTree=as.data.frame(gstar), fvals=fvals)
	return(ret)
}

trimws <- function (x, which = c("both", "left", "right")) 
{
	which <- match.arg(which)
	mysub <- function(re, x) sub(re, "", x, perl = TRUE)
	if (which == "left") 
		return(mysub("^[ \t\r\n]+", x))
	if (which == "right") 
		return(mysub("[ \t\r\n]+$", x))
	mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}

