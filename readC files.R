#readCfiles
readC <- function(filename, numChar = 4, escapement = TRUE) {
	#Read in the file by line
		rawC <- readLines(filename)
	#Line 1 contains the tag code
		tagcode <- rawC[1]
	#Line 2 contains the brood year
		by <- as.numeric(rawC[2])
		by <- ifelse(by >= 50, by+1900, by+2000)
	#Line 3 contains the number of fish tagged
		numTagg <- as.numeric(rawC[3])
	#Line 4 contains the number of fish released
		numFish <- as.numeric(rawC[4])
	#Line 5 contains max age
		maxAge <- as.numeric(rawC[5])
	#Line 6 contains the number of fisheries
		numFisheries <- as.numeric(rawC[6])
	#Lines 7 to numFish have the fishery names
		nameFisheries <- as.vector(rawC[7:(6+numFisheries)])
	#Last lines of the file contain the number of recoveries by age and fishery
		numAges <- length(rawC)-(6+numFisheries)
	#Create a blank matrix to contain the data
		recByFish <- matrix(ncol=numAges, nrow=numFisheries)
		colnames(recByFish) <- paste("age",2:(numAges+1),sep="")
		rownames(recByFish) <- nameFisheries
		for(i in 1:numAges) {
			recov <- rawC[i+6+numFisheries]
			for(j in 1:numFisheries) {
				hold <- substr(recov, start=((j-1)*numChar+1),stop=(j*numChar))
				#additional error checking...
				recByFish[j,i] <- as.numeric(hold)
			}
		}
	#If escapement is set to be true, then the last numChar's in the row are 'escapement'
	#NOTE: I may want to test to see if numFisheries*numChar < nchar(recov), and if it is, read in the last line as escapement...
		if(escapement==TRUE) {
			escapByAge <- matrix(ncol=numAges, nrow=1)
			for(i in 1:numAges) {
				recov <- rawC[i+6+numFisheries]
				hold <- substr(recov, start=nchar(recov)-numChar+1,stop=nchar(recov))
				escapByAge[1,i] <- as.numeric(hold)
			}
			rownames(escapByAge) <- "Escapement"
			colnames(escapByAge) <- paste("age",2:(numAges+1),sep="")
		} else escapByAge = NULL
	#Return output 
		list(TagCode=tagcode, BroodYear=by, FishTagged = numTagg, FishReleased = numFish, AgeMax = maxAge, nFisheries = numFisheries, RecoveriesByFishery = recByFish, Escapement = escapByAge)
}

##################
## READ IN LAST YEAR DATA ##
##################
#Stock Acronym
	StockAcronym <- "AKS" #<- change this
#phase 1 c file working directory
	cfiledir = "C:\\zWork\\ERA\\ERA 2016\\INOUT\\AKS15INcorr" #<- change this
#phase 2 c file working directory
	outdir = "C:\\zWork\\ERA\\ERA 2017\\CFILECOMP" #<- change this
#Change directory to the dir where C files are located
	setwd(cfiledir)
#Create a list of all files in the working directory
	myListNoDir <- dir()
	myListWithDir <- list.files(".", full.names=TRUE)
#This is done in three steps: 
	#1) identify all files that end in the user-specified acronym
		whichFiles <- function(x) substr(x, start=nchar(x)-2, stop=nchar(x))
		myCFileListNoDir <- myListNoDir[sapply(myListNoDir, whichFiles) == StockAcronym]
		myCFileListWithDir <- myListWithDir[sapply(myListWithDir, whichFiles) == StockAcronym]
	#2) open all C files in a directory and get the tag code (line 1)
		readLine <- function(x) readLines(x)[1]
		myTagList <- sapply(myCFileListNoDir, readLine)
		#option for error checking here...
	#3) read c files into R
		CList <- list()
		for(i in 1:length(myCFileListNoDir)) CList[[i]] <- readC(myCFileListNoDir[i])

#############################
## Write the "mega" C File ##
#############################
#Set directory to where to save output
  setwd(outdir)
#Create the file
zz <- file(paste(StockAcronym, "2016era C-File summary.csv"), "w")
writeLines("tagcode,broodyear,fisherynumber,fisheryname,age2,age3,age4,age5,age6", con=zz) 
for(i in 1:length(CList)) {
	for(j in 1:CList[[i]]$nFisheries) {
	#loop through the table
		newText <- paste(CList[[i]]$TagCode, ",", CList[[i]]$BroodYear, ",", j, ",", rownames(CList[[i]]$RecoveriesByFishery)[j], paste0(sep=",",CList[[i]]$RecoveriesByFishery[j,], collapse=""), sep="")
		writeLines(newText, con=zz)
	#plus escapement
		if(j==CList[[i]]$nFisheries) { 
			newText <- paste(CList[[i]]$TagCode, ",", CList[[i]]$BroodYear, ",", j+1, ",", rownames(CList[[i]]$Escapement), paste0(sep=",",CList[[i]]$Escapement, collapse=""), sep="")
			writeLines(newText,con=zz)
		}
	}
}
close(zz)


##################
## READ IN THIS YEAR's DATA ##
##################
#Stock Acronym
	StockAcronym <- "AKS" #<- change this
#phase 1 c file working directory
	cfiledir = "C:\\zWork\\ERA\\ERA 2017\\AK INOUT\\AKS16IN" #<- change this

#phase 2 c file working directory
	outdir = "C:\\zWork\\ERA\\ERA 2017\\CFILECOMP" #<- change this
#Change directory to the dir where C files are located
	setwd(cfiledir)
#Create a list of all files in the working directory
	myListNoDir <- dir()
	myListWithDir <- list.files(".", full.names=TRUE)
#This is done in three steps: 
	#1) identify all files that end in the user-specified acronym
		whichFiles <- function(x) substr(x, start=nchar(x)-2, stop=nchar(x))
		myCFileListNoDir <- myListNoDir[sapply(myListNoDir, whichFiles) == StockAcronym]
		myCFileListWithDir <- myListWithDir[sapply(myListWithDir, whichFiles) == StockAcronym]
	#2) open all C files in a directory and get the tag code (line 1)
		readLine <- function(x) readLines(x)[1]
		myTagList <- sapply(myCFileListNoDir, readLine)
		#option for error checking here...
	#3) read c files into R
		CList <- list()
		for(i in 1:length(myCFileListNoDir)) CList[[i]] <- readC(myCFileListNoDir[i])

#############################
## Write the "mega" C File ##
#############################
#Set directory to where to save output
  setwd(outdir)
#Create the file
zz <- file(paste(StockAcronym, "2017era C-File summary.csv"), "w")
writeLines("tagcode,broodyear,fisherynumber,fisheryname,age2,age3,age4,age5,age6", con=zz) 
for(i in 1:length(CList)) {
	for(j in 1:CList[[i]]$nFisheries) {
	#loop through the table
		newText <- paste(CList[[i]]$TagCode, ",", CList[[i]]$BroodYear, ",", j, ",", rownames(CList[[i]]$RecoveriesByFishery)[j], paste0(sep=",",CList[[i]]$RecoveriesByFishery[j,], collapse=""), sep="")
		writeLines(newText, con=zz)
	#plus escapement
		if(j==CList[[i]]$nFisheries) { 
			newText <- paste(CList[[i]]$TagCode, ",", CList[[i]]$BroodYear, ",", j+1, ",", rownames(CList[[i]]$Escapement), paste0(sep=",",CList[[i]]$Escapement, collapse=""), sep="")
			writeLines(newText,con=zz)
		}
	}
}
close(zz)

