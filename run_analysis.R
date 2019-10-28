#Preparing the file locations for input
#Assuming that the getdata_projectfiles_UCI HarDataset (uncompressed) folder is in wd
mainDataFolder <- paste0(getwd(), "/getdata_projectfiles_UCI Har Dataset/UCI HAR Dataset")
xTestFile <- paste0(mainDataFolder, "/test/X_test.txt")
xTrainFile <- paste0(mainDataFolder, "/train/X_train.txt")
yTestFile <- paste0(mainDataFolder, "/test/y_test.txt")
yTrainFile <- paste0(mainDataFolder, "/train/y_train.txt")
subjTestFile <- paste0(mainDataFolder, "/test/subject_test.txt")
subjTrainFile <- paste0(mainDataFolder, "/train/subject_train.txt")
activityLabelFile <- paste0(mainDataFolder, "/activity_labels.txt")
featureLabelFile <- paste0(mainDataFolder, "/features.txt")


#function dedicated to parsing / cleaning the data coming
#from xTest and xTrain files (containing feature vectors)
#returns clean dataframe without column names
#x is the location of the x file 
processXFiles <- function(x){ 
  
  cleanFeatureVector <- function(v){
    
    featureV <- strsplit(v , " ")   #split a line of features  
    featureV <- featureV[[1]] 
    featureV <- featureV[featureV != ""] #remove empty entries ""
    #clean up is required because lines are not separated uniformally
    return(as.numeric(featureV))
    
  }
  
  df <- read.delim(x, header = FALSE, stringsAsFactors = FALSE)
  df <- sapply(df[,1], cleanFeatureVector) #apply cleanup function to each line
  return(t(df)) #return transpose of dataframe (because sapply flips the original)
  
}

#read the data from files and preform cleanup on xfiles
testData <- data.frame(processXFiles(xTestFile))
trainData <- data.frame(processXFiles(xTrainFile))
subTrainVector <- read.delim(subjTrainFile, header = FALSE, stringsAsFactors = FALSE) #subject vector
subTestVector <- read.delim(subjTestFile, header = FALSE, stringsAsFactors = FALSE)
yTrainVector <- read.delim(yTrainFile, header = FALSE, stringsAsFactors = FALSE) #activity vector
yTestVector <- read.delim(yTestFile, header = FALSE, stringsAsFactors = FALSE)
activityLabelTable <- read.delim(activityLabelFile, header = FALSE, stringsAsFactors = FALSE, sep = " ")
featureLabelTable <- read.delim(featureLabelFile, header = FALSE, stringsAsFactors = FALSE, sep = " ") #feature labels

assignActivtyLabel <- function(x){ #function used to transform numeric activity to string label
  return(activityLabelTable[(activityLabelTable[,1] == x),2])
}

yTrainVector <- data.frame(sapply(yTrainVector$V1, assignActivtyLabel)) #apply above function to subject vectors
yTestVector <- data.frame(sapply(yTestVector$V1, assignActivtyLabel))

names(yTestVector) <- "activity"
names(yTrainVector) <- "activity"   #give subject and activity vectors the appropriate labels
names(subTestVector) <- "subject"
names(subTrainVector) <- "subject"
totalSubjects <- rbind(yTestVector,yTrainVector)
totalActivity <- rbind(subTestVector,subTrainVector)

columnsToExtract <- grepl("mean[(]|std[(]",featureLabelTable$V2) #get logical vector indicating using grepl
#only extract columns with "mean()" or "std()" 

totalData <- rbind(testData,trainData) #bind train and test
totalData <- totalData[,columnsToExtract] #extract columns using logical vector

names(totalData) <- featureLabelTable$V2[columnsToExtract] #add names to extracted columns
totalData <- cbind(totalSubjects,totalActivity,totalData) # add activity and subject columns

rm(list = ls()[ls() != "totalData"]) #delete all variables execept totalData

library(dplyr) #use dplyr for summarizing data

sumData <- group_by(totalData,subject,activity) #group by subject and activity
sumData <- summarise_all(sumData,mean) #apply sum to mean to grouped data

write.table(sumData,file = (paste0(getwd(),"/summarizedData.txt")), row.names = FALSE)




