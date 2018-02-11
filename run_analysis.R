library(dplyr)

givenDirectory = NULL
datasetDirectory = NULL
testDataDirectory = NULL
trainDataDirectory = NULL

xColumnNames = NULL
yColumnNames = NULL

testDataSubject = NULL
testDataX = NULL
testDataY = NULL
testDataMerged = NULL

trainDataSubject = NULL
trainDataX = NULL
trainDataY = NULL
trainDataMerged = NULL

singleMergedData = NULL

setGivenDirectory <- function(inputDirectory) {
    givenDirectory <<- inputDirectory
    print(givenDirectory)
    datasetDirectory <<- paste(givenDirectory, sep = "", "/UCI HAR Dataset")
    print(datasetDirectory)
    testDataDirectory <<- paste(datasetDirectory, sep = "", "/test")
    print(testDataDirectory)
    trainDataDirectory <<- paste(datasetDirectory, sep = "", "/train")
    print(trainDataDirectory)
    print("Directory was successfully set!")
}

matchDataToName <<- function(inputDataset, isTest) {
    dataColLength <- length(inputDataset[,c(1)])
    print(paste("DataColLength:", dataColLength))
    
    inputDataset <- data.frame(lapply(inputDataset, as.character), stringsAsFactors=FALSE)
    
    for(i in 1:dataColLength) {
        nameId <- inputDataset[c(i), c(1)]
        # print(paste("Name ID:", nameId))
        inputDataset[c(i), c(1)] <- yColumnNames[c(nameId), c(2)]
        # print(paste("Activity:", yColumnNames[c(nameId), c(2)]))
    }
    
    if(isTest) {
        print("TEST")
        testDataY <<- inputDataset
    } else {
        trainDataY <<- inputDataset
    }
}

loadData <- function() {
    print("Loading data")
    xColumnNames <<- read.table(paste(datasetDirectory, sep = "", "/features.txt"))
    yColumnNames <<- read.table(paste(datasetDirectory, sep = "", "/activity_labels.txt"))
    yColumnNames[,c(2)] <<- as.character(yColumnNames[,c(2)])
    loadTestData()
    loadTrainData()
    print("Data loaded!")
}

loadTestData <- function() {
    print("Loading test data")
    testDataSubject <<- read.table(paste(testDataDirectory, sep = "", "/subject_test.txt"))
    print(length(testDataSubject))
    testDataSubject <<- rename(testDataSubject, ID = V1)
    testDataX <<- read.table(paste(testDataDirectory, sep = "", "/X_test.txt"))
    colnames(testDataX) <<- xColumnNames[,c(2)]
    print(length(testDataX))
    testDataY <<- read.table(paste(testDataDirectory, sep = "", "/y_test.txt"))
    print(length(testDataY))
    matchDataToName(testDataY, TRUE)
    testDataY <<- rename(testDataY, Activity = V1)
    print("Test data loaded!")
}

loadTrainData <- function() {
    print("Loading train data")
    trainDataSubject <<- read.table(paste(trainDataDirectory, sep = "", "/subject_train.txt"))
    trainDataSubject <<- rename(trainDataSubject, ID = V1)
    print(length(trainDataSubject))
    trainDataX <<- read.table(paste(trainDataDirectory, sep = "", "/X_train.txt"))
    colnames(trainDataX) <<- xColumnNames[,c(2)]
    print(length(trainDataX))
    trainDataY <<- read.table(paste(trainDataDirectory, sep = "", "/y_train.txt"))
    print(length(trainDataY))
    matchDataToName(trainDataY, FALSE)
    trainDataY <<- rename(trainDataY, Activity = V1)
    print("Train data loaded!")
}

mergeTestTrainData <- function() {
    testDataMerged <<- cbind(testDataSubject, testDataY, testDataX)
    trainDataMerged <<- cbind(trainDataSubject, trainDataY, trainDataX)
    singleMergedData <<- rbind(testDataMerged, trainDataMerged)
}

retainMeanStdCol <- function() {
    singleMergedData <<- singleMergedData[,c(1:2,grep("*mean|std*", colnames(singleMergedData)))]
}


initRunAnalysis <- function() {
    print("Run Analysis Start")
    mergeTestTrainData()
    retainMeanStdCol()
    write.table(singleMergedData, file = "TidyMergedData.txt", row.names = FALSE)
    print("Run Analysis End")
}

