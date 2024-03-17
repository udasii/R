#' Suraj Udasi_Assignment 1
#' Purpose: EDA case a1 
#' Suraj Udasi
#' Jan 23, 2024

# WD
setwd("~/Visualizing_Analyzing_Data_with_R/personalFiles")

# Libraries
library(data.table)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)

# Name the case file you are creating
fileName <- 'a1_EDA_case.csv'

# Set this to your Cases/A1_Retail_EDA_data folder
caseData <- '~/Visualizing_Analyzing_Data_with_R/Cases/A1_Retail_EDA/data'

# Let's get all files from the folders
tmp <- list.files(path       = caseData,
                  pattern    = '*.csv',
                  full.names = T,
                  recursive  = T)

# Do you want to get all transactions or a sample?
sampleTransactions   <- F
nTransactionsPerFile <- 10000

caseData <- list()
for(i in 1:length(tmp)){
  print(paste('Reading in file',i, ':',tmp[i]))
  tmpTransactions <- fread(tmp[i])
  
  if(sampleTransactions==T){
    print(paste('Sampling data to',nTransactionsPerFile,'rows.'))
    tmpTransactions <- tmpTransactions[sample(1:nTransactionsPerFile),]
  } else {
    print('Ignoring nTransactionsPerFile & reading all data')
  }
  caseData[[i]] <- tmpTransactions
}

# Organize into a single data frame
caseData <- do.call(rbind, caseData)


# clean columns names
names(Data) <- make.names(names(Data))

# Correct column names
names(Data) <- gsub("\\.", "_", names(Data))

# check missing values
colSums(is.na(Data))

caseData <- na.omit(subset(Data, !is.na(Zip_Code)))
dim(caseData)

# Classes for each column
sapply(caseData, class)

# Correcting Date Format
caseData$Date <- as.Date(caseData$Date)

# Descriptive Analysis

#Summary Statistics:
summary(caseData)
str(caseData)

# Identifying categorical variables
caseData$Store_Number <- as.factor(caseData$Store_Number)
caseData$Zip_Code <- as.factor(caseData$Zip_Code)
caseData$Month <- as.factor(caseData$Month)

str(caseData)


# Save into a single file
if(sampleTransactions==T){
  nam <- paste0(Sys.Date(),'_sampled_', nrow(caseData),'_rows_',fileName)
} else {
    nam <- nam <- paste0(Sys.Date(),'_complete_data_', fileName)
  }

fwrite(caseData, nam)

# End


