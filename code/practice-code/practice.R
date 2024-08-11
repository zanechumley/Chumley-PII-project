# practice code for Project

# loading the data

# The data is spread across 34 compressed folders and takes 71.9Gb of hard drive space.
# Due to capacity limitations: 
# 1). Each compressed folder will have to be unZIPped.
#     - This will result in both a folder and a file inside the folder, both sharing a name.
# 2). Each unZIPped file will have to be processed.
# 3). Each unZIPped file, along with its parent folder, will have to be deleted.

library(jsonlite)
library(archive)
library(R.utils)
library(data.table)
library(lubridate)
# create global functions
SpellOutSeconds <- function (StartingSeconds) {
  # This function takes a number of seconds and converts it to days, hours, minutes, and seconds.
  # Library lubridate's second_to_period function, for whyever reason, always returned a string of zeros.
  Days <- trunc(StartingSeconds/86400)
  RemainingSeconds <- StartingSeconds-(Days*86400)
  Hours <- trunc(RemainingSeconds/3600)
  RemainingSeconds <- RemainingSeconds-(Hours*3600)
  Minutes <- trunc(RemainingSeconds/60)
  RemainingSeconds <- trunc(RemainingSeconds-(Minutes*60))
  ThisString <- paste(Days
                      , "days"
                      , Hours
                      , "hours"
                      , Minutes
                      , "minutes"
                      , RemainingSeconds
                      , "seconds"
                      , sep=" ")
  return (ThisString) # for now
} # SpellOutSeconds
# initialize global variables
TotalProcessStartTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat ("This process started at"
     , TotalProcessStartTime
     , "\n")
TotalProcessTimeStart <- proc.time()
DataLoadStartTime <- proc.time()
AllRatings <- c()
AllReviews <- c()
DataIndex <- 0
# set global project directory
MyGlobalPath <- file.path("C:"
                        , "Users"
                        , "Zane"
                        , "Documents"
                        , "UTSA-MSDA"
                        , "Coursework"
                        , "2024-02 Summer"
                        , "DA 6833 02T - Thursday 6pm"
                        , "P2-Portfolio Clone"
                        , "Chumley-PII-project"
                        , "Chumley-PII-project"
)
# Set paths for Raw and Processed Data
MyRawDataPath <- file.path("data"
                           , "raw-data"
                           )
MyProcessedDataPath <- file.path("data"
                                 , "processed-data")
setwd(MyGlobalPath)
setwd(MyRawDataPath)
# retrieve list of directory files
DirectoryFiles <- list.files()
# Convert list to dataframe to flag zipped folders
DirectoryFiles <- as.data.frame(DirectoryFiles)
colnames(DirectoryFiles) <- c("FileName")
# add compressed file flag
DirectoryFiles$compressed <- FALSE
# flag compressed files
for (ThisFile in 1:nrow(DirectoryFiles)) {
  # is it a compressed file?  The last three characters will be ".gz" if it is
  ThisFileName <- DirectoryFiles$FileName[ThisFile]
  LastThree <- substr(ThisFileName
                      , nchar(ThisFileName)-2
                      , nchar(ThisFileName)
  )
  if (LastThree==".gz") {
    # it is a compressed file, so flag it
    DirectoryFiles$compressed[ThisFile] <- TRUE
  } # if (LastThree==".gz")
} # for (ThisFile in 1:nrow(DirectoryFiles))
# remove files that are not compressed folders from the list of files to process
DirectoryFiles <- subset(DirectoryFiles, DirectoryFiles$compressed==TRUE)
# set up loop for compressed folder
for (ThisCompressedFolder in DirectoryFiles$FileName) {
  gunzip(ThisCompressedFolder
         , remove=FALSE)
  mydatafile <- substr(ThisCompressedFolder
                       , 1
                       , nchar(ThisCompressedFolder)-3
                       )
  cat("Opening and processing file:"
      , mydatafile
      , "\n")
  # initialize variables to process a particular file
  FileSize <- file.info(mydatafile)$size
  MoreToCome <- TRUE
  LineNumber <- 0
  FileStartTime <- proc.time()
  # open a particular file
  cat("Decompressed, the file takes"
      , prettyNum(FileSize, big.mark=",", scientific=FALSE)
      , "bytes of hard drive space."
      , "\n"
      )
  FileLines <- file(mydatafile, 'r')
  while (MoreToCome) {
    # there's another line in the file to process
    ThisLine <- readLines(FileLines, n=1)
    if (length(ThisLine)==0) {
      MoreToCome <- FALSE
    } else {
      LineNumber <- LineNumber + 1
      DataIndex <- DataIndex + 1
      ThisJSONlist <- fromJSON(ThisLine)
      AllRatings[DataIndex] <- ThisJSONlist$rating
      AllReviews[DataIndex] <- ThisJSONlist$text
      # Give update every 100,000 lines processed
      if (LineNumber%%100000==0) {
        SnapshotTime <- proc.time() - FileStartTime
        cat("   * "
            , prettyNum(LineNumber, big.mark=",", scientific=FALSE)
            , "lines processed in"
            , SnapshotTime[3]
            , "seconds."
            , "\n"
            )
      } # if (LineNumber%100000==0)
    } # if (length(ThisLine)==0) {
  } # while (MoreToCome)
  # close the file
  cat("Closing and deleting file:"
      , mydatafile
      , "\n"
  ) 
  close(FileLines)
  # delete the decompressed file to save hard drive space
  file.remove(mydatafile)
  # report file-specific metrics
  cat("A total of"
      , prettyNum(LineNumber, big.mark=",", scientific=FALSE)
      , "lines were processed in the file."
      , "\n"
  )
  FileProcessingTime <- proc.time() - FileStartTime
  cat("Elapsed Time for processing the file was"
      , FileProcessingTime[3]
      , "seconds."
      , "\n")
  # report global metrics
  cat("The Ratings vector now takes"
      , prettyNum(object.size(AllRatings), big.mark=",", scientific=FALSE)
      , "bytes of space."
      , "\n"
  )
  cat("The Reviews vector now takes"
      , prettyNum(object.size(AllReviews), big.mark=",", scientific=FALSE)
      , "bytes of space."
      , "\n"
  )
  cat("A total of"
      , prettyNum(DataIndex, big.mark=",", scientific=FALSE)
      , "reviews and ratings have been processed so far."
      , "\n"
  )
  # report total elapsed time so far
  TotalElapsedProcessProcTime <- proc.time() - TotalProcessTimeStart
  TotalSecondsElapsedTime <- trunc(TotalElapsedProcessProcTime[3])
  TotalElapsedTime <- SpellOutSeconds(TotalSecondsElapsedTime)
  cat("The entire process has taken"
      , TotalElapsedTime
      , "so far."
      , "\n")
  cat("\n")
  # God help me - let's so all of them!
  # break # after the first file, just to be safe
} # for (ThisCompressedFolder in DirectoryFiles$FileName)
DataLoadTime <- proc.time() - DataLoadStartTime
DataLoadElapsedTime <- trunc(DataLoadTime[3])
DataElapsedTime <- SpellOutSeconds(DataLoadElapsedTime)
cat("Loading all the data took"
    , DataElapsedTime
    , "in total."
    , "\n")
# Now let's write the parsed data to a .csv
# create the dataframe to write
MasterData <- cbind(AllRatings
                    , AllReviews)
MasterData <- as.data.frame(MasterData)
cat("The MasterData takes"
    , prettyNum(object.size(MasterData), big.mark=",", scientific=FALSE)
    , "bytes of space."
    , "\n"
)
setwd(MyGlobalPath)
setwd(MyProcessedDataPath)
FileWriteStart <- proc.time()
fwrite(MasterData, "DataImported.csv")
FileWriteElapsed <- proc.time() - FileWriteStart
cat("Writing the Master Data took"
    , FileWriteElapsed[3]
    , "seconds."
    , "\n")
TotalProcessTime <- proc.time() - TotalProcessTimeStart
TotalProcessElapsedTime <- trunc(TotalProcessTime[3])
ProcessElapsedTime <- SpellOutSeconds(TotalProcessElapsedTime)
cat("This entire process took"
    , ProcessElapsedTime
    , "in total."
    , "\n")
TotalProcessEndTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat ("This process started at"
     , TotalProcessStartTime
     , "\n")
cat ("This process ended at"
     , TotalProcessEndTime
     , "\n")
