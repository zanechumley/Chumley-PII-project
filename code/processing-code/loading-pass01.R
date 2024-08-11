# Loading the data, part 1

# Design 2 loading the data

# Design 2 makes the following updates:
# 1). writes the .csv file every 100,000 lines as the data is read and processed
#     - internal memory requirements are greatly reduced
# 2). reports the size of processed file each time a data file's processing has been completed.
# 3). counts data files processed
# 4). virtualizes the root directory for portability

# The data is spread across 34 compressed folders and takes 71.9Gb of hard drive space.
# Due to capacity limitations: 
# 1). Each compressed folder will have to be unZIPped.
#     - This will result in both a folder and a file inside the folder, both sharing a name.
# 2). Each unZIPped file will have to be processed.
# 3). Each unZIPped file, along with its parent folder, will have to be deleted.

library(jsonlite)
# library(archive)
library(R.utils)
# library(data.table)
# library(lubridate)

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
WriteThisData <- function () {
  # This function takes the global vectors of TheseRatings and TheseReviews and:
  # 1). Combines the vectors into a dataframe
  # 2). Writes them to the processed data file
  # 3). Empties the vectors
  # 4). Resets the vector index
  ThisProcessedData <- cbind(TheseRatings
                             , TheseReviews
                             )
  ThisProcessedData <- as.data.frame(ThisProcessedData)
  write.table (ThisProcessedData
               , MyProcessedDataFile
               , sep=","
               , col.names=FALSE
               , row.names=FALSE
               , append=TRUE
  )
  TheseRatings <<- NULL
  TheseReviews <<- NULL
  VectorIndex <<- 0
  } # WriteThisData
# initialize global variables
TotalProcessStartTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat ("This process started at"
     , TotalProcessStartTime
     , "\n")
TotalProcessTimeStart <- proc.time()
DataLoadStartTime <- proc.time()
TheseRatings <- c()
TheseReviews <- c()
DataIndex <- 0
DataFileIndex <- 0
VectorIndex <- 0
TestingLimit <- 100 #maximum number of compressed folders to process for testing
MyGlobalPath <- getwd()
cat("Global Path for this process is "
    , MyGlobalPath
    , "\n")
# Set paths for Raw and Processed Data
MyRawDataPath <- file.path("data"
                           , "raw-data"
)
MyProcessedDataFile <- file.path("data" # supersedes MyProcessedDataPath
                                 , "processed-data"
                                 , "Ratings and Reviews.csv")
# Initialized Ratings and Reviews File with column headers
TheseReviews <- c("Review")
TheseRatings <- c("Rating")
WriteThisData()
# retrieve list of directory files
DirectoryFiles <- list.files(path=MyRawDataPath)
# Convert list to dataframe so compressed folders can be flagged
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
  DataFileIndex <- DataFileIndex + 1
  cat("Processing data file"
      , DataFileIndex
      , "\n")
  FullDataPathFile <- paste(MyRawDataPath
                           , "/"
                           , ThisCompressedFolder
                           , sep="")
  gunzip(FullDataPathFile
         , remove=FALSE)
  mydatafile <- substr(FullDataPathFile
                       , 1
                       , nchar(FullDataPathFile)-3
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
      VectorIndex <- VectorIndex + 1
      ThisJSONlist <- fromJSON(ThisLine)
      TheseRatings[VectorIndex] <- ThisJSONlist$rating
      TheseReviews[VectorIndex] <- ThisJSONlist$text
      # Write data and give update every 100,000 lines processed
      if (LineNumber%%100000==0) {
        WriteThisData ()
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
  # write remaining data and close the file
  WriteThisData ()
  cat("Closing and deleting decompressed file:"
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
  cat("A total of"
      , prettyNum(DataIndex, big.mark=",", scientific=FALSE)
      , "reviews and ratings have been processed so far."
      , "\n"
  )
  FileSize <- file.info(MyProcessedDataFile)$size
  cat("The Ratings and Reviews file now takes"
      , prettyNum(FileSize, big.mark=",", scientific=FALSE)
      , "bytes of hard drive space."
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
  # We break if testing
  if (DataFileIndex==TestingLimit) {
    break # after the specified number of files, just to be safe
  } # if (DataFileIndex==TestingLimit)
} # for (ThisCompressedFolder in DirectoryFiles$FileName)
DataLoadTime <- proc.time() - DataLoadStartTime
DataLoadElapsedTime <- trunc(DataLoadTime[3])
DataElapsedTime <- SpellOutSeconds(DataLoadElapsedTime)
TotalProcessTime <- proc.time() - TotalProcessTimeStart
TotalProcessElapsedTime <- trunc(TotalProcessTime[3])
ProcessElapsedTime <- SpellOutSeconds(TotalProcessElapsedTime)
TotalProcessEndTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
cat ("This process started at"
     , TotalProcessStartTime
     , "\n")
cat ("This process ended at"
     , TotalProcessEndTime
     , "\n")
cat("This entire process took: "
    , ProcessElapsedTime
    , "\n")
