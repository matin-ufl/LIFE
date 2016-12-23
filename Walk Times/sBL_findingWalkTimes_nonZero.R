# Working Parameters -----------------------------------
# Please provide correct file and folder addresses
working.directory <- '~/Workspaces/R workspace/LIFE/Walk Times/'
baseline.folder <- '~/Desktop/Baseline/'
w400.tests.filename <- '~/Desktop/Baseline/400recordtime.csv'
output_csv <- "V:/Matin/R Codes/LIFE-master/walkTimesFound_122316.csv"

# Setting Criteria --------------------------------
SCAN_DURATION <- 35 * 60
NON_ZERO_THRESHOLD <- 70
SLIDING_STEP <- 30
DATA_DAY_LIMIT <- 23 * 60 * 60

# Functions ----------------------------------------
convertToFileName <- function(hid) {
     s <- as.character(hid)
     while(nchar(s) < 4) {
          s <- paste("0", s, sep = '')
     }
     paste("HID", s, ".Rdata", sep = '')
}

just.onePartOfTime <- function(ts, which.part = 1) {
     times <- unlist(strsplit(ts, split = " "))[2]
     as.character(unlist(strsplit(times, split = ":"))[which.part])
}


# The Script ------------- main -------------------------------
setwd(working.directory)
source("f01_functions_findingWalkTimes_nonZero.R")

l <- dir(path = baseline.folder, pattern = '^HID[0-9]{4}.Rdata$')
walktimes.df <- read.csv(file = w400.tests.filename, header = T)
colnames(walktimes.df) <- c("MaskID","PartcipantID","vc","timestart","timestop","Accelerometry","meter400","start","stop")
walktimes.df <- walktimes.df[walktimes.df$vc == "SV1", ]
load("PID_VC_HID.Rdata"); hid_pid_mapping <- REF; rm(REF); hid_pid_mapping <- hid_pid_mapping[hid_pid_mapping$seq == 0, ]
hid_pid_mapping$filename <- sapply(hid_pid_mapping$HID, FUN = convertToFileName)
result <- data.frame(matrix(nrow = 0, ncol = 9))
for (i in 1:length(l)) {
     filename <- l[i]
     print(paste("[", Sys.time(), "] ", filename, " is being scanned... (", i, " out of ", length(l), ")", sep = ""))
     load(paste(baseline.folder, filename, sep = ""))
     AC.1s$TimeStamp <- as.character(AC.1s$TimeStamp)
     AC.1s$hour <- sapply(AC.1s$TimeStamp, FUN = just.onePartOfTime, which.part = 1)
     AC.1s$minute <- sapply(AC.1s$TimeStamp, FUN = just.onePartOfTime, which.part = 2)
     AC.1s$second <- sapply(AC.1s$TimeStamp, FUN = just.onePartOfTime, which.part = 3)
     PID <- hid_pid_mapping$pid[hid_pid_mapping$filename == filename]
     if(length(PID) > 0) {
          ppt.walktimes.df <- walktimes.df[which(walktimes.df$PartcipantID == PID), ]
          if(nrow(ppt.walktimes.df) > 0) {
               ppt.walktimes.df$start <- as.character(ppt.walktimes.df$start)
               ppt.walktimes.df$stop <- as.character(ppt.walktimes.df$stop)
               ppt.walktimes.df$meter400 <- as.numeric(as.character(ppt.walktimes.df$meter400))
               
               # Finding the right indices
               indices <- find.startEndIdx(df = AC.1s[1:DATA_DAY_LIMIT, ], givenStart = ppt.walktimes.df$start)
               
               if(is.na(indices$startIdx)) {
                    indices$startIdx <- 1
               }
               if(is.na(indices$endIdx)) {
                    indices$endIdx <- min(nrow(AC.1s), (indices$startIdx + (SCAN_DURATION * 2)))
               }
               AC.1s$datetime <- AC.1s$TimeStamp
               curr.result <- walktime.finder(df = AC.1s, ppt.walktimes.df = ppt.walktimes.df, indices = indices)
               result <- rbind(result, curr.result)
               colnames(result) <- colnames(curr.result)
          } else {
               print("No walking record found...")
          }
     }
}

write.csv(result, file = output_csv, row.names = F)
rm(list = ls())