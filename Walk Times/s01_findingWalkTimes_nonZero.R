library(xlsx)
setwd("~/Workspaces/R workspace/LIFE/Walk Times/")
SCAN_DURATION <- 35 * 60
NON_ZERO_THRESHOLD <- 70
SLIDING_STEP <- 30
DATA_DAY_LIMIT <- 23 * 60 * 60

# Functions ---------------------------------
source("f01_functions_findingWalkTimes_nonZero.R")
# Script ------------------------------------
walktimes.df <- read.csv(file = "~/Dropbox/Work-Research/Current Directory/LIFE/Project01 - Bruce help out/data/d01/400recordtime.csv", skip = 3, header = F)
colnames(walktimes.df) <- c("MaskID","PartcipantID","vc","timestart","timestop","Accelerometry","meter400","start","stop")
walktimes.df <- walktimes.df[walktimes.df$vc == "F06", ]
data.directory <- "~/../../Volumes/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/F06/"
l <- dir(data.directory)
result <- data.frame(matrix(nrow = 0, ncol = 8))
for(i in 1:length(l)) {
     print(paste("[", Sys.time(), "] ", l[i], " is being scanned... (", i, " out of ", length(l), ")", sep = ""))
     df <- read.csv(paste(data.directory, l[i], sep = ""))
     
     df <- df[df$sortorder, ]
     df$datetime <- as.character(df$datetime)
     df$hour <- sapply(df$datetime, just.hour)
     df$minute <- sapply(df$datetime, just.minute)
     df$second <- sapply(df$datetime, just.second)
     
     PID <- df$pid[1]
     ppt.walktimes.df <- walktimes.df[walktimes.df$PartcipantID == PID, ]
     
     ppt.walktimes.df$start <- as.character(ppt.walktimes.df$start)
     ppt.walktimes.df$stop <- as.character(ppt.walktimes.df$stop)
     ppt.walktimes.df$meter400 <- as.numeric(as.character(ppt.walktimes.df$meter400))
     
     # Finding the right indices
     indices <- find.startEndIdx(df = df[1:DATA_DAY_LIMIT, ], givenStart = ppt.walktimes.df$start)
     
     if(is.na(indices$startIdx)) {
          indices$startIdx <- 1
     }
     if(is.na(indices$endIdx)) {
          indices$endIdx <- min(nrow(df), (indices$startIdx + (SCAN_DURATION * 2)))
     }
     curr.result <- walktime.finder(df, ppt.walktimes.df, indices)
     result <- rbind(result, curr.result)
     colnames(result) <- colnames(curr.result)
     
     
     # Plot to check
     #plot.idx <- seq(1, 2000, by = 300)
     #ggplot(data = df[1:2000, ]) + geom_point(aes(x = datetime, y = axis1), shape = 3, alpha = 0.5) + theme_bw() + 
     #scale_x_discrete(breaks = df$datetime[plot.idx]) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

write.csv(result, file = "~/Dropbox/Work-Research/Current Directory/LIFE/Project01 - Bruce help out/data/out01/walkTimesFound_041616.csv", row.names = F)
rm(list = ls())




