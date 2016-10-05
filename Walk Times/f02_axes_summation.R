#######################################################################################
### For every participants (PID) fed into this function, it provides summations and ###
### standard deviation of each axis and writes them in a csv file.                  ###
###                                                                                 ###
### Inputs:                                                                         ###
###     1. xlsx_file_start_and_end: the .xlsx file containing the following columns ###
###          a. PID                                                                 ###
###          b. correct_starttime                                                   ###
###          c. correct_stoptime                                                    ###
###                                                                                 ###
###     2. dataFiles_folder: full address of the folder containing HIDXXXX.Rdata    ###
###                          files. (e.g., the one in the shared drive)             ###
###                                                                                 ###
###     3. visit_seq: for which visit the summation should be performed.            ###
###                   should take values from {0, 6, 12, 24}                        ###
###                   DEFAULT = 6                                                   ###
###                                                                                 ###
###     4. outputFile_csv: full name (address and name) of the file to save the     ###
###                        output into. (e.g., "C:/axes_sum.csv")                   ###
###                                                                                 ###
### Output:                                                                         ###
###     The data.frame (which is also saved as outputFile_csv) is returned.         ###
###                                                                                 ###
### ------------------------------------------------------------------------------- ###
### Author:                                                                         ###
###         Matin Kheirkhahan (matin@cise.ufl.edu)                                  ###
#######################################################################################

generate.axes.sum <- function(xlsx_file_start_and_end, dataFiles_folder, visit_seq = 6, outputFile_csv) {
     intervals.df <- readWorksheet(loadWorkbook(xlsx_file_start_and_end), sheet = 1)
     intervals.df$correct_starttime <- as.character(intervals.df$correct_starttime);intervals.df$correct_stoptime <- as.character(intervals.df$correct_stoptime)
     
     intervals.df$start <- NA
     intervals.df$end <- NA
     for(i in 1:nrow(intervals.df)) {
          intervals.df$start[i] <- tryCatch(
               {
                    format(as.POSIXct(intervals.df$correct_starttime[i]), "%T")
               },
               error = function(cond) {
                    message(paste("[Warning] Error in reading this PID", intervals.df$PID[i], " ---> Handled."))
                    return(format(as.POSIXct(
                         paste(as.character(as.Date(intervals.df$correct_starttime[i-1])), intervals.df$correct_starttime[i])
                    ), "%T"))
               },
               finally = function(cond) {
                    message(paste("[Error] Shit happened here for this PID", intervals.df$PID[i]))
                    return(NA)
               }
          )
          
          intervals.df$end[i] <- tryCatch(
               {
                    format(as.POSIXct(intervals.df$correct_stoptime[i]), "%T")
               },
               error = function(cond) {
                    message(paste("[Warning] Error in reading this PID", intervals.df$PID[i], " ---> Handled."))
                    return(format(as.POSIXct(
                         paste(as.character(as.Date(intervals.df$correct_stoptime[i-1])), intervals.df$correct_stoptime[i])
                    ), "%T"))
               },
               finally = function(cond) {
                    message(paste("[Error] Shit happened here for this PID", intervals.df$PID[i]))
                    return(NA)
               }
          )
     }
     
     
     load(paste(dataFiles_folder, "PID_VC_HID.Rdata", sep = ""))
     intervals.df$HID <- sapply(intervals.df$PID, FUN = function(x) {
          a <- REF[REF$seq == visit_seq, ]
          as.numeric(a$HID[a$pid == x])
     })
     
     message()
     message("[LOG] Cleaning .xlsx file ended. Times are cleaned and ready to use.")
     message()
     
     out.df <- data.frame(matrix(nrow = 0, ncol = 8))
     
     for(i in 1:nrow(intervals.df)) {
          HID <- as.numeric(unlist(intervals.df$HID[i]))
          PID <- intervals.df$PID[i]
          
          message(paste("[LOG] (", i, " out of ", nrow(intervals.df), ") HID", HID, ".Rdata (PID: ", PID, ") is being analyzed.", sep = ""))
          
          if(is.null(HID) || length(HID) == 0) {
               message(paste("PID (", PID, ") has no corresponding HID... SKIPPED.", sep = ""))
               next()
          }
          
          load(paste(dataFiles_folder, "HID", HID, ".Rdata", sep = "")); AC.1s$TimeStamp <- as.character(AC.1s$TimeStamp)
          AC.1s$time <- sapply(AC.1s$TimeStamp, FUN = function(x){format(as.POSIXct(x), "%T")})
          start.idx <- min(which(AC.1s$time == intervals.df$start[i]))
          end.idx <- min(which(AC.1s$time == intervals.df$end[i]))
          selectedPart.df <- AC.1s[start.idx:end.idx, ]
          temp.df <- data.frame(HID = HID,
                                PID = PID,
                                axis1 = sum(selectedPart.df$axis1, na.rm = T),
                                axis2 = sum(selectedPart.df$axis2, na.rm = T),
                                axis3 = sum(selectedPart.df$axis3, na.rm = T),
                                axis1_std = sd(selectedPart.df$axis1, na.rm = T),
                                axis2_std = sd(selectedPart.df$axis2, na.rm = T),
                                axis3_std = sd(selectedPart.df$axis3, na.rm = T))
          out.df <- rbind(out.df, temp.df)
     }
     
     write.csv(out.df, file = outputFile_csv, row.names = F)
     
     message("[LOG] output is ready. Check the file [", outputFile_csv, "].")
     
     out.df
}

