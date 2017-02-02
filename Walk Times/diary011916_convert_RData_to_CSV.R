#######################################################
#      Converting .Rdata file --to--> .CSV            #
#######################################################

filename <- "HID1609"
RFile <- paste("~/../../Volumes/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/", filename, ".Rdata", sep = "")
csvFile <- paste("~/Desktop/bl_selected_files/", filename, ".csv", sep = "")

load(RFile)
write.csv(AC.1s, file = csvFile, row.names = F)
