load("~/Workspaces/R workspace/LIFE/Gaitrite-Merge/column_names.Rdata")

setwd("~/../../Volumes/SHARE/ARRC/Active_Studies/CHORES-XL_IRB201501057/Participant Data/")
ppt.folders <- dir(pattern = "^[A-Z]{4}[0-9]{11,12}$")

merged.df <- data.frame(matrix(nrow = 0, ncol = 52))
colnames(merged.df) <- c("ID", "Condition", cnames)

for(i in 1:length(ppt.folders)) {
     ppt.folder <- ppt.folders[i]
     message(paste(i, "out of", length(ppt.folders), "-", ppt.folder, "being checked."))
     ID <- as.character(substr(ppt.folder, start = 1, stop = 7))
     filename <- dir(path = paste(ppt.folder, "/", "V1/", sep = ""), pattern = "^.*gait.*.csv$")
     if(length(filename) > 0) {
          a <- read.csv(paste(ppt.folder, "/V1/", filename, sep = ""), header = T)
          b <- as.data.frame(t(a))
          b <- b[2:13, 1:49]
          rownames(b) <- NULL
          colnames(b) <- cnames
          merged.df <- rbind(merged.df,
                             data.frame(id = rep(ID, nrow(b)),
                                        condition = c(rep("normal", 6), rep("count", 6)),
                                        X = rep(c("1", "", "2", "", "3", ""), 2),
                                        b))
     } else {
          message("          ------> no gait file is found.")
     }
}
rm(ID, ppt.folder, Condition, condition, a, b)

write.csv(merged.df, file = "~/Desktop/gaitrite_merged_102116.csv", row.names = F)
