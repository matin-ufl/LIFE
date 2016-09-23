library(data.table)

# Parameters to set ---------------------------------------------------------------
folders <- c("baseline1/", "baseline2/")
output.filename <- "baseline_mean_standardDeviations_092316.csv"

# Script to run -------------------------------------------------------------------

output.df <- data.frame(matrix(nrow = 0, ncol = 9))
for(folder in folders) {
     message(paste("\n\n", folder, "is being processed -----------------------------------\n"))
     files <- dir(path = folder, pattern = "^[0-9]{8}(.csv)$")
     for(f in 1:length(files)) {
          filename <- files[f]
          message(paste("(", f, ")", filename))
          DT <- fread(paste(folder, filename, sep = ""))
          DT <- DT[DT$sortorder, .(pid, sortorder, axis1, axis2, axis3, w400_start_sortorder, dur_form)]
          DT[, VM := sqrt(axis1^2 + axis2^2 + axis3^2), by = sortorder]
          walkperiod.dt <- DT[DT$w400_start_sortorder[1]:(DT$w400_start_sortorder[1] + DT$dur_form[1] - 1), ]
          output.df <- rbind(output.df,
                             data.frame(PID = walkperiod.dt$pid[1],
                                        axis1.avg = walkperiod.dt[, mean(axis1)], axis1.std = walkperiod.dt[, sd(axis1)],
                                        axis2.avg = walkperiod.dt[, mean(axis2)], axis2.std = walkperiod.dt[, sd(axis2)],
                                        axis3.avg = walkperiod.dt[, mean(axis3)], axis3.std = walkperiod.dt[, sd(axis3)],
                                        VM.avg = walkperiod.dt[, mean(VM)], VM.std = walkperiod.dt[, sd(VM)]))
     }
}

write.csv(x = output.df, file = output.filename, row.names = F)

rm(list = ls())
