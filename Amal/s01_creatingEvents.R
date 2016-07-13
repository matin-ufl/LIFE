setwd("~/Workspaces/R workspace/LIFE/Amal/")

df.ref <- read.csv("LIFE01b_ActivityStatus_v2_1_visit days_CLS.csv")
df.ref$events.hospitalevent.no <- 0
df.aux <- read.csv("LIFE04_SAE_v2_1.csv")
df.aux$vc <- "CLS"

for(i in 1:nrow(df.ref)) {
     print(paste("(", i, " out of ", nrow(df.ref), ") checked.", sep = ""))
     maskid <- df.ref$maskid[i]
     visit <- as.character(df.ref$vc[i])
     upperBoundDay <- df.ref$checklist_days[i]
     if((!is.na(upperBoundDay)) && (upperBoundDay > 0)) {
          lowerBoundDay <- df.ref$checklist_days[i-1]
          ppt.df <- df.aux[df.aux$maskid == maskid, ]
          events.df <- ppt.df[ppt.df$daysevntn_eevl > lowerBoundDay, ]
          events.df <- events.df[which(events.df$daysevntn_eevl <= upperBoundDay), ]
          df.ref$events.hospitalevent.no[i] <- sum(events.df$hospital_eevl, na.rm = T)
     }
}

for(i in 1:nrow(df.aux)) {
     print(paste("(", i, " out of ", nrow(df.aux), ") added.", sep = ""))
     maskid <- df.aux$maskid[i]
     ppt.ref <- df.ref[df.ref$maskid == maskid, ]
     df.aux$vc[i] <- findTheVisit(ppt.ref, df.aux$daysevntn_eevl[i])
}

df.aux.reordered <- df.aux[, c(1, ncol(df.aux), 2:(ncol(df.aux)-1))]

write.csv(df.ref, file = "LIFE01b_ActivityStatus_v2_1_visit days_hospitalevent.csv", row.names = F)
write.csv(df.aux.reordered, file = "LIFE04_SAE_v2_1_vc.csv", row.names = F)

# Functions ----------------------------
findTheVisit <- function(ppt.ref, day) {
     prev.visit <- which.max(which(ppt.ref$checklist_days < day))
     as.character(ppt.ref$vc[prev.visit + 1])
}