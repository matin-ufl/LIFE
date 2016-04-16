# Functions --------------------------------

just.hour <- function(timeStr) {
     hour <- as.numeric(substr(timeStr, start = 11, stop = 12))
}
just.minute <- function(timeStr) {
     minute <- as.numeric(substr(timeStr, start = 14, stop = 15))
}
just.second <- function(timeStr) {
     second <- as.numeric(substr(timeStr, start = 17, stop = 18))
}
find.startEndIdx <- function(givenStart, df) {
     tokens <- unlist(strsplit(givenStart, split = ":"))
     hour <- as.numeric(tokens[1])
     minute <- as.numeric(tokens[2])
     second <- as.numeric(tokens[3])
     
     first.chunk <- df[df$hour == hour, ]
     second.chunk <- first.chunk[first.chunk$minute == minute, ]
     final.chunk <- second.chunk[second.chunk$second == second, ]
     
     if(nrow(final.chunk) > 0) {
          exact.idx <- final.chunk$sortorder[1]
     } else {
          if(nrow(second.chunk) > 0) {
               exact.idx <- second.chunk$sortorder[1]
          } else {
               if(nrow(first.chunk) > 0) {
                    exact.idx <- first.chunk$sortorder[1]
               } else {
                    exact.idx <- 1
               }
          }
     }
     
     
     startIdx <- max(1, (exact.idx - (SCAN_DURATION)))
     endIdx <- min(nrow(df), (exact.idx + (SCAN_DURATION * 60)))
     data.frame(startIdx, endIdx)
}

walktime.finder <- function(df, ppt.walktimes.df, indices) {
     # Scanning begins
     window.length <- ppt.walktimes.df$meter400
     best.start <- 1
     best.end <- 1 + window.length
     nonZeroPrc <- 0
     for(i in seq(indices$startIdx, indices$endIdx, by = SLIDING_STEP)) {
          curr.start <- i
          curr.end <- min(indices$endIdx, (i + window.length))
          curr.nonZeroPrc <- (length(which(df$axis1[curr.start:curr.end] > 0)) * 100) / (curr.end - curr.start)
          if(!is.na(curr.nonZeroPrc)) {
               if(curr.nonZeroPrc > nonZeroPrc) {
                    best.start <- curr.start
                    best.end <- curr.end
                    nonZeroPrc <- curr.nonZeroPrc
               }
          }
     }
     
     rm(i, curr.start, curr.end, curr.nonZeroPrc)
     
     # Check if the results are satisfactory for axis 1
     if(nonZeroPrc >= NON_ZERO_THRESHOLD) {
          status <- "OK"
     } else { # If not, we should check axis 2
          alternative.start <- 1
          alternative.end <- 1 + window.length
          alternative.nonZeroPrc <- 0
          for(i in seq(indices$startIdx, indices$endIdx, by = SLIDING_STEP)) {
               curr.start <- i
               curr.end <- min(indices$endIdx, (i + window.length))
               curr.nonZeroPrc <- (length(which(df$axis2[curr.start:curr.end] > 0)) * 100) / (curr.end - curr.start)
               if(!is.na(curr.nonZeroPrc)) {
                    if(curr.nonZeroPrc > alternative.nonZeroPrc) {
                         alternative.start <- curr.start
                         alternative.end <- curr.end
                         alternative.nonZeroPrc <- curr.nonZeroPrc
                    }
               }
          }
          rm(i, curr.start, curr.end, curr.nonZeroPrc)
          if(alternative.nonZeroPrc >= NON_ZERO_THRESHOLD) {
               best.start <- alternative.start
               best.end <- alternative.end
               nonZeroPrc <- alternative.nonZeroPrc
               status <- "Found in axis 2"
          } else {
               status <- "Not fully met the criteria"
          }
          rm(alternative.end, alternative.nonZeroPrc, alternative.start)
     }
     
     
     result <- data.frame(PID,
                          found_walk_start = substr(df$datetime[best.start], start = 11, stop = nchar(df$datetime[best.start])),
                          found_walk_end = substr(df$datetime[best.end], start = 11, stop = nchar(df$datetime[best.end])),
                          written_start = ppt.walktimes.df$start,
                          accelerometer_file_start = substr(df$datetime[1], start = 11, stop = nchar(df$datetime[1])),
                          start_idx = best.start,
                          end_idx = best.end,
                          status)
     result
}