install.packages("XLConnect") # This library should be installed to be able to read the .xlsx files.
library(XLConnect) # Loading the library.

#####################################################################
### This script provides an example how to run the code to obtain ###
### axes' summations for the defined start and end times (when    ###
### walking happened for the visit).                              ###
### ------------------------------------------------------------- ###
### Author:                                                       ###
###          Matin Kheirkhahan (matin@cise.ufl.edu)               ###
#####################################################################

source("f02_axes_summation.R")

xlsx_file_start_and_end <- "update_walktime_found0518.xlsx"
dataFiles_folder <- "~/../../Volumes/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/"
visit_seq <- 6
outputFile_csv <- "axes_sum.csv"
axes.sum.df <- generate.axes.sum(xlsx_file_start_and_end, dataFiles_folder, visit_seq, outputFile_csv)
