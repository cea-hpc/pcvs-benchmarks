#!/usr/bin/env Rscript
#
# Copyright (c) 2018      Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

library(stringr)

options(max.print=100000)

processdirectory <- function(dirname) {
    threaddata = list.files (dirname, pattern = "^threads_[[:digit:]]+-1$")
    print(paste("Processing data directory:", dirname, ", Number of different thread runs:", length(threaddata)))

    for (i in 1:length(threaddata)) {
        threadcount = gsub("threads_([[:digit:]]+)-1", "\\1", threaddata[i])
        print (paste("Processing data for thread count:", threadcount))
        datafiles = list.files(dirname, pattern = paste("^threads_", threadcount, "-", sep=""), full.names = TRUE)
        file_count = length(datafiles)

        masterdf <- data.frame()
        data = read.csv (datafiles[1], comment.char = '#')
        rows = length(data[,1])
        masterdf <- rbind(masterdf, data);

        # There are two columns we do not want to average (BpT, BxT). Everything else is fair game
        variable_count = ncol(masterdf) - 2
        tmp=array(dim=c(variable_count, rows, file_count))
        for (k in 1:variable_count) {
            tmp[k,,1] = data[[colnames(data)[k + 2]]]
            masterdf[[paste(colnames(data)[k + 2], "_stddev", sep="")]] = 1:rows
            masterdf[[paste(colnames(data)[k + 2], "_stderr", sep="")]] = 1:rows
        }
        
        for (j in 2:file_count) {
            data = read.csv (datafiles[j], skip = 11)
            for (k in 1:variable_count) {
                tmp[k,,j] = data[[colnames(data)[k + 2]]]
            }
        }
        
        for (k in 1:variable_count) {
            for (j in 1:rows) {
                tmp_mean = mean(tmp[k,j,])
                tmp_sd = sd(tmp[k,j,])
                tmp_err = tmp_sd / sqrt (file_count)
                masterdf[j,k+2] = tmp_mean
                masterdf[j,paste(colnames(data)[k + 2], "_stddev", sep="")] = tmp_sd
                masterdf[j,paste(colnames(data)[k + 2], "_stderr", sep="")] = tmp_err
            }
        }

        write.csv(masterdf, file=paste(dirname, "/threads", threadcount, ".csv", sep=""), fileEncoding="utf8")
    }
}

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
    stop("Need to provide the path to the base data directory")
}

datadirectory = args[1]

files=list.files (datadirectory, pattern = "rmamt_", recursive = TRUE, include.dirs = TRUE, no.. = TRUE, all.files = FALSE, full.names = TRUE)

if (length(files) == 0) {
    stop("Could not find any RMA-MT benchmark data")
}

for (i in 1:length(files)) {
    processdirectory(files[i])
}

