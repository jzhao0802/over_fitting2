# main functions for creating descriptive tables for univariate type for categorical variables
# author - Jie Zhao
# Sep26-27, 2016

rm(list=ls())
library(plyr)
library(dplyr)
library(caret)

# 
main.wkDir <- "./"
setwd(main.wkDir)
source("./functions/funs_univar_descriptiveTb.R")

main.inDir <- "F:\\Jie\\MS\\01_Data\\"
main.bTest <- F
main.inFileNm <- "MS_decsupp_analset_20160701"
main.inFileExt <- ".csv"

main.na_represents <- c('', 'NA', 'unknown', 'ambiguous')

main.nonChar_nonNum_varList <- c('new_pat_id', 'firstdt', 'idx_dt', 'last_from_dt')

main.qtlCuts <- c(5, 10, 25, 50, 75, 90, 95, 99)

main.numKept <- 2

main.timeStamp <- as.character(Sys.time())
main.timeStamp <- gsub(":", ".", main.timeStamp)  # replace ":" by "."
main.outDir <- paste("./Results/",  main.timeStamp, "/", sep = '')
dir.create(main.outDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

create_overviewTb(inDir=main.inDir
                  , outDir=main.outDir
                  , outFileNm="OVERVIEW"
                  , bTest=main.bTest
                  , inFileNm=main.inFileNm
                  , inFileExt=main.inFileExt
                  , na_represents=main.na_represents
                  , nonChar_nonNum_varList=main.nonChar_nonNum_varList
                  )

create_univarCatTb(inDir=main.inDir
                   , outDir=main.outDir
                   , outFileNm="univar-CAT"
                   , bTest=main.bTest
                   , inFileNm=main.inFileNm
                   , inFileExt=main.inFileExt
                   , na_represents=main.na_represents
                   , nonChar_nonNum_varList=main.nonChar_nonNum_varList
                   , numKept=main.numKept
)

create_univarNumTb(inDir=main.inDir
                   , outDir=main.outDir
                   , outFileNm="univar-NUM"
                   , bTest=main.bTest
                   , inFileNm=main.inFileNm
                   , inFileExt=main.inFileExt
                   , na_represents=main.na_represents
                   , nonChar_nonNum_varList=main.nonChar_nonNum_varList
                   , qtlCuts=main.qtlCuts
                   , numKept=main.numKept
)