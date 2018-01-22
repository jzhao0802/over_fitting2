# functions for create descriptive tables for univariate type for categorical variables
# author - Jie Zhao
# Sep26-27, 2016

mySprintf <- function(digit, numKept){
  digitFormated <- sprintf(paste0("%1.", numKept, 'f%%'), 100*digit)
  return(digitFormated)
}

getVarType <- function(data, varList){
  dt <- data[, varList]
  varClass <- sapply(dt, function(x)class(x))
  lgVars <- varList[varClass=="logical"] #2 vars are all NA
  charVars <- varList[varClass %in% c("character", "factor")] #5
  numVars <- varList[varClass %in% c('numeric', 'integer')] #406
  typeList <- list(lgVars=lgVars, charVars=charVars, numVars=numVars)
  return(typeList)
}

renameVars <- function(varList, charVars, numVars, nonChar_nonNum_varList, lgVars){
  varList[match(charVars, varList)] <- 
    paste0('c_', charVars)
  varList[match(numVars, varList)] <- 
    paste0('n_', numVars)
  varList[match(nonChar_nonNum_varList, varList)] <-
    paste0('o_', nonChar_nonNum_varList)
  varList[match(lgVars, varList)] <- 
    paste0('na_', lgVars)
  return(varList)
}

create_overviewTb <- function(inDir, outDir, outFileNm, bTest, inFileNm, inFileExt, na_represents
                              , nonChar_nonNum_varList){
  dt <- read.table(paste0(inDir, inFileNm, inFileExt)
                   , sep=','
                   , header = T
                   , stringsAsFactors = F
                   , na.strings = na_represents)
  
  cat("raw data readin successfully!\n")
  varListAll <- names(dt)
  varList4DescTb <- setdiff(varListAll, nonChar_nonNum_varList)
  
  # identify the numerical vars and categorical vars
  varTypeLst <- getVarType(data=dt, varList=varList4DescTb)
  lgVars <- varTypeLst$lgVars
  charVars <- varTypeLst$charVars
  numVars <- varTypeLst$numVars
  if(bTest==T){
    if(length(lgVars)+length(charVars)+length(numVars)!=length(varList4DescTb)){
      stop("variable number not matched after type classification!\n")
    }
  }
  # renames variables
  varListRenamed <- 
    renameVars(varList=varListAll
               , charVars=charVars
               , numVars=numVars
               , nonChar_nonNum_varList=nonChar_nonNum_varList
               , lgVars=lgVars)
  if(bTest==T){
    if(length(
      varListRenamed[!grepl('^[c|o|na|n]_'
                            , varListRenamed
                            , ignore.case = T
      )
      ]
    )>0){
      stop("there is still variables that have not been renamed!\n")
    }
  }
  
  saveRDS(varListRenamed, paste0(outDir, 'varListRenamed.RDS'))
  # create OVERVIEW table
  summaryTb <- cbind(col1=c('num_of_obs'
                            , 'num_of_numerical'
                            , 'num_of_categorical')
                     , col2=c(nrow(dt)
                              , length(numVars)
                              , length(charVars)
                     )
  )
  if(length(lgVars)>0){
    summaryTb <- rbind(summaryTb, c('num_of_NA', length(lgVars)))
  }
  
  # table for details of variable type
  firstLetter <- gsub('(^\\w{1,2})_(.+$)', '\\1', varListRenamed, ignore.case = T, perl=T)
  typeTb <- cbind(variable_name=varListAll
                  , var_type=ifelse(firstLetter=='n'
                                    , 'numerical'
                                    , ifelse(firstLetter=='c'
                                             , 'categorical'
                                             , ifelse(firstLetter=='o'
                                                      , 'other'
                                                      , 'all_missing')
                                    )
                  )
  )
  
  # concatenate two tables
  names(typeTb) <- names(summaryTb)
  identical(names(typeTb), names(summaryTb))
  overviewTb <- rbind(summaryTb, c('variable_name', 'variable_type'), typeTb)
  write.table(overviewTb
              , file=paste0(outDir, outFileNm, '.csv')
              , row.names = F
              , sep=','
  )
}



univar_cat_4eachVar <- function(v, dt, levels_max){
  vct <- dt[, v]
  num_of_missing <- sum(is.na(vct))
  
  vct_nomissing <- vct[!is.na(vct)]
  tb <- table(vct_nomissing)
  desc4eachLv <- unlist(
    lapply(1:length(tb), function(i.lv){
      nm.lv <- names(tb)[i.lv]
      obsNum.lv <- tb[i.lv]
      obsPct.lv <- obsNum.lv/length(vct_nomissing)
      return(c(nm.lv, obsNum.lv, obsPct.lv))
    }
    )
  )
  line4Tb <- c(sum(!is.na(vct))
               , sum(!is.na(vct))/nrow(dt)
               , num_of_missing
               , num_of_missing/nrow(dt)
               , length(tb)
               , desc4eachLv
               , rep(NA, 3*(levels_max-length(tb)))
  )
  
  return(line4Tb)
  
}
create_univarCatTb <- function(inDir, outDir, outFileNm, bTest, inFileNm, inFileExt, na_represents
                               , nonChar_nonNum_varList, numKept)
{
  dt <- read.table(paste0(inDir, inFileNm, inFileExt)
                   , sep=','
                   , header = T
                   , stringsAsFactors = F
                   , na.strings = na_represents)
  
  cat("raw data readin successfully!\n")
  varListRenamed <- readRDS(paste0(outDir, 'varListRenamed.RDS'))
  names(dt) <- varListRenamed
  varList_cat <- grep('^c_\\w+.+$', varListRenamed, ignore.case = T, perl = T, value = T)
  
  # check the levels of variables
  levels_summary <- unlist(lapply(varList_cat, function(v)length(table(dt[, v]))))
  levels_max <- max(levels_summary)
  if(bTest){
    if(levels_max > 20){
      cat('there are categorical variables that have more than 20 levels. Is that what you expexted?\n')
    }
  }
  
  temp <- lapply(varList_cat, function(v)univar_cat_4eachVar(v, dt, levels_max))
  
  tb.df <- ldply(temp, quickdf)
  
  names(tb.df) <- c("num_of_nonmissing"
                    , "pc_of_nonmissing"
                    , "num_of_missing"
                    , "pc_of_missing"
                    , 'num_of_levels'
                    , unlist(lapply(1:levels_max, function(i.lv)c(paste0("level_", i.lv)
                                                                  , paste0("level_", i.lv, '_num_of_obs')
                                                                  , paste0("level_", i.lv, '_pc_of_obs')
                    )
                    )
                    )
  )
  
  tb.df4QC <- tb.df %>%
    .[, -grep('^level_\\d+$', names(.), ignore.case = T)] %>%
    sapply(as.numeric) %>%
    as.data.frame
  
  if(bTest){
    if(any(apply(tb.df4QC[, c('num_of_missing', 'num_of_nonmissing')], 1, sum)!=nrow(dt))){
      stop("number of missing or nonmissing is wrong!\n")
    }
    if(any(apply(tb.df4QC[, c('pc_of_nonmissing', 'pc_of_missing')], 1, sum) != 1)){
      stop('percentage of missing or nonmising is wrong!\n')
    }
    sum_levels <- apply(
      tb.df4QC[, grep('^level_\\d+_num_of_obs$'
                   , names(tb.df4QC)
                   , ignore.case = T
                   , value = T
                   )
            ]
      , 1
      , sum, na.rm=T
      )
    if(any(sum_levels != tb.df4QC$num_of_nonmissing)){
      stop("sum of the obs number of all the effective levels is not equal to the number of nonmissing!\n")
    }
  }
  
  tb.df$pc_of_nonmissing <- mySprintf(as.numeric(tb.df$pc_of_nonmissing), numKept)
  tb.df$pc_of_missing <- mySprintf(as.numeric(tb.df$pc_of_missing), numKept)
  
  tb.df <- cbind("var_name"=varList_cat
                 , tb.df)
  
  write.table(tb.df
              , file=paste0(outDir, outFileNm, '.csv')
              , sep=','
              , row.names = F)
  
}

univar_num_4eachVar <- function(v, dt, qtlCuts, numKept){
  vct <- dt[, v]
  num_of_missing <- sum(is.na(vct))
  vct_nonmissing <- vct[!is.na(vct)]
  qtl <- quantile(vct_nonmissing, prob=qtlCuts/100)
  
  line4Tb <- c(sum(!is.na(vct))
               , sum(!is.na(vct))/nrow(dt)
               , num_of_missing
               , num_of_missing/nrow(dt)
               , round(mean(vct_nonmissing), numKept)
               , round(sd(vct_nonmissing), numKept)
               , min(vct_nonmissing)
               , qtl
               , max(vct_nonmissing)
  )
  
  return(line4Tb)
  
}


create_univarNumTb <- function(inDir, outDir, outFileNm, bTest, inFileNm, inFileExt, na_represents
                               , nonChar_nonNum_varList, qtlCuts, numKept){
  
  dt <- read.table(paste0(inDir, inFileNm, inFileExt)
                   , sep=','
                   , header = T
                   , stringsAsFactors = F
                   , na.strings = na_represents)
  
  cat("raw data readin successfully!\n")
  varListRenamed <- readRDS(paste0(outDir, 'varListRenamed.RDS'))
  names(dt) <- varListRenamed
  varList_num <- grep('^n_\\w+.+$', varListRenamed, ignore.case = T, perl = T, value = T)
  
  temp <- lapply(varList_num, function(v)univar_num_4eachVar(v, dt=dt, qtlCuts=qtlCuts, numKept=numKept))
  
  tb.df <- ldply(temp, quickdf)
  
  names(tb.df) <- c('num_of_nonmissing'
                    , 'pc_of_nonmissing'
                    , 'num_of_missing'
                    , 'pc_of_missing'
                    , 'mean'
                    , 'std'
                    , 'min'
                    , paste0('p', qtlCuts)
                    , 'max'
                    )
  
  if(bTest){
    if(any(apply(tb.df[, c('num_of_missing', 'num_of_nonmissing')], 1, sum)!=nrow(dt))){
      stop("number of missing or nonmissing is wrong!\n")
    }
    
    if(any(apply(tb.df[, c('pc_of_nonmissing', 'pc_of_missing')], 1, sum) != 1)){
      stop('percentage of missing or nonmising is wrong!\n')
    }
    
    if(any(tb.df$min > tb.df$max | tb.df$min > tb.df$mean | tb.df$mean > tb.df$max)){
      stop('min, mean and max are calculated wrong!\n')
    }
  }
  
  tb.df$pc_of_nonmissing <- mySprintf(tb.df$pc_of_nonmissing, numKept)
  tb.df$pc_of_missing <- mySprintf(tb.df$pc_of_missing, numKept)
  
  tb.df <- cbind("var_name"=varList_num
                 , tb.df)
  write.table(tb.df
              , file=paste0(outDir, outFileNm, '.csv')
              , sep=','
              , row.names = F)
  
}