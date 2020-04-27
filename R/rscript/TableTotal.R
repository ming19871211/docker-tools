# 小计与总计####
# df            数据源
# dfOrig        原始数据
# rowDim        行维度          c('','')
# colDim        列维度          c('','')
# measures      做合计的度量    c('','')
# measureList   度量列表，包括度量名称以及公式
# aggType       汇总类型        c(1,2,3)  1——小计;2——列总计;3——行总计
# basis         汇总依据        list(fun=auto,funName="auto")
#         fun   值：auto——自动;sum——总计;mean——平均;max——最大;min——最小
tableSubtotal <- function(df,dfOrig,measureList,queryTotalObject){
  #plyr包对数据进行分割—处理—汇总
  library(plyr)
  rowDim <- queryTotalObject[['rowDim']]      #行维度
  colDim <- queryTotalObject[['colDim']]      #列维度
  measures <- queryTotalObject[['measures']]  #计算度量
  aggType <- queryTotalObject[['aggType']]    #汇总类型
  basis <- queryTotalObject[['basis']]        #汇总依据
  tatolTable <- data.frame()                  #合计表
  
  #将原始数据中的缺失维度成员行删除
  for(dim in c(rowDim,colDim)){
    dfOrig <- dfOrig[!is.na(dfOrig[,dim]),]
  }
  
  #进行小计与总计运算
  if(1 %in% aggType){
    #列小计
    tatolTable <- ColSubTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    #行小计
    tatolTable <- RowSubTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    #行列小计
    tatolTable <- Row_ColSubTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
  }
  #列总计
  if(2 %in% aggType){ 
    tatolTable <- ColTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    #行小计+列总计
    if(1 %in% aggType){
      tatolTable <- RowS_ColTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    }
  }
  #行总计
  if(3 %in% aggType){
    tatolTable <- RowTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    #列小计+行总计
    if(1 %in% aggType){
      tatolTable <- ColS_RowTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    }
    #行总计+列总计
    if(2 %in% aggType){
      tatolTable <- Row_ColTotal(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis)
    }
  }
  return(tatolTable)
  #return(tableTotalRbind(df,tatolTable,c(rowDim,colDim)))
}

#合并合计数据####
tableTotalRbind <- function(resdf,tatolTable,dimension){
  if(length(tatolTable) != 0){
    #将合计的数据合并
    resdf <- rbind.fill(resdf,tatolTable)
    dfDim <- resdf[,dimension]
    #如果维度只有一列，则要将dfDim转变为数据框
    if(length(dimension) == 1){
      dfDim <- as.data.frame(dfDim)
      names(dfDim) <- dimension
    }
    for (i in length(dimension):1) {
      Dim <- dimension[i]
      dfDim[,Dim] <- as.character(dfDim[,Dim])
    }
    #dfDim[is.na(dfDim)] <- "-"
    resdf[,dimension] <- dfDim
  }
  return(resdf)
}

#计算列小计####
ColSubTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  rowDimnum <- length(rowDim)
  while (rowDimnum > 1) {
    rowDimnum <- rowDimnum - 1
    #分组维度
    groupDim <- c(rowDim[c(1:rowDimnum)],colDim)
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,groupDim,measures,measureList,basis)
    #标记小计列
    subtaDa[,rowDim[rowDimnum+1]] <- c(rep("小计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#计算行小计####
RowSubTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  colDimnum <- length(colDim)
  while (colDimnum > 1) {
    colDimnum <- colDimnum - 1
    #分组维度
    groupDim <- c(colDim[c(1:colDimnum)],rowDim)
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,groupDim,measures,measureList,basis)
    #标记小计列
    subtaDa[,colDim[colDimnum+1]] <- c(rep("小计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#行小计+列小计####
Row_ColSubTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  rowi <- length(rowDim)
  while (rowi > 1) {
    rowi <- rowi - 1
    coli <- length(colDim)
    while (coli > 1) {
      coli <- coli - 1
      groupDim <- c(colDim[c(1:coli)],rowDim[c(1:rowi)])
      #进行合计运算
      subtaDa <- getSubtaDa(df,dfOrig,groupDim,measures,measureList,basis)
      #标记小计列
      subtaDa[,colDim[coli+1]] <- c(rep("小计",nrow(subtaDa)))
      subtaDa[,rowDim[rowi+1]] <- c(rep("小计",nrow(subtaDa)))
      #合并数据框
      tatolTable <- rbind.fill(tatolTable,subtaDa)
    }
  }
  return(tatolTable)
}

#计算列总计####
ColTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  #当行维度为空时，没有列总计
  if(length(rowDim) > 0){
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,colDim,measures,measureList,basis)
    #标记小计列
    subtaDa[,rowDim[1]] <- c(rep("总计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#计算行总计####
RowTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  #当列维度为空时，没有行总计
  if(length(colDim) > 0){
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,rowDim,measures,measureList,basis)
    #标记小计列
    subtaDa[,colDim[1]] <- c(rep("总计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#列小计+行总计####
ColS_RowTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  rowDimnum <- length(rowDim)
  while(rowDimnum > 1 && length(colDim) > 0){
    rowDimnum <- rowDimnum - 1
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,rowDim[c(1:rowDimnum)],measures,measureList,basis)
    #标记小计列
    subtaDa[,colDim[1]] <- c(rep("总计",nrow(subtaDa)))
    subtaDa[,rowDim[rowDimnum + 1]] <- c(rep("小计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#行小计+列总计####
RowS_ColTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  colDimnum <- length(colDim)
  while(colDimnum > 1 && length(rowDim) > 0){
    colDimnum <- colDimnum - 1
    #进行合计运算
    subtaDa <- getSubtaDa(df,dfOrig,colDim[c(1:colDimnum)],measures,measureList,basis)
    #标记小计列
    subtaDa[,rowDim[1]] <- c(rep("总计",nrow(subtaDa)))
    subtaDa[,colDim[colDimnum + 1]] <- c(rep("小计",nrow(subtaDa)))
    #合并数据框
    tatolTable <- rbind.fill(tatolTable,subtaDa)
  }
  return(tatolTable)
}

#行总计+列总计####
Row_ColTotal <- function(df,dfOrig,tatolTable,rowDim,colDim,measures,measureList,basis){
  if((length(rowDim) * length(colDim)) == 0)
    return(tatolTable)
  
  if(basis[['funName']] == 'auto'){
    subtaDa <- dataQueriesMetrics(dfOrig,measureList)[[1]]
    for (measure in setdiff(measures,names(subtaDa))) {
      subtaDa[1,measure] <- edusum(df[,measure])
    }
  }else{
    subtaDa <- data.frame()
    for (measure in measures) {
      subtaDa[1,measure] <- basis[['fun']](df[,measure],na.rm = TRUE)
    }
  }
  #标记小计列
  subtaDa[1,rowDim[1]] <- "总计"
  subtaDa[1,colDim[1]] <- "总计"
  #合并数据框
  tatolTable <- rbind.fill(tatolTable,subtaDa)
  return(tatolTable)
}

#进行合计运算####
getSubtaDa <- function(df,dfOrig,groupDim,measures,measureList,basis){
  #当汇总依据为自动且分组维度为空时
  if(basis[['funName']] == 'auto' && (is.null(groupDim) || length(groupDim) == 0)){
    subtaDa <- dataQueriesMetrics(dfOrig,measureList)[[1]]
    for (measure in setdiff(measures,names(subtaDa))) {
      subtaDa[1,measure] <- edusum(df[,measure])
    }
  }else{
    #循环度量列表，依次进行合计运算
    for (i in 1:length(measures)) {
      #如果汇总依据为自动，则对原始数据进行度量运算
      isFa <- TRUE
      if(basis[['funName']] == 'auto'){
        basis[['fun']] <- edusum
        for (measureLi in measureList) {
          if(measureLi[["resultColumn"]] == measures[i]){
            subtaTap <- resolveMetrics(dfOrig,measureLi,groupDim)
            isFa <- FALSE
            break
          }
        }
      }
      #当汇总依据不为自动或者需要汇总的度量不在度量列表中
      if(isFa){
        if(is.null(groupDim) || length(groupDim) == 0){
          subtaTap <- basis[['fun']](df[,measures[i]],na.rm = TRUE)
        }else{
          subtaTap <- tapply(df[,measures[i]], df[,groupDim], basis[['fun']],na.rm = TRUE)
        }
      }
      #对分组后的数据melt
      if(i == 1){
        subtaDa <- melt(subtaTap,na.rm = TRUE)
        names(subtaDa) <- c(groupDim,"value")
      }else{
        da <- melt(subtaTap,na.rm = TRUE)
        names(da) <- c(groupDim,"value")
        if(length(groupDim) == 1){
          subtaDa <- merge(subtaDa,da,by=names(subtaDa)[1],all=T)
        }else{
          subtaDa <- merge(subtaDa,da,by=groupDim,all=T)
        }
      }
    }
    names(subtaDa) <- c(groupDim,measures)
  }
  return(subtaDa)
}