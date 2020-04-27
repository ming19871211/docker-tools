#维度的合并显示####
# list(list(list(),dims=c()),list())
mergeShowDim <- function(df,resdf,mergeShow){
  if(!missing(mergeShow)){
    colName <- names(resdf)
    for (mergeList in mergeShow) {
      if (length(mergeList[['dims']]) == 0)
        mergeDf <- dataQueriesMetrics(df,mergeList[[1]])[[1]]
      else
        mergeDf <- singleGroupQueries(df,mergeList[[1]],mergeList[['dims']])
      colName <- c(colName,tail(names(mergeDf),-length(mergeList[['dims']])))
      resdf <- merge(resdf,mergeDf,all.x=TRUE)
    }
    resdf <- resdf[,colName]
  }
  return(resdf)
}


#对一组维度进行查询####
singleGroupQueries <- function(df,measureList,dimension){
  
  #将字符串型的数字用#标记
  spLi <- list()
  
  #如果维度只有一个且这个维度的成员为字符型数字
  if(length(dimension) == 1 && !is.na(suppressWarnings(as.numeric(as.character(df[1,dimension]))))){
    #在这些维度成员前添加#
    df[,dimension] <- paste("#",df[,dimension],sep = "")
    spLi[length(spLi)+1] <- dimension
    groupCol <- dimension
  }else if(length(dimension) > 1){
    
    # #去掉维度成员两端的空格
    # library(stringr)
    # for(dim in dimension)
    #   df[,dim] <- str_trim(df[,dim],"both")
    
    #如果维度个数大于1，则将这些维度合并为groupCol列
    sers <- c(paste("paste(",dimension[1]),dimension[-1],"sep = '`%r%`')")
    sers <- paste(sers,collapse = ",")
    #添加compaName列
    df$groupCol <- with(df,eval(parse(text = sers)))
    groupCol <- "groupCol"
  }else{
    groupCol <- dimension
  }
  
  #指定范围的指标列
  for(i in 1:length(measureList)){
    #进行分组统计
    measure <- measureList[[i]]
    
    if (length(measure["dimensionScopes"][[1]]) > 0) {
      if (measure["dimensionScopes"][[1]][1] == 'all') {
        # df[measure["resultColumn"][[1]][1]] <- onlyMetrics(df,measure["measureList"][[1]])
        df[paste("scopeGroupCol",i,sep = "")] <- paste("scopeGroupCol",i,sep = "")
        measureList[[i]]$groupCol <- paste("scopeGroupCol",i,sep = "")
      }
        else {
        #将这些维度合并为groupCol列
        sers <- c(paste("paste(",measure["dimensionScopes"][[1]][1]),measure["dimensionScopes"][[1]][-1],"sep = '`%r%`')")
        sers <- paste(sers,collapse = ",")
        #添加指标指定范围列
        df[paste("scopeGroupCol",i,sep = "")] <- with(df,eval(parse(text = sers)))
        measureList[[i]]$groupCol <- paste("scopeGroupCol",i,sep = "")
      }
      
    }
  }
  
  #如果统计的度量只有一个，那么在tAaaplyDataFrame中去掉缺少值
  if(length(measureList) <= 1){
    resDataF <- singleMeasure(df,measureList[[1]],groupCol,TRUE)
  }else{
    resDataF <- multipleMeasure(df,measureList,groupCol)
  }
  
  #将添加#的列去除#
  if(length(spLi) > 0){
    spdim <- spLi[[1]]
    resDataF[,spdim] <- as.character(resDataF[,spdim])
    resDataF[,spdim] <- vapply(strsplit(resDataF[,spdim],"#"),function(x){x[2]},FUN.VALUE = c(""))
  }else if(length(dimension) > 1){
    library(splitstackshape)
    colNa <- names(resDataF)[-1]
    resDataF <- as.data.frame(cSplit(resDataF,"groupCol", "`%r%`",drop = TRUE, stripWhite = FALSE))
    names(resDataF) <- c(colNa,dimension)
    resDataF <- resDataF[,c(dimension,colNa)]
  }
  
  #将因子列转变为向量列
  for(dim in dimension)
    resDataF[,dim] <- as.character(resDataF[,dim])
  
  #对查询结果简单排序
  resDataF <- resDataF[order(resDataF[,dimension[1]]),]
  return(resDataF)
}


#分组统计，返回一个指标的矩阵####
singleMeasure <- function(df,measure,dimension,rmNa){
  
  if(!require("reshape2"))
    stop("not install RMySQL")
  
  library(reshape2)
  
  dimesionName <- dimension
  #如果指标自定义统计维度范围
  if(length(measure["dimensionScopes"][[1]]) > 0) {
    
    dimesionName <- measure["groupCol"][[1]]
  }
  
  #进行分组统计
  resTapply <- resolveMetrics(df,measure,dimesionName)
  
  #得到指标列名称
  #columnName = measureColumnNames(measure)
  columnName = measure["resultColumn"][[1]]
  #只有维度的情况下虚拟字段
  # if (measure["resultColumn"][[1]] == "virtualMetrics") {
  #   columnName = measure["resultColumn"][[1]]
  # }
  
  #调用reshape2包melt转换resTapply
  dfNames <- c(dimesionName,columnName)
  resdf <- melt(resTapply,value.name = columnName,na.rm = rmNa)
  names(resdf) <- dfNames
  #如果指标自定义统计维度范围
  if(length(measure["dimensionScopes"][[1]]) > 0) {
    #只有指标查询，传入的维度为空
    if (is.null(dimension)) {
      resdf <- resdf[,c(columnName)]
    } else {
      uniqueData <- unique(df[,c(dimension,dimesionName)])
      if (length(which(is.na(uniqueData))) == 0) {
        resdf <- merge(uniqueData,resdf, c(dimesionName = dimesionName))[,c(dimension,columnName)]
      } else {
        resdf <- merge(uniqueData[-which(is.na(uniqueData)),],resdf, c(dimesionName = dimesionName))[,c(dimension,columnName)]
      }
      
    }
  }
  return(resdf)
}


#多指标统计####
multipleMeasure <- function(df,measureList,dimension){
  #如果统计的度量一大于1个，那么首先计算第一个度量，得到数据框
  resDataF <- singleMeasure(df,measureList[[1]],dimension,FALSE)
  
  #columnsNames为度量列名称向量
  # columnsNames <- c(measureColumnNames(measureList[[1]]))
  measure <- measureList[[1]]
  columnsNames <- measure["resultColumn"][[1]]
  
  for(i in 2:length(measureList)){
    #进行分组统计
    measure <- measureList[[i]]
    # resTapply <- resolveMetrics(df,measureList[[i]],dimension)
    resDataF_ <- singleMeasure(df,measureList[[i]],dimension,FALSE)
    
    #columnsNames为度量列名称向量
    columnName <- measure["resultColumn"][[1]]
    columnsNames <- c(columnsNames,columnName)
    resDataF <- merge(resDataF,resDataF_, c(dimension = dimension))
    # resDataF[,columnName] <- as.vector(resTapply) 
  }
  
  #如果存在缺失值则处理缺失值
  if(sum(is.na(resDataF)) > 0){
    #有缺失值，并且表计算为空，则处理缺失值
    resDataF <- melt(resDataF,id.vars = dimension,measure.vars = unique(columnsNames),na.rm = TRUE)
    castName <- paste(paste(dimension,collapse = "+"),"variable",sep = " ~ ")
    resDataF <- dcast(resDataF,eval(parse(text = castName)))
  }
  
  return(resDataF)
}

#分层排序####
sortStratifie <- function(resdf,myorder,dimGropList,dimkeyList){
  if(!missing(myorder)){
    lastDim <- rev(dimGropList)[[1]]                      # 最后一层的维度
    lastDimKey <- rev(dimkeyList)[[1]]                    # 最后一层的维度key
    orderDim <- sapply(myorder, function(x){x$column})    # 需要排序的列名集合
    newMyorder <- list()
    for (i in 1:length(orderDim)){
      index <- which(lastDim == orderDim[i])
      len <- length(newMyorder)
      if (length(index) > 0){
        if(len < index - 1){
          for(j in (len+1):(index-1)){
            value <- unique(resdf[,lastDimKey[j]])
            value <- value[order(value,na.last = FALSE)]
            newMyorder[[j]] <- list(column=lastDimKey[j],type='1',values=value)
          }
        }
        oldOrder <- myorder[[i]]
        oldOrder$column <- lastDimKey[index]
        newMyorder[[index]] <- oldOrder
      }else{
        if (len < length(lastDimKey) - 1){
          for(j in (len+1):length(lastDimKey)){
            value <- unique(resdf[,lastDimKey[j]])
            value <- value[order(value,na.last = FALSE)]
            newMyorder[[j]] <- list(column=lastDimKey[j],type='1',values=value)
          }
        }
        newMyorder[[length(newMyorder)+1]] <- myorder[[i]]
      }
    }
    resdf <- sortDataFrame(newMyorder,resdf,TRUE)
  }
  return(resdf)
}

#对结果排序####
sortDataFrame <- function(myorder,resdf,stratifie){
  columns <- names(resdf)     #获取数据框的列名
  if(!missing(myorder)){
    #加载plyr包，用于逆序排序
    require(plyr)
    orderlist <- list()
    orderStr <- ""
    for(i in 1:length(myorder)){
      myorderOne <- myorder[[i]]
      if(myorderOne['column'][[1]] %in% columns){
        #当排序列是维度时，用传入的维度值顺序排序
        if(myorderOne['type'][1]==0){
          # 默认排序
          sta <- resdf[,myorderOne['column'][[1]]]
        }else if(myorderOne['type'][1]==1){
          # 自定义排序
          sta <- factor(resdf[,myorderOne['column'][[1]]],levels = myorderOne['values'][[1]])
        }else{
          # 顺序排序
          if(is.character(resdf[,myorderOne['column'][[1]]]) || is.factor(resdf[,myorderOne['column'][[1]]])){
            # 如果排序的列为字符串或因子，则将其转换为配音再排序。在有些系统中由于系统编码不同，导致排序的方式不同
            library(tmcn)
            if(myorderOne['orders'][1]=='desc'){
              #逆序
              sta <- desc(vapply(resdf[,myorderOne['column'][[1]]], toPinyin, FUN.VALUE = c('')))
            }else{
              #正序
              sta <- vapply(resdf[,myorderOne['column'][[1]]], toPinyin, FUN.VALUE = c(''))
            }
          }else{
            if(myorderOne['orders'][1]=='desc'){
              #逆序
              sta <- desc(resdf[,myorderOne['column'][[1]]])
            }else{
              #正序
              sta <- resdf[,myorderOne['column'][[1]]]
            }
          }
        }
        
        #将排序的结果放入集合中
        orderlist[[i]] <- sta
        #拼接排序字符串
        str <- paste(paste('orderlist[[',i,sep = ''),']]',sep = '')
        orderStr <- paste(orderStr,str,sep = ',')    
      }
    }
    if(orderStr == "")
      return(resdf)
    #拼接排序字符串
    orderStr <- substr(orderStr,2,nchar(orderStr))
    if (!missing(stratifie) && stratifie){
      orderStr <- paste('order(',orderStr,',na.last = FALSE)',sep = '')
    }else{
      orderStr <- paste('order(',orderStr,')',sep = '')
    }
    #将字符串转变为命令行
    resdf <- resdf[eval(parse(text = orderStr)),]
    resdf <- data.frame(resdf)
  }else{
    resdf <- resdf[order(resdf[,1]),]
    if(!is.data.frame(resdf))
      resdf <- as.data.frame(resdf)
  }
  names(resdf) <- columns
  return(resdf)
}


#对原始数据过滤####
#list(measureList=list(),noagRe="",aggRel="")
oriFilter <- function(df,dimension=c(),doWhere){
  if(!missing(doWhere) && length(doWhere[['oriFilter']]) > 0){
    ori <- doWhere[['oriFilter']]
    
    if (length(ori[['topColumn']]) > 0) {
      for(topColumn in ori[['topColumn']]){
        df <- eval(parse(text = topColumn))
      }
    }
    
    if(length(ori[['noagRe']]) > 0){
      with(df,{df <<- df[eval(parse(text = ori[['noagRe']])),]})
    }
    if(length(ori[['measureList']]) > 0){
      if(missing(dimension) || length(dimension) == 0){
        resdf <- onlyMetrics(df,ori[['measureList']])
      }else{
        resdf <- singleGroupQueries(df,ori[['measureList']],dimension)
      }
      with(resdf,{resdf <<- resdf[eval(parse(text = ori[['aggRel']])),]})
      df <- merge(df,resdf)
    }
    
    #将数据中的缺失维度成员行删除
    for(dim in dimension){
      if(dim %in% names(df)){
        df <- df[!is.na(df[,dim]),]
      }
    }
  }
  return(df)
}

#对结果集过滤####
#list(relation="",aggcolumn=c())
resFilter <- function(df,dimension = c(),doWhere){
  name <- names(df)
  #结果集过滤条件不为TRUE时，进行结果集过滤
  if(!missing(doWhere) && length(doWhere[['resFilter']]) > 0){
    
    #将数据中的缺失维度成员行删除
    #（之后也删除，是因为linux加载的数据为空，记录为NA,
    #做top时根据NA统计后被删除了，所以这里加了一遍）
    for(dim in dimension){
      df <- df[!is.na(df[,dim]),]
    }
    if (length(doWhere[['topColumn']]) > 0) {
      for(topColumn in doWhere[['topColumn']]){
        df <- eval(parse(text = topColumn))
      }
    }
    
    res <- doWhere[['resFilter']]
    tryCatch(with(df,df <<- df[eval(parse(text = res[['relation']])),!(names(df) %in% res[['aggcolumn']])]),
             warning = function(w) {"出警告啦"},error = function(e) { "出错啦"})
    #将数据中的缺失维度成员行删除
    for(dim in dimension){
      df <- df[!is.na(df[,dim]),]
    }
  }
  if(!is.data.frame(df) && length(df) > 0){
    df <- as.data.frame(df)
    names(df) <- name[!(name %in% res[['aggcolumn']])]
  }
  return(df)
}

#模糊过滤####
fuzzyFilter <- function(df,colName,filterValues){
  isFaArr <- c()
  for (value in filterValues) {
    isFa <- grepl(value,df[,colName])
    if (length(isFaArr) == 0)
      isFaArr <- isFa
    else
      isFaArr <- isFaArr|isFa
  }
  return(isFaArr)
}

#分组过滤函数####
filterDataFrame <- function(df,dimGropList,doWhere,dffilter){
  #在查询统计之前对数据框先进行过滤
  dffilter <<- oriFilter(df,dimGropList,doWhere=doWhere)
}


#表计算与合计####
tableCompute_SubTotal <- function(resdf,df,seriesDim,dimension,measureList,i,queryTotalObj,tableCalculaList,mergeShow){
  #对每个系列做表计算和合计
  if(length(seriesDim) > 0){
    #将结果集根据系列分块
    seriesSplitDf <- split(resdf,resdf$compaName)
    #将原始数据根据系列分块
    originSplitDf <- split(df,df$compaName)
  }else{
    seriesSplitDf <- list(resdf)
    originSplitDf <- list(df)
  }
  
  #表计算
  if(!missing(tableCalculaList)){
    if(i == 1){
      for(tableCalcula in tableCalculaList){
        
        tableCalcula <- resTableCompute(df,tableCalcula,measureList,seriesDim)
        
        paramlist <- tableCalcula[["paramlist"]]               #参数列表
        tableDimen <- tableCalcula[["tableDimen"]]             #维度列（计算依据）
        tableMeasure <- tableCalcula[["tableMeasure"]]         #计算指标
        tableComputeType <- tableCalcula[["tableComputeType"]] #计算类型
        
        seriesSplitDf <- lapply(seriesSplitDf,function(x){
          if(length(paramlist[['deno']]) > 0){
            rowName <- row.names(x)
            colName <- names(x)
            x[,ncol(x)+1] <- c(1:nrow(x))
            names(x)[ncol(x)] <- "ordernumber"
            x <- merge(x,paramlist[['deno']],all.x=TRUE)
            x <- x[order(x$ordernumber),]
            x <- x[,c(colName,"value")]
            row.names(x) <- rowName
          }
          x <- tableCompute(x,tableDimen,tableMeasure,tableComputeType,paramlist)
        })
      }
      
      if(length(seriesDim) > 0)
        resdf <- unsplit(seriesSplitDf,resdf$compaName)
      else
        resdf <- seriesSplitDf[[1]]
    }else{
      measureNames <- sapply(tableCalculaList,function(y){y[['paramlist']][['newcolumn']]})
      sourceColumn <- sapply(tableCalculaList,function(x){x[['sourceColumn']]} )
      resdf[,measureNames] <- resdf[,sourceColumn]
    }
  }
  
  subtotalDf <<- data.frame()
  #计算合计与小计
  if(!missing(queryTotalObj)){
    splitIndex <<- 1
    lapply(seriesSplitDf,function(x){
      #在每组维度中去除多余的合计计算的维度
      queryTotalObj[['rowDim']] <- intersect(queryTotalObj[['rowDim']],dimension)
      queryTotalObj[['colDim']] <- intersect(queryTotalObj[['colDim']],dimension)
      subDf <- tableSubtotal(x,originSplitDf[[splitIndex]],measureList,queryTotalObj) 
      if(nrow(subDf) > 0){
        if(length(seriesDim) > 0){
          subDf$compaName <- x[1,"compaName"]
          for(ser in seriesDim){
            subDf[,ser] <- x[1,ser]
          }
        }
        subtotalDf <<- rbind.fill(subtotalDf,subDf)
      }
      splitIndex <<- splitIndex + 1
    })
  
    if(!is.null(queryTotalObj[['sfOnlyTotal']]) && queryTotalObj[['sfOnlyTotal']]){
      resdf <- rbind.fill(resdf[1,],subtotalDf)
      return(resdf[-1,])
    }
  }
  
  if(nrow(subtotalDf) > 0){
    subtotalDf <- mergeShowDim(df,subtotalDf,mergeShow)
    resdf <- rbind.fill(resdf,subtotalDf)
  }
  
  return(resdf)
}

#在进行表计算前的处理####
resTableCompute <- function(df,tableCalcula,measureList,seriesDim){
  tableDimen <- tableCalcula["tableDimen"][[1]]             #维度列（计算依据）
  
  for (ele in measureList) {
    if(ele[["resultColumn"]] == tableCalcula[['sourceColumn']]){
      measure <- ele
      break
    }
  }
  
  tableComputeType <- tableCalcula["tableComputeType"][[1]] #计算类型
  paramlist <- tableCalcula["paramlist"][[1]]               #参数列表
  #如果计算类型为总额百分比，则将原始数据进行tapply分组聚合
  if(paramlist['tableComputeType'][[1]]=='totalPercent' || length(paramlist[['relativeToScope']]) > 0){
    deno <- data.frame()
    measureOne <- measure[['measureList']][['measure']]     #度量
    
    if(length(paramlist[['relativeToScope']]) > 0){
      #进行相对于运算
      relativeToFun <- eval(parse(text = paramlist[['relativeTo']]))
      relativeToScope <- paramlist[['relativeToScope']]
      
      #相对于全量
      if(relativeToScope[1] == "FULL"){
        deno[1,1] <- relativeToFun(df[,measureOne],na.rm = TRUE)
        names(deno) <- c('value')
      }else{
        tap <- tapply(df[,measureOne],df[,relativeToScope],relativeToFun,na.rm = TRUE)
        tableDimen[['blockdim']] <- relativeToScope        #将分块维度设为选定的维度
        tableDimen[['orderdim']] <- c()                    #将排序维度设为空，特定维度不需要排序维度
        tableCalcula[['tableDimen']] <- tableDimen
        
        deno <- melt(tap)
        names(deno) <- c(relativeToScope,"value")
      }
    }else{
      #进行总额百分比前的运算
      blockdim <- tableDimen['blockdim'][[1]]
      if(is.null(blockdim) && length(seriesDim) == 0){
        deno <- dataQueriesMetrics(df,list(measure))[[1]]
        names(deno) <- c('value')
      }else{
        if(is.null(blockdim)){
          blockdim <- "compaName"
        }else if(length(seriesDim) > 0){
          blockdim <- c(blockdim,"compaName")
        }
        tap <- resolveMetrics(df,measure,blockdim)
        deno <- melt(tap)
        names(deno) <- c(blockdim,'value')
      }
    }
    paramlist[['deno']] <- deno
    tableCalcula[["paramlist"]] <- paramlist
  }
  
  return(tableCalcula)
}
