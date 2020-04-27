###单个维度的查询方法####
dataQueriesSingleDim <- function(df,dimension,dimkey,doWhere,myorder,seriesDim=c(),runType,mergeShow){
  #创建一个空的list用于保存结果集
  resList <- list()
  
  #对原始数据过滤
  df <- oriFilter(df,dimension,doWhere=doWhere)
  
  if (nrow(df) == 0){
    return(resList)
  }
  
  if(length(seriesDim) > 0 ){
    #拼接组合系列条件的字符串
    sers <- c(paste("paste(",seriesDim[1]),seriesDim[-1],"sep = ',')")
    sers <- paste(sers,collapse = ",")
    #添加compaName列
    df$compaName <- with(df,eval(parse(text = sers)))
    
    dimension <- c(dimension,seriesDim,'compaName')
    dimkey <- c(dimkey,seriesDim,'compaName')
  }
  
  #得到维度的成员列表
  resMember <- unique(df[,c(dimension)])
  if(length(dimension) == 1)
    resMember <- data.frame(resMember)
  #删除维度成员中的空值，并将其转换成数据框
  resdf <- as.data.frame(resMember[complete.cases(resMember),])
  
  #给数据框加上列标题
  names(resdf) <- c(dimension)
  
  #合并计算
  resdf <- mergeShowDim(df,resdf,mergeShow)
  
  #对数据框进行排序
  resdf <- sortDataFrame(myorder,resdf)
  #如果没有排序列，则进行默认排序，排序完成之后返回的是一个向量
  if(!missing(myorder)){
    resdf[,1] <- as.character(resdf[,1])
  }
  resdf <- resFilter(resdf,doWhere=doWhere)
  names(resdf) <- c(dimkey,tail(names(resdf),-length(dimkey)))
  
  #如果为设计模式且系列维度不为空，则返回范例数据
  if((missing(runType) || runType == 2) && length(seriesDim) > 0){
    
    #根据原始数据的第一行的系列维度值过滤,拼接过滤条件
    resdf <- resdf[resdf$compaName == resdf[1,'compaName'],]
  }
  
  resList[[1]] <- resdf
  return(resList)
}


###单个指标的查询方法####
dataQueriesMetrics <- function(df,measureList,doWhere,myorder,mergeShow){
  
  #创建一个空的list用于保存结果集
  l <- list()
  
  #对原始数据过滤
  dffilter <- oriFilter(df,doWhere=doWhere)
  
  #遍历度量列表，分别对度量进行运算
  for (ele in measureList) {
    
    #如果度量的公式类型为计算公式
    if(ele["metricsType"][[1]] != 3){
      measure <- ele["measureList"][[1]]
      res <- onlyMetrics(dffilter,measure)
      
      columnName <- measure["measure"][[1]]  
      #dataColumnName = paste(columnName,measure["exprName"][[1]],sep = "_")
      #dataColumnName = measureColumnNames(ele)
      dataColumnName = ele["resultColumn"][[1]] 
    }else{
      res <- noDimenstionFormula(dffilter,ele)
      dataColumnName <- ele["resultColumn"][[1]]  
    }
    
    l[dataColumnName] <- res
  }
  
  resdf <- as.data.frame(l)
  #合并计算
  resdf <- mergeShowDim(df,resdf,mergeShow)
  resList <- list()
  
  resdf <- sortDataFrame(myorder,resdf)
  resdf <- resFilter(resdf,doWhere=doWhere)
  #用指标的key值替换数据集相应的列名
  #resdf <- replaceMeasureKey(resdf,measureList)
  #对结果集过滤
  resList[[1]] <- resdf
 
  return(resList)
}

###多维度多指标查询方法####
# 参数列表
# df:操作数据框
# measureList:度量列表，包括度量名称以及公式
# dimGropList:查询的维度列表，如果为并列分析dimGropList的长度将大于>=2
# doWhere：过滤条件，过滤条件分为原始数据过滤和结果集过滤 list(oriFilter=list(),resFilter=list())
#     oriFilter: 原始数据过滤   list(measureList=list(),noagRe="",aggRel="")
#     resFilter: 结果集过滤     list(measureList=list(),relation="")
# myorder: 排序列
# tableCalculaList:表计算说明
dataQueries <- function(df,measureList,dimGropList,dimkeyList,doWhere,myorder,tableCalculaList,
                        queryTotalObj,comparison,seriesDim=c(),runType,mergeShow){
  
  #创建一个空的list用于保存结果集
  resList <- list()
  #维度key的长度
  keyslength <- length(dimkeyList)
  #遍历维度list，实会是一个维度组，一组维度构成list中的一个元素

  i <- 1
  for (dimension in rev(dimGropList)) {
    
    #对于在dimension中存在系列维度,
    #需要在统计运算时排除系列的维度，在计算结束后在添加该列，使计算速度更快
    #系列或对比中剩余的维度
    surplusDim <- setdiff(seriesDim,dimension)
    
    #对原始数据过滤
    df <- oriFilter(df,dimension,doWhere)
    
    if (nrow(df) == 0){
      return(resList)
    }
    
    #对一组维度进行查询
    resdf <- singleGroupQueries(df,measureList,c(dimension,surplusDim))
    
    #合并计算
    resdf <- mergeShowDim(df,resdf,mergeShow)
    
    #对数据进行排序
    resdf <- sortDataFrame(myorder,resdf)
    
    #对结果集过滤
    resdf <- resFilter(resdf,dimension,doWhere)
    
    #用指标的key值替换数据集相应的列名
    # resdf <- replaceMeasureKey(resdf,measureList)
    
    #如果存在系列条件，则在结果集上添加compaName列，值为系列条件以逗号间隔的组合
    #同时去掉系列维度，在做完表计算与合计后将compaName列以逗号拆分为系列维度
    seriesDimRe <- c()
    if(length(seriesDim) > 0 ){
      #去掉维度成员两端的空格
      # library(stringr)
      # for(dim in seriesDim)
      #   df[,dim] <- str_trim(df[,dim],"both")
      #拼接组合系列条件的字符串
      sers <- c(paste("paste(",seriesDim[1]),seriesDim[-1],"sep = ',')")
      sers <- paste(sers,collapse = ",")
      #添加compaName列
      resdf$compaName <- with(resdf,eval(parse(text = sers)))
      #同时在原始数据中添加compaName列
      df$compaName <- with(df,eval(parse(text = sers)))
      #将系列维度添加到数据框中，如果数据框中原本存在该列，则再次添加会改变系列的列名，在后续需要重新命名
      resdf <- resdf[,c(setdiff(names(resdf),surplusDim),seriesDim)]
      colNameLen <- length(names(resdf))
      seriesDimRe <- names(resdf)[(colNameLen-length(seriesDim)+1):colNameLen]
    }
    # resdf <- transform(resdf,column504991=edurank(bbsj_xscj_YW_avg,'asc',bbsj_xscj_Q))

        #表计算与合计
    if(!missing(tableCalculaList) || !missing(queryTotalObj))
      resdf <- tableCompute_SubTotal(resdf,df,seriesDimRe,dimension,measureList,i,queryTotalObj,tableCalculaList,mergeShow)
    
    
    #用维度的key值替换数据集相应的列名
    key <- NULL
    if(length(dimension) > 0){
      colname <- names(resdf)
      key <- dimkeyList[[keyslength-i+1]]
      names(resdf) <- c(key,tail(colname,-length(key)))
    }
    
    #重新命名系列列名
    if(length(seriesDim) > 0){
      # library(splitstackshape)
      # colNa <- names(resdf)
      # resdf <- as.data.frame(cSplit(resdf,"compaName", ",",drop = FALSE))
      # names(resdf) <- c(colNa,seriesDim)
      colNa <- names(resdf)
      for (serIndex in 1:length(seriesDimRe)) {
        names(resdf)[which(seriesDimRe[serIndex]==colNa)] <- seriesDim[serIndex]
      }
    }
    
    #做对比运算
    if(!missing(comparison))
      resList <- compaFun(resdf,df,dimension,key,seriesDim,measureList,comparison)
    else
      resList[[i]] <- resdf
    i <- i + 1
  }
  
  #如果为设计模式且系列维度不为空，则返回范例数据
  if((missing(runType) || runType == 2) && length(seriesDim) > 0){
    
    #如果存在对比对象，则结果集列表中只过滤第一个数据集
    if(!missing(comparison))
      index <- 1
    else
      index <- length(resList)
    
    #根据原始数据的第一行的系列维度值过滤,拼接过滤条件
    filterValue <- list()
    for(seri in seriesDim){
      filterValue[[length(filterValue)+1]] <- paste(seri," == '",resList[[1]][1,seri],"'",sep = "")
    }
    filterValue <- paste(filterValue,collapse = " & ")
    
    #进行过滤
    for(j in c(1:index)){
      filterDa <- resList[[j]]
      isArr <- with(filterDa,eval(parse(text = filterValue)))
      isArr[is.na(isArr)] <- TRUE
      resList[[j]] <- filterDa[isArr,]
    }
  }
  
  #如果分组维度只有一组，则返回计算结果
  if (i == 2)
    return(resList)
  
  #并列计算合并数据
  library(plyr)
  resda <- rbind.fill(resList)
  for(dim in rev(Reduce(union,rev(dimkeyList)))){
    resda[,dim] <- as.character(resda[,dim])
    resda <- resda[order(resda[,dim],na.last=FALSE),]
  }
  resda <- sortStratifie(resda,myorder,dimGropList,dimkeyList)
  #返回果集
  return(list(resda))
}



###用指标的key值替换数据集相应的列名####
replaceMeasureKey <- function(resdf, measureList) {
  if(length(measureList) > 0){
    colnames <- names(resdf)
    for(j in 1:length(measureList)){
      measure <- measureList[[j]]
      colName <- measure["measure"][[1]]
      key <- measureList[[j]]["resultColumn"][[1]]
      for(k in 1:length(colnames)){
        if (colnames[k] == colName){
          colnames[k] <- key
          break
        }
      }
    }
    names(resdf) <- colnames
  }
  return(resdf)
}

###记录数查询方法####
dataCount <- function(df,doWhere){
  #对始数据框进行过滤得到新的数据框
  dffilter <- oriFilter(df,doWhere=doWhere)
  dffilter <- resFilter(dffilter,doWhere=doWhere)
  totalCount <- nrow(dffilter)
  return(totalCount)
}

###明细查询方法####
dataDetailCount <- function(df,column,keys,pageObject,doWhere,myorder,seriesColumn=c(),runType){
  #对始数据框进行过滤得到新的数据框
  dffilter <- oriFilter(df,doWhere=doWhere)
  dffilter <- resFilter(dffilter,doWhere=doWhere)
  
  #对数据进行排序
  dffilter <- sortDataFrame(myorder,dffilter)
  

  #如果为设计模式且系列维度不为空，则返回范例数据
  if((missing(runType) || runType == 2) && length(seriesColumn) > 0){
    
    #根据原始数据的第一行的系列维度值过滤,拼接过滤条件
    filterValue <- list()
    for(seri in seriesColumn){
      filterValue[[length(filterValue)+1]] <- paste(seri," == '",dffilter[1,seri],"'",sep = "")
    }
    filterValue <- paste(filterValue,collapse = " & ")
    
    #进行过滤
    isArr <- with(dffilter,eval(parse(text = filterValue)))
    isArr[is.na(isArr)] <- FALSE
    dffilter <- dffilter[isArr,]
  }

 totalCount <- nrow(dffilter) 
 return(totalCount)
}

###明细查询方法####
dataDetail <- function(df,column,keys,pageObject,doWhere,myorder,seriesColumn=c(),runType){
  #对始数据框进行过滤得到新的数据框
  dffilter <- oriFilter(df,doWhere=doWhere)
  dffilter <- resFilter(dffilter,doWhere=doWhere)
  
  #对数据进行排序
  resdf <- sortDataFrame(myorder,dffilter)

  #如果为设计模式且系列维度不为空，则返回范例数据
  if((missing(runType) || runType == 2) && length(seriesColumn) > 0){
    
    #根据原始数据的第一行的系列维度值过滤,拼接过滤条件
    filterValue <- list()
    for(seri in seriesColumn){
      filterValue[[length(filterValue)+1]] <- paste(seri," == '",resdf[1,seri],"'",sep = "")
    }
    filterValue <- paste(filterValue,collapse = " & ")
    
    #进行过滤
    isArr <- with(resdf,eval(parse(text = filterValue)))
    isArr[is.na(isArr)] <- FALSE
    resdf <- resdf[isArr,]
  }
  
  totalCount <- nrow(resdf) 
  #确定分页信息
  page <- pageObject["page"][[1]]
  pageSize <- pageObject["pageSize"][[1]]
  
  endIndex <- page*pageSize
  if(endIndex > totalCount)
    endIndex = totalCount
  startIndex <- (page - 1)*pageSize + 1
  
  if(length(seriesColumn > 0)){
  	column <- c(column,seriesColumn)
  	keys <- c(keys,seriesColumn)
  }

  resdf <- resdf[c(startIndex:endIndex),column]
  
  if(length(column) == 1){
    resdf <- as.data.frame(resdf)
    #给数据框加上列标题
    names(resdf)[1] <- column
  }
  names(resdf) <- keys

  return(resdf)
}

###添加维度范围分组####
addGroup <- function(sdf,dimensition,groupObject){
  #如果维度只有一个且这个维度的成员为字符型数字
  spLi <- list()
  if(length(dimensition) == 1 && !is.na(suppressWarnings(as.numeric(as.character(sdf[1,dimensition]))))){
    #在这些维度成员前添加#
    sdf[,dimensition] <- paste("#",sdf[,dimensition],sep = "")
    spLi[length(spLi)+1] <- dimensition
    groupCol <- dimensition
  }else if(length(dimensition) > 1){
    #如果维度个数大于1，则将这些维度合并为groupCol列
    sers <- c(paste("paste(",dimensition[1]),dimensition[-1],"sep = '`%r%`')")
    sers <- paste(sers,collapse = ",")
    #添加compaName列
    sdf$groupCol <- with(sdf,eval(parse(text = sers)))
    groupCol <- "groupCol"
  }else{
    groupCol <- dimensition
  }
  
  dflist <- split(sdf,sdf[,groupCol])
  dflist <- lapply(dflist, function(x){ 
    if(!is.null(groupObject[["point"]]))
      eval(parse(text = groupObject[["point"]]))
    
    eval(parse(text = groupObject[["newCol"]]))
  })
  
  sdf <- unsplit(dflist,f = sdf[,groupCol])
  
  #将添加#的列去除#
  if(length(spLi) > 0){
    spdim <- spLi[[1]]
    sdf[,spdim] <- as.character(sdf[,spdim])
    sdf[,spdim] <- vapply(strsplit(sdf[,spdim],"#"),function(x){x[2]},FUN.VALUE = c(""))
  }
  
  if(nrow(sdf) != nrow(sdf))
    sdf <- merge.data.frame(sdf,sdf,all = T)
  return(sdf)
}


#对比对象####
# df              运算后的数据
# dfOrig          原始数据
# measureList     度量集合
# dimGropList     维度集合
# compaObject     对比对象
#   compaName		      是	String	      对比对象名称
#   compaType		      是	String	      对比类型：1—对比维度；2—对比维度成员；3—对比指标
#   dimension	        是	String	      维度列名
#   filter	          否	String      	维度过滤
#   compaMeasure		  否	Object	      对比指标对象
#   resType           是  String        结果集类型：1、join；2、nojoin
compaFun <- function(df,dfOrig,dimension,dimkey,seriesDim,measureList,compaObject){
  
  #获得对比对象中的维度
  compaDim <- c()
  for(com in compaObject)
    compaDim <- c(compaDim,com[['dimension']])
  
  #制作一个大的数据集，包含行列的维度、系列维度、对比维度
  ser_comDim <- unique(c(seriesDim,compaDim))
  temDf <- unique(dfOrig[,c(dimension,ser_comDim)])
  if(!is.data.frame(temDf))
    temDf <- data.frame(temDf)
  names(temDf) <- c(dimkey,ser_comDim)
  
  #在系列的结果集中添加空的对比对象维度列
  dfCol <- names(df)
  for(dim in compaDim){
    if(!dim %in% dfCol)
      df[,dim] <- NA
  } 
  
  library(plyr)
  #需要join的集合
  compaDas_join <- list(df)
  #不要join的集合
  compaDas_nojo <- list()
  
  #获得度量列表中的列名
  measureNames <- vapply(measureList,function(x){x[['resultColumn']]},c(""))
  
  for(compa in compaObject){
    
    #存在过滤
    if(length(compa[['filter']]) != 0)
      with(dfOrig,{dffilter <<- dfOrig[eval(parse(text = compa[['filter']])),]})
    else
      dffilter <- dfOrig
    
    #替换度量列表中的度量公式
    if(length(measureList) > 0 && length(compa[['compaMeasure']]) > 0){
      for(i in 1:length(measureList)){
        if(measureList[[i]][['metricsType']] %in% c(1,2)){
          compa[['compaMeasure']][['measure']] <- measureList[[i]][['measure']]
          measureList[[i]][['measureList']] <- compa[['compaMeasure']]
        }
      }
    }
    
    #做统计运算
    if(length(c(compa[['dimension']],dimension)) != 0){
      da <- singleGroupQueries(dffilter,measureList,c(dimension,compa[['dimension']]))
      if(length(dimension) > 0)
        names(da) <- c(dimkey,compa[['dimension']],tail(names(da),-length(c(dimension,compa[['dimension']]))))
    }else{
      da <- dataQueriesMetrics(dffilter,measureList)[[1]]
    }
    
    #判断是否join数据集
    if(compa[['resType']] == 1){
      da <- join(temDf,da,by=c(dimkey,compa[['dimension']]),match="first")
      #添加标记列，用对比对象的名称标记
      da[,'compaName'] <- rep(compa[['compaName']],nrow(da))
      compaDas_join[[length(compaDas_join)+1]] <- da
    }else{
      #添加标记列，用对比对象的名称标记
      da[,'compaName'] <- rep(compa[['compaName']],nrow(da))
      compaDas_nojo[[length(compaDas_nojo)+1]] <- da
    }
  }
  
  #join数据集
  compaDas_join <- rbind.fill(compaDas_join)
  #将join的数据集与不用join的数据集放到一个list中
  union(list(compaDas_join),compaDas_nojo)
}

#判断字符型数字是否需要添加一个
