#### df : 数据集
#### measureObj：待解析的指标
#### dimension:维度
resolveMetrics <- function(df,measureObj,dimension){
  #如果指标的类型为1，表示是一个简单的指标
  #measureObj的结构为：list(metricsType=1,measureList = list(measure = 'physical',isDoNa = FALSE,expr = length,exprName = 'length'))
  if(measureObj["metricsType"][[1]] == 1 | measureObj["metricsType"][[1]] == 2){
    #取出度量表达式
    measure <- measureObj["measureList"][[1]]
    #计算
    return(computerExpression(df,measure,dimension))
  }
  
  #如果指标的类型为3，表示是一个计算公式，需要对这个公式进行解析
  if(measureObj["metricsType"][[1]] == 3){
    return(resolveFormula(df,measureObj,dimension))
  }
  
}

#measureObj结构：  eval(parse(text = f))
#list(
#metricsType=3, measure = 'yswzf', formual = 'A+B',
#variableList = list(
#list(variableName = 'A',column = 'key6$hyt_zcj_ZF+key6$hyt_zcj_PM',expr = max,exprName = 'max',constParams=c(0.25),filter='key6$hyt_zcj_PM > 120'),
#list(variableName = 'B',column = 'key6$hyt_zcj_ZF+key6$hyt_zcj_PM',expr = max,exprName = 'max',constParams=c(0.25),filter='key6$hyt_zcj_PM > 120'))))
resolveFormula <- function(df,measureObj,dimension){
  options(digits = 4)
  
  measureList <- measureObj["variableList"][[1]]
  formual <- measureObj["formual"][[1]]
  
  i <- 1
  for(measure in measureList){
    variableName <- measure["variableName"][[1]]  #得到计算公式中的变量名
    
    filterCondition = measure['filter'][[1]]
    if(is.null(filterCondition)){
      measureStr <- paste("wrapTapply(df,measureList,dimension,",i,")",sep="",collapse = "")
    }else{
      measureStr <- paste("wrapTapply(df,measureList,dimension,",i,",'",filterCondition,"')",sep="",collapse = "") 
    }
    formual <- sub(variableName,measureStr,formual)
    i <- i + 1
  }
  evalRes <- eval(parse(text = formual))
  if(!is.array(evalRes)){
    evalRes <- convertArray(as.vector(evalRes),df,dimension)
  }
  return(evalRes)
}

#计算表达式函数
computerExpression <- function(df,measure,dimension){
  options(digits = 4)
  
  if(length(measure) < 5 ){
    #如果表达式只有两个到三个参数，则表式没有其它的参数
    if(measure["isDoNa"][[1]] == TRUE){
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]],na.rm = TRUE)  
    }else{
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]])
    }
  }else{
    #对于大于4个参数的function，需要进行特殊处理，因为无法知道表达式的各个参数的名称与位置
    if(measure["exprName"][[1]] == "quantile"){
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]],na.rm = TRUE,probs=measure["probs"][[1]])
    }
    #对于大于4个参数的function，需要进行特殊处理，因为无法知道表达式的各个参数的名称与位置
    if(measure["exprName"][[1]] == "percentile"){
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]],na.rm = TRUE,probs=measure["probs"][[1]],orderType=measure["orderType"][[1]])
    }
    if(measure["exprName"][[1]] == "dequantile"){
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]],na.rm = TRUE,loci=measure["loci"][[1]])
    }
    if(measure["exprName"][[1]] == "distinguishdegree"){
      resTapply <- tapply(df[,measure["measure"][[1]]], df[,dimension], measure["expr"][[1]],na.rm = TRUE,mark=measure["mark"][[1]],ratio=measure["ratio"][[1]])
    }
    #其它的表达式再一个一个的添加
  }
  return(resTapply)
}

wrapTapply <- function(df,measureList,dimension,i,doWhere){
  #判断有无过滤条件
  if(missing(doWhere)){
    doWhere <- TRUE
  }
  measure <- measureList[[i]]
  #在查询统计之前对数据框先进行过滤
  attach(df)
  dffilter <- df[eval(parse(text = doWhere)),]
  detach(df)
  #查询数据
  tapplyRes <- computerFormula(dffilter,measure,dimension)
  
  #计算返回结果集总数
  if(length(tapplyRes) == 0){
    eleCount <- 0
  }else{
    eleCount <- 1
    w <- dimnames(tapplyRes)
    for (i in 1:length(w)) {
      eleCount <- eleCount * length(w[[i]])
    }
  }
  
  #计算本譔返回的结果集总数
  oEleCount <- 1
  for (i in 1:length(dimension)) {
    oEleCount <- oEleCount * length(unique(df[,dimension[i]]))
  }
  #如果结果数相等，则返回原有结果集
  if(oEleCount == eleCount){
    return(tapplyRes)
  }
  
  #如果结果为空
  emptyArray <- createEmptyArray(df,dimension)
  if(eleCount == 0){
    return(emptyArray)
  }
  
  #如果结果集中的元素与本改返回的数目不相等则将tappy的结果填充到emptyArra中
  if(!require("abind"))
    stop("not install RMySQL")
  
  library(abind)
  afill(emptyArray) <- tapplyRes

  return(emptyArray)
}

# list(variableName = 'A',column = 'key6$hyt_zcj_ZF+key6$hyt_zcj_PM',expr = max,exprName = 'max',constParams=c(0.25),filter='key6$hyt_zcj_PM > 120'),
computerFormula <- function(df,measure,dimension){
  
  exprName <- measure["exprName"][[1]]        #公式名称
  if(!is.null(measure["column"][[1]])){
    attach(df)
    measureCol <- eval(parse(text = measure["column"][[1]]))#指标列
    detach(df)
  }
  
  constParams <- measure["constParams"][[1]]
  
  #判断公式
  if(exprName == "length"){
    resTapply <- tapply(measureCol, df[,dimension], length)
  }else if(exprName == "quantile"){
    if(constParams[1] > 1){
      constParams[1] <- constParams[1] / 100
    }
    resTapply <- tapply(measureCol, df[,dimension], quantile,probs = constParams[1],na.rm = TRUE)
  }else if(exprName == "dequantile"){
    resTapply <- tapply(measureCol, df[,dimension], dequantile,loci = constParams[1],na.rm = TRUE)
  }else if(exprName == "distinguishdegree"){
    resTapply <- tapply(measureCol, df[,dimension], distinguishdegree,mark = constParams[1],ratio = constParams[2],na.rm = TRUE)
  }else if(exprName == "relatedanalysis"){
    columnParams <- measure["columnParams"][[1]]
    dfnames <- names(df)
    #如果数据框中没有相关分析的列，则创建
    diffcolumn <- setdiff(columnParams,dfnames)
    if(length(diffcolumn)){
      attach(df)
      for(column in diffcolumn){
        df[,column] <- eval(parse(text = column))
      }
      detach(df)
    }
    resTapply <- relatedanalysis(df,columnParams,dimension)
  }else{
    resTapply <- tapply(measureCol, df[,dimension], measure["expr"][[1]],na.rm = TRUE)
  }
  return(resTapply)
}

createEmptyArray <- function(df,dimension){
  #计算本譔返回的结果集总数
  dimNameList <- list()
  oEleCount <- 1
  
  #数组维度个数
  arrayDimNum <- c()
  
  for (i in 1:length(dimension)) {
    dvector <- unique(df[,dimension[i]])        #维度因子
    dvector <- dvector[!is.na(dvector)]
    
    l <- length(dvector)#维度成员个数
    oEleCount <- oEleCount * l            #计算数组的长度
    arrayDimNum <- c(arrayDimNum,l)       #数组各个维度的长度
    
    #先将维度的因子按默认排序，再给维度赋值
    dimNameList[i] <- list(dvector[order(dvector)])
  }
  
  res <- array(c(rep(NA,oEleCount)),arrayDimNum,dimnames = dimNameList)
  return(res)
}

convertArray <- function(vec,df,dimension){
  #计算本譔返回的结果集总数
  dimNameList <- list()
  oEleCount <- 1
  #数组维度个数
  arrayDimNum <- c()
  
  for (i in 1:length(dimension)) {
    l <- length(unique(df[,dimension[i]]))#维度成员个数
    oEleCount <- oEleCount * l            #计算数组的长度
    arrayDimNum <- c(arrayDimNum,l)       #数组各个维度的长度
    dvector <- unique(df[,dimension[i]])        #维度因子
    
    #先将维度的因子按默认排序，再给维度赋值
    dimNameList[i] <- list(dvector[order(dvector)])
  }
  res <- array(vec,arrayDimNum,dimnames = dimNameList)
  return(res)
}