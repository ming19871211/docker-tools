onlyMetrics <- function(df,measure){
  columnName <- measure["measure"][[1]]           #列名
  
  if(measure["exprName"][[1]] == "quantile"){
    res <- measure["expr"][[1]](df[,c(columnName)],na.rm = measure["isDoNa"][[1]],probs = measure["probs"][[1]]) 
  }else if(measure["exprName"][[1]] == "percentile"){
    res <- measure["expr"][[1]](df[,c(columnName)],na.rm = measure["isDoNa"][[1]],probs = measure["probs"][[1]],orderType = measure["orderType"][[1]]) 
  }else if(measure["exprName"][[1]] == "dequantile"){
    res <- measure["expr"][[1]](df[,c(columnName)],na.rm = measure["isDoNa"][[1]],loci = measure["loci"][[1]]) 
  }else if(measure["exprName"][[1]] == "distinguishdegree"){
    res <- measure["expr"][[1]](df[,c(columnName)],na.rm = measure["isDoNa"][[1]],mark = measure["mark"][[1]],ratio = measure["ratio"][[1]]) 
  }else if((measure["exprName"][[1]] == "length")){
    res <- measure["expr"][[1]](df[,c(columnName)]) 
  }else{
    res <- measure["expr"][[1]](df[,c(columnName)],na.rm = measure["isDoNa"][[1]]) 
  }
  return(res)
}



noDimenstionFormula <- function(df,ele){
  
  measureList <- ele["variableList"][[1]]
  formual <- ele["formual"][[1]]
  
  i <- 1
  for(measure in measureList){
    variableName <- measure["variableName"][[1]]  #得到计算公式中的变量???
    
    filterCondition = measure['filter'][[1]]
    if(is.null(filterCondition)){
      measureStr <- paste("wrapRformula(df,measureList,",i,")",sep="",collapse = "")
    }else{
      measureStr <- paste("wrapRformula(df,measureList,",i,",'",filterCondition,"')",sep="",collapse = "") 
    }
    formual <- sub(variableName,measureStr,formual)
    i <- i + 1
  }
  evalRes <- eval(parse(text = formual))
  return(evalRes)
}

wrapRformula <- function(df,variableList,i,doWhere){
  
  #在查询统计之前对数据框先进行过滤
  if(missing(doWhere)){
    doWhere <- TRUE
  }
  attach(df)
  df <- df[eval(parse(text = doWhere)),]
  
  
  measure <- variableList[[i]]
  exprName <- measure["exprName"][[1]]        #公式名称
  
  if(!is.null(measure["column"][[1]]))
    measureCol <- eval(parse(text = measure["column"][[1]]))#指标???
  constParams <- measure["constParams"][[1]]
  detach(df)
  #判断公式
  if(exprName == "length"){
    value <- length(measureCol)
  }else if(exprName == "quantile"){
  	if(constParams[1] > 1){
      constParams[1] <- constParams[1] / 100
    }
    value <- quantile(measureCol,probs = constParams[1],na.rm = TRUE)
  }else if(exprName == "dequantile"){
    value <- dequantile(measureCol,loci = constParams[1],na.rm = TRUE)
  }else if(exprName == "distinguishdegree"){
    value <- distinguishdegree(measureCol,mark = constParams[1],ratio = constParams[2],na.rm = TRUE)
  }else if(exprName == "percentile"){
    value <- percentile(measureCol,probs = constParams[1],orderType = constParams[2],na.rm = TRUE)
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
    value <- relatedanalysis(df,columnParams)
  }else{
    value <- measure["expr"][[1]](measureCol,na.rm = TRUE)
  }
  return(value)
}