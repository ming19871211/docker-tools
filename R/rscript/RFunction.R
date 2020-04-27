# 计字符串 ####
computeString <- function(e,na.rm = TRUE){
  e <- unique(e)
  if(na.rm)
    e <- e[!is.na(e)]
  e <- sort(e)
  return(paste(e,collapse = ","))
}


#考试难度,e:某一元素,x:总分####
difficultyExam <- function(e,x){
  return(mean(e,na.rm=TRUE) / x)
}

#求离差，e:某一元素，col:某一列####
deviationfromAverage <- function(e,col){
  return(e - mean(col,na.rm = TRUE))
}

#全距####
range <- function(e,na.rm = TRUE){
  if(na.rm == TRUE){
    return(max(e,na.rm = TRUE) - min(e,na.rm = TRUE)) 
  }else{
    return(max(e,na.rm = FALSE) - min(e,na.rm = FALSE))
  }
}

#众数####
modenumber <- function(e,na.rm = TRUE){
  e <- e[e != 0] #求众数时，去掉0，成总说的，但是最好是做成参数传数
  mVector <- as.character(names(table(e))[table(e)==max(table(e))])
  return(paste(mVector,collapse = ","))
}

#计数####
len_na <- function(e,na.rm="false",...){
  if(missing(...)){
    if(tolower(na.rm) == "true")
      e <- na.omit(e)
    return(length(e))
  }else{
    spDa <- data.frame(...)
    sp <- split(e,spDa)
    res <- lapply(sp,function(x){
      if(tolower(na.rm) == "true"){
        isFa <- !is.na(x)
      }else{
        isFa <- TRUE
      }
      x[isFa] <- length(x[isFa])
    })
    res <- unsplit(res,spDa)
    return(res)
  }
}
 
#计数（不同）####
len_diff <- function(e,na.rm="false",...){
  if(missing(...)){
    if(tolower(na.rm) == "true")
      e <- na.omit(e)
    return(length(unique(e)))
  }else{
    spDa <- data.frame(...)
    sp <- split(e,spDa)
    res <- lapply(sp,function(x){
      if(tolower(na.rm) == "true"){
        isFa <- !is.na(x)
      }else{
        isFa <- TRUE
      }
      x[isFa] <- length(unique(x[isFa]))
    })
    res <- unsplit(res,spDa)
    return(res)
  }
}

#计数根据维度分组####
count_dim <- function(e,lc1="false",lc2="false",...){
  if(tolower(lc2) == "true"){
    len_diff(e,lc1,...)
  }else{
    len_na(e,lc1,...)
  }
}


#区分度e:某一元素,z:总分####
distinguishdegree <- function(e,mark = 100,ratio=0.27,na.rm=TRUE){
  #从小到大排列
  s <- sort(e,decreasing = TRUE)
  sl <- length(s)
  return((mean(s[1:round(sl*ratio)],na.rm = TRUE) - mean(s[round(sl*ratio):sl],na.rm = TRUE)) / mark)
}

#求百分位等级####
dequantile <- function(e,loci,na.rm=TRUE){
 return((length(e[e <= loci]) - 1) / (length(e) - 1))
}

#求百分位####
percentile <- function(e,probs=0.1,orderType="desc",na.rm = TRUE){
  if(is.na(orderType) | is.null(orderType) | orderType == "desc"){
    return(quantile(e,probs = (1-probs),na.rm = TRUE))
  }
  return(quantile(e,probs = probs,na.rm = TRUE))
}


myrank <- function(pv,orders = "asc"){
  v <- pv
  #升序排名
  if(orders == "asc"){
    #处理缺失值,因为是从小到大排，所以只需要将缺失值
    #替换成比最大值还大就行，所有的缺失值都为最大值 
    vMax <- max(v,na.rm = TRUE)
    if(vMax < 0)
      vMax <- abs(vMax)
    v[is.na(v)] <- vMax + 10
    
    #进行rank排名,从小到大
    vrank <- rank(v,ties.method = "min")
    
    #将排名后的最大的值处理成缺失值
    if(length(pv[is.na(pv)]) > 0){
      vrank[vrank == max(vrank)] <- NA 
    }
    return(vrank) 
  }else{
    #倒序排名
    vLength <- length(v)
    
    #处理缺失值,因为是从大到小排，所以只需要将缺失值
    #替换成比最小值还小就行
    vMin <- min(v,na.rm = TRUE)
    if(vMin < 0){
      vMin <- vMin - 1
    }else{
      vMin <- 0
    }
    v[is.na(v)] <- vMin
    
    #从大到小排
    ov <- vLength - rank(v,ties.method = "max") + 1
    #将排名后的最大的值处理成缺失值
    if(length(pv[is.na(pv)]) > 0){
      ov[ov == max(ov)] <- NA 
    }
    return(ov) 
  }
}

edurank <- function(pv,orders="asc",...){
  if(missing(orders) || (orders[1] %in% c("desc","asc") && missing(...)))
    daspl <- myrank(pv,orders)
  else{
    if(!(orders[1] %in% c("desc","asc"))){
      groupDa <- data.frame(orders,...)
      orders <- "asc"
    }else{
      groupDa <- data.frame(...)
      daspl <- split(pv,data.frame(...))
    }
    daspl <- split(pv,groupDa)
    daspl <- lapply(daspl,myrank,orders)
    daspl <- unsplit(daspl,groupDa)
  }
}

#sum####
edusum <- function(v,na.rm = TRUE){
  naLength <- length(v[complete.cases(v)])
  if(naLength == 0){
    return(NA)
  }
  return(sum(v,na.rm = TRUE))
}

#相关性分析####
#dimension 维度向量
#measure  度量向量，只有两个
relatedanalysis <- function(df,measure,dimension){
  if(missing(dimension)){
    return(cor(df[,measure[1]],df[,measure[2]]))
  }
  
  dfList <- split(df,df[,dimension])
  
  #将结果保存到向量中
  relatedRes <- vector()
  corList <- lapply(dfList, function(x){relatedRes <<- c(relatedRes,cor(x[,measure[1]],x[,measure[2]]))})
  
  #计算数组维度长度与维度名
  arrayDimNum <- c()
  dimNameList <- list()
  for (i in 1:length(dimension)) {
    dvector <- unique(df[,dimension[i]])
    arrayDimNum <- c(arrayDimNum,length(dvector))       #数组各个维度的长度
    dimNameList <- append(dimNameList,list(sort(dvector)))    #生成维度名
  }
  names(dimNameList) <- dimension
  res <- array(relatedRes,arrayDimNum,dimnames = dimNameList)
  
  return(res)
}

# Z分####
#(number-avg(number))/sd(number) --dim
z_score <- function(pv,...){
  if(missing(...))
    spldf <- list(pv)
  else{
    df <- data.frame(...)
    spldf <- split(pv,df)
  }
  resdf <- lapply(spldf,function(x){
    mun <- (x - mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE)
  })
  if(missing(...))
    resdf <- unlist(resdf)
  else
    resdf <- unsplit(resdf,df)
}

# T分####
#50+10*(number-avg(number))/sd(number)
t_score <- function(pv,...){
  50+10*z_score(pv,...)
}

#{"customColumns":[{"column":"column47002","expressionDesc":{"computercolumn":"excessavgrate(bbsj_xscj!YW,bbsj_xscj!Q,bbsj_xscj!WLLX)","rmNa":true},"expressionType":"1"}],"dimensions":[{"column":"bbsj_xscj!WLLX","dimType":"2"},{"column":"bbsj_xscj!XX","dimType":"1"}],"hybridsMatrics":[{"column":"column47002","columnDesc":"column47002","expression":{"measureArgs":[],"name":"avg","rmNa":true},"metricsType":"1"}],"queryType":"1","relationDimension":"1"}
#超均率####
#avg(number-avg(number))/avg(number)
#   avg((number - avg(number,dim))/avg(number,dim),dim1)
#     dim  对比维度
#     ...  分组维度
excessavgrate <- function(pv,dim,...){
  #如果没有对比维度，则对比整个数据的平均值
  if(missing(dim)){
    daSpl <- list(pv)
  }else{
    splDa <- data.frame(dim,...)
    daSpl <- split(pv,splDa)
  }
  
  res <- lapply(daSpl,function(x){
    x_mean <- mean(x,na.rm=TRUE)
    value <- (x-x_mean)/x_mean
  })
  
  if(missing(dim)){
    res <- unlist(res)
  }else{
    res <- unsplit(res,splDa)
  }
}

NaToFalse <- function(vec){
  vec[is.na(vec)] <- FALSE
  return(vec)
}
