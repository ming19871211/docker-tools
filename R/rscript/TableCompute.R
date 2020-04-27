#表计算####
#
# dimensionlist = list(blockdim=c('','',''),orderdim=c('','',''))
#                      blockdim 为分块维度; orderdim 为排序维度
#                     orderdim=list(list(dim='',level=c('','','')),list())
# measure  为字符串
# paramlist 为参数列表，list集合
# computeType 计算类型，值有：
#     computeOrder  排序
#         在paramlist中的参数有：
#             order         排序类型，值为'desc'或'asc';表示逆序或顺序 
#             orderMethod   排序方式，表示的是:
#                 'first'     c(1,2,3,4)    唯一排序
#                 'min'       c(1,2,2,4)    竞争排序
#                 'max'       c(1,3,3,4)    调整后竞争排序
#     discrepancy   差异
#         在paramlist中的参数有：
#             relativeTo参数：
#                 1——相对于上一个
#                 2——相对于下一个
#                 3——相对于第一个
#     percDisc  百分比差异
#         在paramlist中的参数有：
#             relativeTo参数：
#                 1——相对于上一个
#                 2——相对于下一个
#                 3——相对于第一个
#     percent 百分比
#         在paramlist中的参数有：
#             relativeTo参数：
#                 1——相对于上一个
#                 2——相对于下一个
#                 3——相对于第一个
#     summary_max       汇总最大
#     summary_mean      汇总平均
#     summary_min       汇总最小
#     summary_sum       汇总求和
#     totalPercent      总额百分比
#     computeQuantile   百分位
#         在paramlist中的参数有：
#             order         排序类型，值为'desc'或'asc';表示逆序或顺序 

tableCompute <- function(df,dimensionlist,measure,computeType,paramlist){
  
  #如果存在多个分块维度，则按`%r%`将其合并为一列；如果只有一列，则用#拼接
  #这样处理，可以防止因为维度中存在NA值导致分块缺失，同时可以减少分块提高运行速度
  if(!is.null(dimensionlist["blockdim"][[1]])){
    dimension <- dimensionlist["blockdim"][[1]]
    if(length(dimension) == 1)
      rmfirstdim <- "'#'"
    else
      rmfirstdim <- dimension[-1]
    sers <- c(paste("paste(",dimension[1]),rmfirstdim,"sep = '`%r%`')")
    sers <- paste(sers,collapse = ",")
    #添加groupCol列
    df$groupCol <- with(df,eval(parse(text = sers)))
  }
  
  dflist <- getsplitdf(df,dimensionlist)
  dflist <- lapply(dflist, function(x) computeType(x,measure,paramlist))
  if(!is.null(dimensionlist["blockdim"][[1]])){
    df <- unsplit(dflist,f = df$groupCol)
    df <- subset(df,select = -groupCol)
  }else{
    df <- dflist[[1]]
  }
  #表计算结果取反
  if(!is.null(paramlist['negate'][[1]])){
    #if(length(paramlist[['negate']]) > 0){
    df[,paramlist[['newcolumn']]] <- paramlist[['negate']]*df[,paramlist[['newcolumn']]]
  }
  df <- df[order(df$ordernumber),]
  return(subset(df,select = -ordernumber))
}

#将数据分组####
getsplitdf <- function(df,dimensionlist){
  df[,ncol(df)+1] <- c(1:nrow(df))
  names(df)[ncol(df)] <- "ordernumber"
  #数据分块
  dflist <- list()
  if(!is.null(dimensionlist["blockdim"][[1]])){
    dflist <- split(df,df$groupCol)
  }else{
    dflist[[1]] <- df 
  }
  
  #数据排序
  if(!is.null(dimensionlist["orderdim"][[1]])){
    orderdim <- dimensionlist["orderdim"][[1]]
    # orderStr <- ""
    # for(i in 1:length(orderdim)){
    #   if(is.null(orderdim[[i]][['level']])){
    #     sta <- 'x[,c(\'column\')]'
    #     sta <- gsub('column',orderdim[[i]][['dim']],sta)
    #   }else{
    #     sta <- 'factor(x[,c(\'column\')],levels = levelvalue)'
    #     sta <- gsub('column',orderdim[[i]][['dim']],sta)
    #     sta <- gsub('levelvalue',orderdim[[i]]['level'],sta)
    #   }
    #   orderStr <- paste(orderStr,sta,sep = ',')
    # }
    # orderStr <- substr(orderStr,2,nchar(orderStr))
    # orderStr <- paste('order(',paste(orderStr,')',sep = ''),sep = '')
    # dflist <- lapply(dflist, function(x){x[eval(parse(text = orderStr)),]})
    dflist <- lapply(dflist, function(x){sortDataFrame(orderdim,x)})
  }
  return(dflist)
}

#总额百分比####
totalPercent <- function(x,measure,paramlist){
  arr <- x[,c(measure)]/x$value
  x <- subset(x,select = -value)
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#汇总求和####
summary_sum <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  arr <- cumsum(arr)
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#汇总平均####
summary_mean <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  arr <- cumsum(arr)
  i <- 1
  arr <- vapply(arr, function(x){
    x <- x/i
    i <<- i+1
    return(x)
  }, FUN.VALUE = c(0))
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#汇总最小####
summary_min <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  arr <- cummin(arr)
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#汇总最大####
summary_max <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  arr <- cummax(arr)
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#差异####
#relativeTo参数：
#     1——相对于上一个
#     2——相对于下一个
#     3——相对于第一个
#     "min"——相对于最小值
#     "max"——相对于最大值
#     "mean"——相对于平均值
#relativeToScope    相对与参数的范围，只作用与最大、最小、平均
#     无        c()          
#     全量      "FULL"  所有的行维度或列维度
#     特定维度  c("","")
discrepancy <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  if(nrow(x)!=0){
    if(paramlist["relativeTo"][[1]] %in% c(1,2)){
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
      i <- arr[1]
      arr <- vapply(arr[-1], function(x){
        y <- x-i
        i <<- x
        return(y)
      }, FUN.VALUE = c(0))
      arr <- append(arr,NA,0)
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
    }else if(paramlist["relativeTo"][[1]]==3){
      arr <- x[,measure] - x[1,measure]
    }else if(length(paramlist[['relativeToScope']]) == 0){
      #无范围的相对于
      i <- eval(parse(text = paramlist[["relativeTo"]]))(arr,na.rm=TRUE)
      arr <- arr - i
    }else{
      arr <- arr - x$value
      x <- subset(x,select = -value)
    }
  }
  
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#百分比####
percent <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  
  if(nrow(x) != 0){
    if(paramlist["relativeTo"][[1]] %in% c(1,2)){
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
      i <- arr[1]
      arr <- vapply(arr[-1], function(x){
        y <- x/i
        i <<- x
        return(y)
      }, FUN.VALUE = c(0))
      arr <- append(arr,NA,0)
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
    }else if(paramlist["relativeTo"][[1]]==3){
      arr <- x[,measure] / x[1,measure]
    }else if(length(paramlist[['relativeToScope']]) == 0){
      #无范围的相对于
      i <- eval(parse(text = paramlist[["relativeTo"]]))(arr,na.rm=TRUE)
      arr <- arr / i
    }else{
      arr <- arr / x$value
      x <- subset(x,select = -value)
    }
  }
    
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#百分比差异####
percDisc <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  
  if(nrow(x)!=0){
    if(paramlist["relativeTo"][[1]] %in% c(1,2)){
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
      i <- arr[1]
      arr <- vapply(arr[-1], function(x){
        y <- (x-i)/i
        i <<- x
        return(y)
      }, FUN.VALUE = c(0))
      arr <- append(arr,NA,0)
      if(paramlist["relativeTo"][[1]]==2){
        arr <- rev(arr)     #倒置
      }
    }else if(paramlist["relativeTo"][[1]]==3){
      arr <- (x[,measure] - x[1,measure])/x[1,measure]
    }else if(length(paramlist[['relativeToScope']]) == 0){
      #无范围的相对于
      i <- eval(parse(text = paramlist[["relativeTo"]]))(arr,na.rm=TRUE)
      arr <- (arr - i)/i
    }else{
      arr <- (arr - x$value)/x$value
      x <- subset(x,select = -value)
    }
  }
  
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#排序####
#在参数列表paramlist中，排序类型（order）值为'desc'或'asc';表示逆序或顺序 
# 排序方式（orderMethod）表示的是:
#         唯一排序        'first'     c(1,2,3,4)
#         竞争排序        'min'       c(1,2,2,4)
#         调整后竞争排序  'max'       c(1,3,3,4)
#         密集排序        'dense'     c(1,2,2,3)
computeOrder <- function(x,measure,paramlist){
  
  arr <- x[,c(measure)]
  #判断是否为密集排序
  if(paramlist['orderMethod'][[1]]=='dense'){
    if(nrow(x)!=0){
      #将相同的值分为一组
      sp <- split(arr,f = factor(arr))
      #如果倒序，则将分组集合倒置
      if(paramlist['order'][[1]]=='desc'){
        sp <- rev(sp)
      }
      
      i <- 0    #用于记录排序的序号
      newarr <- vapply(sp, function(x){
        i <<- i + 1
        list(rep(i,length(x)))  
      }, FUN.VALUE = list(0))
      
      #如果倒序，将之前倒置的分组集合再次倒置，以便复原分组集合
      if(paramlist['order'][[1]]=='desc'){
        newarr <- rev(newarr)
      }
      #复原分组集合
      arr <- unsplit(newarr,f = factor(arr))
    }
  }else{
    if(paramlist['order'][[1]]=='desc'){
      library(plyr)
      arr <- rank(desc(arr),ties.method = paramlist['orderMethod'][[1]])
    }else{
      arr <- rank(arr,ties.method = paramlist['orderMethod'][[1]])
    }
  }
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#百分位####
computeQuantile <- function(x,measure,paramlist){
  arr <- x[,c(measure)]
  if(nrow(x)!=0){
    arr <- quantileArr(arr,myorder = paramlist['order'][[1]])
  }
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}


#移动计算####
#在参数列表paramlist中 
#   汇总值(funName)   sum;mean;max;min
#   当前值(current)   值为TRUE--有；FALSE--无
#   前(preced)        当前值的前几个数  值为1、2、3、4...
#   后(behind)        当前值的后几个数  值为1、2、3、4...    
#   若无足够值为空(isNull)    值为TRUE--是；FALSE--否
#paramlist <- list(preced=1,behind=2,funName=mean,current=TRUE,isNull=TRUE,newcolumn='mobile')
mobileCompute <- function(x,measure,paramlist){
  arr <- x[,c(measure)]                 #参与表计算的列
  len <- length(arr)                    #列的长度
  preced <- paramlist['preced'][[1]]    #参数前几个数
  behind <- paramlist['behind'][[1]]    #参数后几个数
  num <- preced + behind                
  
  arr <- vapply(c(1:len),function(x){
    if(paramlist['isNull'][[1]] && (x-preced < 1 || x+behind > len)){
      return(NA)
    }else{
      a <- arr[c(max(x-preced,1):min(len,x+behind))] #进行移动计算的一组数
      if(!paramlist['current'][[1]]){
        a <- a[-(preced+min(x-preced,1))]     #当前值不参与计算，则移除
      }
      return(paramlist['funName'][[1]](a))
    }
  },FUN.VALUE = c(0))
  
  x[,ncol(x)+1] <- arr
  names(x)[ncol(x)] <- paramlist['newcolumn'][[1]]
  return(x)
}

#将一个数组转变为百分位数组####
quantileArr <- function(vec,myorder='asc'){
  len <- length(vec)        #获取数组的长度
  ve <- split(vec,f = factor(vec))     #将数组根据值分组，相同的值分为一组
  if(myorder=='desc'){
    ve <- rev(ve)
  }
  num <- 0        #用于记录小于当前数的个数
  ve <- vapply(ve, function(x){
    num <<- num + length(x)
    #返回当前值的百分位集合
    return(list(rep((num-1)/(len-1),length(x))))
  }, FUN.VALUE = list(0))
  if(myorder=='desc'){
    ve <- rev(ve)
  }
  #将分组的集合还原
  return(unsplit(ve,f = factor(vec)))
}
