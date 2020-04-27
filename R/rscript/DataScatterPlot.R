#
# 散点图统计函数
#
# 参数列表
# df:操作数据框
# measureList:度量列表，包括度量名称以及公式
# dimGropList:查询的维度列表，如果为并列分析dimGropList的长度将大于>=2
# doWhere：过滤条件，过滤条件分为原始数据过滤和结果集过滤
# scatterPlot: 回归线属性
dataScatterPlot <- function(df,measureList,dimGropList,scatterPlot,doWhere){
  
  #在查询统计之前对数据框先进行过滤
  dffilterDf <- oriFilter(df,dimGropList,doWhere=doWhere)
  #df[doWhere[1][[1]],]
  
  if(isHaveBigDim(dffilterDf,dimGropList)){
    #当存在非常大的维度，则将维度与行量度量过滤出来成新的数据集
    columns <- c(dimGropList[[1]])                #原始数据集的列名称
    scatterPlotColumns <- c(dimGropList[[1]])     #散点图数据集的列名称
    for(mObject in measureList){
      columns <- c(columns,mObject["measure"][[1]])
      scatterPlotColumns <- c(scatterPlotColumns,mObject["resultColumn"][[1]])
    }
    dffilterDf <- dffilterDf[,columns]
    
   
    #更改列名称
    names(dffilterDf) <- scatterPlotColumns
    scatterPlotDf <- dffilterDf
  }else{
    
    #如果不存在大度量，则对数据进行统计处理再进行散点图绘制
    if(length(dimGropList) == 0){
      dataqueryRes <- list(dffilterDf)
    }else{
      dataqueryRes <- dataQueries(dffilterDf,measureList,dimGropList,doWhere)
    }
    scatterPlotDf <- dataqueryRes[[1]]
    
  }
  
  #组装散点图参数
  row <- scatterPlot["row"][[1]]
  col <- scatterPlot["col"][[1]]
  fileName <- scatterPlot["pathFilename"][[1]]
  width <- scatterPlot["width"][[1]]
  height <- scatterPlot["height"][[1]]
  regreList <- scatterPlot["regreList"][[1]]
  urlPath <- scatterPlot["urlPath"][[1]] 
  pcolor <- scatterPlot[['pcolor']]
  lcolor <- scatterPlot[['lcolor']]
  
  #绘制散点图与回归线
  res <- regressionAnalysis(scatterPlotDf,row,col,fileName,urlPath,regreList,width,height,pcolor,lcolor)
  return(res)
}


#判断是否有成员比较多的度量
isHaveBigDim <- function(df,dimGropList){
  dfrows <- nrow(df)
  for (dimVector in dimGropList) {
    for(dim in dimVector){
      if(length(unique(df[,dim])) >= dfrows){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}


#回归分析
# df            数据框
# row           行度量的个数
# col           列度量的个数
# pathFilename  保存图片名            例: "E://img.png"
# width         图片宽度
# height        图片高度
# pcolor        散点图的颜色 
# lcolor        回归线的颜色
# regreList     回归分析的集合        regreList = list(list(formula,axisObject,ispredict = FALSE),list(),...)
#       regressionType 回归类型              值：-1.无;1.线性;2.多项式;3.对数;4.指数
#       formula       回归公式              例: bbsj_xscj_YW ~ I(bbsj_xscj_YY^3)+I(bbsj_xscj_YY^2)+bbsj_xscj_YY
#       axisObject    坐标轴上的x轴和y轴    例: axisObject = list(x=bbsj_xscj_YW,y=bbsj_xscj_YY)
#       ispredict     是否显示置信区间      值：TRUE;FALSE
#       axisName      坐标名称              例: axisName = list(x="",y="")
regressionAnalysis <- function(df,row,col,fileName,urlpath,regreList,width=640,height=340,pcolor="#FF0000",lcolor="red"){
  regreData <- data.frame()
  #画图
  #library(Cairo)
  #CairoFonts(regular = "STKaiti:style=Regular", bold="微软雅黑:style=Regular")
  #CairoPNG(file=fileName,width,height)
  #png(filename = fileName,width,height)
  #MSungStd-Light-Acro
  png(filename = fileName,width,height,family="MSungStd-Light-Acro",type="cairo")
  #将多个坐标画在一张图中
  opar <- par(no.readonly = TRUE)
  par(mfrow = c(row,col))
  #par(mfrow = c(row,col),mar = c(2,2,2,2))
  
  for(regre in regreList){
    #regre <- regreList[[i]]
    formula <- regre[['formula']]
    axisObject <- regre[['axisObject']]
    axisName <- regre[['axisName']]
    
    ispredict <- regre[['ispredict']]
    regressionType <- regre[['regressionType']]
    #进行回归分析
    regdf <- df[order(df[,c(axisObject[['x']])]),c(axisObject[['x']],axisObject[['y']])]
    regdf <- na.omit(regdf)           #去掉缺失值
    
    #如果数据为空，则返回
    if(nrow(regdf) == 0)
      break
    #画散点图
    plot(regdf[,c(axisObject[['x']])],regdf[,c(axisObject[['y']])],col=pcolor,xlab=axisName[['x']],ylab=axisName[['y']])
    
    #regressionType为-1或数据只有一行时,只画散点图不进行回归分析
    if(regressionType != -1 && nrow(regdf) > 1){
      if(regressionType %in% c(3,4)){
        regdf <- regdf[regdf[,c(axisObject[['x']])]>0 & regdf[,c(axisObject[['y']])]>0,]
      }
      lms <- lm(formula,data = regdf)   #回归
      fsta <- summary(lms)$fstatistic
      #画趋势线
      if(regressionType != 4){
        points(regdf[,c(axisObject[['x']])],lms$fitted.values,type = 'l',col=lcolor)
        #lines(regdf[,c(axisObject[['x']])],lms$fitted.values,lty = 1,col=lcolor)
      }else{
        points(regdf[,c(axisObject[['x']])],exp(lms$fitted.values),type = 'l',col=lcolor)
      }
      #置信区间
      if(ispredict && !is.null(fsta)){
        pre <- predict(lms,newdata = regdf,interval = 'c',level = 0.95,type = 'response')
        if(regressionType == 4){
          lwr <- exp(pre[,c("lwr")])
          upr <- exp(pre[,c("upr")])
        }else{
          lwr <- pre[,c("lwr")]
          upr <- pre[,c("upr")]
        }
        lines(regdf[,c(axisObject[['x']])],lwr,lty = 2,col='green')
        lines(regdf[,c(axisObject[['x']])],upr,lty = 2,col='blue')
      }
      
      regreData <- getRegreData(regreData,lms,axisObject,axisName,formula)
    }
  }
  regreData[1,ncol(regreData)+1] <- urlpath
  par(opar)
  dev.off()
  if(ncol(regreData) == 1){
    names(regreData) <- 'urlpath'
  }else{
    names(regreData) <- c('row','col','R_squared','p_value','standard_error','MSE','SSE','DF','model','item','coef','st_error','t_value','oneP_value','urlpath')
  }
  return(regreData)
}

#将回归信息存放到数据框中
getRegreData <- function(df,lms,axisObject,axisName,formula){
  sumvalue <- summary(lms)
  fsta <- sumvalue$fstatistic
  regreDa <- data.frame()
  regreDa[1,1] <- axisName[['x']]               #行
  regreDa[1,2] <- axisName[['y']]               #列
  if(is.null(fsta)){
    regreDa <- cbind(regreDa,as.data.frame(array(data = NA,dim = c(1,12))))
  }else{
    regreDa[1,3] <- sumvalue$r.squared              #R平方值
    regreDa[1,4] <- 1-pf(fsta[1],fsta[2],fsta[3])   #p值
    regreDa[1,5] <- sd(sumvalue$residuals)          #标准误差
    regreDa[1,6] <- anova(lms)[nrow(anova(lms)),3]  #均方误差
    regreDa[1,7] <- anova(lms)[nrow(anova(lms)),2]  #误差平方和
    regreDa[1,8] <- fsta[3]                         #残差自由度
    coefData <- sumvalue$coefficients           #回归方程各项的参数矩阵
    rowname <- rownames(coefData)                       
    coefData <- cbind(rowname,coefData)             
    equation <- paste(gsub(axisObject[['y']],axisName[['y']],as.character(formula)[2]),'=',sep = "")
    for(i in 1:length(rowname)){
      name <- rowname[i]
      chinaName <- gsub(axisObject[['x']],axisName[['x']],name)   #中文名
      number <- round(as.numeric(coefData[name,2]),8)
      #number <- as.numeric(coefData[name,2])
      if(number>=0 && i != 1){
        equation <- paste(equation,'+',sep = "")
      }
      index <- gregexpr("\\(", name)[[1]][1]
      #当name为log(x)或x时
      if(index == -1 || index == 4){
        op <- paste(number,chinaName,sep = "*")
        n <- gsub(axisObject[['x']],axisName[['x']],name)
      }else{
        if(index == 1){
          n <- "截距"
          op <- number
        }else if(index == 2){
          n <- gsub(axisObject[['x']],axisName[['x']],gsub("I\\(","",gsub("\\)","",name)))
          op <- paste(number,n,sep = "*")
        }
      }
      coefData[name,1] <- n
      equation <- paste(equation,op,sep = "")
    }
    regreDa[1,9] <- equation                        #模型
    regreDa[c(2:nrow(coefData)),] <- c(rep(NA,ncol(regreDa)))
    regreDa <- cbind(regreDa,coefData)
  }
  df <- rbind(df,regreDa)
  return(df)
}