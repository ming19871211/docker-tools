#path <- "/home/work/R-3.3.0/rscript/"
path <- Sys.getenv("R_SOURCE_PATH")

#加载查询脚本
source(paste(path,"Main.R",sep=""))
#加载查询脚本
source(paste(path,"NotDimensitionMeasure.R",sep=""))
#加载查询脚本
source(paste(path,"QueryService.R",sep=""))
#加载封装与自定义函数脚本
source(paste(path,"RFunction.R",sep=""))
#加载表计算脚本
source(paste(path,"TableCompute.R",sep=""))
#加载合计与小计脚本
source(paste(path,"TableTotal.R",sep=""))
#加载初始化mysql数据脚本
source(paste(path,"InitDBData.R",sep=""))
#加载散点图与回归分析脚本
source(paste(path,"DataScatterPlot.R",sep=""))
#加载自定度量公式脚本
source(paste(path,"Formula.R",sep=""))
