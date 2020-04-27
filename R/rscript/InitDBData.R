initDBData <- function(dbName,user,pwd,dbHost,dbPort,sql){
  if(!require("RMySQL"))
      stop("not install RMySQL")
  
  library(RMySQL)
  conn <- dbConnect(MySQL(),
                    dbname=dbName,
                    username=user,
                    password=pwd,
                    host=dbHost,
                    port=dbPort)
  
  ##linux服务器
  dbSendQuery(conn,"set names utf8")
  ##windows
  # dbSendQuery(conn,"set names gbk")
  rs <- dbSendQuery(conn,sql)
  result <-fetch(rs,n=-1)
  
  dbClearResult(rs)
  dbDisconnect(conn)
  
  return(result)
}