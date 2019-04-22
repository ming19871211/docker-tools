#!/bin/bash
docker build -t ming19871211/closure-compiler:last .
docker build -t ming19871211/closure-compiler:20190415 .
# 运行测试
#docker run -d  -t --rm  --name  closure-compiler ming19871211/closure-compiler:last  cat