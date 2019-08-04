#!/bin/bash
docker build -t ming19871211/closure-compiler:arm64 .
# 运行测试
#docker run -d  -t --rm  --name  closure-compiler ming19871211/closure-compiler:arm64  cat
docker push ming19871211/closure-compiler:arm64