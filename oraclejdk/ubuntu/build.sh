#!/bin/bash
docker build -t ming19871211/oraclejdk:8 .
#测试
#docker run -d --name oraclejdk ming19871211/oraclejdk:8 tail -f /dev/null
#上传镜像
docker push ming19871211/oraclejdk:8
