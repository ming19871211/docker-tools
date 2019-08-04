#!/bin/bash
docker build -t ming19871211/maven-oraclejdk:8-arm64 .
#测试
#docker run -d --name maven-oraclejdk ming19871211/maven-oraclejdk:8-arm64 tail -f /dev/null
#上传镜像
docker push ming19871211/maven-oraclejdk:8-arm64