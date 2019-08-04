#!/bin/bash
docker build -t ming19871211/oraclejdk:8 .
#测试
#docker run -d --name oraclejdk ming19871211/oraclejdk:8 tail -f /dev/null
#上传镜像
docker push ming19871211/oraclejdk:8


# arm64架构打包（下载oracle的arm64的jdk8包）
# docker build -t ming19871211/oraclejdk:8-arm64 .
# docker push ming19871211/oraclejdk:8-arm64