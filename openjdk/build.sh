#!/bin/bash
docker build -t ming19871211/skywaiking-openjdk:8-jre .
#测试
#docker run -d --name skywaiking-openjdk ming19871211/skywaiking-openjdk:8-jre  tail -f /dev/null
#上传镜像
docker push ming19871211/skywaiking-openjdk:8-jre .