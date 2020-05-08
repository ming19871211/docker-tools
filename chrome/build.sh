#!/bin/bash
docker build -t ming19871211/chrome-python:latest .
docker build -t ming19871211/chrome-python:v81 .
# 运行测试
#docker run -d  -t --rm  --name  chrome-python ming19871211/chrome-python:latest  cat