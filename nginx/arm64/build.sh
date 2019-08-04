#!/bin/bash
docker build -t ming19871211/nginx-concat:1.16.0-arm64 .
docker tag ming19871211/nginx-concat:1.16.0-arm64 ming19871211/nginx-concat:1.16-arm64
docker tag ming19871211/nginx-concat:1.16.0-arm64 ming19871211/nginx-concat:arm64
docker push ming19871211/nginx-concat:1.16.0-arm64
docker push ming19871211/nginx-concat:1.16-arm64
docker push ming19871211/nginx-concat:arm64