#创建目录
mkdir -p /data/zookeeper/{db,dblog,logs}
mkdir -p /data/kafka/kafka-logs
#启动部署
docker-compose -f docker-compose.yml up -d
#删部署
docker-compose -f docker-compose.yml down --volumes