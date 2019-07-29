#创建目录
```
mkdir -p /data/zookeeper/{db,dblog,logs} && mkdir -p /data/kafka/kafka-logs
```

#生成部署文件
``` yaml
#创建目录
mkdir  deploy-kafka && cd  deploy-kafka

#生成docker-entrypoint.sh
cat <<\EOF  >docker-entrypoint.sh
#!/bin/bash

set -e

# Generate the config only if it doesn't exist
if [[ ! -f "$KAFKA_CONF_DIR/server.properties" ]]; then
    CONFIG="$KAFKA_CONF_DIR/server.properties"
    echo "############################# Server Basics #############################" >> "$CONFIG"
    echo "broker.id=$KAFKA_BROKER_ID" >> "$CONFIG"
    echo "############################# Socket Server Settings #############################" >> "$CONFIG"
    echo "advertised.listeners=PLAINTEXT://$KAFKA_ADVERTISED_HOST_NAME:9092" >> "$CONFIG"
    echo "num.network.threads=$KAFKA_NUM_NETWORK_THREADS" >> "$CONFIG"
    echo "num.io.threads=$KAFKA_NUM_IO_THREADS" >> "$CONFIG"
    echo "socket.send.buffer.bytes=$KAFKA_SOCKET_SEND_BUFFER_BYTES" >> "$CONFIG"
    echo "socket.receive.buffer.bytes=$KAFKA_SOCKET_RECEIVE_BUFFER_BYTES" >> "$CONFIG"
    echo "socket.request.max.bytes=$KAFKA_SOCKET_RECEIVE_BUFFER_BYTES" >> "$CONFIG"
    echo "############################# Log Basics #############################" >> "$CONFIG"
    echo "log.dirs=$KAFKA_LOG_DIRS" >> "$CONFIG"
    echo "num.partitions=$KAFKA_NUM_PARTITIONS" >> "$CONFIG"
    echo "num.recovery.threads.per.data.dir=$KAFKA_NUM_RECOVERY_THREADS_PER_DATA_DIR" >> "$CONFIG"
    echo "############################# Internal Topic Settings  #############################" >> "$CONFIG"
    echo "offsets.topic.replication.factor=$KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR" >> "$CONFIG"
    echo "transaction.state.log.replication.factor=$KAFKA_TRANSACTION_STATE_LOG_REPLICATION_FACTOR" >> "$CONFIG"
    echo "transaction.state.log.min.isr=$KAFKA_TRANSACTION_STATE_LOG_MIN_ISR" >> "$CONFIG"
    if [[  -n $KAFKA_LOG_FLUSH_INTERVAL_MESSAGES or  -n $LOG_FLUSH_INTERVAL_MS ]]
    echo "############################# Log Flush Policy #############################" >> "$CONFIG"
    fi
    if [[ -n $KAFKA_LOG_FLUSH_INTERVAL_MESSAGES ]]; then
      echo "log.flush.interval.messages=$KAFKA_LOG_FLUSH_INTERVAL_MESSAGES" >> "$CONFIG"
    fi
    if [[ -n $LOG_FLUSH_INTERVAL_MS ]]; then
      echo "log.flush.interval.ms=$LOG_FLUSH_INTERVAL_MS" >> "$CONFIG"
    fi
    echo "############################# Log Retention Policy #############################" >> "$CONFIG"
    echo "log.retention.hours=$KAFKA_LOG_RETENTION_HOURS" >> "$CONFIG"
    echo "log.segment.bytes=$KAFKA_LOG_SEGMENT_BYTES" >> "$CONFIG"
    echo "log.retention.check.interval.ms=$KAFKA_LOG_RETENTION_CHECK_INTERVAL_MS" >> "$CONFIG"
    
    echo "############################# Zookeeper #############################" >> "$CONFIG"
    echo "zookeeper.connect=$KAFKA_ZOOKEEPER_CONNECT" >> "$CONFIG"
    echo "zookeeper.connection.timeout.ms=$KAFKA_ZOOKEEPER_CONNECTION_TIMEOUT_MS" >> "$CONFIG"
    
    echo "############################# Group Coordinator Settings #############################" >> "$CONFIG"
    if [[ -z $GROUP_INITIAL_REBALANCE_DELAY_MS ]]; then
        echo "group.initial.rebalance.delay.ms=0" >> "$CONFIG"
    else
        echo "group.initial.rebalance.delay.ms=$GROUP_INITIAL_REBALANCE_DELAY_MS" >> "$CONFIG"
    fi
fi

exec "$@"
EOF
#生成Dockerfile
cat <<\EOF  >Dockerfile
FROM openjdk:8-jre-slim
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ENV KAFKA_VERSION=2.2.0 \
    SCALA_VERSION=kafka_2.12 \
    KAFKA_HOME=/kafka \
    KAFKA_CONF_DIR=/kafka/config \
    KAFKA_BROKER_ID=0 \
    KAFKA_ADVERTISED_HOST_NAME=localhost \
    KAFKA_NUM_NETWORK_THREADS=3 \
    KAFKA_NUM_IO_THREAD=8 \
    KAFKA_SOCKET_SEND_BUFFER_BYTES=102400 \
    KAFKA_SOCKET_RECEIVE_BUFFER_BYTES=102400 \
    KAFKA_SOCKET_RECEIVE_BUFFER_BYTES=104857600 \
    KAFKA_LOG_DIRS=/kafka/kafka-logs \
    KAFKA_NUM_PARTITIONS=1 \
    KAFKA_NUM_RECOVERY_THREADS_PER_DATA_DIR=1 \
    KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR=1 \
    KAFKA_TRANSACTION_STATE_LOG_REPLICATION_FACTOR=1 \
    KAFKA_TRANSACTION_STATE_LOG_MIN_ISR=1 \
    KAFKA_LOG_RETENTION_HOURS=168 \
    KAFKA_LOG_SEGMENT_BYTES=1073741824 \
    KAFKA_LOG_RETENTION_CHECK_INTERVAL_MS=300000 \
    KAFKA_ZOOKEEPER_CONNECT=zookeeper:2181 \
    KAFKA_ZOOKEEPER_CONNECTION_TIMEOUT_MS=6000 

# Install required packges
RUN set -eux; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive \
    apt-get install -y --no-install-recommends wget; \
    rm -rf /var/lib/apt/lists/*; \
    wget -q "http://mirrors.tuna.tsinghua.edu.cn/apache/kafka/$KAFKA_VERSION/kafka_$SCALA_VERSION-$KAFKA_VERSION.tgz"; \
    tar -zxf "kafka_$SCALA_VERSION-$KAFKA_VERSION.tgz"; \
    rm kafka_${SCALA_VERSION}-${KAFKA_VERSION}.tgz; \
    mv kafka_$SCALA_VERSION-$KAFKA_VERSION $KAFKA_HOME; \
    rm $KAFKA_CONF_DIR/server.properties; \
    mkdir /kafka/kafka-logs;

WORKDIR ${KAFKA_HOME}
EXPOSE 9092
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["bin/kafka-server-start.sh","config/server.properties"]

EOF
#生成docker-compose.yml
cat <<EOF  >docker-compose.yml
version: '3.1'
services:
  zookeeper:
    image: zookeeper:3.5
    restart: always
    container_name: zookeeper
    ports:
      - "2181:2181"
    environment:
      TZ: Asia/Shanghai 
    volumes:
      - /etc/localtime:/etc/localtime:ro
      - /data/zookeeper/db:/data
      - /data/zookeeper/dblog:/datalog
      - /data/zookeeper/logs:/logs
  kafka:
    build: ./
    restart: always
    container_name: kafka
    ports:
      - "9092:9092"
    environment:
      TZ: Asia/Shanghai 
      KAFKA_ADVERTISED_HOST_NAME: 192.168.14.253
      KAFKA_ZOOKEEPER_CONNECT: zookeeper:2181
    volumes:
      - /etc/localtime:/etc/localtime:ro
      - /data/kafka/kafka-logs:/kafka/kafka-logs
EOF
```

#启动部署
```
docker-compose -f docker-compose.yml up -d
```
#删部署
``` bash
docker-compose -f docker-compose.yml down --volumes
```