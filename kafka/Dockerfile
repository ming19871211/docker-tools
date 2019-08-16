FROM openjdk:8-jre-slim
#FROM arm64v8/openjdk:8-jre-slim
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ENV KAFKA_VERSION=2.3.0 \
    SCALA_VERSION=2.12 \
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
    apt-get install -y --no-install-recommends  wget; \
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
RUN set -eux; \
    groupadd -r kafka --gid=1000; \
    useradd -r -g kafka --uid=1000 kafka; \
    chown -R kafka:kafka  "$KAFKA_HOME"; \
    chmod 777 /docker-entrypoint.sh;

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["bin/kafka-server-start.sh","config/server.properties"]
