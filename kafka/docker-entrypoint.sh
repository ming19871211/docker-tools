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