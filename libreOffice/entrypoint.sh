#!bin/bash
echo "启动soffice"
/usr/lib64/libreoffice/program/soffice --headless --accept="socket,host=127.0.0.1,port=8100;urp;" --nofirststartwizard &
#echo "启动java"
/usr/bin/java ${JAVA_OPTS} -jar /usr/share/myservice/myservice.jar --spring.cloud.config.profile=${PROFILE}  \
 --eureka.instance.prefer-ip-address=false --eureka.instance.hostname=${DOMAIN_NAME}
