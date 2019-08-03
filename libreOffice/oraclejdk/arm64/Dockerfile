FROM centos:7
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ADD jdk-8*.tar.gz /app/
ENV JAVA_HOME=/app/jdk1.8 JRE_HOME=/app/jdk1.8/jre CLASSPATH=.:/app/jdk1.8/lib:/app/jdk1.8/lib PATH=/app/jdk1.8/bin:$PATH
RUN mv /app/jdk1.8* /app/jdk1.8 \
    && yum -y install libreoffice-* ghostscript && yum -y groupinstall 'Fonts' \
    && yum groupinstall "Input Methods" -y \
    && yum -y install kde-l10n-Chinese \
    && yum -y reinstall glibc-common && localedef -c -f UTF-8 -i zh_CN zh_CN.utf8 \ 
    && ln -s /usr/lib64/libreoffice /usr/lib/libreoffice  \
    && rm -f /usr/bin/java && ln -s /app/jdk1.8/bin/java /usr/bin/java \
    && yum clean all 
ENV LC_ALL zh_CN.utf8