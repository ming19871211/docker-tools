#!/bin/sh
# 定义java启动命令
java_bin=`which java`;
java_opt="-Xms128m -Xmx512m";
# closure-compiler的位置
compiler=$(find /opt -type f -name "closure-compiler*");
# 启动java
$java_bin -jar $compiler "$@";