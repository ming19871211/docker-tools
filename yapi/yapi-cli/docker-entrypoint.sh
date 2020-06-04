#!/bin/bash
set -e

exec_cmd="$@"

function exec::all_yapi_import(){
  for element in `ls $1`
  do  
    echo
    dir_or_file=$1"/"$element
    if [ -d $dir_or_file ]; then
      exec::all_yapi_import $dir_or_file
    elif [ "$element" == "yapi-import.json" ]; then
      echo "在$1目录执行数据导入"
      exec "cd $1 && $exec_cmd"
    else
      echo "$1目录包括没有yapi-import.json文件，不能执行导入操作哦。"
    fi  
  done
}
# 执行/data目录下数据导入
exec::all_yapi_import /data
