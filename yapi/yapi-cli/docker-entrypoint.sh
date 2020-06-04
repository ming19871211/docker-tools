#!/bin/bash
set -e

exec_cmd="$@"

function exec::all_yapi_import(){
  local count=0
  local isYapiImport="false"
  for element in `ls $1`
  do  
    count=$(( $count+1 ))
    dir_or_file=$1"/"$element
    if [ -d $dir_or_file ]; then
      exec::all_yapi_import $dir_or_file
    elif [ "$element" == "yapi-import.json" ]; then
      isYapiImport='true'
      echo "在$1目录执行数据导入"
      cd $1 && $exec_cmd
    fi  
  done
  if [[ "$count" == 0 ]]; then
    echo "$1目录为空目录，不能执行导入操作哦。"
  elif [[ "$isYapiImport" == "false" ]]; then
    echo "$1目录包括没有yapi-import.json文件，不能执行导入操作哦。"
  fi;
}
# 执行/data目录下数据导入
exec::all_yapi_import /data
