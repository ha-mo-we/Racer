#!/bin/bash
for file in $*
do
  echo $file
  echo "-->"
  echo $file.response
  curl -0 --crlf -d@$file http://localhost:8080 > $file.response
  cat $file.response
  echo "---------------------"
done



  
