#!/bin/bash

# run the gs server

CurDir=$(cd $(dirname $0); pwd)
APP=egs
CONFIG=./configs/egs.config
SERVER=gs@localhost
COOKIE="egs.zwj187.cn"
ENV=$1
ERL=$(which erl)
PROCESS=100000000
EBIN=$CurDir/../_build/ebin

if [ "$ENV" = "dev" ]; then
  ## dev
  echo "dev"
  cd $CurDir/../ && rebar3 shell --eval "egs:go()" --name $SERVER --setcookie $COOKIE
else
  ## prod
  echo "prod"
  sh $CurDir/export.sh
  cd $CurDir/../ && $ERL --config $CONFIG -name $SERVER --setcookie $COOKIE -s egs go -connect_all false -pa $EBIN -smp enable +K true +P $PROCESS +t 40485760 +e 65536 +fnu +hms 8192 +hmbs 8192 +zdbbl 81920 +sub true -env ERL_MAX_PORTS 65536
fi