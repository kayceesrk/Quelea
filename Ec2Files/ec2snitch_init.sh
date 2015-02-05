#!/bin/bash

[ -z $1 ] && echo "Arg 1 missing" && exit
SEEDS=$1
CLUSTERNAME=QueleaCluster
printf "127.0.0.1 localhost\n127.0.0.1 `hostname`\n" > /etc/hosts
MYIP=`ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'`
cp cassandra.yaml.ec2snitch_dsc_2_0_12 cassandra.yaml
perl -p -i -e "s/__NAME/$CLUSTERNAME/g" cassandra.yaml
perl -p -i -e "s/__MYIP/$MYIP/g" cassandra.yaml
perl -p -i -e "s/__SEEDS/$SEEDS/g" cassandra.yaml
mv cassandra.yaml /home/ubuntu/dsc-cassandra-2.0.12/conf

## Keyspace -- create keyspace Codeec with replication = { 'class' : 'NetworkTopologyStrategy', 'us-west-2' : 2 }
