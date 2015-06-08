#!/usr/bin/env bash
set -ex

# accepted input params: 
#  app_id, first vm IP, first vm MAC, first vm dom_id/uuid for xenserver 6.5/6.6.

source params.sh

# TODO need to check argument number and fail if the number is incorrect

APP_ID=$1
IP=$2
MAC=$3
UUID=$4

# extract port number when $4 is uuid (xenserver >= 6.6)
#vif=$(xenstore-ls /xapi/$UUID/hotplug | sed -n 4p | sed 's/.*vif.*=.*\(vif.*\)\"/\1/')
#port_id=$(ovs-ofctl show $BRIDGE | grep $vif | sed 's/\s*\([0-9][0-9]*\)(.*/\1/')

# TODO if we get dom_id eventually, this the env var to use to discover the port (xenserver 6.5)
port_id=`ovs-ofctl dump-ports xenbr0 vif$UUID.0 | head -2 | tail -1 | awk '{print $2}' | tr -d \:`

ovs-ofctl add-group -OOpenFlow13 $BRIDGE group_id=$APP_ID,type=select,\
bucket=mod_nw_dst:$2,weight=50,mod_dl_dst:$MAC,output:$port_id

ovs-ofctl add-flow -OOpenFlow13 $BRIDGE priority=1,actions=normal
ovs-ofctl add-flow -OOpenFlow13 $BRIDGE priority=10,ip,nw_dst=$IP,actions=group:$APP_ID 

# rewriting the same thing?
ovs-ofctl add-flow -OOpenFlow13 $BRIDGE priority=10,ip,nw_src=$IP,\
actions=mod_dl_src:$MAC,mod_nw_src:$IP,output:$EXT_PORT
