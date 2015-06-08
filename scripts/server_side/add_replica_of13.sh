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

# TODO if we get dom_id eventually, this the env var to use to discover the port
port_id=`ovs-ofctl dump-ports $BRIDGE vif$UUID.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`

new_group=`ovs-ofctl -OOpenFlow13 dump-groups $BRIDGE group_id=$APP_ID | grep "group_id" | \
sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
    -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
    -e "s/output:\([0-9]*\)/output:\1/g" \
    -e "s/actions=//g"`

ovs-ofctl -OOpenFlow13 mod-group $BRIDGE $new_group,bucket=weight:50,mod_nw_dst:$IP,mod_dl_dst:$MAC,output:$port_id

# group_id=1 or $1? xenbr0
eval `ovs-ofctl -OOpenFlow13 dump-groups $BRIDGE group_id=$APP_ID | grep "group_id=$APP_ID" | \
grep -o "bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst" | head -1 | \
sed -e "s/bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst/SIP=\2;SMAC=\3;/g"`

ovs-ofctl add-flow -OOpenFlow13 $BRIDGE priority=10,ip,nw_src=$IP,\
actions=mod_nw_src:$SIP,mod_dl_src:$SMAC,output:$EXT_PORT 

