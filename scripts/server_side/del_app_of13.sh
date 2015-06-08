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

#TODO if we get dom_id eventually, this the env var to use to discover the port
# port_id=`sudo ovs-ofctl dump-ports xenbr0 vif$UUID.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`

ovs-ofctl del-groups -OOpenFlow13 $BRIDGE group_id=$APP_ID
ovs-ofctl del-flows  -OOpenFlow13 $BRIDGE ip,nw_dst=$IP
ovs-ofctl del-flows  -OOpenFlow13 $BRIDGE ip,nw_src=$IP

