#!/usr/bin/env bash
set -ex

# accepted input params: app_id, init replica IP, initial replica MAC, initial replica Port.

source params.sh

# TODO need to check argument number and fail if the number is incorrect

#TODO if we get dom_id eventually, this the env var to use to discover the port
# port_id=`sudo ovs-ofctl dump-ports xenbr0 vif$4.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`

LOCAL_XEN=$6
BRIDGE=tcp:$LOCAL_XEN:6633

ovs-ofctl del-groups -OOpenFlow13 $BRIDGE group_id=$1
ovs-ofctl del-flows  -OOpenFlow13 $BRIDGE ip,nw_dst=$2
ovs-ofctl del-flows  -OOpenFlow13 $BRIDGE ip,nw_src=$2 

