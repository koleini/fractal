#!/usr/bin/env bash
set -ex

# accepted input params: app_id, replica IP, replica MAC, 
# replica domid, service IP, xen host IP.

source params.sh

# TODO need to check argument number and fail if the number is incorrect

LOCAL_XEN=$6
BRIDGE=tcp:$LOCAL_XEN:6633

#TODO if we get dom_id eventually, this the env var to use to discover the port
port_id=$4
port_id=`sudo ovs-ofctl dump-ports $BRIDGE vif$4.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`

ovs-ofctl add-flow tcp:$LOCAL_XEN:6633 priority=2,in_port=$port_id,actions=output:$EXT_PORT
ovs-ofctl add-flow tcp:$LOCAL_XEN:6633 priority=3,arp,dl_dst=$3,actions=output:$port_id
ovs-ofctl add-flow tcp:$LOCAL_XEN:6633 priority=3,ip,nw_dst=$2,actions=output:$port_id
ovs-ofctl add-flow tcp:$LOCAL_XEN:6633 priority=3,arp,nw_dst=$2,actions=output:$port_id

ovs-ofctl add-group -OOpenFlow13 $BRIDGE group_id=$1,type=select,\
bucket=mod_nw_dst:$2,weight=50,mod_dl_dst:$3,output:$port_id

# ovs-ofctl add-flow -OOpenFlow13 $BRIDGE priority=1,actions=normal
ovs-ofctl add-flow  -OOpenFlow13 tcp:$LOCAL_XEN:6633 priority=10,ip,nw_dst=$5,actions=group:$1 
ovs-ofctl add-flow  -OOpenFlow13  tcp:$LOCAL_XEN:6633 priority=10,ip,nw_src=$2,tcp,tp_src=$SPORT,\
actions=mod_nw_src:$5,output:$EXT_PORT 

