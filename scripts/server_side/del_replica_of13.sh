#!/usr/bin/env bash
set -ex

# accepted input params: app_id, replica IP, replica MAC, initial replica domid,
# initial xen host, replica xen host

source params.sh

# TODO need to check argument number and fail if the number is incorrect

LOCAL_XEN=$5
REMOTE_XEN=$6

eval `sudo ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id" | \
    grep -o "bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst" \
    | head -1 | \
    sed -e "s/bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst/SIP=\2;SMAC=\3;/g"`

if [ "$LOCAL_XEN" == "$REMOTE_XEN" ]; then
    port_id=`sudo ovs-ofctl dump-ports tcp:$LOCAL_XEN:6633 vif$4.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`
    new_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id=$1," | \
        sed -e "s/bucket=weight:\([0-9]*\),actions=set_field:$2->ip_dst,set_field:$3->eth_dst,output:$port_id//g" |\
        sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
        -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
        -e "s/output:\([0-9]*\)/output:\1/g"`
else 
    crc=`echo $REMOTE_XEN | cksum | cut -d \  -f 1`
    TUNNEL_NAME=gre$1`printf "%x" $crc`$4
    local_port_id=`sudo ovs-ofctl dump-ports tcp:$LOCAL_XEN:6633 $TUNNEL_NAME  | grep rx | awk '{print $2}' | tr -d \:`
    remote_port_id=`sudo ovs-ofctl dump-ports tcp:$REMOTE_XEN:6633 $TUNNEL_NAME  \
        | grep rx | awk '{print $2}' | tr -d \:`


    new_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id" | \
        sed -e "s/bucket=weight:\([0-9]*\),actions=output:$local_port_id//g" |\
        sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
        -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
        -e "s/output:\([0-9]*\)/output:\1/g"`
    ovs-ofctl del-flows  -OOpenFlow13 tcp:$REMOTE_XEN:6633 ip,in_port=$remote_port_id,nw_dst=$SIP
    ovs-vsctl   --db=tcp:$LOCAL_XEN:6634 del-port $BRIDGE $TUNNEL_NAME 
    ovs-vsctl   --db=tcp:$REMOTE_XEN:6634 del-port $BRIDGE $TUNNEL_NAME 
fi

ovs-ofctl -OOpenFlow13 mod-group tcp:$LOCAL_XEN:6633 $new_group
ovs-ofctl del-flows -OOpenFlow13 tcp:$REMOTE_XEN:6633 ip,nw_src=$2,tcp,tp_src=$SPORT
