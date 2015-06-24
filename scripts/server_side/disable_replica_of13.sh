#!/usr/bin/env bash
set -ex

# accepted input params: app_id, replica IP, replica MAC, initial replica domid,
# initial xen host, replica xen host

source params.sh

# TODO need to check argument number and fail if the number is incorrect

LOCAL_XEN=$5
REMOTE_XEN=$6

if [ "$LOCAL_XEN" == "$REMOTE_XEN" ]; then
    port_id=`sudo ovs-ofctl dump-ports tcp:$LOCAL_XEN:6633 vif$4.0  | head -2 | tail -1 | awk '{print $2}' | tr -d \:`
    new_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id=$1," | \
        sed -e "s/bucket=weight:50,actions=set_field:$2->ip_dst,set_field:$3->eth_dst,output:$port_id/bucket=weight:0,actions=mod_nw_dst:$2,mod_dl_dst:$3,output:$port_id/g" |\
        sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
        -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
        -e "s/output:\([0-9]*\)/output:\1/g"`
else
    crc=`echo $1.$REMOTE_XEN.$4 | cksum | cut -d \  -f 1`
    TUNNEL_NAME=gre`printf "%x" $crc`
    local_port_id=`sudo ovs-ofctl dump-ports tcp:$LOCAL_XEN:6633 $TUNNEL_NAME  | \
        grep rx | awk '{print $2}' | tr -d \:`
    new_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id=$1," | \
        sed -e "s/bucket=weight:50,actions=output:$local_port_id/bucket=weight:0,output:$local_port_id/g" |\
        sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
        -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
        -e "s/output:\([0-9]*\)/output:\1/g"`
fi

ovs-ofctl -OOpenFlow13 mod-group tcp:$LOCAL_XEN:6633 $new_group
