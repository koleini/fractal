#!/usr/bin/env bash
set -x

# accepted input params: app_id, replica IP, replica MAC, replica domid,
# initial Xen host, replica Xen host.

source params.sh

# TODO need to check argument number and fail if the number is incorrect

#TODO if we get dom_id eventually, this the env var to use to discover the port
LOCAL_XEN=$5
REMOTE_XEN=$6

port_id=$4
port_id=`sudo ovs-ofctl dump-ports tcp:$REMOTE_XEN:6633 vif$4.0  | \
    head -2 | tail -1 | awk '{print $2}' | tr -d \:`

# limiting traffic bradcasting for traffic towards the new replica IP
ovs-ofctl add-flow tcp:$REMOTE_XEN:6633 priority=3,ip,nw_dst=$2,actions=output:$port_id
ovs-ofctl add-flow tcp:$REMOTE_XEN:6633 priority=3,arp,nw_dst=$2,actions=output:$port_id
ovs-ofctl add-flow tcp:$REMOTE_XEN:6633 priority=3,arp,dl_dst=$3,actions=output:$port_id

ovs-ofctl add-flow tcp:$REMOTE_XEN:6633 priority=2,in_port=$port_id,actions=output:$EXT_PORT

eval `sudo ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id" | \
    grep -o "bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst" \
    | head -1 | \
    sed -e "s/bucket=weight:\([0-9]*\),actions=set_field:\([0-9\.]*\)->ip_dst,set_field:\([0-9a-f:]*\)->eth_dst/SIP=\2;SMAC=\3;/g"`

# transform the output of show groups to look like the input to a mod-group
new_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$LOCAL_XEN:6633 group_id=$1 | grep "group_id" | \
    sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
    -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
    -e "s/output:\([0-9]*\)/output:\1/g"  | sed -e 's/[[:space:]]*\$//'`

if [ "$LOCAL_XEN" == "$REMOTE_XEN" ]; then
    # in the local machine add an additional bucket in the group entry and 
    # forward traffic without any modification
    ovs-ofctl -OOpenFlow13 mod-group tcp:$LOCAL_XEN:6633 $new_group,bucket=weight:50,mod_nw_dst:$2,mod_dl_dst:$3,output:$port_id
else 
    # add a gre tunnel between the two end-points
    # TODO: is this gre key unique? Is the uniqueness required for a pair of hosts, or between all tunnel?
#    TUNNEL_NAME=gre.$1.$REMOTE_XEN.$4
    crc=`echo $1.$REMOTE_XEN.$4 | cksum | cut -d \  -f 1`
    TUNNEL_NAME=gre`printf "%x" $crc`
    ovs-vsctl --db=tcp:$REMOTE_XEN:6634 add-port $BRIDGE $TUNNEL_NAME -- \
        set interface $TUNNEL_NAME type=gre options:remote_ip=$LOCAL_XEN options:key=$port_id
    ovs-vsctl --db=tcp:$LOCAL_XEN:6634 add-port $BRIDGE $TUNNEL_NAME -- \
        set interface $TUNNEL_NAME type=gre options:remote_ip=$REMOTE_XEN options:key=$port_id

    # get gre device port number on the openflow bridge
    local_port_id=`sudo ovs-ofctl dump-ports tcp:$LOCAL_XEN:6633 $TUNNEL_NAME  \
        | grep rx | awk '{print $2}' | tr -d \:`
    remote_port_id=`sudo ovs-ofctl dump-ports tcp:$REMOTE_XEN:6633 $TUNNEL_NAME  \
        | grep rx | awk '{print $2}' | tr -d \:`

    # in the local machine add an additional bucket in the group entry and 
    # forward traffic without any modification
    ovs-ofctl add-flow  -OOpenFlow13 tcp:$REMOTE_XEN:6633 priority=10,ip,in_port=$remote_port_id,nw_dst=$SIP,actions=mod_nw_dst:$2,mod_dl_dst:$3,output:$port_id
    ovs-ofctl -OOpenFlow13 mod-group tcp:$LOCAL_XEN:6633 $new_group,bucket=weight:50,output:$local_port_id

    # drop DHCP traffic from the tunnels
    ovs-ofctl add-flow  -OOpenFlow13 tcp:$REMOTE_XEN:6633 priority=9,ip,in_port=$remote_port_id,actions=drop
    ovs-ofctl add-flow  -OOpenFlow13 tcp:$LOCAL_XEN:6633 priority=9,ip,in_port=$local_port_id,actions=drop

    # in the remote server, add a new group entry, with modifies the entry and forward traffic
    # check if an app group entry already exists
#    old_group=`ovs-ofctl -OOpenFlow13 dump-groups tcp:$REMOTE_XEN:6633 | grep "group_id=$1,"`
#    if [ "`echo $old_group | wc -c`" -gt "1"  ]; then
#        new_group=` echo $old_group | sed -e "s/set_field:\([0-9a-z:]*\)->eth_dst/mod_dl_dst:\1/g" \
#            -e "s/set_field:\([0-9\.]*\)->ip_dst/mod_nw_dst:\1/g" \
#            -e "s/output:\([0-9]*\)/output:\1/g"  | sed -e 's/[[:space:]]*\$//'`;
#        ovs-ofctl -OOpenFlow13 mod-group tcp:$LOCAL_XEN:6633 $new_group,bucket=weight:50,mod_nw_dst:$2,mod_dl_dst:$3,output:$port_id;
#    else 
#        ovs-ofctl add-group -OOpenFlow13 tcp:$REMOTE_XEN:6633 group_id=$1,type=select,bucket=weight=50,mod_nw_dst:$2,mod_dl_dst:$3,output:$port_id;
#    fi
fi

# in the remote server simply do IP spoofing
ovs-ofctl add-flow  -OOpenFlow13 tcp:$REMOTE_XEN:6633 priority=10,ip,nw_src=$2,tcp,tp_src=$SPORT,\
actions=mod_nw_src:$SIP,output:$EXT_PORT 

