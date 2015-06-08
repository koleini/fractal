#!/bin/bash

ERR='\033[31m[ERROR]\033[0m'
WRN='\033[33m[WARNING]\033[0m'
INF='\033[34m[INFO]\033[0m'

set -ex

# for both the servers
XS=$(cat ~/IP)
PASSWORD=$(cat ~/PASSWORD)
SCRIPT=$1

XENSERVER="sshpass -p $PASSWORD ssh -oStrictHostKeyChecking=no -l root $XS"

# add flows
$XENSERVER "bash ./$SCRIPT $2 $3 $4 $5"
