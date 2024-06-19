#!/bin/sh

xfce4-terminal -H -x rlwrap make ARGS="-i -a 192.168.1.30:7777" &&
sleep 0.4 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:7777 -a 192.168.1.30:8888" 

# sleep 0.4 &&
# xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8888 -a 192.168.1.30:9999"
