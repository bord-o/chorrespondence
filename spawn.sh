#!/bin/sh

xfce4-terminal -H -x rlwrap make ARGS="-i -a 10.170.1.199:7777" &&
sleep 0.4 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 10.170.1.199:7777 -a 10.170.1.199:8888" &&
sleep 2 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 10.170.1.199:8888 -a 10.170.1.199:9999" &&
sleep 0.4 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 10.170.1.199:8888 -a 10.170.1.199:6666" 

# xfce4-terminal -H -x rlwrap make ARGS="-i -a 192.168.1.30:7777" &&
# sleep 0.4 &&
# xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:7777 -a 192.168.1.30:8888" &&
# sleep 2 &&
# xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8888 -a 192.168.1.30:9999" &&
# sleep 0.4 &&
# xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8888 -a 192.168.1.30:6666" 
