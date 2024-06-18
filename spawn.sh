#!/bin/sh

xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8090 -a 192.168.1.30:8070" &&
sleep 0.4 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8080 -a 192.168.1.30:8090" && 
sleep 0.4 &&
xfce4-terminal -H -x rlwrap make ARGS="-j 192.168.1.30:8070 -a 192.168.1.30:8080"
