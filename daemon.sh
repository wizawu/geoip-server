#!/bin/sh
sudo erl -pz ./ebin \
         -smp auto +K true \
         -sname geoip \
         -detached -run geoip start 80 16
