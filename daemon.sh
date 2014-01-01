#!/bin/sh
sudo erl -smp auto +K true -pz ./ebin -detached -run geoip start 80 8
