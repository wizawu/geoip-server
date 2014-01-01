#!/bin/sh
sudo erl -smp auto +K true -pz ./bin -detached -run geoip start 80 8
