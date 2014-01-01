.PHONY: all clean

dir = ebin
beam = geoip.beam mochijson2.beam

all:
	erlc -smp +native -o $(dir) geoip.erl mochijson2.erl

clean:
	-cd $(dir) && rm $(beam)
