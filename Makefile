.PHONY: all clean

dir = ebin

all: geoip.erl mochijson2.erl
	erlc -W -smp +native -o $(dir) $^

clean:
	-cd $(dir) && rm *.beam
