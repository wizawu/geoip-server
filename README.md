geoip-server
============

Simple, fast and stable

+ ets as backend
+ thread pool and high water mark
+ regular snapshot
+ home-made http server

### Requirements

+ Erlang R16B03 or later versions

### HOWTO

<pre><code>make
erl -pz ebin
1> geoip:init([careful]).
2> erlang:halt().
./daemon.sh</code></pre>
Then you can visit http://127.0.0.1/api/ip/get/<b>110.64.91.39</b> or any other IPs.
