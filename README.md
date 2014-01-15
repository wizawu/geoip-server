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

Build the code,
<pre><code>make</code></pre>

initialize(or overwrite) the data store,
<pre><code>erl -pz ebin -s geoip init careful</code></pre>

start the server,
<pre><code>./daemon.sh</code></pre>

then visit [http://127.0.0.1/api/ip/get/110.64.91.39](http://127.0.0.1/api/ip/get/110.64.91.39) or look up any other IPs.<br />

If you want to check out the server status,
<pre><code>sudo erl -sname <i>name</i> -remsh geoip@<i>host</i></code></pre>
