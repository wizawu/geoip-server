%%! -smp auto +K true

%%================================
%% Geographic IP Informatin Server
%%================================

%% TODO: erlang:decode_packet
%% TODO: inet:parse_ipv4strict_address

-module(geoip).
-author('wuhualiang@meituan.com').

-mode(compile).

-export([start/2]).

-define(DEV, _).
-ifdef(DEV).
-export([http_response/2]).
-export([special_ip/1]).
-export([multicast_ip/1]).
-endif.

-define(IPS_FILE, "ips.dump").
-define(GEO_FILE, "geo.meta").
-define(IPS_TABLE, mt_ips).
-define(GEO_TABLE, mt_geo).
-define(GEO_MANAGER, geoman).
-define(HIGH_WATER_MARK, 1000).
-define(SNAPSHOT_INTV, 86400*1000).
-define(WRITEBACK_THR, 100000).

start(Port, Threads) ->
    inets:start(),
    % ets:new(?IPS_TABLE, [set, public, named_table])
    % ets:new(?GEO_TABLE, [set, public, named_table])
    {ok, ?IPS_TABLE} = ets:file2tab(?IPS_FILE),
    {ok, ?GEO_TABLE} = ets:file2tab(?GEO_FILE),
    GeoMan = spawn(geoman),
    register(?GEO_MANAGER, GeoMan),
    lists:map(fun worker_init/1, [0, Threads-1]),
    {ok, LSock} = gen_tcp:listen(Port, []),
    listen(Port).

%% ======== Internal Functions ======== %%

listen(Port) ->
   ok. 

snapshot() ->
    timer:sleep(?SNAPSHOT_INTV),
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    Suffix = "." ++ integer_to_list(Y * 10000 + M * 100 + D) ++
             "-" ++ integer_to_list(H * 10000 + Mi * 100 + S),
    ets:tab2file(?IPS_TABLE, ?IPS_FILE ++ Suffix),
    ets:tab2file(?GEO_TABLE, ?GEO_FILE ++ Suffix),
    snapshot().

worker_init(X) when is_integer(X) ->
    Tid = ets:new(taskqueue, [set, public]),
    put("worker" ++ integer_to_list(X), Tid),
    put("latest_task" ++ integer_to_list(X), 0),
    spawn(?MODULE, worker, [Tid, 1]).

worker(Tid, Todo) ->
    case ets:lookup(Tid, Todo) of
        [] ->
            timer:sleep(1),
            worker(Tid, Todo);
        [{Sock, IP}] ->
            {Json, Code} = lookup(IP),
            gen_tcp:send(Sock, http_response(Json, Code)),
            gen_tcp:close(Sock),
            worker(Tid, Todo + 1)
    end.

lookup(IP) when is_list(IP) ->
    case inet:parse_ipv4strict_address(IP) of
        {ok, {A, B, C, D}} ->
            if
                A == 0;
                A == 10;
                A == 100, (B band 2#11000000) == 64;
                A == 127;
                [A, B] == [169, 254];
                A == 172, (B band 2#11110000) == 16;
                [A, B, C] == [192, 0, 0];
                [A, B, C] == [192, 0, 2];
                [A, B, C] == [192, 88, 99];
                [A, B] == [192, 168];
                A == 198, (B band 2#11111110) == 18;
                [A, B, C] == [198, 51, 100];
                [A, B, C] == [203, 0, 113];
                (A band 2#11110000) == 240;
                [A, B, C, D] == [255, 255, 255, 255] ->
                    special_ip(IP);
                (A band 2#11110000) == 224 ->
                    multicast_ip(IP);
                true ->
                    lookup_ets(IP)
            end;
        _ -> invalid_ip()
    end.

lookup_ets(IP) ->
    ok.

special_ip(IP) ->
    Json = "{\"province\":\"https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry.xhtml\"," ++
           "\"city\":\"\"," ++ 
           "\"ip\":\"" ++ IP ++ "\"," ++
           "\"isp\":\"\"," ++
           "\"county\":\"\"," ++
           "\"country\":\"IANA Special-Purpose Address\"}",
    {200, Json}.

multicast_ip(IP) ->
    Json = "{\"province\":\"https://www.iana.org/assignments/multicast-addresses/multicast-addresses.xhtml\"," ++
           "\"city\":\"\"," ++ 
           "\"ip\":\"" ++ IP ++ "\"," ++
           "\"isp\":\"\"," ++
           "\"county\":\"\"," ++
           "\"country\":\"IANA Multicast Address\"}",
    {200, Json}.

invalid_ip() ->
    {400, "{\"message\":\"invalid ip.\"}"}.

months() ->
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"].

weekdays() ->
    ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].

http_response(Code, Json) ->
    Status = case Code of
        200 -> "HTTP/1.1 200 OK\r\nServer: Erlang\r\n";
        400 -> "HTTP/1.1 BAD REQUEST\r\nServer: Erlang\r\n"
    end,
    {{Year, Month, Mday}, {Hour, Min, Sec}} = calendar:universal_time(),
    Wday = calendar:day_of_the_week({Year, Month, Mday}),
    Args = [lists:nth(Wday, weekdays()), Mday, lists:nth(Month, months()),
            Year, Hour, Min, Sec],
    Date = io_lib:format("Date: ~s, ~b ~s ~b ~b:~b:~b GMT\r\n", Args),
    lists:concat([Status, Date,
                  "Content-Type: application/json\r\n",
                  "Transfer-Encoding: chunked\r\n",
                  "Connection: close\r\n\r\n",
                  Json]).


save(IP, ISP, Country, Province, City, County) ->
    {ok, {A, B, C, D}} = inet:parse_ipv4strict_address(IP),
    Key = (A bsl 16) + (B bsl 8) + C,
    ok.

taobao_api(IP) ->
    URL = "http://ip.taobao.com/service/getIpInfo.php?ip=" ++ IP,
    case httpc:request(get, {URL, []}, [], [{full_result, false}]) of
        {ok, {200, Json}} ->
            Decoded = try
                mochijson2:decode(Json)
            catch
                error: _Reason -> error
            end,
            case Decoded of
                [{<<"code">>, 0}, {<<"data">>, Info}] ->
                    {_, ISP} = lists:keyfind(<<"isp">>, 1, Info),
                    {_, Country} = lists:keyfind(<<"country">>, 1, Info),
                    {_, Province} = lists:keyfind(<<"region">>, 1, Info),
                    {_, City} = lists:keyfind(<<"city">>, 1, Info),
                    {_, County} = lists:keyfind(<<"county">>, 1, Info),
                    save(IP, ISP, Country, Province, City, County),
                    ok;
                _ -> error
            end;
        _ -> error
    end.

geoman() ->
    ok.
