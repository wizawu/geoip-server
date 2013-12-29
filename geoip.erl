%%================================
%% Geographic IP Informatin Server
%%================================

%% TODO: erlang:decode_packet
%% TODO: inet:parse_ipv4strict_address

-module(geoip).
-author('wuhualiang@meituan.com').

-export([start/2]).

-define(DEV, _).
-ifdef(DEV).
-export([http_response/2]).
-endif.

-define(IPS_FILE, "ips.dump").
-define(GEO_FILE, "geo.meta").
-define(IPS_TABLE, mt_ips).
-define(GEO_TABLE, mt_geo).
-define(GEO_MANAGER, geoman).
-define(HIGH_WATER_MARK, 1000).

start(Port, Threads) ->
    inets:start(),
    % ets:new(?IPS_TABLE, [set, public, named_table])
    % ets:new(?GEO_TABLE, [set, public, named_table])
    {ok, ?IPS_TABLE} = ets:file2tab(?IPS_FILE),
    {ok, ?GEO_TABLE} = ets:file2tab(?GEO_FILE),
    lists:map(fun worker_init/1, [0, Threads-1]),
    ok.

%% ======== Internal Functions ======== %%

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
        {ok, {A, B, C, D}} -> if
            A == 0; A == 10; A == 100, B bsr 
            ok;
        _ -> invalid_ip()
    end.

invalid_ip() ->
    {"{\"message\":\"invalid ip.\"}", 400}.

months() ->
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"].

weekdays() ->
    ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].

http_response(Json, Code) ->
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
