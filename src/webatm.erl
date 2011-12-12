%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : webatm.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(webatm).
-version('1.0').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("../include/backend.hrl").
-include_lib("kernel/include/inet.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
% Management Interface
%
-export([start/2, start_link/2, stop/1]).

%%%%%
% Webpage callbacks
%
-export([do/3]).

%%%%%
% User API
%
-export([do/2, display/1, append_line/1, append/1, high_light/1, eject/0, wait/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SERVER_ADDR, {127,0,0,1}).
-define(HTTP_SERVER, "http://127.0.0.1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MANAGEMENT INTERFACE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Port) ->
    Root = get_root(),
    Conf = [
        {port, Port},
        {server_name, atom_to_list(Name)},
        {bind_address, ?SERVER_ADDR},
        {server_root, filename:join(Root, "priv")}, % logs
        {document_root, filename:join(Root, "priv/www")}, % files
        {modules, [mod_alias, mod_auth, mod_esi, mod_actions,
                   mod_get, mod_head, mod_log, mod_trace]},
        {error_log, "logs/atm_error_log.txt"},
        {security_log, "logs/atm_security_log.txt"},
        {transfer_log, "logs/atm_transfer_log.txt"},
        {directory_index, ["atm.html"]},
        {erl_script_alias, {"/atm", [?MODULE]}}
    ],
    io:format("Visit ~s:~p to see the ATM. Javascript must be enabled.~n", [?HTTP_SERVER, Port]),
    {ok, Pid} = inets:start(httpd, Conf, stand_alone),
    make_name(Name, Port, undefined),
    {ok, Pid}.

start_link(Name, Port) ->
    {ok, Pid} = start(Name, Port),
    link(Pid),
    {ok, Pid}.

stop(Name) ->
    Pid = lookup_pid(Name),
    delete_name(Name),
    inets:stop(httpd, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do(Name, ListOfCmds) ->
    lookup_pid(Name) ! {'#do', ListOfCmds},
    ok.

display(Txt) -> {display, Txt}.

append_line(Txt) -> {append_line, Txt}.

append(Txt) -> {append, Txt}.

high_light(off) -> {highlight, off};
high_light(Section) -> {highlight, Section}.

wait(Duration) -> {wait, Duration}.

eject() -> {eject, "eject"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HTTPD CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do(SessId, Env, Input) ->
    Port = proplists:get_value(server_port, Env),
    update_pid(Port, self()),
    Name = lookup_name(Port),
    run(SessId, Input, Name).

%% Fetches cards
run(SessId, "start/"++QueryString, _Name) ->
    Accounts = backend:account(all),
    Cmds = [{account, [integer_to_list(Number),$: | Name]}
             || {Number, Name} <- Accounts],
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Cmds);

%% User picked a card
run(SessId, "event/card/"++Rest, Name) ->
    [Num, QueryString] = re:split(Rest, "\/", [{return, list}]),
    atm:card_inserted(Name, list_to_integer(Num)),
    Reply = get_cmds(),
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Reply);

%% User pressed a button
run(SessId, "event/"++Rest, Name) ->
    [Button, QueryString] = re:split(Rest, "\/", [{return, list}]),
    try list_to_integer(Button) of
        _ -> % digit button pressed
            atm:event(Name, {digit, Button})
    catch
        error:badarg -> % other button!
            case Button of
                "withdraw" -> atm:event(Name, {selection, withdraw});
                "balance" -> atm:event(Name, {selection, balance});
                "statement" -> atm:event(Name, {selection, statement});
                _ -> atm:event(Name, list_to_atom(Button))
            end
    end,
    Reply = get_cmds(),
    Callback = proplists:get_value("callback", httpd:parse_query(QueryString)),
    jsonp(SessId, Callback, Reply);


run(SessId, _Args, _Name) ->
    jsonp(SessId, "fsm.run", [{append_line, "Unexpected Query"}]).

headers(SessId, Data) ->
    Headers = [[[Key,": ", Val, "\r\n"] || {Key, Val} <- Data], "\r\n"],
    mod_esi:deliver(SessId, lists:flatten(Headers)).

body(SessId, Data) ->
    mod_esi:deliver(SessId, Data).

jsonp(SessId, Callback, Val) ->
    headers(SessId, [{"Content-Type", "text/javascript"}]),
    Vals = [["{'command':'",atom_to_list(Command), "', 'val':'", fix(Str), "'},"]
             || {Command, Str} <- Val],
    JSON = "["++string:strip(lists:flatten(Vals), right, $,)++"]",
    body(SessId, [Callback, $(, JSON, $), $;]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_root() ->
    {ok, CWD} = file:get_cwd(),
    Parts = lists:reverse(filename:split(CWD)),
    Parts1 =
	lists:dropwhile(fun(Dir) -> not lists:prefix("ebank", Dir) end,
			Parts),
    filename:join(lists:reverse(Parts1)).

get_cmds() ->
    receive
        {'#do', ListOfCmds} -> lists:flatten(ListOfCmds)
    end.

fix(N) when is_integer(N) -> integer_to_list(N);
fix(Str) ->
    re:replace(Str, "\n", "", [global, {return, list}]).

make_name(Name, Port, Pid) when is_atom(Name) ->
    try
        ets:new(?MODULE, [named_table, public, {heir, whereis(application_controller), permanent}])
    catch
        error:badarg -> ok  % table exists
    end,
    ets:insert(?MODULE, {Name, Port, Pid}),
    ok.

lookup_name(Port) when is_integer(Port) ->
    [[Name]] = ets:match(?MODULE, {'$1', Port, '_'}),
    Name.

lookup_pid(Name) when is_atom(Name) ->
    [[Pid]] = ets:match(?MODULE, {Name, '_', '$1'}),
    Pid.

update_pid(Port, Pid) when is_integer(Port) ->
    [[Name]] = ets:match(?MODULE, {'$1', Port, '_'}),
    ets:insert(?MODULE, {Name, Port, Pid}),
    ok.

delete_name(Name) when is_atom(Name) ->
    ets:delete(?MODULE, Name),
    ok.

