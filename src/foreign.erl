%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : foreign.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(foreign).
-behaviour(gen_fsm).
-include("../include/backend.hrl").
-record(state, {pid}).

-export([start/1, start_link/1]).
-export([init/1, auth/2, info/2, op/2, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).


start(Pid) -> gen_fsm:start(?MODULE, Pid, []).

start_link(Pid) -> gen_fsm:start_link(?MODULE, Pid, []).

init(Pid) ->
    gen_fsm:send_event(self(), start),
    {ok, auth, #state{pid=Pid}}.

auth(start, State) -> auth(denied, State); % redirect, same code.
auth(denied, State) ->
    case gen_fsm:sync_send_event(State#state.pid, rand_auth()) of
        ok ->
            gen_fsm:send_event(self(), 'try'),
            {next_state, info, State};
        denied ->
            gen_fsm:send_event(self(), denied),
            {next_state, auth, State}
end.

info('try', State) ->
    case gen_fsm:sync_send_event(State#state.pid, {account, id()}) of
        ok ->
            gen_fsm:send_event(self(), do),
            {next_state, op, State};
        error ->
            gen_fsm:send_event(self(), 'try'),
            {next_state, info, State}
    end.

%% We never care about what we receive back, although we should in a real
%% use case.
op(_Event, State) ->
    case action() of
        withdraw ->
            gen_fsm:sync_send_event(State#state.pid, {withdraw, sum()}),
            gen_fsm:send_event(self(), do),
            {next_state, op, State};
        deposit ->
            gen_fsm:sync_send_event(State#state.pid, {deposit, sum()}),
            gen_fsm:send_event(self(), do),
            {next_state, op, State};
        change_account ->
            gen_fsm:sync_send_event(State#state.pid, change_account),
            gen_fsm:send_event(self(), 'try'),
            {next_state, info, State};
        off ->
            gen_fsm:sync_send_all_state_event(State#state.pid, off),
            {stop, normal, State}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_Vsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

action() ->
    R = random:uniform(16),
    if R =< 5   -> withdraw;
       R =< 10  -> deposit;
       R =< 15  -> change_account;
       R =:= 16 -> off
    end.

sum() -> random:uniform(500).

rand_auth() ->
    Fake = [{a,12},{b,32423},{c,353123}],
    Banks = ?VALID_BANKS ++ Fake,
    L = length(Banks),
    lists:nth(random:uniform(L), Banks).

id() -> random:uniform(10).
