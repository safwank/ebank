%%% -------------------------------------------------------------------
%%% Author  : safwan
%%% Description :
%%%
%%% Created : Dec 9, 2011
%%% -------------------------------------------------------------------
-module(thieves).
-version('1.0').
-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {stream}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init(_Args) ->
	{ok, Stream} = file:open("../priv/logs/EbankLog.txt", [write]),
	{ok, #state{stream = Stream}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event({set_alarm, Alarm}, State) ->
	io:format(State#state.stream, "Set ~p~n", [Alarm]),
	{ok, State};
handle_event({clear_alarm, Alarm}, State) ->
	io:format(State#state.stream, "Clear ~p~n", [Alarm]),
	{ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	{ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(swap, _State) -> {alarm_handler, ok};
terminate(_Arg, State) -> file:close(State#state.stream).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

