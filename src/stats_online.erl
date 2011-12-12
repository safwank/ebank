%%% -------------------------------------------------------------------
%%% Author  : safwan
%%% Description : Keeps track of how many users are online
%%%
%%% Created : Dec 12, 2011
%%% -------------------------------------------------------------------
-module(stats_online).
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

-record(state, {num=0, users=[]}).

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
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event({login, Id, true}, S = #state{num=N, users=Users}) ->
	case lists:member(Id, Users) of
		true -> {ok, S};
		false -> {ok, S#state{num=N+1, users=[Id|Users]}}
	end;
handle_event({eject, Id}, S = #state{num=N, users=Users}) ->
	case lists:member(Id, Users) of
		true -> {ok, S#state{num=N-1, users = Users -- [Id]}};
		false -> {ok, S}
	end;
handle_event(_, State) ->
	{ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(get_stats, S = #state{num=N, users=Users}) ->
	{ok, [{count, N}, {users, Users}], S}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	{ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

