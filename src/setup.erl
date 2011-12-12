%% Author: safwan
%% Created: Dec 9, 2011
%% Description: TODO: Add description to setup
-module(setup).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_common/0, start_thieves/0, start_advanced_events/0]).

%%
%% API Functions
%%
start_common() ->
	application:start(sasl),
	backend:start_link(),
	atm:start_link(atm1),
	webatm:start_link(atm1, 8081),
	atm:start_link(atm2),
	webatm:start_link(atm2, 8082).

start_thieves() ->
	start_common(),
	backend:block(1),
	alarm_handler:add_alarm_handler(thieves).
	
start_advanced_events() ->
	start_common(),
	stats:start_link(),
	stats:add_handler(stats_failed, none),
	stats:add_handler(stats_online, none),
	stats:add_handler(stats_trx, none).

%%
%% Local Functions
%%

