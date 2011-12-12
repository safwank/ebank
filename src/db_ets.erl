%% Author: safwan
%% Created: Dec 8, 2011
%% Description: TODO: Add description to db_ets
-module(db_ets).

%%
%% Include files
%%
-include("../include/backend.hrl").

%%
%% Exported Functions
%%
-export([empty/0,
		 insert/2, db_to_list/1, db_size/1,
		 list_to_db/2,
		 lookup/2, lookup_all/3,
		 update/2,
		 close/1]).

-record(db, {table}).

%%
%% API Functions
%%
empty() -> #db{table = ets:new(backend, [{keypos, 2}])}.

insert(Account, DB = #db{table = Tid}) ->
	ets:insert(Tid, Account),
	DB.

db_to_list(#db{table = Tid}) -> ets:tab2list(Tid).

list_to_db(List, DB = #db{table = Tid}) ->
	ets:insert(Tid, List),
	DB.

db_size(#db{table = Tid}) -> ets:info(Tid, size).

lookup(No, #db{table = Tid}) -> hd(ets:lookup(Tid, No)).

lookup_all(Field, X, #db{table = Tid}) ->
	[Account || Account <- ets:tab2list(Tid), element(Field, Account) == X].

update(Account, DB) -> insert(Account, DB).

close(#db{table = Tid}) -> ets:delete(Tid), ok.

%%
%% Local Functions
%%

