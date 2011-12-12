%% Author: safwan
%% Created: Dec 7, 2011
%% Description: TODO: Add description to db
-module(db).

%%
%% Include files
%%
-include("../include/backend.hrl").

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%
new() -> [].

destroy(_Db) -> return.

write(#account{}, Db) -> [#account{} | Db].

delete(_, []) -> [];
delete(Key, [#account{no=Key}|T]) -> T;
delete(Key, [H|T]) -> [H|delete(Key,T)].

read(_, []) -> {error, instance};
read(Key, [Account = #account{no=Key} | _T]) -> {ok, Account}; 
read(Key, [_H|T]) -> read(Key, T).

%% match(Account, Db) -> match(Account, Db, []).
%% 
%% match(_, [], Acc) -> Acc;
%% match(Element, [{Key, Element} | T], Acc) -> match(Element, T, [Key|Acc]);
%% match(Element, [{_Key, _Element} | T], Acc) -> match(Element, T, Acc).

%%
%% Local Functions
%%

