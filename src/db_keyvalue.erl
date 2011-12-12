%% Author: safwan
%% Created: Dec 7, 2011
%% Description: TODO: Add description to db
-module(db_keyvalue).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%
new() -> [].

destroy(_Db) -> return.

write(Key, Element, Db) -> [{Key, Element} | Db].

delete(_, []) -> [];
delete(Key, [{Key,_}|T]) -> T;
delete(Key, [H|T]) -> [H|delete(Key,T)].

read(_, []) -> {error, instance};
read(Key, [{Key, Element} | _T]) -> {ok, Element}; 
read(Key, [_H|T]) -> read(Key, T).

match(Element, Db) -> match(Element, Db, []).

match(_, [], Acc) -> Acc;
match(Element, [{Key, Element} | T], Acc) -> match(Element, T, [Key|Acc]);
match(Element, [{_Key, _Element} | T], Acc) -> match(Element, T, Acc).

%%
%% Local Functions
%%

