-module(db_list).
-compile(export_all).
-include("../include/backend.hrl").

empty() -> [].

insert(A = #account{}, []) -> [A];
insert(#account{no=N}, [#account{no=N} | _Rest]) ->
	{error, exists};
insert(A = #account{}, [Current | Rest]) ->
	[Current | insert(A, Rest)].

db_to_list(DB) -> DB.

db_size(DB) -> length(DB).

lookup(_, []) -> {error, instance};
lookup(N, [A = #account{no=N} | _Rest]) -> A;
lookup(N, [_ | Rest]) -> lookup(N, Rest).

lookup_all(_N, _Key, []) -> [];
lookup_all(N, Key, [Rec|Db]) ->
	if element(N,Rec) =:= Key -> [Rec | lookup_all(N, Key, Db)];
	   element(N,Rec) =/= Key -> lookup_all(N, Key, Db)
	end.

update(A = #account{}, []) -> [A];
update(A = #account{}, [Current|Rest]) ->
	if A#account.no =:= Current#account.no -> [A|Rest];
	   A#account.no =/= Current#account.no -> [Current|update(A, Rest)]
	end.

close(_DB) -> ok.



