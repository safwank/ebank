%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : backend.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(backend_db_list).
-version('1.0').
-behaviour(gen_server).
-include("../include/backend.hrl").
-export([start/0, start_link/0, stop/0, 
		 account/1, pin_valid/2, change_pin/3,
		 balance/2, transactions/2,
		 withdraw/3, transfer/4
		]).
-export([init/1,handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

-define(DB, db_list).
-define(ACCOUNTS,
		[{1, 100, "1234", "Henry Nystrom"},
		 {2, 200, "4321", "Francesco Cesarini"},
		 {3, 1000, "1111", "Donald Duck"},
		 {4, 5000, "1234", "Henry Nystrom"}
		]).

-record(state, {accounts}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, no_args, []).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, stop).

account(Account) -> gen_server:call(?MODULE, {account, Account}).

pin_valid(AccountNo, Input) ->
	gen_server:call(?MODULE, {is_pin_valid, AccountNo, Input}).

change_pin(User, OldPin, NewPin) ->
	gen_server:call(?MODULE, {change_pin, User, OldPin, NewPin}).

withdraw(AccountNo, Pin, Ammount) ->
	gen_server:call(?MODULE, {withdraw, AccountNo, Pin, Ammount}).

transfer(Ammount, From, To, Pin) ->
	gen_server:call(?MODULE, {transfer, From, To, Pin, Ammount}).

balance(AccountNo, Pin) ->
	gen_server:call(?MODULE, {balance, AccountNo, Pin}).

transactions(AccountNo, Pin) ->
	gen_server:call(?MODULE, {transactions, AccountNo, Pin}).

reply(To, X) -> To ! {?MODULE, reply, X}.

init(no_args) ->
	process_flag(trap_exit, true),
	Accounts =
		lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
							?DB:insert(new_account(No, Balance, Pin, Name), DB)
					end,
					?DB:empty(),
					?ACCOUNTS),
	{ok, #state{accounts = Accounts}}.

handle_call({account, Accounts}, _, State) ->
	Reply = case Accounts of
				all ->
					lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
							  ?DB:db_to_list(State#state.accounts));
				Name when list(Name) -> find_account(Name, State);
				No when integer(No) -> [find_account(No, State)]
			end,
	{reply, Reply, State};
handle_call({is_pin_valid, AccountNumber, Pin}, _, State) ->
	Account = find_account(AccountNumber, State),
	{reply, is_pin_valid(Account, Pin), State};
handle_call({new_account, [Balance, Pin, Name]}, _, State) ->
	Accounts = State#state.accounts,
	No = ?DB:db_size(Accounts),
	NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
	{reply, ok, State#state{accounts = NewAccounts}};
handle_call({balance, AccountN, Pin}, _, State) ->
	{reply, balance(AccountN, Pin, State), State};
handle_call({transactions, AccountN, Pin}, _, State) ->
	{reply, transactions(AccountN, Pin, State), State};
handle_call({withdraw, FromAccountN, Pin, Amount}, _, State) ->
	case withdraw(FromAccountN, Pin, Amount, State) of
		{ok, NewState} -> {reply, ok, NewState};
		{error, Reason} -> {reply, {error, Reason}, State}
	end;
handle_call({deposit, ToAccountN, Amount}, _, State) ->
	case deposit(ToAccountN, Amount, State) of
		{ok, NewState} -> {reply, ok, NewState};
		{error, Reason} -> {reply, {error, Reason}, State}
	end;
handle_call({transfer, FromAccountN, ToAccountN, Pin, Amount}, _, State) ->
	case transfer(FromAccountN, ToAccountN, Pin, Amount, State) of
		{ok, NewState} -> {reply, ok, NewState};
		{error, Reason} -> {reply, {error, Reason}, State}
	end;
handle_call({change_pin, User, OldPin, NewPin}, _, State) ->
	case change_pin_i(User, OldPin, NewPin, State) of
		{ok, NewState} -> {reply, ok, NewState};
		{error, Reason} -> {reply, {error, Reason}, State}
	end;
handle_call(stop, _, State) ->
	{stop, normal, State}.

handle_cast(Cast, State) -> {stop, {"Can not handle cast", Cast}, State}.

handle_info(Info, State) -> {stop, {"Can not handle info", Info}, State}.
code_change(_, State, _) ->
	{ok, State}.

terminate(shutdown, State) -> ?DB:close(State#state.accounts);
terminate(_, _) -> ok.

new_account(No, Balance, Pin, Name) ->
	#account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, State) when is_integer(AccountN) ->
	?DB:lookup(AccountN, State#state.accounts);
find_account(User, State) when is_list(User) ->
	?DB:lookup_all(#account.name, User, State#state.accounts).

withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
withdraw(AccountN, Pin, Amount, State) ->
	Account = #account{balance = OldBalance, transactions = OldTransactions} =
						  find_account(AccountN, State),
	case is_pin_valid(Account, Pin) of
		false -> {error, "PIN code not valid!"};
		true when OldBalance < Amount -> {error, "Not enough money on account!"};
		true ->
			NewBalance = OldBalance - Amount,
			NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
			AccountUpdated =
				Account#account{balance = NewBalance, transactions = NewTransactions},
			NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
			{ok, State#state{accounts = NewAccounts}}
	end.

deposit(AccountN, Amount, State) ->
	Account = #account{balance = OldBalance, transactions = OldTransactions} =
						  find_account(AccountN, State),
	NewBalance = OldBalance + Amount,
	NewTransactions = [{deposit, date(), Amount} | OldTransactions],
	AccountUpdated =
		Account#account{balance = NewBalance, transactions = NewTransactions},
	NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
	{ok, State#state{accounts = NewAccounts}}.

balance(AccountN, Pin, State) ->
	Account = find_account(AccountN, State),
	case is_pin_valid(Account, Pin) of
		true -> Account#account.balance;
		false -> {error, "PIN code not valid!"}
	end.

transactions(AccountN, Pin, State) ->
	Account = find_account(AccountN, State),
	case is_pin_valid(Account, Pin) of
		true -> Account#account.transactions;
		false -> {error, "PIN code not valid!"}
	end.

transfer(FromAccountN, ToAccountN, Pin, Amount, State) ->
	case withdraw(FromAccountN, Pin, Amount, State) of
		{ok, NewState} -> deposit(ToAccountN, Amount, NewState);
		{error, Reason} -> {error, Reason}
	end.

is_pin_valid([], _) -> false;
is_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
is_pin_valid(Account, Pin) -> Account#account.pin == Pin.

change_pin_i(User, OldPin, NewPin, State) ->
	Accounts = find_account(User, State),
	case is_pin_valid(Accounts, OldPin) of
		false -> {error, "Wrong Pin"};
		true ->
			Accounts1 =
				lists:foldl(fun(Account, Acc) ->
									?DB:update(Account#account{pin = NewPin}, Acc)
							end,
							State#state.accounts,
							Accounts),
			{ok, State#state{accounts = Accounts1}}
	end.

