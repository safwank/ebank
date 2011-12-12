%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File     : atm.erl
%%% Author   : <trainers@erlang-solutions.com>
%%% Copyright: 1999-2011 Erlang Solutions Ltd.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(atm).

-export([start/1, start_link/1, stop/1, init/1,
         card_inserted/2, event/2]).

-record(state, {name, accountNo, input = [], pin}).


start(Name) -> spawn(?MODULE, init, [Name]).

start_link(Name) -> {ok, spawn_link(?MODULE, init, [Name])}.

send_event(Name, Event) -> Name ! Event.

stop(Name) -> send_event(Name, stop).

event(Name, E) -> send_event(Name, E).

card_inserted(Name, Account) -> send_event(Name, {card_inserted, Account}).

init(Name) ->
    register(Name, self()),
    idle(#state{name=Name}).

idle(State = #state{name=Name}) ->
    receive
        {card_inserted, AccountNumber} ->
            webatm:do(Name, [webatm:display("Please type your PIN code")]),
            get_pin(#state{name=Name, accountNo = AccountNumber});
        stop -> normal;
        _ -> idle(State)
    end.

get_pin(State = #state{name=Name, accountNo = AccountNo, input = Input}) ->
    receive
        clear -> clear(fun get_pin/1, State);
        cancel -> cancel(State);
        {digit, Digit} ->
            Digits = State#state.input ++ Digit,
            webatm:do(Name, [webatm:display(Digits)]),
            get_pin(State#state{input = Digits});
        enter ->
            case backend:pin_valid(AccountNo, Input) of
                true ->
                    webatm:do(Name, [webatm:display("Please make your selection")]),
                    selection(State#state{pin = Input, input = []});
                false ->
                    webatm:do(Name, [
                        webatm:display("PIN code incorrect!"),
                        webatm:append_line("Please try again.")
                    ]),
                    get_pin(State#state{input = []})
            end;
        {selection, _} -> get_pin(State);
        stop -> normal
    end.

selection(State = #state{name=Name}) ->
    receive
        clear -> clear(fun selection/1, State);
        cancel -> cancel(State);
        {selection, withdraw} ->
            webatm:do(Name, [
                webatm:high_light("withdraw"),
                webatm:display("How much would you like to withdraw?")
            ]),
            withdraw(State);
        {selection, balance} ->
            webatm:do(Name, [
                webatm:high_light("balance"),
                balance(State)
            ]),
            selection(State);
        {selection, statement} ->
            webatm:do(Name, [
                webatm:high_light("statement"),
                mini_statement(State)
            ]),
            selection(State);
        {digit, _} -> selection(State);
        enter -> selection(State);
        stop -> normal
    end.

withdraw(State = #state{name=Name, accountNo=AccNo, pin=Pin, input=Input}) ->
    receive
        clear -> clear(fun withdraw/1, State);
        cancel -> cancel(State);
        {digit, Digit} ->
            Digits = State#state.input ++ Digit,
            webatm:do(Name, [webatm:display(Digits)]),
            withdraw(State#state{input = Digits});
        enter ->
            Input1 = list_to_integer(Input),
            case backend:withdraw(AccNo, Pin, Input1) of
                ok ->
                    io:format("ok!~n"),
                    webatm:do(Name, [
                        webatm:display("Take the money and run."),
                        webatm:wait(3500),
                        webatm:high_light("off"),
                        webatm:eject()
                    ]),
					backend:eject(AccNo),
                    timer:sleep(3500);
                {error, Reason} ->
                    io:format("fail: ~p~n", [Reason]),
                    webatm:do(Name, [
                        webatm:display("Could not withdraw money!"),
                        webatm:append_line(io_lib:format("~p",[Reason])),
                        webatm:wait(3500),
                        webatm:high_light("off"),
                        webatm:eject()
                    ]),
					backend:eject(AccNo),
                    timer:sleep(3500)
            end,
            idle(#state{name=Name});
        {selection, _} -> withdraw(State);
        stop -> normal
    end.

clear(StateName, State) ->
    webatm:do(State#state.name, [webatm:display(" ")]),
    StateName(State#state{input = []}).

cancel(#state{accountNo=No, name=Name}) ->
    webatm:do(Name, [
        webatm:display("cancel: Cancel button pressed"),
        webatm:eject()
    ]),
	backend:eject(No),
    idle(#state{name=Name}).

balance(#state{accountNo = No, pin = Pin}) ->
    [webatm:display("Balance:"), webatm:append_line("-------------------------"),
     webatm:append_line(io_lib:format("£ ~p", [backend:balance(No, Pin)]))].

mini_statement(#state{accountNo = No, pin = Pin}) ->
    Trs = backend:transactions(No, Pin),
    Balance = backend:balance(No, Pin),
    Trs1 = select10(Trs, [], 9),
    Trs2 =
        lists:map(fun({Type, {Year, Month, Day}, Sum}) ->
                    Con = case Type of
                        deposit -> "";
                        withdraw -> "-"
                    end,
                    webatm:append_line(
                        io_lib:format("~p/~p/~p ~s ~p~n", [Day, Month, Year, Con, Sum])
                    )
            end,
            Trs1),
    [webatm:display("Mini Statement:"), webatm:append_line("---------------------"),
        Trs2, webatm:append_line(io_lib:format("Balance: £ ~p", [Balance]))].

select10([], Acc, _) -> Acc;
select10(_, Acc, 0) -> Acc;
select10([H | T], Acc, N) -> select10(T, [H | Acc], N - 1).
