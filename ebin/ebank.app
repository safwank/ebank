%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: ebank.app
%%% Author: <safwan.kamarrudin@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{application, ebank, [{description,"The Erlang bank"},
{vsn,"1.0"},
{modules,[atm, atm_sup, backend, db_list, ebank_sup, web, webatm, thieves]},
{registered, [atm_sup, ebank_sup]},
{mod,{ebank_app,[]}},
{env, []},
{applications,[kernel, stdlib, sasl]}]}.
