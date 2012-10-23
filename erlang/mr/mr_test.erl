-include_lib("eunit/include/eunit.hrl").
-module(mr_test).
-compile(export_all).

init_stop() ->
    {ok, CPid} = mr:start(3),
    mr:stop(CPid),
    CPid.

init_stop_test() ->
    Pid=init_stop(),
    ?assert(is_pid(Pid) and not is_process_alive(Pid)).

sum() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    mr:stop(MR),
    Sum.

sum_test() -> ?assert(sum() =:= 55).

fac(N) ->
    {ok,MR} = mr:start(3),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,N)),
    mr:stop(MR),
    Fac.

fac_test() ->
    ?assert(fac(6) =:= 720).

fac_sum() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    mr:stop(MR),
    {Sum,Fac}.

fac_sum_test() ->
    {Sum, Fac} = fac_sum(),
    ?assert((Sum =:= 55) and (Fac =:= 3628800)).

word_count() ->
    {ok,MR} = mr:start(3),
    {ok,Dict} = mr:job(MR,
		       fun(X) -> {X, 1} end,
		       fun({K, V}, Dict) ->
			       dict:update_counter(K, V, Dict)
		       end,
		       dict:new(),
		       ["Hello", "World", "Goodbye", "World"]),
    mr:stop(MR),
    dict:to_list(Dict).

word_count_test() ->
    Res=word_count(),
    ?assert(Res=:=[{"Hello", 1}
                  ,{"Goodbye", 1}
                  ,{"World", 2}]).
    
