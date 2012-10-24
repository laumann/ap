-include_lib("eunit/include/eunit.hrl").
-module(mr_test).
-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unit tests for MR & MXM operations    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Use mr_test:test(). to run all tests.

%% Tests for the general purpose MR skeleton

%% Check if coordinator starts and stops successfully
init_status_stop_test() ->
    {ok, Pid} = mr:start(3),
    % Process should now be running
    ?assert(is_pid(Pid) 
            and is_process_alive(Pid)),
    {ok, {Pid, NMaps}}=mr:status(Pid),
    % We can now call status, which should give us the right
    % Pid and number of mappers
    ?assert(NMaps=:=3),
    mr:stop(Pid),
    % The process should have stopped now.
    ?assert(not is_process_alive(Pid)).

%% It should not be possible to start the skeleton with less than 1 mapper
init_zero_mappers_test() ->
    ?assert(fail =:= mr:start(0)).

%% Use the MR skeleton with two simple functions to calculate a sum, check the result
sum_test() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,10)),
    % The sum of 1..10 is 55
    ?assert(Sum=:=55),
    mr:stop(MR),
    Sum.

%% Same thing for a factorial function, has to work with only one worker as well
fac_test() ->
    {ok,MR} = mr:start(1),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,6)),
    % 6! is 720
    ?assert(Fac=:=720),
    mr:stop(MR),
    Fac.

%% Make sure the skeleton behaves correctly with an empty data set
fac_empty_test() ->
    {ok,MR} = mr:start(8),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
                      % Empty data set
		      []),
    % The result should be the initial element of the reduce function
    ?assert(Fac=:=1),
    mr:stop(MR),
    Fac.


%% Make sure mapper and reducer functions are switched correctly, i.e.
%% it is possible to run sequences of different jobs with the same coordinator.
fac_sum_test() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X, Acc) -> X+Acc end,
		      0,
		      lists:seq(1,9)),
    {ok,Fac} = mr:job(MR,
		      fun(X) -> X end,
		      fun(X,Acc) -> X*Acc end,
		      1,
		      lists:seq(1,10)),
    % The results should be 45 and 3628800
    ?assert((Sum =:= 45) and (Fac =:= 3628800)),
    mr:stop(MR).


%% Do a word count using the MR skeleton
word_count_test() ->
    {ok,MR} = mr:start(3),
    {ok,Dict} = mr:job(MR,
		       fun(X) -> {X, 1} end,
		       fun({K, V}, Dict) ->
			       dict:update_counter(K, V, Dict)
		       end,
		       dict:new(),
		       ["Hello", "World", "Goodbye", "World"]),
    mr:stop(MR),
    Res=dict:to_list(Dict),
    ?assert(Res=:=[{"Hello", 1}
                  ,{"Goodbye", 1}
                  ,{"World", 2}]).

%% Tests for MXM functions

%% Test all functions on a very very small set so that we can actually be sure
%% the results are correct
mxm_miniset_test() ->
    {ok, MR} = mr:start(4),
    % Count
    {TWords, TTracks} = read_mxm:from_file("mini_dataset.txt"),
    Sum = mr_wc:count(MR,TTracks),
    ?assert(Sum=:=47),
    % Grep
    ContainingYou = mr_wc:grep(MR, "you", {TWords, TTracks}),
    ?assert((ContainingYou=:=[<<"3811449">>,<<"5325944">>]) or (ContainingYou=:=[<<"5325944">>,<<"3811449">>])),
    % Compute averages
    {TAvgDiff, TAvgWords} = mr_wc:compute_averages(MR, {TWords, TTracks}),
    ?assert(TAvgWords=:=15.666666666666666),
    ?assert(TAvgDiff=:=4.0),
    % RevInf
    TRevInd = mr_wc:reverse_index(MR, {TWords, TTracks}),
    ContYou2 = dict:fetch("you", TRevInd),
    ?assert((ContYou2=:=[<<"3811449">>,<<"5325944">>]) or (ContYou2=:=[<<"5325944">>,<<"3811449">>])),
    ?assert(dict:fetch("acabar", TRevInd)=:=[<<"1548880">>]),
    mr:stop(MR),
    ok.



    
